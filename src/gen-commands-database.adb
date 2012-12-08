-----------------------------------------------------------------------
--  gen-commands-database -- Database creation from application model
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with GNAT.Command_Line;
with GNAT.Expect;
with GNAT.OS_Lib;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Exceptions;

with Util.Strings;
with Util.Files;
with Util.Log.Loggers;

with ADO.Drivers.Connections;
with ADO.Sessions.Factory;
with ADO.Statements;
with ADO.Queries;
with ADO.Parameters;

with System;

with Gen.Database.Model;
package body Gen.Commands.Database is

   use GNAT.Command_Line;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Commands.Database");

   --  Check if the database with the given name exists.
   function Has_Database (DB   : in ADO.Sessions.Session'Class;
                          Name : in String) return Boolean;

   --  Check if the database with the given name has some tables.
   function Has_Tables (DB   : in ADO.Sessions.Session'Class;
                        Name : in String) return Boolean;

   --  Expect filter to print the command output/error
   procedure Command_Output (Descriptor : in GNAT.Expect.Process_Descriptor'Class;
                             Data       : in String;
                             Closure    : in System.Address);

   --  Execute the external command <b>Name</b> with the arguments in <b>Args</b>
   --  and send the content of the file <b>Input</b> to that command.
   procedure Execute_Command (Name  : in String;
                              Args  : in GNAT.OS_Lib.Argument_List;
                              Input : in String);

   --  Create the MySQL tables in the database.  The tables are created by launching
   --  the external command 'mysql' and using the create-xxx-mysql.sql generated scripts.
   procedure Create_Mysql_Tables (Name   : in String;
                                  Model  : in String;
                                  Config : in ADO.Drivers.Connections.Configuration;
                                  Generator : in out Gen.Generator.Handler);

   --  Create the database identified by the given name.
   procedure Create_Database (DB   : in ADO.Sessions.Master_Session;
                              Name : in String);

   --  Create the user and grant him access to the database.
   procedure Create_User_Grant (DB       : in ADO.Sessions.Master_Session;
                                Name     : in String;
                                User     : in String;
                                Password : in String);

   --  ------------------------------
   --  Check if the database with the given name exists.
   --  ------------------------------
   function Has_Database (DB   : in ADO.Sessions.Session'Class;
                          Name : in String) return Boolean is
      Query : ADO.Queries.Context;
      Stmt  : ADO.Statements.Query_Statement;
   begin
      Query.Set_Query (Gen.Database.Model.Query_Database_List);
      Stmt := DB.Create_Statement (Query);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         declare
            D : constant String := Stmt.Get_String (0);
         begin
            if Name = D then
               return True;
            end if;
         end;
         Stmt.Next;
      end loop;
      return False;
   end Has_Database;

   --  ------------------------------
   --  Check if the database with the given name has some tables.
   --  ------------------------------
   function Has_Tables (DB   : in ADO.Sessions.Session'Class;
                        Name : in String) return Boolean is
      Query : ADO.Queries.Context;
      Stmt  : ADO.Statements.Query_Statement;
   begin
      Query.Set_Query (Gen.Database.Model.Query_Table_List);
      Stmt := DB.Create_Statement (Query);
      Stmt.Bind_Param ("name", ADO.Parameters.Token (Name));
      Stmt.Execute;
      return Stmt.Has_Elements;
   end Has_Tables;

   --  ------------------------------
   --  Create the database identified by the given name.
   --  ------------------------------
   procedure Create_Database (DB   : in ADO.Sessions.Master_Session;
                              Name : in String) is
      use Ada.Strings.Unbounded;
      Query : ADO.Queries.Context;
      Stmt  : ADO.Statements.Query_Statement;
   begin
      Log.Info ("Creating database '{0}'", Name);

      Query.Set_Query (Gen.Database.Model.Query_Create_Database);
      Stmt := DB.Create_Statement (Query);
      Stmt.Bind_Param ("name", ADO.Parameters.Token (Name));
      Stmt.Execute;

   end Create_Database;

   --  ------------------------------
   --  Create the user and grant him access to the database.
   --  ------------------------------
   procedure Create_User_Grant (DB       : in ADO.Sessions.Master_Session;
                                Name     : in String;
                                User     : in String;
                                Password : in String) is
      use Ada.Strings.Unbounded;

      Query : ADO.Queries.Context;
      Stmt  : ADO.Statements.Query_Statement;
   begin
      Log.Info ("Granting access for user '{0}' to database '{1}'", User, Name);

      if Password'Length > 0 then
         Query.Set_Query (Gen.Database.Model.Query_Create_User_With_Password);
      else
         Query.Set_Query (Gen.Database.Model.Query_Create_User_No_Password);
      end if;

      Stmt := DB.Create_Statement (Query);
      Stmt.Bind_Param ("name", ADO.Parameters.Token (Name));
      Stmt.Bind_Param ("user", ADO.Parameters.Token (User));
      if Password'Length > 0 then
         Stmt.Bind_Param ("password", Password);
      end if;
      Stmt.Execute;

      Query.Set_Query (Gen.Database.Model.Query_Flush_Privileges);
      Stmt := DB.Create_Statement (Query);
      Stmt.Execute;
   end Create_User_Grant;

   --  ------------------------------
   --  Expect filter to print the command output/error
   --  ------------------------------
   procedure Command_Output (Descriptor : in GNAT.Expect.Process_Descriptor'Class;
                             Data       : in String;
                             Closure    : in System.Address) is
      pragma Unreferenced (Descriptor, Closure);
   begin
      Log.Error ("{0}", Data);
   end Command_Output;

   --  ------------------------------
   --  Execute the external command <b>Name</b> with the arguments in <b>Args</b>
   --  and send the content of the file <b>Input</b> to that command.
   --  ------------------------------
   procedure Execute_Command (Name  : in String;
                              Args  : in GNAT.OS_Lib.Argument_List;
                              Input : in String) is
      Proc    : GNAT.Expect.Process_Descriptor;
      Status  : Integer;
      Func    : constant GNAT.Expect.Filter_Function := Command_Output'Access;
      Result  : GNAT.Expect.Expect_Match;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Files.Read_File (Path => Input, Into => Content);
      GNAT.Expect.Non_Blocking_Spawn (Descriptor  => Proc,
                                      Command     => Name,
                                      Args        => Args,
                                      Buffer_Size => 4096,
                                      Err_To_Out  => True);
      GNAT.Expect.Add_Filter (Descriptor => Proc,
                              Filter     => Func,
                              Filter_On  => GNAT.Expect.Output);
      GNAT.Expect.Send (Descriptor   => Proc,
                        Str          => Ada.Strings.Unbounded.To_String (Content),
                        Add_LF       => False,
                        Empty_Buffer => False);
      GNAT.Expect.Expect (Proc, Result, ".*");
      GNAT.Expect.Close (Descriptor => Proc,
                         Status     => Status);
      if Status = 0 then
         Log.Info ("Database schema created successfully.");
      else
         Log.Error ("Error while creating the database schema.");
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot read {0}", Input);
   end Execute_Command;

   --  ------------------------------
   --  Create the MySQL tables in the database.  The tables are created by launching
   --  the external command 'mysql' and using the create-xxx-mysql.sql generated scripts.
   --  ------------------------------
   procedure Create_Mysql_Tables (Name      : in String;
                                  Model     : in String;
                                  Config    : in ADO.Drivers.Connections.Configuration;
                                  Generator : in out Gen.Generator.Handler) is
      Database : constant String := Config.Get_Database;
      Username : constant String := Config.Get_Property ("user");
      Password : constant String := Config.Get_Property ("password");
      Dir      : constant String := Util.Files.Compose (Model, "mysql");
      File     : constant String := Util.Files.Compose (Dir, "create-" & Name & "-mysql.sql");
   begin
      Log.Info ("Creating database tables using schema '{0}'", File);

      if not Ada.Directories.Exists (File) then
         Generator.Error ("SQL file '{0}' does not exist.", File);
         Generator.Error ("Please, run the following command: dynamo generate db");
         return;
      end if;

      if Password'Length > 0 then
         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 3);
         begin
            Args (1) := new String '("--user=" & Username);
            Args (2) := new String '("--password=" & Password);
            Args (3) := new String '(Database);
            Execute_Command ("mysql", Args, File);
         end;
      else
         declare
            Args : GNAT.OS_Lib.Argument_List (1 .. 2);
         begin
            Args (1) := new String '("--user=" & Username);
            Args (2) := new String '(Database);
            Execute_Command ("mysql", Args, File);
         end;
      end if;
   end Create_Mysql_Tables;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);

      use Ada.Strings.Unbounded;

      procedure Create_Database (Model    : in String;
                                 Database : in String;
                                 Username : in String;
                                 Password : in String);

      --  ------------------------------
      --  Create the database, the user and the tables.
      --  ------------------------------
      procedure Create_Database (Model    : in String;
                                 Database : in String;
                                 Username : in String;
                                 Password : in String) is
         Factory         : ADO.Sessions.Factory.Session_Factory;
         Config          : ADO.Drivers.Connections.Configuration;
         Root_Connection : Unbounded_String;
         Pos             : Natural;
      begin
         Config.Set_Connection (Database);
         if Config.Get_Property ("user") = "" then
            Generator.Error ("Invalid database connection: missing user property");
            return;
         end if;
         if Config.Get_Database = "" then
            Generator.Error ("Invalid database connection: no database name specified");
            return;
         end if;

         --  Build a connection string to create the database.
         Pos := Util.Strings.Index (Database, ':');
         Append (Root_Connection, Database (Database'First .. Pos));
         Append (Root_Connection, "//");
         Append (Root_Connection, Config.Get_Server);
         if Config.Get_Port > 0 then
            Append (Root_Connection, ':');
            Append (Root_Connection, Util.Strings.Image (Config.Get_Port));
         end if;
         Append (Root_Connection, "/?user=");
         Append (Root_Connection, Username);
         if Password'Length > 0 then
            Append (Root_Connection, "&password=");
            Append (Root_Connection, Password);
         end if;

         Log.Info ("Connecting to {0} for database setup", Root_Connection);

         --  Initialize the session factory to connect to the
         --  database defined by root connection (which should allow the database creation).
         Factory.Create (To_String (Root_Connection));

         declare
            Name : constant String := Generator.Get_Project_Name;
            DB   : constant ADO.Sessions.Master_Session := Factory.Get_Master_Session;
         begin
            --  Create the database only if it does not already exists.
            if not Has_Database (DB, Config.Get_Database) then
               Create_Database (DB, Config.Get_Database);
            end if;

            --  If some tables exist, don't try to create tables again.
            --  We could improve by reading the current database schema, comparing with our
            --  schema and create what is missing (new tables, new columns).
            if Has_Tables (DB, Config.Get_Database) then
               Generator.Error ("The database {0} exists", Config.Get_Database);
            else
               --  Create the user grant.  On MySQL, it is safe to do this several times.
               Create_User_Grant (DB, Config.Get_Database,
                                  Config.Get_Property ("user"),
                                  Config.Get_Property ("password"));

               --  And now create the tables by using the SQL script generated by Dyanmo.
               Create_Mysql_Tables (Name, Model, Config, Generator);
            end if;

            --  Remember the database connection string.
            Generator.Set_Project_Property ("database", Database);
            Generator.Save_Project;

         exception
            when E : others =>
               Generator.Error (Ada.Exceptions.Exception_Message (E));
         end;
      end Create_Database;

      Model  : constant String := Get_Argument;
      Arg1   : constant String := Get_Argument;
      Arg2   : constant String := Get_Argument;
      Arg3   : constant String := Get_Argument;
   begin
      Generator.Read_Project ("dynamo.xml");

      --  Initialize the database drivers.
      ADO.Drivers.Initialize (Generator.Get_Properties);

      --  Check if a database is specified in the command line and use it.
      if Ada.Strings.Fixed.Index (Arg1, "://") > 0 or Arg3'Length > 0 then
         Create_Database (Model, Arg1, Arg2, Arg3);
      else
         declare
            Database : constant String := Generator.Get_Project_Property ("database");
         begin
            --  Otherwise, get the database identification from dynamo.xml configuration.
            if Ada.Strings.Fixed.Index (Database, "://") = 0 then
               Generator.Error ("No database specified.");
               return;
            end if;
            Create_Database (Model, Database, Arg1, Arg2);
         end;
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("create-database: Creates the database");
      Put_Line ("Usage: create-database MODEL [CONNECTION] ADMIN-USER [ADMIN-PASSWORD]");
      New_Line;
      Put_Line ("  Create the database specified by the connection string.");
      Put_Line ("  The connection string has the form:");
      Put_Line ("     driver://host[:port]/database");
      New_Line;
      Put_Line ("  The database must not exist.  The user specified in the connection string");
      Put_Line ("  is granted the access to the new database.");
   end Help;

end Gen.Commands.Database;
