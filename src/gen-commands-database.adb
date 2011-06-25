-----------------------------------------------------------------------
--  gen-commands-database -- Database creation from application model
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Text_IO;

with Util.Strings;

with ADO.Drivers;
with ADO.Sessions.Factory;
with ADO.Statements;
with ADO.Queries;

with Gen.Database.Model;
package body Gen.Commands.Database is

   use GNAT.Command_Line;

   --  Check if the database with the given name exists.
   function Has_Database (DB   : in ADO.Sessions.Session'Class;
                          Name : in String) return Boolean;

   procedure Create_Database (DB   : in ADO.Sessions.Master_Session;
                              Name : in String;
                              User : in String;
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

   procedure Create_Database (DB   : in ADO.Sessions.Master_Session;
                              Name : in String;
                              User : in String;
                              Password : in String) is
      use Ada.Strings.Unbounded;
      Stmt  : ADO.Statements.Query_Statement := DB.Create_Statement ("create database " & Name);
      Grant : Unbounded_String;
   begin
      Stmt.Execute;

      Append (Grant, "grant select, insert, update, delete, "
              & "create temporary tables, execute, show view on `");
      Append (Grant, Name);
      Append (Grant, "`.* to '");
      Append (Grant, User);
      Append (Grant, "'@'localhost'");
      if Password'Length > 0 then
         Append (Grant, " identified by ");
         Append (Grant, Password);
      end if;
      Stmt := DB.Create_Statement (To_String (Grant));
      Stmt.Execute;
   end Create_Database;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);

      use Ada.Strings.Unbounded;

      Factory    : ADO.Sessions.Factory.Session_Factory;

      Database   : constant String := Get_Argument;
      Username   : constant String := Get_Argument;
      Password   : constant String := Get_Argument;

      Config          : ADO.Drivers.Configuration;
      Root_Connection : Unbounded_String;
      Pos             : Natural;
   begin
      Generator.Read_Project ("dynamo.xml");

      --  Initialize the database drivers.
      ADO.Drivers.Initialize (Generator.Get_Properties);

      Config.Set_Connection (Database);

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

      --  Initialize the session factory to connect to the
      --  database defined by root connection (which should allow the database creation).
      Factory.Create (To_String (Root_Connection));

      declare
         DB   : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
      begin
         DB.Begin_Transaction;

         if Has_Database (DB, Config.Get_Database) then
            Generator.Error ("The database {0} exists", Config.Get_Database);
            return;
         end if;

         Create_Database (DB, Config.Get_Database,
                          Config.Get_Property ("user"),
                          Config.Get_Property ("password"));

         --  Remember the database connection string.
         Generator.Set_Project_Property ("database", Database);
         Generator.Save_Project;
      end;
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
      Put_Line ("Usage: create-database CONNECTION ADMIN-USER [ADMIN-PASSWORD]");
      New_Line;
      Put_Line ("  Create the database specified by the connection string.");
      Put_Line ("  The connection string has the form:");
      Put_Line ("     driver://host[:port]/database");
      New_Line;
      Put_Line ("  The database must not exist.  The user specified in the connection string");
      Put_Line ("  is granted the access to the new database.");
   end Help;

end Gen.Commands.Database;
