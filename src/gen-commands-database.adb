-----------------------------------------------------------------------
--  gen-commands-database -- Database creation from application model
--  Copyright (C) 2011, 2012, 2016, 2017, 2018, 2019, 2022 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Exceptions;

with Util.Strings;
with Util.Files;
with Util.Log.Loggers;
with Util.Strings.Vectors;

with ADO.Drivers;
with ADO.Sessions.Sources;
with ADO.Schemas.Databases;

package body Gen.Commands.Database is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Commands.Database");

   function Get_Schema_Path (Model_Dir : in String;
                             Model     : in String;
                             Config    : in ADO.Sessions.Sources.Data_Source) return String;

   function Get_Schema_Path (Model_Dir : in String;
                             Model     : in String;
                             Config    : in ADO.Sessions.Sources.Data_Source) return String is
      Driver : constant String := Config.Get_Driver;
      Dir    : constant String := Util.Files.Compose (Model_Dir, Driver);
   begin
      return Util.Files.Compose (Dir, "create-" & Model & "-" & Driver & ".sql");
   end Get_Schema_Path;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name);

      procedure Create_Database (Model_Dir : in String;
                                 Database  : in String;
                                 Username  : in String;
                                 Password  : in String);

      --  ------------------------------
      --  Create the database, the user and the tables.
      --  ------------------------------
      procedure Create_Database (Model_Dir : in String;
                                 Database  : in String;
                                 Username  : in String;
                                 Password  : in String) is
         Admin    : ADO.Sessions.Sources.Data_Source;
         Config   : ADO.Sessions.Sources.Data_Source;
         Messages : Util.Strings.Vectors.Vector;
      begin
         Config.Set_Connection (Database);
         Admin := Config;
         if Config.Get_Database = "" then
            Generator.Error ("Invalid database connection: no database name specified");
            return;
         end if;

         declare
            Name : constant String := Generator.Get_Project_Name;
            Path : constant String := Get_Schema_Path (Model_Dir, Name, Config);
         begin
            Log.Info ("Creating database tables using schema '{0}'", Path);

            if not Ada.Directories.Exists (Path) then
               Generator.Error ("SQL file '{0}' does not exist.", Path);
               Generator.Error ("Please, run the following command: dynamo generate db");
               return;
            end if;

            if Config.Get_Driver in "mysql" | "postgresql" then
               if Config.Get_Property ("user") = "" then
                  Generator.Error ("Invalid database connection: missing user property");
                  return;
               end if;
               Admin.Set_Property ("user", Username);
               Admin.Set_Property ("password", Password);

            elsif Config.Get_Driver /= "sqlite" then
               Generator.Error ("Database driver {0} is not supported.", Config.Get_Driver);
               return;
            end if;
            Admin.Set_Database ("");
            ADO.Schemas.Databases.Create_Database (Admin, Config, Path, Messages);

            --  Report the messages
            for Msg of Messages loop
               Log.Error ("{0}", Msg);
            end loop;
         end;
         --  Remember the database connection string.
         Generator.Set_Project_Property ("database", Database);
         Generator.Save_Project;

      exception
         when E : others =>
            Generator.Error (Ada.Exceptions.Exception_Message (E));
      end Create_Database;

      Model  : constant String := (if Args.Get_Count > 0 then Args.Get_Argument (1) else "");
      Arg1   : constant String := (if Args.Get_Count > 1 then Args.Get_Argument (2) else "");
      Arg2   : constant String := (if Args.Get_Count > 2 then Args.Get_Argument (3) else "");
      Arg3   : constant String := (if Args.Get_Count > 3 then Args.Get_Argument (4) else "");
   begin
      Generator.Read_Project ("dynamo.xml");

      --  Initialize the database drivers.
      ADO.Drivers.Initialize (Generator.Get_Properties);

      --  Check if a database is specified in the command line and use it.
      if Ada.Strings.Fixed.Index (Arg1, "://") > 0 or else Arg3'Length > 0 then
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
   overriding
   procedure Help (Cmd       : in out Command;
                   Name      : in String;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name, Generator);
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
