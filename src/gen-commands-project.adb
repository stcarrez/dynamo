-----------------------------------------------------------------------
--  gen-commands-project -- Project creation command for dynamo
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
with Ada.Directories;
with Ada.Text_IO;
with Gen.Artifacts;
with GNAT.Command_Line;
with GNAT.OS_Lib;

with Util.Log.Loggers;
with Util.Strings.Transforms;
package body Gen.Commands.Project is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Commands.Project");

   --  ------------------------------
   --  Generator Command
   --  ------------------------------

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;

      Name : constant String := Get_Argument;
   begin
      if Name'Length = 0 then
         Gen.Commands.Usage;
         return;
      end if;

      Generator.Set_Project_Name (Name);
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project");

      Generator.Save_Project;
      declare
         Path   : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("autoconf");
         Args   : GNAT.OS_Lib.Argument_List (1 .. 0);
         Status : Boolean;
      begin
         Ada.Directories.Set_Directory (Generator.Get_Result_Directory);
         Log.Info ("Executing {0}", Path.all);
         GNAT.OS_Lib.Spawn (Path.all, Args, Status);
         if not Status then
            Generator.Error ("Execution of {0} failed", Path.all);
         end if;
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
      Put_Line ("create-project: Create a new Ada Web Application project");
      Put_Line ("Usage: create-project NAME");
      New_Line;
   end Help;

end Gen.Commands.Project;
