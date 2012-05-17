-----------------------------------------------------------------------
--  gen-commands-plugins -- Plugin creation and management commands for dynamo
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Files;
with Util.Log.Loggers;
with Util.Strings.Transforms;
package body Gen.Commands.Plugins is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Commands.Project");

   --  ------------------------------
   --  Generator Command
   --  ------------------------------

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;

   begin
      --  If a dynamo.xml file exists, read it.
      if Ada.Directories.Exists ("dynamo.xml") then
         Generator.Read_Project ("dynamo.xml");
      else
         Generator.Set_Project_Property ("license", "apache");
         Generator.Set_Project_Property ("author", "unknown");
         Generator.Set_Project_Property ("author_email", "unknown@company.com");
      end if;
      --  Parse the command line
      loop
         case Getopt ("l:") is
         when ASCII.NUL => exit;

         when 'l' =>
            declare
               L : constant String := Util.Strings.Transforms.To_Lower_Case (Parameter);
            begin
               Log.Info ("License {0}", L);

               if L = "apache" then
                  Generator.Set_Project_Property ("license", "apache");
               elsif L = "gpl" then
                  Generator.Set_Project_Property ("license", "gpl");
               elsif L = "proprietary" then
                  Generator.Set_Project_Property ("license", "proprietary");
               else
                  Generator.Error ("Invalid license: {0}", L);
                  Generator.Error ("Valid licenses: apache, gpl, proprietary");
                  return;
               end if;
            end;

         when others =>
            null;
         end case;
      end loop;
      declare
         Name  : constant String := Get_Argument;
         Arg2  : constant String := Get_Argument;
         Arg3  : constant String := Get_Argument;
         Dir   : constant String := Generator.Get_Plugin_Directory;
         Path  : constant String := Util.Files.Compose (Dir, Name);
      begin
         if Name'Length = 0 then
            Generator.Error ("Missing plugin name");
            Gen.Commands.Usage;
            return;
         end if;

         if Ada.Directories.Exists (Path) then
            Generator.Error ("Plugin {0} exists already", Name);
            return;
         end if;
         if not Ada.Directories.Exists (Dir) then
            Ada.Directories.Create_Directory (Dir);
         end if;
         Ada.Directories.Create_Directory (Path);

         Generator.Set_Result_Directory (Ada.Strings.Unbounded.To_Unbounded_String (Path));

         Generator.Set_Project_Name (Name);
         Generator.Set_Force_Save (False);
         Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "create-plugin");

         Generator.Save_Project;
--           declare
--              Path   : constant GNAT.OS_Lib.String_Access
--                := GNAT.OS_Lib.Locate_Exec_On_Path ("autoconf");
--              Args   : GNAT.OS_Lib.Argument_List (1 .. 0);
--              Status : Boolean;
--           begin
--              Ada.Directories.Set_Directory (Generator.Get_Result_Directory);
--              Log.Info ("Executing {0}", Path.all);
--              GNAT.OS_Lib.Spawn (Path.all, Args, Status);
--              if not Status then
--                 Generator.Error ("Execution of {0} failed", Path.all);
--              end if;
--           end;
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
      Put_Line ("create-plugin: Create a new plugin for the current project");
      Put_Line ("Usage: create-plugin NAME");
      New_Line;
      Put_Line ("  Creates a new AWA plugin for the application with the name passed in NAME.");
   end Help;

end Gen.Commands.Plugins;
