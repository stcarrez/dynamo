-----------------------------------------------------------------------
--  gen-commands-plugins -- Plugin creation and management commands for dynamo
--  Copyright (C) 2012, 2015, 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Gen.Model.Projects;
with GNAT.Command_Line;

with Util.Files;
with Util.Log.Loggers;
with Util.Strings.Transforms;
package body Gen.Commands.Plugins is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Commands.Project");

   --  ------------------------------
   --  Generator Command
   --  ------------------------------

   --  Execute the command with the arguments.
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Name, Args);
      use GNAT.Command_Line;

      function Get_Directory_Name (Name     : in String;
                                   Name_Dir : in UString)
        return String;

      function Get_Directory_Name (Name     : in String;
                                   Name_Dir : in UString)
                                   return String is
      begin
         if Ada.Strings.Unbounded.Length (Name_Dir) = 0 then
            return Name;
         else
            return To_String (Name_Dir);
         end if;
      end Get_Directory_Name;

      Result_Dir : constant String := Generator.Get_Result_Directory;
      Name_Dir   : UString;
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
         case Getopt ("l: d:") is
         when ASCII.NUL => exit;

         when 'd' =>
            Name_Dir := To_UString (Parameter);

         when 'l' =>
            declare
               L : constant String := Util.Strings.Transforms.To_Lower_Case (Parameter);
            begin
               Log.Info ("License {0}", L);

               if L = "apache" then
                  Generator.Set_Project_Property ("license", "apache");
               elsif L = "gpl" then
                  Generator.Set_Project_Property ("license", "gpl");
               elsif L = "gpl3" then
                  Generator.Set_Project_Property ("license", "gpl3");
               elsif L = "mit" then
                  Generator.Set_Project_Property ("license", "mit");
               elsif L = "bsd3" then
                  Generator.Set_Project_Property ("license", "bsd3");
               elsif L = "proprietary" then
                  Generator.Set_Project_Property ("license", "proprietary");
               else
                  Generator.Error ("Invalid license: {0}", L);
                  Generator.Error ("Valid licenses: apache, gpl, gpl3, mit, bsd3, proprietary");
                  return;
               end if;
            end;

         when others =>
            null;
         end case;
      end loop;
      declare
         Name       : constant String := Get_Argument;
         Kind       : constant String := Get_Argument;
         Dir        : constant String := Generator.Get_Plugin_Directory;
         Plugin_Dir : constant String := Get_Directory_Name (Name, Name_Dir);
         Path       : constant String := Util.Files.Compose (Dir, Plugin_Dir);
      begin
         if Name'Length = 0 then
            Generator.Error ("Missing plugin name");
            Cmd.Usage (Name, Generator);
            return;
         end if;
         if Kind /= "ada" and then Kind /= "web" then
            Generator.Error ("Invalid plugin type (must be 'ada' or 'web')");
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

         Generator.Set_Result_Directory (Path);

         --  Create the plugin project instance and generate its dynamo.xml file.
         --  The new plugin is added to the current project so that it will be referenced.
         declare
            procedure Create_Plugin (Project : in out Model.Projects.Root_Project_Definition);

            procedure Create_Plugin (Project : in out Model.Projects.Root_Project_Definition) is
               Plugin : Gen.Model.Projects.Project_Definition_Access;
               File   : constant String := Util.Files.Compose (Path, "dynamo.xml");
            begin
               Project.Create_Project (Name    => Name,
                                       Path    => File,
                                       Project => Plugin);
               Project.Add_Module (Plugin);

               Plugin.Props.Set ("license", Project.Props.Get ("license", "none"));
               Plugin.Props.Set ("author", Project.Props.Get ("author", ""));
               Plugin.Props.Set ("author_email", Project.Props.Get ("author_email", ""));
               Plugin.Save (File);
            end Create_Plugin;

         begin
            Generator.Update_Project (Create_Plugin'Access);
         end;

         --  Generate the new plugin content.
         Generator.Set_Force_Save (False);
         Generator.Set_Global ("pluginName", Name);
         Generator.Set_Global ("pluginDir", Plugin_Dir);
         Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE,
                                     "create-plugin-" & Kind);

         --  And save the project dynamo.xml file which now refers to the new plugin.
         Generator.Set_Result_Directory (Result_Dir);
         Generator.Save_Project;

      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            Generator.Error ("Cannot create directory {0}", Path);
      end;
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
      Put_Line ("create-plugin: Create a new plugin for the current project");
      Put_Line ("Usage: create-plugin [-l apache|gpl|gpl3|mit|bsd3|proprietary] "
                & "[-d DIR] NAME [ada | web]");
      New_Line;
      Put_Line ("  Creates a new AWA plugin for the application with the name passed in NAME.");
      Put_Line ("  The plugin license is controlled by the -l option.");
      Put_Line ("  The plugin type is specified as the last argument which can be one of:");
      New_Line;
      Put_Line ("    ada  the plugin contains Ada code and a GNAT project is created");
      Put_Line ("    web  the plugin contains XHTML, CSS, Javascript files only");
      New_Line;
      Put_Line ("  The -d option allows to control the plugin directory name.  The plugin NAME");
      Put_Line ("  is used by default.  The plugin is created in the directory:");
      Put_Line ("    plugins/NAME or plugins/DIR");
      New_Line;
      Put_Line ("  For the Ada plugin, the command generates the following files"
                & " in the plugin directory:");
      Put_Line ("    src/<project>-<plugin>.ads");
      Put_Line ("    src/<project>-<plugin>-<module>.ads");
      Put_Line ("    src/<project>-<plugin>-<module>.adb");
   end Help;

end Gen.Commands.Plugins;
