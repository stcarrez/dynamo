-----------------------------------------------------------------------
--  gen-commands -- Commands for dynamo
--  Copyright (C) 2011, 2012, 2017, 2021 Stephane Carrez
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
with Ada.Command_Line;

with Gen.Configs;
with Gen.Commands.Generate;
with Gen.Commands.Project;
with Gen.Commands.Page;
with Gen.Commands.Layout;
with Gen.Commands.Model;
with Gen.Commands.Propset;
with Gen.Commands.Database;
with Gen.Commands.Info;
with Gen.Commands.Distrib;
with Gen.Commands.Plugins;
with Gen.Commands.Docs;

package body Gen.Commands is

   --  ------------------------------
   --  Print dynamo short usage.
   --  ------------------------------
   procedure Short_Help_Usage is
      use Ada.Text_IO;
   begin
      New_Line;
      Put ("Type '");
      Put (Ada.Command_Line.Command_Name);
      Put_Line (" help' for the list of commands.");
   end Short_Help_Usage;

   --  Generate command.
   Generate_Cmd       : aliased Gen.Commands.Generate.Command;

   --  Create project command.
   Create_Project_Cmd : aliased Gen.Commands.Project.Command;

   --  Add page command.
   Add_Page_Cmd       : aliased Gen.Commands.Page.Command;

   --  Add layout command.
   Add_Layout_Cmd     : aliased Gen.Commands.Layout.Command;

   --  Add model command.
   Add_Model_Cmd      : aliased Gen.Commands.Model.Command;

   --  Sets a property on the dynamo.xml project.
   Propset_Cmd      : aliased Gen.Commands.Propset.Command;

   --  Create database command.
   Database_Cmd       : aliased Gen.Commands.Database.Command;

   --  Project information command.
   Info_Cmd           : aliased Gen.Commands.Info.Command;

   --  Distrib command.
   Dist_Cmd           : aliased Gen.Commands.Distrib.Command;

   --  Create plugin command.
   Create_Plugin_Cmd  : aliased Gen.Commands.Plugins.Command;

   --  Documentation command.
   Doc_Plugin_Cmd     : aliased Gen.Commands.Docs.Command;

   --  Help command.
   Help_Cmd           : aliased Drivers.Help_Command_Type;
begin
   Driver.Set_Description (Gen.Configs.RELEASE);
   Driver.Set_Usage ("[-v] [-o directory] [-t templates] {command} {arguments}" & ASCII.LF &
                       "where:" & ASCII.LF &
                       "   -v           Print the version, configuration and installation paths" &
                       ASCII.LF &
                       "   -o directory Directory where the Ada mapping files are generated" &
                       ASCII.LF &
                       "   -t templates Directory where the Ada templates are defined" &
                       ASCII.LF &
                       "   -c dir       Directory where the Ada templates " &
                       "and configurations are defined");
   Driver.Add_Command (Name => "help", Command => Help_Cmd'Access);
   Driver.Add_Command (Name => "generate", Command => Generate_Cmd'Access);
   Driver.Add_Command (Name => "create-project", Command => Create_Project_Cmd'Access);
   Driver.Add_Command (Name => "add-page", Command => Add_Page_Cmd'Access);
   Driver.Add_Command (Name => "add-layout", Command => Add_Layout_Cmd'Access);
   Driver.Add_Command (Name => "add-model", Command => Add_Model_Cmd'Access);
   Driver.Add_Command (Name => "propset", Command => Propset_Cmd'Access);
   Driver.Add_Command (Name => "create-database", Command => Database_Cmd'Access);
   Driver.Add_Command (Name => "create-plugin", Command => Create_Plugin_Cmd'Access);
   Driver.Add_Command (Name => "dist", Command => Dist_Cmd'Access);
   Driver.Add_Command (Name => "info", Command => Info_Cmd'Access);
   Driver.Add_Command (Name => "build-doc", Command => Doc_Plugin_Cmd'Access);
end Gen.Commands;
