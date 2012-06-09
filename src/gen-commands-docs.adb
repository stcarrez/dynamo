-----------------------------------------------------------------------
--  gen-commands-docs -- Extract and generate documentation for the project
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

with GNAT.Command_Line;

with Ada.Text_IO;

with Gen.Artifacts.Docs;
with Gen.Model.Packages;
package body Gen.Commands.Docs is

   use GNAT.Command_Line;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);

      Doc : Gen.Artifacts.Docs.Artifact;
      M   : Gen.Model.Packages.Model_Definition;
   begin
      Generator.Read_Project ("dynamo.xml", False);

      --  Setup the target directory where the distribution is created.
      declare
         Target_Dir : constant String := Get_Argument;
      begin
         if Target_Dir'Length = 0 then
            Generator.Error ("Missing target directory");
            return;
         end if;
         Generator.Set_Result_Directory (Target_Dir);
      end;

      Doc.Prepare (M, Generator);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("build-doc: Extract and generate the project documentation");
      Put_Line ("Usage: build-doc");
      New_Line;
      Put_Line ("  Extract the documentation from the project source files and generate the");
      Put_Line ("  project documentation.  The following files are scanned:");
      Put_Line ("    - Ada specifications (src/*.ads)");
      Put_Line ("    - XML configuration files (config/*.xml)");
      Put_Line ("    - XML database model files (db/*.xml)");
   end Help;

end Gen.Commands.Docs;
