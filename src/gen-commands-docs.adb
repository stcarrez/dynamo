-----------------------------------------------------------------------
--  gen-commands-docs -- Extract and generate documentation for the project
--  Copyright (C) 2012, 2015, 2017, 2018, 2021 Stephane Carrez
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
with Ada.Directories;

with Gen.Artifacts.Docs;
package body Gen.Commands.Docs is

   use GNAT.Command_Line;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name, Args);

      Doc    : Gen.Artifacts.Docs.Artifact;
      Arg1   : constant String := Get_Argument;
      Arg2   : constant String := Get_Argument;
      Footer : Boolean := True;
      Fmt    : Gen.Artifacts.Docs.Doc_Format := Gen.Artifacts.Docs.DOC_WIKI_GOOGLE;
   begin
      if Ada.Directories.Exists ("dynamo.xml") then
         Generator.Read_Project ("dynamo.xml", False);
      end if;

      if Arg1'Length = 0 then
         Generator.Error ("Missing target directory");
         return;
      elsif Arg1 (Arg1'First) /= '-' then
         if Arg2'Length /= 0 then
            Generator.Error ("Invalid markup option " & Arg1);
            return;
         end if;
         --  Setup the target directory where the distribution is created.
         Generator.Set_Result_Directory (Arg1);
      else
         if Arg1 = "-markdown" then
            Fmt := Gen.Artifacts.Docs.DOC_MARKDOWN;
         elsif Arg1 = "-google" then
            Fmt := Gen.Artifacts.Docs.DOC_WIKI_GOOGLE;
         elsif Arg1 = "-pandoc" then
            Fmt := Gen.Artifacts.Docs.DOC_MARKDOWN;
            Footer := False;
         else
            Generator.Error ("Invalid markup option " & Arg1);
            return;
         end if;
         if Arg2'Length = 0 then
            Generator.Error ("Missing target directory");
            return;
         end if;
         Generator.Set_Result_Directory (Arg2);
      end if;
      Doc.Set_Format (Fmt, Footer);
      Doc.Read_Links (Generator.Get_Project_Property ("links", "links.txt"));
      Doc.Generate (Generator);
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
      Put_Line ("build-doc: Extract and generate the project documentation");
      Put_Line ("Usage: build-doc [-markdown|-google|-pandoc] target-dir");
      New_Line;
      Put_Line ("  Extract the documentation from the project source files and generate the");
      Put_Line ("  project documentation.  The following files are scanned:");
      Put_Line ("    - Ada specifications (src/*.ads)");
      Put_Line ("    - XML configuration files (config/*.xml)");
      Put_Line ("    - XML database model files (db/*.xml)");
   end Help;

end Gen.Commands.Docs;
