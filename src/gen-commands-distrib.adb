-----------------------------------------------------------------------
--  gen-commands-distrib -- Distrib command for dynamo
--  Copyright (C) 2012, 2013, 2014, 2017, 2018, 2020, 2021, 2022 Stephane Carrez
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
with GNAT.Regpat;
with Util.Strings;
package body Gen.Commands.Distrib is

   use Ada.Strings.Unbounded;

   procedure Increment_Build (Generator : in out Gen.Generator.Handler);

   procedure Increment_Build (Generator : in out Gen.Generator.Handler) is
      Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (".*([0-9]+).*");
      Matches : GNAT.Regpat.Match_Array (0 .. 1);
      Build   : constant String := Generator.Get_Project_Property ("build", "1");
      Number  : Natural := 0;
      Result  : UString;
   begin
      if GNAT.Regpat.Match (Pattern, Build) then
         GNAT.Regpat.Match (Pattern, Build, Matches);
         Number := Natural'Value (Build (Matches (1).First .. Matches (1).Last));
         Number := Number + 1;
         if Matches (1).First > Build'First then
            Append (Result, Build (Build'First .. Matches (1).First - 1));
         end if;
         Append (Result, Util.Strings.Image (Number));
         if Matches (1).Last < Build'Last then
            Append (Result, Build (Matches (1).Last + 1 .. Build'Last));
         end if;
         Generator.Set_Project_Property ("build", To_String (Result));
      end if;
      Generator.Save_Project;
   end Increment_Build;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name);
   begin
      if Args.Get_Count = 0 or else Args.Get_Count > 2 then
         Generator.Error ("Missing target directory");
         return;
      end if;
      Generator.Read_Project ("dynamo.xml", True);
      Increment_Build (Generator);

      --  Setup the target directory where the distribution is created.
      Generator.Set_Result_Directory (Args.Get_Argument (1));

      --  Read the package description.
      if Args.Get_Count = 2 then
         Gen.Generator.Read_Package (Generator, Args.Get_Argument (2));
      else
         Gen.Generator.Read_Package (Generator, "package.xml");
      end if;

      --  Run the generation.
      Gen.Generator.Prepare (Generator);
      Gen.Generator.Generate_All (Generator);
      Gen.Generator.Finish (Generator);
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
      Put_Line ("dist: Build the distribution files to prepare the server installation");
      Put_Line ("Usage: dist target-dir [package.xml]");
      New_Line;
      Put_Line ("  The dist command reads the XML package description to build the"
                & " distribution tree.");
      Put_Line ("  This command is intended to be used after the project is built.  It prepares");
      Put_Line ("  the files for their installation on the target server in a "
                & "production environment.");
      Put_Line ("  The package.xml describes what files are necessary on the server.");
      Put_Line ("  It allows to make transformations such as compressing Javascript, CSS and "
                & "images");
   end Help;

end Gen.Commands.Distrib;
