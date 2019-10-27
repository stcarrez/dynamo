-----------------------------------------------------------------------
--  gen-commands-distrib -- Distrib command for dynamo
--  Copyright (C) 2012, 2013, 2014, 2017, 2018 Stephane Carrez
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
package body Gen.Commands.Distrib is

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
      if Args.Get_Count = 0 or Args.Get_Count > 2 then
         Generator.Error ("Missing target directory");
         return;
      end if;
      Generator.Read_Project ("dynamo.xml", True);

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
