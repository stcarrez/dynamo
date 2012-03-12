-----------------------------------------------------------------------
--  gen-commands-distrib -- Distrib command for dynamo
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

with Ada.Directories;
with Ada.Text_IO;
package body Gen.Commands.Distrib is

   use GNAT.Command_Line;
   use Ada.Directories;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);

      File_Count : Natural := 0;
   begin
      Generator.Read_Project ("dynamo.xml", True);

      --  Read the package description.
      loop
         declare
            Model_File : constant String := Get_Argument;
         begin
            exit when Model_File'Length = 0;
            File_Count := File_Count + 1;
            Gen.Generator.Read_Package (Generator, Model_File);
         end;
      end loop;

      if File_Count = 0 then
         Gen.Generator.Read_Models (Generator, "db");
      end if;

      --  Run the generation.
      Gen.Generator.Prepare (Generator);
      Gen.Generator.Generate_All (Generator);
      Gen.Generator.Finish (Generator);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("distrib: Generate the Ada files for the database model or queries");
      Put_Line ("Usage: distrib [package.xml]");
      New_Line;
      Put_Line ("  Read the XML package description and build the distribution tree");
   end Help;

end Gen.Commands.Distrib;
