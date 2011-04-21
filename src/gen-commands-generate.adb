-----------------------------------------------------------------------
--  gen-commands-generate -- Generate command for dynamo
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

with GNAT.Command_Line;

with Ada.Command_Line;
with Ada.Text_IO;
package body Gen.Commands.Generate is

   use Ada.Command_Line;
   use GNAT.Command_Line;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is

      File_Count : Natural := 0;
   begin
      --  Read the model files.
      loop
         declare
            Model_File : constant String := Get_Argument;
         begin
            exit when Model_File'Length = 0;
            File_Count := File_Count + 1;
            Gen.Generator.Read_Model (Generator, Model_File);
         end;
      end loop;

      if File_Count = 0 then
         Cmd.Usage;
         Set_Exit_Status (2);
         return;
      end if;

      --  Run the generation.
      Gen.Generator.Prepare (Generator);
      Gen.Generator.Generate_All (Generator);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("generate: Generate the Ada files for the database model or queries");
      Put_Line ("Usage: generate MODEL...");
      New_Line;
      Put_Line ("  Read the XML model description (Hibernate mapping, query mapping, ...)");
      Put_Line ("  and generate the Ada model files that correspond to the mapping.");
   end Help;

end Gen.Commands.Generate;
