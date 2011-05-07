-----------------------------------------------------------------------
--  gen-commands-model -- Model creation command for dynamo
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
with Ada.Text_IO;
with Ada.Command_Line;
with Gen.Artifacts;
with GNAT.Command_Line;

with Gen.Utils;
with Util.Files;
package body Gen.Commands.Model is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;
      use Ada.Command_Line;
      use Ada.Strings.Unbounded;

      Name     : constant String := Get_Argument;
      Arg2     : constant String := Get_Argument;
      Root_Dir : constant String := Generator.Get_Result_Directory;
      Dir      : constant String := Util.Files.Compose (Root_Dir, "db");
   begin
      if Name'Length = 0 then
         Gen.Commands.Usage;
         return;
      end if;

      Generator.Read_Project ("dynamo.xml");
      Generator.Set_Force_Save (False);
      Generator.Set_Result_Directory (To_Unbounded_String (Dir));
      if Arg2'Length = 0 then
         --  Verify that we can use the name for an Ada identifier.
         if not Gen.Utils.Is_Valid_Name (Name) then
            Generator.Error ("The mapping name should be a valid Ada identifier.");
            raise Gen.Generator.Fatal_Error with "Invalid mapping name: " & Name;
         end if;
         Generator.Set_Global ("moduleName", "");
         Generator.Set_Global ("modelName", Name);
      else
         --  Verify that we can use the name for an Ada identifier.
         if not Gen.Utils.Is_Valid_Name (Name) then
            Generator.Error ("The module name should be a valid Ada identifier.");
            raise Gen.Generator.Fatal_Error with "Invalid module name: " & Name;
         end if;

         --  Likewise for the mapping name.
         if not Gen.Utils.Is_Valid_Name (Arg2) then
            Generator.Error ("The mapping name should be a valid Ada identifier.");
            raise Gen.Generator.Fatal_Error with "Invalid mapping name: " & Arg2;
         end if;
         Generator.Set_Global ("moduleName", Name);
         Generator.Set_Global ("modelName", Arg2);
      end if;
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "add-model");

      --  If the generation succeeds, run the generate command to generate the Ada files.
      if Generator.Get_Status = Ada.Command_Line.Success then

         Generator.Set_Result_Directory (To_Unbounded_String (Root_Dir));
         Generator.Set_Force_Save (True);
         Gen.Generator.Read_Models (Generator);

         --  Run the generation.
         Gen.Generator.Prepare (Generator);
         Gen.Generator.Generate_All (Generator);
      end if;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("add-model: Add a new database table model to the application");
      Put_Line ("Usage: add-model [MODULE] NAME");
      New_Line;
      Put_Line ("  The database table model is an XML file that describes the mapping");
      Put_Line ("  for of a database table to an Ada representation.");
      Put_Line ("  The XML description is similar to Hibernate mapping files.");
      Put_Line ("  (See http://docs.jboss.org/hibernate/core/3.6/reference/en-US/html/mapping.html)");
      Put_Line ("  If a MODULE is specified, the Ada package will be: <PROJECT>.<MODULE>.Model.<NAME>");
      Put_Line ("  Otherwise, the Ada package will be: <PROJECT>.Model.<NAME>");
    end Help;

end Gen.Commands.Model;
