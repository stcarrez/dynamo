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
with Ada.Directories;
with Ada.Text_IO;
with Gen.Artifacts;
with GNAT.Command_Line;

with Util.Strings.Transforms;
package body Gen.Commands.Model is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;
      use Ada.Strings.Unbounded;

      Name   : constant String := Get_Argument;
      Dir    : constant String := Generator.Get_Result_Directory & "db/";

      function Get_Layout return String is
         Layout : constant String := Get_Argument;
      begin
         if Layout'Length = 0 then
            return "layout";
         end if;
         if Ada.Directories.Exists (Dir & "WEB-INF/layouts/" & Layout & ".xhtml") then
            return Layout;
         end if;

         Generator.Error ("Layout file {0} not found.  Using 'layout' instead.", Layout);
         return "layout";
      end Get_Layout;

      Layout : constant String := Get_Layout;
   begin
      if Name'Length = 0 then
         Gen.Commands.Usage;
         return;
      end if;

      Generator.Read_Project ("dynamo.xml");
      Generator.Set_Force_Save (False);
      Generator.Set_Result_Directory (To_Unbounded_String (Dir));
      Generator.Set_Global ("modelName", Name);
      Generator.Set_Global ("projectName", Generator.Get_Project_Name);
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "add-model");
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use Ada.Text_IO;
   begin
      Put_Line ("add-model: Add a new database table model to the application");
      Put_Line ("Usage: add-model NAME");
      New_Line;
      Put_Line ("  The database table model is an XML file that describes the mapping");
      Put_Line ("  for of a database table to an Ada representation.");
      Put_Line ("  The XML description is similar to Hibernate mapping files.");
      Put_Line ("  (See http://docs.jboss.org/hibernate/core/3.6/reference/en-US/html/mapping.html)");
    end Help;

end Gen.Commands.Model;
