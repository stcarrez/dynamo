-----------------------------------------------------------------------
--  gen-commands-project -- Project creation command for dynamo
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
with Gen.Artifacts;
with GNAT.Command_Line;
with Util.Strings.Transforms;
package body Gen.Commands.Project is

   --  ------------------------------
   --  Generator Command
   --  ------------------------------

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      use GNAT.Command_Line;

      Name : constant String := Get_Argument;
   begin
      if Name'Length = 0 then
         Gen.Commands.Usage;
         return;
      end if;

      Generator.Set_Global ("projectName", Name);
      Generator.Set_Global ("projectCode", Util.Strings.Transforms.To_Upper_Case (Name));
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project");
   end Execute;

   --  Write the help associated with the command.
   procedure Help (Cmd : in Command) is
   begin
      null;
   end Help;

end Gen.Commands.Project;
