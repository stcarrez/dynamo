-----------------------------------------------------------------------
--  gen-commands -- Commands for dynamo
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
with Gen.Commands.Generate;
with Gen.Commands.Project;
package body Gen.Commands is

   use Ada.Strings.Unbounded;

   Commands : Command_Maps.Map;


   --  Write the command usage.
   procedure Usage (Cmd : in Command) is
   begin
      null;
   end Usage;

   --  ------------------------------
   --  Print dynamo usage
   --  ------------------------------
   procedure Usage is
      use Ada.Text_IO;
   begin
      Put_Line (Gen.RELEASE);
      New_Line;
      Put ("Usage: ");
      Put (Ada.Command_Line.Command_Name);
      Put_Line (" [-v] [-o directory] [-t templates] {command} {arguments}");
      Put_Line ("where:");
      Put_Line ("   -v           Verbose");
      Put_Line ("   -q           Query mode");
      Put_Line ("   -o directory Directory where the Ada mapping files are generated");
      Put_Line ("   -t templates Directory where the Ada templates are defined");
      Put_Line ("   -c dir       Directory where the Ada templates and configurations are defined");
      New_Line;
      Put_Line ("   -h           Requests this info.");
   end Usage;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Help_Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);

      use Ada.Text_IO;

      procedure Print (Position : in Command_Maps.Cursor) is
         Name : constant Unbounded_String := Command_Maps.Key (Position);
      begin
         Put_Line ("   " & To_String (Name));
      end Print;

   begin
      Usage;
      New_Line;
      Put_Line ("Available subcommands:");

      Commands.Iterate (Process => Print'Access);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd : in Help_Command) is
   begin
      null;
   end Help;

   --  ------------------------------
   --  Register the command under the given  name.
   --  ------------------------------
   procedure Add_Command (Cmd  : in Command_Access;
                          Name : in String) is
   begin
      Commands.Include (Key => To_Unbounded_String (Name), New_Item => Cmd);
   end Add_Command;

   --  ------------------------------
   --  Find the command having the given name.
   --  ------------------------------
   function Find_Command (Name : in String) return Command_Access is
      Pos : constant Command_Maps.Cursor := Commands.Find (To_Unbounded_String (Name));
   begin
      if Command_Maps.Has_Element (Pos) then
         return Command_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find_Command;

   --  Generate command.
   Generate_Cmd       : aliased Gen.Commands.Generate.Command;

   --  Create project command.
   Create_Project_Cmd : aliased Gen.Commands.Project.Command;

   --  Help command.
   Help_Cmd           : aliased Help_Command;
begin
   Add_Command (Name => "help", Cmd => Help_Cmd'Access);
   Add_Command (Name => "generate", Cmd => Generate_Cmd'Access);
   Add_Command (Name => "create-project", Cmd => Create_Project_Cmd'Access);
end Gen.Commands;
