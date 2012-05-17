-----------------------------------------------------------------------
--  gen-commands-layout -- Layout creation command for dynamo
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Util.Files;
package body Gen.Commands.Layout is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd);
      use GNAT.Command_Line;
      use Ada.Strings.Unbounded;

      function Get_Layout return String;

      Name       : constant String := Get_Argument;
      Dir        : constant String := Generator.Get_Result_Directory;
      Layout_Dir : constant String := Util.Files.Compose (Dir, "web/WEB-INF/layouts");

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

      Generator.Set_Force_Save (False);
      Generator.Set_Result_Directory (Layout_Dir);
      Generator.Set_Global ("pageName", Name);
      Generator.Set_Global ("layout", Layout);
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "layout");
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Generator);
      use Ada.Text_IO;

   begin
      Put_Line ("add-layout: Add a new layout page to the application");
      Put_Line ("Usage: add-layout NAME [FORM]");
      New_Line;
      Put_Line ("  The layout page allows to give a common look to a set of pages.");
      Put_Line ("  You can create several layouts for your application.");
      Put_Line ("  Each layout can reference one or several building blocks that are defined");
      Put_Line ("  in the original page.");
   end Help;

end Gen.Commands.Layout;
