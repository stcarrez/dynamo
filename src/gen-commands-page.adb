-----------------------------------------------------------------------
--  gen-commands-page -- Page creation command for dynamo
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2018, 2019, 2022 Stephane Carrez
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
with Util.Strings;

package body Gen.Commands.Page is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is

      function Get_Layout return String;
      function Get_Name return String;

      Dir    : constant String := Generator.Get_Result_Directory & "web/";

      function Get_Name return String is
         Name : constant String := Args.Get_Argument (1);
         Pos  : constant Natural := Util.Strings.Rindex (Name, '.');
      begin
         if Pos = 0 then
            return Name;
         elsif Name (Pos .. Name'Last) = ".xhtml" then
            return Name (Name'First .. Pos - 1);
         elsif Name (Pos .. Name'Last) = ".html" then
            return Name (Name'First .. Pos - 1);
         else
            return Name;
         end if;
      end Get_Name;

      function Get_Layout return String is
      begin
         if Args.Get_Count = 1 then
            return "layout";
         end if;
         declare
            Layout : constant String := Args.Get_Argument (2);
         begin
            if Ada.Directories.Exists (Dir & "WEB-INF/layouts/" & Layout & ".xhtml") then
               return Layout;
            end if;

            Generator.Info ("Layout file {0} not found.", Layout);
            return Layout;
         end;
      end Get_Layout;

   begin
      if Args.Get_Count = 0 or else Args.Get_Count > 2 then
         Cmd.Usage (Name, Generator);
         return;
      end if;

      Generator.Set_Force_Save (False);
      Generator.Set_Result_Directory (Dir);
      Generator.Set_Global ("pageName", Get_Name);
      Generator.Set_Global ("layout", Get_Layout);
      Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "page");
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Cmd       : in out Command;
                   Name      : in String;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name);
      use Ada.Text_IO;
      use Ada.Directories;

      Path    : constant String := Generator.Get_Result_Directory & "web/WEB-INF/layouts";
      Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
      Search  : Search_Type;
      Ent     : Directory_Entry_Type;
   begin
      Put_Line ("add-page: Add a new web page to the application");
      Put_Line ("Usage: add-page NAME [LAYOUT]");
      New_Line;
      Put_Line ("  The web page is an XHTML file created under the 'web' directory.");
      Put_Line ("  The NAME can contain a directory that will be created if necessary.");
      Put_Line ("  The new web page can be configured to use the given layout.");
      Put_Line ("  The layout file must exist to be used.  The default layout is 'layout'.");
      Put_Line ("  You can create a new layout with 'add-layout' command.");
      Put_Line ("  You can also write your layout by adding an XHTML file in the directory:");
      Put_Line ("     " & Path);
      if Exists (Path) then
         New_Line;
         Put_Line ("  Available layouts:");

         Start_Search (Search, Directory => Path, Pattern => "*.xhtml", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Name   : constant String := Simple_Name (Ent);
               Layout : constant String := Base_Name (Name);
            begin
               Put_Line ("      " & Layout);
            end;
         end loop;
      end if;
      New_Line;
      Put_Line ("  The following files are generated:");
      Put_Line ("    web/<name>.xhtml");
   end Help;

end Gen.Commands.Page;
