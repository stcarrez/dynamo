-----------------------------------------------------------------------
--  dynamo -- Ada Code Generator
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.Traceback.Symbolic;

with Sax.Readers;
with Ada.Text_IO;

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Command_Line;

with Util.Log.Loggers;
with Gen.Generator;
with Gen.Commands;
procedure Dynamo is
   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use Ada.Command_Line;
   use Gen.Commands;

   Out_Dir      : Unbounded_String;
   Config_Dir   : Unbounded_String;
   Template_Dir : Unbounded_String;
   Status       : Exit_Status := Success;

   --  ------------------------------
   --  Verify and set the configuration path
   --  ------------------------------
   procedure Set_Config_Directory (Path   : in String;
                                   Silent : in Boolean := False) is
      Log_Path : constant String := Ada.Directories.Compose (Path, "log4j.properties");
   begin
      --  Ignore if the config directory was already set.
      if Length (Config_Dir) > 0 then
         return;
      end if;

      --  Check that we can read some configuration file.
      if not Ada.Directories.Exists (Log_Path) then
         if not Silent then
            Ada.Text_IO.Put_Line ("Invalid config directory: " & Path);
            Status := Failure;
         end if;
         return;
      end if;

      --  Configure the logs
      Util.Log.Loggers.Initialize (Log_Path);
      Config_Dir := To_Unbounded_String (Path);
   end Set_Config_Directory;

begin
   --  Parse the command line
   loop
      case Getopt ("o: t: c:") is
         when ASCII.Nul => exit;

         when 'o' =>
            Out_Dir := To_Unbounded_String (Parameter & "/");

         when 't' =>
            Template_Dir := To_Unbounded_String (Parameter & "/");

         when 'c' =>
            Set_Config_Directory (Parameter);

         when others =>
            null;
      end case;
   end loop;

   if Length (Config_Dir) = 0 then
      declare
         Name : constant String := Ada.Command_Line.Command_Name;
         Path : constant String := Ada.Directories.Containing_Directory (Name);
      begin
         Set_Config_Directory (Compose (Containing_Directory (Path), "config"), True);
         Set_Config_Directory (Gen.CONFIG_DIR, True);
      end;
   end if;
   if Status /= Success then
      Ada.Command_Line.Set_Exit_Status (Status);
      return;
   end if;

   declare
      Cmd_Name  : constant String := Get_Argument;
      Cmd       : constant Gen.Commands.Command_Access := Gen.Commands.Find_Command (Cmd_Name);
      Generator : Gen.Generator.Handler;
   begin
      --  Check that the command exists.
      if Cmd = null then
         if Cmd_Name'Length > 0 then
            Ada.Text_IO.Put_Line ("Invalid command: '" & Cmd_Name & "'");
         else
            Ada.Text_IO.Put_Line (Gen.RELEASE);
            Ada.Text_IO.Put ("Type '");
            Ada.Text_IO.Put (Ada.Command_Line.Command_Name);
            Ada.Text_IO.Put_Line (" help' for usage.");
         end if;
         Set_Exit_Status (Failure);
         return;
      end if;
      if Length (Out_Dir) > 0 then
         Gen.Generator.Set_Result_Directory (Generator, Out_Dir);
      end if;
      if Length (Template_Dir) > 0 then
         Gen.Generator.Set_Template_Directory (Generator, Template_Dir);
      end if;

      Gen.Generator.Initialize (Generator, Config_Dir);

      Cmd.Execute (Generator);

      Ada.Command_Line.Set_Exit_Status (Gen.Generator.Get_Status (Generator));
   end;

exception
   when Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option.");
      Ada.Text_IO.Put_Line ("Use the 'help' command.");
      Ada.Command_Line.Set_Exit_Status (2);

   when E : Gen.Generator.Fatal_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);

   when E : Sax.Readers.XML_Fatal_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (1);

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);

end Dynamo;
