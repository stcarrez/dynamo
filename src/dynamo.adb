-----------------------------------------------------------------------
--  dynamo -- Ada Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2017, 2020 Stephane Carrez
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
with Ada.Environment_Variables;

with Util.Files;
with Util.Log.Loggers;
with Util.Systems.Os;
with Util.Commands;
with Gen.Utils.GNAT;
with Gen.Generator;
with Gen.Commands;
with Gen.Configs;
procedure Dynamo is
   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use Ada.Command_Line;
   use Gen.Commands;

   procedure Set_Config_Directory (Path   : in String;
                                   Silent : in Boolean := False);

   procedure Print_Configuration (Generator : in Gen.Generator.Handler);

   --  Print environment variable setup
   procedure Print_Environment (Generator : in Gen.Generator.Handler;
                                C_Env     : in Boolean := False);

   function Get_Installation_Directory return String;

   Out_Dir      : Unbounded_String;
   Config_Dir   : Unbounded_String;
   Template_Dir : Unbounded_String;
   Status       : Exit_Status := Success;
   Debug        : Boolean := False;
   Print_Config : Boolean := False;
   Print_Env    : Boolean := False;
   Print_CEnv   : Boolean := False;
   First        : Natural := 0;

   --  ------------------------------
   --  Print information about dynamo configuration
   --  ------------------------------
   procedure Print_Configuration (Generator : in Gen.Generator.Handler) is
   begin
      Ada.Text_IO.Put_Line ("Dynamo version          : " & Gen.Configs.VERSION);
      Ada.Text_IO.Put_Line ("Config directory        : " & Generator.Get_Config_Directory);
      Ada.Text_IO.Put_Line ("UML directory           : "
                            & Generator.Get_Parameter (Gen.Configs.GEN_UML_DIR));
      Ada.Text_IO.Put_Line ("Templates directory     : "
                            & Generator.Get_Parameter (Gen.Configs.GEN_TEMPLATES_DIRS));
      Ada.Text_IO.Put_Line ("GNAT project directory  : "
                            & Generator.Get_Parameter (Gen.Configs.GEN_GNAT_PROJECT_DIRS));
   end Print_Configuration;

   --  ------------------------------
   --  Print environment variable setup
   --  ------------------------------
   procedure Print_Environment (Generator : in Gen.Generator.Handler;
                                C_Env     : in Boolean := False) is
   begin
      if C_Env then
         Ada.Text_IO.Put ("setenv " & Gen.Utils.GNAT.ADA_PROJECT_PATH_NAME & " """);
      else
         Ada.Text_IO.Put ("export " & Gen.Utils.GNAT.ADA_PROJECT_PATH_NAME & "=""");
      end if;
      if Ada.Environment_Variables.Exists (Gen.Utils.GNAT.ADA_PROJECT_PATH_NAME) then
         Ada.Text_IO.Put (Ada.Environment_Variables.Value (Gen.Utils.GNAT.ADA_PROJECT_PATH_NAME));
         Ada.Text_IO.Put (Util.Systems.Os.Path_Separator);
      end if;
      Ada.Text_IO.Put_Line (Generator.Get_Parameter (Gen.Configs.GEN_GNAT_PROJECT_DIRS) & """");
   end Print_Environment;

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

   function Get_Installation_Directory return String is
      Name : constant String := Ada.Command_Line.Command_Name;
      Path : constant String := Ada.Directories.Containing_Directory (Name);
   begin
      if Path = "." then
         return ".";
      else
         return Ada.Directories.Containing_Directory (Path);
      end if;
   end Get_Installation_Directory;

begin
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");
   --  Parse the command line
   loop
      case Getopt ("* v d e E o: t: c:") is
         when ASCII.NUL => exit;

         when 'o' =>
            Out_Dir := To_Unbounded_String (Parameter & "/");
            First := First + 1;

         when 't' =>
            Template_Dir := To_Unbounded_String (Parameter & "/");
            First := First + 1;

         when 'c' =>
            Set_Config_Directory (Parameter);
            First := First + 1;

         when 'e' =>
            Print_Env := True;

         when 'E' =>
            Print_CEnv := True;

         when 'd' =>
            Debug := True;

         when 'v' =>
            Print_Config := True;

         when '*' =>
            exit;

         when others =>
            null;
      end case;
      First := First + 1;
   end loop;

   if Length (Config_Dir) = 0 then
      declare
         Dir  : constant String := Get_Installation_Directory;
      begin
         Set_Config_Directory (Compose (Dir, "config"), True);
         Set_Config_Directory (Util.Files.Compose (Dir, "share/dynamo/base"), True);
         Set_Config_Directory (Gen.Configs.CONFIG_DIR, True);
      end;
   end if;
   if Status /= Success then
      Gen.Commands.Short_Help_Usage;
      Ada.Command_Line.Set_Exit_Status (Status);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 0 then
      Gen.Commands.Short_Help_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      use type Gen.Commands.Command_Access;

      Args      : Util.Commands.Default_Argument_List (First + 1);
      Cmd_Name  : constant String := Full_Switch;
      Cmd       : Gen.Commands.Command_Access;
      Generator : Gen.Generator.Handler;
   begin
      if Length (Out_Dir) > 0 then
         Gen.Generator.Set_Result_Directory (Generator, To_String (Out_Dir));
      end if;
      if Length (Template_Dir) > 0 then
         Gen.Generator.Set_Template_Directory (Generator, Template_Dir);
      end if;

      Gen.Generator.Initialize (Generator, Config_Dir, Debug);
      if Print_Config then
         Print_Configuration (Generator);
         return;
      elsif Print_Env then
         Print_Environment (Generator, False);
         return;
      elsif Print_CEnv then
         Print_Environment (Generator, True);
         return;
      end if;

      Cmd := Gen.Commands.Driver.Find_Command (Cmd_Name);

      --  Check that the command exists.
      if Cmd = null then
         if Cmd_Name'Length > 0 then
            Ada.Text_IO.Put_Line ("Invalid command: '" & Cmd_Name & "'");
         end if;
         Gen.Commands.Short_Help_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Cmd.Execute (Cmd_Name, Args, Generator);

      Ada.Command_Line.Set_Exit_Status (Gen.Generator.Get_Status (Generator));
   end;

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      Gen.Commands.Short_Help_Usage;
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
