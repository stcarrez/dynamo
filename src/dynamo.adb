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

with Sax.Readers;        use Sax.Readers;
with Ada.Text_IO;

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;

with Util.Log.Loggers;
with Gen.Generator;
with Gen.Commands;
procedure Dynamo is
   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Command_Line;
   use Gen.Commands;

   Out_Dir      : Unbounded_String;
   Config_Dir   : Unbounded_String;
   Template_Dir : Unbounded_String;
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
            Config_Dir := To_Unbounded_String (Parameter & "/");

         when others =>
            null;
      end case;
   end loop;

   if Length (Config_Dir) = 0 then
      Config_Dir := To_Unbounded_String ("config/");
   end if;

   --  Configure the logs
   Util.Log.Loggers.Initialize (To_String (Config_Dir) & "log4j.properties");

   declare
      Cmd_Name  : constant String := Get_Argument;
      Cmd       : constant Gen.Commands.Command_Access := Gen.Commands.Find_Command (Cmd_Name);
      Generator : Gen.Generator.Handler;
   begin
      --  Check that the command exists.
      if Cmd = null then
         if Cmd_Name'Length > 0 then
            Ada.Text_IO.Put_Line ("Invalid command: '" & Cmd_Name & "'");
         end if;
         Gen.Commands.Usage;
         Set_Exit_Status (1);
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
   when E : XML_Fatal_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end Dynamo;
