-----------------------------------------------------------------------
--  dbmapper -- Database Mapper Generator
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
procedure DBMapper is
   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Command_Line;

   Release : constant String
     := "ADO Generator 0.3, Stephane Carrez";

   Copyright : constant String
     := "Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.";

   -----------------
   -- Output_File --
   -----------------
   --------------------------------------------------
   --  Usage
   --------------------------------------------------
   procedure Usage is
      use Ada.Text_IO;
   begin
      Put_Line (Release);
      Put_Line (Copyright);

      New_Line;
      Put ("Usage: ");
      Put (Command_Name);
      Put_Line (" [-v] [-o directory] [-t templates] model.xml");
      Put_Line ("where:");
      Put_Line ("   -v           Verbose");
      Put_Line ("   -q           Query mode");
      Put_Line ("   -o directory Directory where the Ada mapping files are generated");
      Put_Line ("   -t templates Directory where the Ada templates are defined");
      Put_Line ("   -c dir       Directory where the Ada templates and configurations are defined");
      New_Line;
      Put_Line ("   -h           Requests this info.");
      New_Line;
   end Usage;

   File_Count : Natural := 0;

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
      Generator : Gen.Generator.Handler;
   begin
      if Length (Out_Dir) > 0 then
         Gen.Generator.Set_Result_Directory (Generator, Out_Dir);
      end if;
      if Length (Template_Dir) > 0 then
         Gen.Generator.Set_Template_Directory (Generator, Template_Dir);
      end if;

      Gen.Generator.Initialize (Generator, Config_Dir);

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
         Usage;
         Set_Exit_Status (2);
         return;
      end if;

      --  Run the generation.
      Gen.Generator.Prepare (Generator);
      Gen.Generator.Generate_All (Generator);
--        Gen.Generator.Generate_All (Generator, Gen.Generator.ITERATION_PACKAGE, "model");
--        Gen.Generator.Generate_All (Generator, Gen.Generator.ITERATION_TABLE, "sql");

      Ada.Command_Line.Set_Exit_Status (Gen.Generator.Get_Status (Generator));
   end;
exception
   when E : XML_Fatal_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end DBMapper;
