-----------------------------------------------------------------------
--  dbmapper -- Database Mapper Generator
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Ada.Exceptions;
with Ada.Command_Line;

with Gen.Generator;
procedure DBMapper is
   use Ada;

   use Ada.Command_Line;

   Generator : Gen.Generator.Handler;

   Release : constant String
     := "ADO Generator 0.2, Stephane Carrez";

   Copyright : constant String
     := "Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.";

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
      New_Line;
      Put_Line ("   -h           Requests this info.");
      New_Line;
   end Usage;

begin
   --  Parse the command line
   loop
      case Getopt ("o: t:") is
         when ASCII.Nul => exit;

         when 'o' =>
            Gen.Generator.Set_Result_Directory (Generator, Parameter & "/");

         when 't' =>
            Gen.Generator.Set_Template_Directory (Generator, Parameter & "/");

         when others =>
            null;
      end case;
   end loop;

   declare
      Model_File : constant String := Get_Argument;
   begin
      if Model_File'Length = 0 then
         Usage;
         Set_Exit_Status (2);
         return;
      end if;
      Gen.Generator.Initialize (Generator);
      Gen.Generator.Read_Model (Generator, Model_File);
      Gen.Generator.Generate_All (Generator, "model");
--        Gen.Generator.Generate (Generator, "model/package-body");
   end;

exception
   when E : XML_Fatal_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
end DBMapper;
