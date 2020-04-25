-----------------------------------------------------------------------
--  gen-artifacts-distribs-concat -- Concatenate based distribution artifact
--  Copyright (C) 2012, 2020 Stephane Carrez
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
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;

with Util.Log.Loggers;
with Util.Streams;
with Util.Streams.Files;

package body Gen.Artifacts.Distribs.Concat is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Distribs.Concat");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      pragma Unreferenced (Node);

      Result : constant Concat_Rule_Access := new Concat_Rule;
   begin
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Concat_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "concat";
   end Get_Install_Name;

   --  ------------------------------
   --  Install the file <b>File</b> according to the distribution rule.
   --  Concatenate the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   --  ------------------------------
   overriding
   procedure Install (Rule    : in Concat_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is
      procedure Concat_File (File : in File_Record);

      Dir    : constant String := Ada.Directories.Containing_Directory (Path);
      Output : Util.Streams.Files.File_Stream;

      --  ------------------------------
      --  Append the file to the output
      --  ------------------------------
      procedure Concat_File (File : in File_Record) is
         File_Path : constant String := Rule.Get_Source_Path (File);
         Input     : Util.Streams.Files.File_Stream;
      begin
         if Rule.Level >= Util.Log.INFO_LEVEL then
            Log.Info ("  concat {0} to {1}", File_Path, Path);
         end if;
         Input.Open (Name => File_Path, Mode => Ada.Streams.Stream_IO.In_File);
         Util.Streams.Copy (From => Input, Into => Output);
         Input.Close;

      exception
         when Ex : Ada.IO_Exceptions.Name_Error =>
            Context.Error ("Cannot read {0}: ", File_Path,
                           Ada.Exceptions.Exception_Message (Ex));

      end Concat_File;

      Iter   : File_Cursor := Files.First;
   begin
      Ada.Directories.Create_Path (Dir);
      Output.Create (Name => Path, Mode => Ada.Streams.Stream_IO.Out_File);
      while File_Record_Vectors.Has_Element (Iter) loop
         File_Record_Vectors.Query_Element (Iter, Concat_File'Access);
         File_Record_Vectors.Next (Iter);
      end loop;
      Output.Close;
   end Install;

end Gen.Artifacts.Distribs.Concat;
