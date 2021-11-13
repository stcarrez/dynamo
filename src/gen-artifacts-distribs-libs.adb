-----------------------------------------------------------------------
--  gen-artifacts-distribs-libs -- Unix shared library extraction and distribution
--  Copyright (C) 2012, 2018, 2020, 2021 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Streams.Pipes;
with Util.Streams.Texts;
with Util.Beans.Objects;
with Util.Files;

with EL.Variables.Default;
with EL.Contexts.Default;

with Gen.Utils;
package body Gen.Artifacts.Distribs.Libs is

   Log : constant Util.Log.Loggers.Logger
     := Util.Log.Loggers.Create ("Gen.Artifacts.Distribs.Exec");

   --  ------------------------------
   --  Create a distribution rule to extract the shared libraries used by an executable
   --  and copy a selected subset in the target directory.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is

      procedure Collect_Libraries (Rule  : in out Libs_Rule'Class;
                                   Node  : in DOM.Core.Node);

      --  ------------------------------
      --  Collect the library patterns for the distribution rule.
      --  ------------------------------
      procedure Collect_Libraries (Rule  : in out Libs_Rule'Class;
                                   Node  : in DOM.Core.Node) is
         Name : constant String := Gen.Utils.Get_Data_Content (Node);
      begin
         Rule.Libraries.Append (Name);
      end Collect_Libraries;

      procedure Iterate is
        new Gen.Utils.Iterate_Nodes (T => Libs_Rule'Class,
                                     Process => Collect_Libraries);

      Ctx     : EL.Contexts.Default.Default_Context;
      Command : constant String := Gen.Utils.Get_Data_Content (Node, "command");
      Result  : constant Libs_Rule_Access := new Libs_Rule;
   begin
      if Command = "" then
         Result.Command := EL.Expressions.Create_Expression ("ldd #{src}", Ctx);
      else
         Result.Command := EL.Expressions.Create_Expression (Command, Ctx);
      end if;
      Iterate (Result.all, Node, "library");
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Libs_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "libs";
   end Get_Install_Name;

   --  ------------------------------
   --  Get the target path associate with the given source file for the distribution rule.
   --  ------------------------------
   overriding
   function Get_Target_Path (Rule : in Libs_Rule;
                             Base : in String;
                             File : in File_Record) return String is
      pragma Unreferenced (File);

      Dir  : constant String := To_String (Rule.Dir);
   begin
      return Util.Files.Compose (Base, Dir);
   end Get_Target_Path;

   --  ------------------------------
   --  Check if the library whose absolute path is defined in <b>Source</b> must be
   --  copied in the target directory and copy that library if needed.
   --  ------------------------------
   procedure Copy (Rule   : in Libs_Rule;
                   Dir    : in String;
                   Source : in String) is
      Name  : constant String := Ada.Directories.Simple_Name (Source);
      Path  : constant String := Ada.Directories.Compose (Dir, Name);

      Iter  : Util.Strings.Vectors.Cursor := Rule.Libraries.First;
      Found : Boolean := not Util.Strings.Vectors.Has_Element (Iter);
   begin
      Log.Debug ("Checking library {0}", Path);

      while not Found and then Util.Strings.Vectors.Has_Element (Iter) loop
         declare
            Lib     : constant String := Util.Strings.Vectors.Element (Iter);
            Matcher : constant GNAT.Regpat.Pattern_Matcher := Make_Regexp (Lib);
         begin
            Found := GNAT.Regpat.Match (Matcher, Name);
         end;
         Util.Strings.Vectors.Next (Iter);
      end loop;

      if Found then
         Log.Info ("Copy {0} To {1}", Source, Path);

         --  Make sure the target directory exists.
         Ada.Directories.Create_Path (Dir);

         Ada.Directories.Copy_File (Source_Name => Source,
                                    Target_Name => Path,
                                    Form        => "preserve=all_attributes, mode=overwrite");
      end if;
   end Copy;

   overriding
   procedure Install (Rule    : in Libs_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is

      Ctx       : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Source    : constant String := Get_Source_Path (Files);
   begin
      if Rule.Level >= Util.Log.INFO_LEVEL then
         Log.Info ("install {0} to {1}", Source, Path);
      end if;

      Variables.Bind ("src", Util.Beans.Objects.To_Object (Source));

      Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
      declare
         Cmd     : constant Util.Beans.Objects.Object := Rule.Command.Get_Value (Ctx);
         Command : constant String := Util.Beans.Objects.To_String (Cmd);
         Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
         Reader  : Util.Streams.Texts.Reader_Stream;
      begin
         --  Execute 'ldd' with the executable and read the output to extract the library path.
         --  Lines have the form:
         --          libsqlite3.so.0 => /usr/lib/libsqlite3.so.0 (0xb758a000)
         --  and we extract the absolute path to make the copy.
         Pipe.Open (Command);
         Reader.Initialize (Pipe'Unchecked_Access);
         while not Reader.Is_Eof loop
            declare
               Line : UString;
               Pos  : Natural;
               Last : Natural;
            begin
               Reader.Read_Line (Line, True);
               Pos := Ada.Strings.Unbounded.Index (Line, "=> ");
               if Pos > 0 then
                  Pos := Pos + 3;
                  Last := Ada.Strings.Unbounded.Index (Line, " ", Pos);
                  if Last = 0 then
                     Last := Ada.Strings.Unbounded.Length (Line);
                  else
                     Last := Last - 1;
                  end if;
                  if Pos < Last then
                     Rule.Copy (Path, Ada.Strings.Unbounded.Slice (Line, Pos, Last));
                  end if;
               end if;
            end;
         end loop;
         Pipe.Close;
         if Pipe.Get_Exit_Status /= 0 then
            Context.Error ("Command {0} exited with status {1}", Command,
                           Integer'Image (Pipe.Get_Exit_Status));
         end if;
      end;
   end Install;

end Gen.Artifacts.Distribs.Libs;
