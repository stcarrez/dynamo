-----------------------------------------------------------------------
--  gen-artifacts-distribs-merges -- Web file merge
--  Copyright (C) 2020-2022 Stephane Carrez
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
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Files;
with Util.Strings;
with Util.Streams.Files;
with Util.Streams.Texts;
with Util.Streams.Buffered;

with EL.Expressions;

with Gen.Utils;
package body Gen.Artifacts.Distribs.Merges is

   use Util.Log;
   use Ada.Strings.Fixed;
   use Util.Beans.Objects;

   procedure Process_Property (Rule : in out Merge_Rule;
                               Node : in DOM.Core.Node);
   procedure Process_Replace (Rule : in out Merge_Rule;
                              Node : in DOM.Core.Node);

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Distribs.Merges");

   --  ------------------------------
   --  Extract the <property name='{name}'>{value}</property> to setup the
   --  EL context to evaluate the source and target links for the merge.
   --  ------------------------------
   procedure Process_Property (Rule : in out Merge_Rule;
                               Node : in DOM.Core.Node) is
      use Util.Beans.Objects.Maps;

      Name  : constant String := Gen.Utils.Get_Attribute (Node, "name");
      Value : constant String := Gen.Utils.Get_Data_Content (Node);
      Pos   : constant Natural := Util.Strings.Index (Name, '.');
   begin
      if Pos = 0 then
         Rule.Variables.Bind (Name, To_Object (Value));
         return;
      end if;

      --  A composed name such as 'jquery.path' must be split so that we store
      --  the 'path' value within a 'jquery' Map_Bean object.  We handle only
      --  one such composition.
      declare
         Param : constant String := Name (Name'First .. Pos - 1);
         Tag   : constant UString := To_UString (Param);
         Var   : constant EL.Expressions.Expression := Rule.Variables.Get_Variable (Tag);
         Val   : Object := Rule.Params.Get_Value (Param);
         Child : Map_Bean_Access;
      begin
         if Is_Null (Val) then
            Child := new Map_Bean;
            Val := To_Object (Child);
            Rule.Params.Set_Value (Param, Val);
         else
            Child := Map_Bean_Access (To_Bean (Val));
         end if;
         Child.Set_Value (Name (Pos + 1 .. Name'Last), To_Object (Value));
         if Var.Is_Null then
            Rule.Variables.Bind (Param, Val);
         end if;
      end;
   end Process_Property;

   procedure Process_Replace (Rule : in out Merge_Rule;
                              Node : in DOM.Core.Node) is
      From : constant String := Gen.Utils.Get_Data_Content (Node, "from");
      To   : constant String := Gen.Utils.Get_Data_Content (Node, "to");
   begin
      Rule.Replace.Include (From, To);
   end Process_Replace;

   procedure Iterate_Properties is
      new Gen.Utils.Iterate_Nodes (Merge_Rule, Process_Property);

   procedure Iterate_Replace is
      new Gen.Utils.Iterate_Nodes (Merge_Rule, Process_Replace);

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      Result : constant Merge_Rule_Access := new Merge_Rule;
   begin
      Iterate_Properties (Result.all, Node, "property", False);
      Iterate_Replace (Result.all, Node, "replace", False);
      Result.Context.Set_Variable_Mapper (Result.Variables'Access);
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Merge_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "merge";
   end Get_Install_Name;

   overriding
   procedure Install (Rule    : in Merge_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is

      use Ada.Streams;

      type Mode_Type is (MERGE_NONE, MERGE_LINK, MERGE_SCRIPT);

      procedure Error (Message : in String);
      function Get_Target_Path (Link : in String) return String;
      function Get_Filename (Line : in String) return String;
      function Get_Source (Line : in String;
                           Tag  : in String) return String;
      function Find_Match (Buffer : in Stream_Element_Array) return Stream_Element_Offset;
      procedure Prepare_Merge (Line : in String);
      procedure Include (Source : in String);
      procedure Process (Line : in String);

      Root_Dir : constant String :=
        Util.Files.Compose (Context.Get_Result_Directory,
                            To_String (Rule.Dir));
      Source   : constant String := Get_Source_Path (Files, False);
      Dir      : constant String := Ada.Directories.Containing_Directory (Path);
      Build    : constant String := Context.Get_Parameter ("dynamo.build");
      Output   : aliased Util.Streams.Files.File_Stream;
      Merge    : aliased Util.Streams.Files.File_Stream;
      Text     : Util.Streams.Texts.Print_Stream;
      Mode     : Mode_Type := MERGE_NONE;
      Line_Num : Natural := 0;

      procedure Error (Message : in String) is
         Line : constant String := Util.Strings.Image (Line_Num);
      begin
         Context.Error (Source & ":" & Line & ": " & Message);
      end Error;

      function Get_Target_Path (Link : in String) return String is
         Expr : EL.Expressions.Expression;
         File : Util.Beans.Objects.Object;
      begin
         Expr := EL.Expressions.Create_Expression (Link, Rule.Context);
         File := Expr.Get_Value (Rule.Context);
         return Util.Files.Compose (Root_Dir, To_String (File));
      end Get_Target_Path;

      function Get_Filename (Line : in String) return String is
         Pos  : Natural := Index (Line, "link=");
         Last : Natural;
      begin
         if Pos > 0 then
            Mode := MERGE_LINK;
         else
            Pos := Index (Line, "script=");
            if Pos > 0 then
               Mode := MERGE_SCRIPT;
            end if;
            if Pos = 0 then
               return "";
            end if;
         end if;
         Pos := Index (Line, "=");

         Last := Util.Strings.Index (Line, ' ', Pos + 1);
         if Last = 0 then
            return "";
         end if;
         return Line (Pos + 1 .. Last - 1);
      end Get_Filename;

      function Get_Source (Line : in String;
                           Tag  : in String) return String is
         Pos  : Natural := Index (Line, Tag);
         Last : Natural;
      begin
         if Pos = 0 then
            return "";
         end if;
         Pos := Pos + Tag'Length;
         if Pos > Line'Last or else (Line (Pos) /= '"' and then Line (Pos) /= ''') then
            return "";
         end if;
         Last := Util.Strings.Index (Line, Line (Pos), Pos + 1);
         if Last = 0 then
            return "";
         end if;
         return Line (Pos + 1 .. Last - 1);
      end Get_Source;

      procedure Prepare_Merge (Line : in String) is
         Name : constant String := Get_Filename (Line);
         Path : constant String := Get_Target_Path (Name);
      begin
         if Name'Length = 0 then
            Error ("invalid file name");
            return;
         end if;
         case Mode is
            when MERGE_LINK =>
               Text.Write ("<link media='screen' type='text/css' rel='stylesheet'"
                           & " href='");
               Text.Write (Name);
               Text.Write ("?build=");
               Text.Write (Build);
               Text.Write ("'/>" & ASCII.LF);

            when MERGE_SCRIPT =>
               Text.Write ("<script type='text/javascript' src='");
               Text.Write (Name);
               Text.Write ("?build=");
               Text.Write (Build);
               Text.Write ("'></script>" & ASCII.LF);

            when MERGE_NONE =>
               null;

         end case;
         if Rule.Level >= Util.Log.INFO_LEVEL then
            Log.Info ("  create {0}", Path);
         end if;
         Merge.Create (Mode => Ada.Streams.Stream_IO.Out_File,
                       Name => Path);
      end Prepare_Merge;

      Current_Match : Util.Strings.Maps.Cursor;

      function Find_Match (Buffer : in Stream_Element_Array) return Stream_Element_Offset is
         Iter  : Util.Strings.Maps.Cursor := Rule.Replace.First;
         First : Stream_Element_Offset := Buffer'Last + 1;
      begin
         while Util.Strings.Maps.Has_Element (Iter) loop
            declare
               Value : constant String := Util.Strings.Maps.Key (Iter);
               Match : Stream_Element_Array (1 .. Value'Length);
               for Match'Address use Value'Address;
               Pos   : Stream_Element_Offset;
            begin
               Pos := Buffer'First;
               while Pos + Match'Length < Buffer'Last loop
                  if Buffer (Pos .. Pos + Match'Length - 1) = Match then
                     if First > Pos then
                        First := Pos;
                        Current_Match := Iter;
                     end if;
                     exit;
                  end if;
                  Pos := Pos + 1;
               end loop;
            end;
            Util.Strings.Maps.Next (Iter);
         end loop;
         return First;
      end Find_Match;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Ada.Streams.Stream_Element_Array,
                                        Name   => Util.Streams.Buffered.Buffer_Access);

      procedure Include (Source : in String) is
         Target : constant String := Get_Target_Path (Source);
         Input  : Util.Streams.Files.File_Stream;
         Size   : Stream_Element_Offset;
         Last   : Stream_Element_Offset;
         Pos    : Stream_Element_Offset;
         Next   : Stream_Element_Offset;
         Buffer : Util.Streams.Buffered.Buffer_Access;
      begin
         if Rule.Level >= Util.Log.INFO_LEVEL then
            Log.Info ("    include {0}", Target);
         end if;

         Input.Open (Name => Target, Mode => Ada.Streams.Stream_IO.In_File);

         Size := Stream_Element_Offset (Ada.Directories.Size (Target));
         Buffer := new Stream_Element_Array (1 .. Size);
         Input.Read (Buffer.all, Last);
         Input.Close;

         Pos := 1;
         while Pos < Last loop
            Next := Find_Match (Buffer (Pos .. Last));
            if Next > Pos then
               Merge.Write (Buffer (Pos .. Next - 1));
            end if;
            exit when Next >= Last;
            declare
               Value   : constant String := Util.Strings.Maps.Key (Current_Match);
               Replace : constant String := Util.Strings.Maps.Element (Current_Match);
               Content : Stream_Element_Array (1 .. Replace'Length);
               for Content'Address use Replace'Address;
            begin
               Merge.Write (Content);
               Pos := Next + Value'Length;
            end;
         end loop;
         Free (Buffer);

      exception
         when Ex : Ada.IO_Exceptions.Name_Error =>
            Error ("Cannot read: " & Ada.Exceptions.Exception_Message (Ex));
      end Include;

      procedure Process (Line : in String) is
         Pos : Natural;
      begin
         Line_Num := Line_Num + 1;
         case Mode is
            when MERGE_NONE =>
               Pos := Index (Line, "<!-- DYNAMO-MERGE-START ");
               if Pos = 0 then
                  Text.Write (Line);
                  Text.Write (ASCII.LF);
                  return;
               end if;
               Text.Write (Line (Line'First .. Pos - 1));
               Prepare_Merge (Line (Pos + 10 .. Line'Last));

            when MERGE_LINK =>
               Pos := Index (Line, "<!-- DYNAMO-MERGE-END ");
               if Pos > 0 then
                  Merge.Close;
                  Mode := MERGE_NONE;
                  return;
               end if;
               Include (Get_Source (Line, "href="));

            when MERGE_SCRIPT =>
               Pos := Index (Line, "<!-- DYNAMO-MERGE-END ");
               if Pos > 0 then
                  Merge.Close;
                  Mode := MERGE_NONE;
                  return;
               end if;
               Text.Write (Line (Line'First .. Pos - 1));
               Include (Get_Source (Line, "src="));

         end case;
      end Process;

   begin
      if Rule.Level >= Util.Log.INFO_LEVEL then
         Log.Info ("webmerge {0}", Path);
      end if;

      Ada.Directories.Create_Path (Dir);
      Output.Create (Name => Path, Mode => Ada.Streams.Stream_IO.Out_File);
      Text.Initialize (Output'Unchecked_Access, 16 * 1024);
      Util.Files.Read_File (Source, Process'Access);
      Text.Flush;
      Output.Close;
   end Install;

end Gen.Artifacts.Distribs.Merges;
