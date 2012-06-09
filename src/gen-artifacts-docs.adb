-----------------------------------------------------------------------
--  gen-artifacts-docs -- Artifact for documentation
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Files;
with Util.Log.Loggers;
with Util.Strings;
with Util.Streams.Pipes;
with Util.Streams.Texts;
with Util.Processes;

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Strings.Maps;

with Gen.Utils;
package body Gen.Artifacts.Docs is

   use Util.Log;
   use type Ada.Strings.Maps.Character_Set;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Docs");

   Spaces : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (' ')
     or Ada.Strings.Maps.To_Set (ASCII.HT)
     or Ada.Strings.Maps.To_Set (ASCII.VT)
     or Ada.Strings.Maps.To_Set (ASCII.CR)
     or Ada.Strings.Maps.To_Set (ASCII.LF);

   --  ------------------------------
   --  Documentation artifact
   --  ------------------------------

   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is
   begin
      null;
   end Initialize;

   --  Prepare the model after all the configuration files have been read and before
   --  actually invoking the generation.
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Context : in out Generator'Class) is
      Docs : Doc_Maps.Map;
      Command : constant String := Context.Get_Parameter ("generator.doc.xslt.command");
   begin
      Log.Info ("Using command: {0}", Command);

      Handler.Xslt_Command := Ada.Strings.Unbounded.To_Unbounded_String (Command);
      Handler.Scan_Files (".", Docs);
      Generate (Docs, Context.Get_Result_Directory);
   end Prepare;

   --  ------------------------------
   --  Include the document extract represented by <b>Name</b> into the document <b>Into</b>.
   --  The included document is marked so that it will not be generated.
   --  ------------------------------
   procedure Include (Docs     : in out Doc_Maps.Map;
                      Into     : in out File_Document;
                      Name     : in String;
                      Position : in Natural) is
      --  Include the lines from the document into another.
      procedure Do_Include (Source : in String;
                            Doc    : in out File_Document);

      --  ------------------------------
      --  Include the lines from the document into another.
      --  ------------------------------
      procedure Do_Include (Source : in String;
                            Doc    : in out File_Document) is
         pragma Unreferenced (Source);

         Iter : Line_Vectors.Cursor := Doc.Lines.Last;
      begin
         while Line_Vectors.Has_Element (Iter) loop
            Into.Lines.Insert (Before => Position, New_Item => Line_Vectors.Element (Iter));
            Line_Vectors.Previous (Iter);
         end loop;
         Doc.Was_Included := True;
      end Do_Include;

      Pos : constant Doc_Maps.Cursor := Docs.Find (Name);
   begin
      if not Doc_Maps.Has_Element (Pos) then
         Log.Error ("Cannot include document '{0}'", Name);
         return;
      end if;
      Docs.Update_Element (Pos, Do_Include'Access);
   end Include;

   --  ------------------------------
   --  Generate the project documentation that was collected in <b>Docs</b>.
   --  The documentation is merged so that the @include tags are replaced by the matching
   --  document extracts.
   --  ------------------------------
   procedure Generate (Docs : in out Doc_Maps.Map;
                       Dir  : in String) is
      --  Merge the documentation.
      procedure Merge (Source : in String;
                       Doc    : in out File_Document);

      --  Generate the documentation.
      procedure Generate (Source : in String;
                          Doc    : in File_Document);

      --  ------------------------------
      --  Merge the documentation.
      --  ------------------------------
      procedure Merge (Source : in String;
                       Doc    : in out File_Document) is
         pragma Unreferenced (Source);

         Pos : Natural := 1;
      begin
         while Pos <= Natural (Doc.Lines.Length) loop
            declare
               L : constant Line_Type := Line_Vectors.Element (Doc.Lines, Pos);
            begin
               if L.Kind = L_INCLUDE then
                  Line_Vectors.Delete (Doc.Lines, Pos);
                  Include (Docs, Doc, L.Content, Pos);
               else
                  Pos := Pos + 1;
               end if;
            end;
         end loop;
      end Merge;

      --  ------------------------------
      --  Generate the documentation.
      --  ------------------------------
      procedure Generate (Source : in String;
                          Doc    : in File_Document) is
         pragma Unreferenced (Source);

         procedure Write (Line : in Line_Type);

         Name : constant String := Ada.Strings.Unbounded.To_String (Doc.Name) & ".wiki";
         Path : constant String := Util.Files.Compose (Dir, Name);
         File : Ada.Text_IO.File_Type;
         Iter : Line_Vectors.Cursor := Doc.Lines.First;

         procedure Write (Line : in Line_Type) is
         begin
            Ada.Text_IO.Put_Line (File, Line.Content);
         end Write;

      begin
         if Doc.Lines.Is_Empty or Doc.Was_Included then
            return;
         end if;

         Log.Info ("Generating doc {0}", Path);
         Ada.Directories.Create_Path (Dir);

         Ada.Text_IO.Create (File => File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Path);
         Ada.Text_IO.Put_Line (File, "#summary " & Ada.Strings.Unbounded.To_String (Doc.Title));
         Ada.Text_IO.New_Line (File);

         while Line_Vectors.Has_Element (Iter) loop
            Line_Vectors.Query_Element (Iter, Write'Access);
            Line_Vectors.Next (Iter);
         end loop;
         Ada.Text_IO.Close (File);
      end Generate;

      Iter : Doc_Maps.Cursor := Docs.First;
   begin
      --  First pass: merge the documentation.
      while Doc_Maps.Has_Element (Iter) loop
         Docs.Update_Element (Position => Iter, Process => Merge'Access);
         Doc_Maps.Next (Iter);
      end loop;

      --  Second pass: build the documentation.
      Iter := Docs.First;
      while Doc_Maps.Has_Element (Iter) loop
         Doc_Maps.Query_Element (Iter, Generate'Access);
         Doc_Maps.Next (Iter);
      end loop;
   end Generate;

   procedure Scan_Files (Handler : in out Artifact;
                         Path    : in String;
                         Docs    : in out Doc_Maps.Map) is
      use Ada.Directories;

      File_Filter  : constant Filter_Type := (Ordinary_File => True,
                                              Directory     => False,
                                              others        => False);
      Dir_Filter  : constant Filter_Type := (Ordinary_File => False,
                                             Directory     => True,
                                             others        => False);
      Ent     : Ada.Directories.Directory_Entry_Type;
      Search  : Search_Type;

   begin
      Start_Search (Search, Directory => Path,
                    Pattern => "*", Filter => File_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name      : constant String := Simple_Name (Ent);
            Full_Path : constant String := Ada.Directories.Full_Name (Ent);
            Doc       : File_Document;
            Pos       : constant Natural := Util.Strings.Rindex (Name, '.');
         begin
            if Gen.Utils.Is_File_Ignored (Name) or Pos = 0 then
               Log.Debug ("File {0} ignored", Name);

            else
               Log.Debug ("Collect {0}", Full_Path);

               if Name (Pos .. Name'Last) = ".ads" then
                  Handler.Read_Ada_File (Full_Path, Doc);

               elsif Name (Pos .. Name'Last) = ".xml" then
                  Handler.Read_Xml_File (Full_Path, Doc);

               end if;
               Log.Info ("Adding document '{0}'", Name);
               Docs.Include (Name, Doc);
            end if;
         end;
      end loop;

      Start_Search (Search, Directory => Path,
                    Pattern => "*", Filter => Dir_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name      : constant String := Simple_Name (Ent);
         begin
            if not Gen.Utils.Is_File_Ignored (Name)
              and Name /= "regtests" then
               Handler.Scan_Files (Ada.Directories.Full_Name (Ent), Docs);
            end if;
         end;
      end loop;
   end Scan_Files;

   --  ------------------------------
   --  Returns True if the line indicates a bullet or numbered list.
   --  ------------------------------
   function Is_List (Line : in String) return Boolean is
   begin
      if Line'Length <= 3 then
         return False;
      else
         return Line (Line'First + 2) = '*';
      end if;
   end Is_List;

   --  ------------------------------
   --  Returns True if the line indicates a code sample.
   --  ------------------------------
   function Is_Code (Line : in String) return Boolean is
   begin
      if Line'Length <= 3 then
         return False;
      else
         return Line (Line'First) =  ' ' and Line (Line'First + 1) = ' ';
      end if;
   end Is_Code;

   --  ------------------------------
   --  Append a raw text line to the document.
   --  ------------------------------
   procedure Append_Line (Doc  : in out File_Document;
                          Line : in String) is
   begin
      Doc.Lines.Append (Line_Type '(Len => Line'Length, Kind => L_TEXT, Content => Line));
   end Append_Line;

   --  ------------------------------
   --  Look and analyze the tag defined on the line.
   --  ------------------------------
   procedure Append_Tag (Doc : in out File_Document;
                         Tag : in String) is
      use Ada.Strings.Unbounded;
      use Ada.Strings;

      Pos : constant Natural := Util.Strings.Index (Tag, ' ');
   begin
      if Pos = 0 then
         return;
      end if;
      declare
         Value : constant String := Ada.Strings.Fixed.Trim (Tag (Pos .. Tag'Last), Spaces, Spaces);
      begin
         if Tag (Tag'First .. Pos - 1) = TAG_TITLE then
            Doc.Title := To_Unbounded_String (Value);

         elsif Tag (Tag'First .. Pos - 1) = TAG_SEE then
            Doc.Lines.Append (Line_Type '(Len     => Value'Length,
                                          Kind    => L_SEE,
                                          Content => Value));

         elsif Tag (Tag'First .. Pos - 1) = TAG_INCLUDE then
            Doc.Lines.Append (Line_Type '(Len     => Value'Length,
                                          Kind    => L_INCLUDE,
                                          Content => Value));

         end if;
      end;
   end Append_Tag;

   --  ------------------------------
   --  Analyse the documentation line and collect the documentation text.
   --  ------------------------------
   procedure Append (Doc   : in out File_Document;
                     Line  : in String) is
   begin
      if Line'Length >= 1 and then Line (Line'First) = TAG_CHAR then
         Append_Tag (Doc, Line (Line'First + 1 .. Line'Last));
         return;
      end if;

      case Doc.State is
         when IN_PARA =>
            if Line'Length = 0 then
               Doc.State := IN_SEPARATOR;

            elsif Is_List (Line) then
               Doc.State := IN_LIST;
            end if;

         when IN_SEPARATOR =>
            if Is_List (Line) then
               Doc.State := IN_LIST;

            elsif Is_Code (Line) then
               Doc.State := IN_CODE;
               Append_Line (Doc, "{{{");
            end if;

         when IN_CODE =>
            if Line'Length = 0 then
               Doc.State := IN_CODE_SEPARATOR;
               return;
            end if;

         when IN_CODE_SEPARATOR =>
            if Line'Length > 0 and then Ada.Characters.Handling.Is_Letter (Line (Line'First)) then
               Append_Line (Doc, "}}}");
               Append_Line (Doc, "");
               Doc.State := IN_PARA;
            end if;

         when IN_LIST =>
            if Line'Length = 0 then
               Doc.State := IN_SEPARATOR;
            end if;
      end case;
      Append_Line (Doc, Line);
   end Append;

   --  ------------------------------
   --  After having collected the documentation, terminate the document by making sure
   --  the opened elements are closed.
   --  ------------------------------
   procedure Finish (Doc : in out File_Document) is
   begin
      if Doc.State = IN_CODE or Doc.State = IN_CODE_SEPARATOR then
         Append_Line (Doc, "}}}");
         Doc.State := IN_PARA;
      end if;
   end Finish;

   --  ------------------------------
   --  Set the name associated with the document extract.
   --  ------------------------------
   procedure Set_Name (Doc  : in out File_Document;
                       Name : in String) is
      S1 : String := Ada.Strings.Fixed.Trim (Name, Ada.Strings.Both);
   begin
      for I in S1'Range loop
         if S1 (I) = '.' or S1 (I) = '/' or S1 (I) = '\' then
            S1 (I) := '_';
         end if;
      end loop;
      Doc.Name := Ada.Strings.Unbounded.To_Unbounded_String (S1);
   end Set_Name;

   --  ------------------------------
   --  Set the title associated with the document extract.
   --  ------------------------------
   procedure Set_Title (Doc   : in out File_Document;
                        Title : in String) is
      use Ada.Strings;

      Pos : Natural := Ada.Strings.Fixed.Index (Title, " -- ");
   begin
      if Pos = 0 then
         Pos := Title'First;
      else
         Pos := Pos + 4;
      end if;
      Doc.Title := Unbounded.To_Unbounded_String (Fixed.Trim (Title (Pos .. Title'Last), Both));
   end Set_Title;

   --  ------------------------------
   --  Read the Ada specification file and collect the useful documentation.
   --  To keep the implementation simple, we don't use the ASIS packages to scan and extract
   --  the documentation.  We don't need to look at the Ada specification itself.  Instead,
   --  we assume that the Ada source follows some Ada style guidelines.
   --  ------------------------------
   procedure Read_Ada_File (Handler : in out Artifact;
                            File    : in String;
                            Result  : in out File_Document) is
      procedure Process (Line : in String);

      Done              : Boolean := False;
      Doc_Block         : Boolean := False;

      procedure Process (Line : in String) is
      begin
         Result.Line_Number := Result.Line_Number + 1;
         if Done then
            return;

         elsif Line'Length <= 1 then
            Doc_Block := False;

         elsif Line (Line'First) = '-' and Line (Line'First + 1) = '-' then
            if Doc_Block then
               if Line'Length < 4 then
                  Append (Result, "");

               elsif Line (Line'First + 2) = ' ' and Line (Line'First + 3) = ' ' then
                  Append (Result, Line (Line'First + 4 .. Line'Last));
               end if;
            elsif Line'Length >= 7 and then Line (Line'First .. Line'First + 6) = "--  == " then
               Doc_Block := True;
               Append (Result, Line (Line'First + 4 .. Line'Last));

            elsif Result.Line_Number = 2 then
               Set_Title (Result, Line (Line'First + 2 .. Line'Last));

            end if;

         else
            declare
               Pos  : Natural := Ada.Strings.Fixed.Index (Line, "package");
               Last : Natural;
            begin
               if Pos > 0 then
                  Done := True;
                  Pos := Ada.Strings.Fixed.Index (Line, " ", Pos);
                  if Pos > 0 then
                     Last := Ada.Strings.Fixed.Index (Line, " ", Pos + 1);
                     Set_Name (Result,  Line (Pos .. Last));
                  end if;
               end if;
            end;

         end if;
      end Process;
   begin
      Util.Files.Read_File (File, Process'Access);
      Finish (Result);
   end Read_Ada_File;

   --  Read the XML file and extract the documentation.  For this extraction we use
   --  an XSLT stylesheet and run the external tool <b>xstlproc</b>.
   procedure Read_Xml_File (Handler : in out Artifact;
                            File    : in String;
                            Result  : in out File_Document) is
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
      Reader  : Util.Streams.Texts.Reader_Stream;
      Name    : constant String := Ada.Directories.Base_Name (File);
      Command : constant String := Ada.Strings.Unbounded.To_String (Handler.Xslt_Command);
   begin
      Pipe.Open (Command & " " & File, Util.Processes.READ);
      Reader.Initialize (Pipe'Unchecked_Access);

      while not Reader.Is_Eof loop
         declare
            Line : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Reader.Read_Line (Line, True);
            Log.Debug ("Doc: {0}", Line);

            Append_Line (Result, Ada.Strings.Unbounded.To_String (Line));
         end;
      end loop;
      Pipe.Close;
      Set_Title (Result, Name);
      Set_Name (Result, Name);
      if Pipe.Get_Exit_Status /= 0 then
         Log.Error ("Command {0} exited with status {1}", Command,
                    Integer'Image (Pipe.Get_Exit_Status));
      end if;
   end Read_Xml_File;

end Gen.Artifacts.Docs;