-----------------------------------------------------------------------
--  gen-artifacts-docs-markdown -- Artifact for GitHub Markdown documentation format
--  Copyright (C) 2015, 2017, 2018, 2019, 2020, 2022 Stephane Carrez
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Util.Strings;
with Util.Log.Loggers;
package body Gen.Artifacts.Docs.Markdown is

   use type Ada.Text_IO.Positive_Count;
   use type Ada.Strings.Maps.Character_Set;

   function Has_Scheme (Link : in String) return Boolean;
   function Is_Image (Link : in String) return Boolean;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.Docs.Mark");

   Marker : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" .,;:!?)")
     or Ada.Strings.Maps.To_Set (ASCII.HT)
     or Ada.Strings.Maps.To_Set (ASCII.VT)
     or Ada.Strings.Maps.To_Set (ASCII.CR)
     or Ada.Strings.Maps.To_Set (ASCII.LF);

   --  ------------------------------
   --  Get the document name from the file document (ex: <name>.wiki or <name>.md).
   --  ------------------------------
   overriding
   function Get_Document_Name (Formatter : in Document_Formatter;
                               Document  : in File_Document) return String is
      pragma Unreferenced (Formatter);
   begin
      return Ada.Strings.Unbounded.To_String (Document.Name) & ".md";
   end Get_Document_Name;

   --  ------------------------------
   --  Start a new document.
   --  ------------------------------
   overriding
   procedure Start_Document (Formatter : in out Document_Formatter;
                             Document  : in File_Document;
                             File      : in Ada.Text_IO.File_Type) is
      pragma Unreferenced (Formatter);
   begin
      if Document.Print_Footer then
         Ada.Text_IO.Put_Line (File, "# " & Ada.Strings.Unbounded.To_String (Document.Title));
         Ada.Text_IO.New_Line (File);
      end if;
   end Start_Document;

   --  ------------------------------
   --  Return True if the link has either a http:// or a https:// scheme.
   --  ------------------------------
   function Has_Scheme (Link : in String) return Boolean is
   begin
      if Link'Length < 8 then
         return False;
      elsif Link (Link'First .. Link'First + 6) = "http://" then
         return True;
      elsif Link (Link'First .. Link'First + 7) = "https://" then
         return True;
      else
         return False;
      end if;
   end Has_Scheme;

   function Is_Image (Link : in String) return Boolean is
   begin
      if Link'Length < 4 then
         return False;
      elsif Link (Link'Last - 3 .. Link'Last) = ".png" then
         return True;
      elsif Link (Link'Last - 3 .. Link'Last) = ".jpg" then
         return True;
      elsif Link (Link'Last - 3 .. Link'Last) = ".gif" then
         return True;
      else
         Log.Info ("Link {0} not an image", Link);
         return False;
      end if;
   end Is_Image;

   procedure Write_Text_Auto_Links (Formatter : in out Document_Formatter;
                                    File      : in Ada.Text_IO.File_Type;
                                    Text      : in String) is
      Start : Natural := Text'First;
      Last  : Natural := Text'Last;
      Link  : Util.Strings.Maps.Cursor;
      Pos   : Natural;
   begin
      Log.Debug ("Auto link |{0}|", Text);
      loop
         --  Emit spaces at beginning of line or words.
         while Start <= Text'Last and then Ada.Strings.Maps.Is_In (Text (Start), Marker) loop
            Ada.Text_IO.Put (File, Text (Start));
            Start := Start + 1;
         end loop;
         exit when Start > Text'Last;

         --  Find a possible link.
         Link := Formatter.Links.Find (Text (Start .. Last));
         if Util.Strings.Maps.Has_Element (Link) then
            Ada.Text_IO.Put (File, "[");
            Ada.Text_IO.Put (File, Text (Start .. Last));
            Ada.Text_IO.Put (File, "](");
            Ada.Text_IO.Put (File, Util.Strings.Maps.Element (Link));
            Ada.Text_IO.Put (File, ")");
            Start := Last + 1;
            Last := Text'Last;
         else
            Pos := Ada.Strings.Fixed.Index (Text (Start .. Last), Marker,
                                            Going => Ada.Strings.Backward);
            if Pos = 0 then
               Ada.Text_IO.Put (File, Text (Start .. Last));
               Start := Last + 1;
               Last := Text'Last;
            else
               Last := Pos - 1;

               --  Skip spaces at end of words for the search.
               while Last > Start and then Ada.Strings.Maps.Is_In (Text (Last), Marker) loop
                  Last := Last - 1;
               end loop;
            end if;
         end if;
      end loop;
   end Write_Text_Auto_Links;

   --  ------------------------------
   --  Write a line doing some link transformation for Markdown.
   --  ------------------------------
   procedure Write_Text (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Text      : in String) is
      Pos      : Natural;
      Start    : Natural := Text'First;
      End_Pos  : Natural;
      Last_Pos : Natural;
      Link     : Util.Strings.Maps.Cursor;
   begin
      --  Transform links
      --  [Link]   -> [[Link]]
      --  [Link Title] -> [[Title|Link]]
      --
      --  Do not change the following links:
      --  [[Link|Title]]
      loop
         Pos := Util.Strings.Index (Text, '[', Start);
         if Pos = 0 or else Pos = Text'Last then
            Formatter.Write_Text_Auto_Links (File, Text (Start .. Text'Last));
            return;
         end if;
         Formatter.Write_Text_Auto_Links (File, Text (Start .. Pos - 1));

         if Pos - 1 >= Text'First
           and then (Text (Pos - 1) = '\' or else Text (Pos - 1) = '`')
         then
            Ada.Text_IO.Put (File, "[");
            Start := Pos + 1;

         --  Parse a markdown link format.
         elsif Text (Pos + 1) = '[' then
            Start := Pos + 2;
            Pos := Util.Strings.Index (Text, ']', Pos + 2);
            if Pos = 0 then
               if Is_Image (Text (Start .. Text'Last)) then
                  Ada.Text_IO.Put (File, Text (Start - 2 .. Text'Last));
               else
                  Ada.Text_IO.Put (File, Text (Start - 2 .. Text'Last));
               end if;
               return;
            end if;
            if Is_Image (Text (Start .. Pos - 1)) then
               Ada.Text_IO.Put (File, "![](");
               Ada.Text_IO.Put (File, Text (Start .. Pos - 1));
               Ada.Text_IO.Put (File, ")");
               Start := Pos + 2;
            else
               Ada.Text_IO.Put (File, Text (Start .. Pos));
               Start := Pos + 1;
            end if;
         else
            Pos := Pos + 1;
            End_Pos := Pos;
            while End_Pos < Text'Last
              and then Text (End_Pos) /= ' '
              and then Text (End_Pos) /= ']'
            loop
               End_Pos := End_Pos + 1;
            end loop;
            Last_Pos := End_Pos;
            while Last_Pos < Text'Last and then Text (Last_Pos) /= ']' loop
               Last_Pos := Last_Pos + 1;
            end loop;
            if Is_Image (Text (Pos .. Last_Pos - 1)) then
               Ada.Text_IO.Put (File, "![");
               --  Ada.Text_IO.Put (File, Text (Pos .. End_Pos));
               Ada.Text_IO.Put (File, "](");
               Ada.Text_IO.Put (File, Text (Pos .. Last_Pos - 1));
               Ada.Text_IO.Put (File, ")");
            elsif Is_Image (Text (Pos .. End_Pos)) then
               Last_Pos := Last_Pos - 1;
               Ada.Text_IO.Put (File, "![");
               Ada.Text_IO.Put (File, Text (Pos .. End_Pos));
               Ada.Text_IO.Put (File, "](");
               Ada.Text_IO.Put (File, Text (Pos .. End_Pos));
               Ada.Text_IO.Put (File, ")");
            elsif Has_Scheme (Text (Pos .. End_Pos)) then
               Ada.Text_IO.Put (File, "[");
               Ada.Text_IO.Put (File, Text (End_Pos + 1 .. Last_Pos));
               Ada.Text_IO.Put (File, "(");
               Ada.Text_IO.Put (File,
                                Ada.Strings.Fixed.Trim (Text (Pos .. End_Pos), Ada.Strings.Both));
               Ada.Text_IO.Put (File, ")");
            else
               Last_Pos := Last_Pos - 1;
               Log.Info ("Check link {0}", Text (Pos .. Last_Pos));
               Link := Formatter.Links.Find (Text (Pos .. Last_Pos));
               if Util.Strings.Maps.Has_Element (Link) then
                  Ada.Text_IO.Put (File, "[");
                  Ada.Text_IO.Put (File, Text (Pos .. Last_Pos));
                  Ada.Text_IO.Put (File, "](");
                  Ada.Text_IO.Put (File, Util.Strings.Maps.Element (Link));
                  Ada.Text_IO.Put (File, ")");
                  Last_Pos := Last_Pos + 1;
               else
                  Ada.Text_IO.Put (File, "[");
                  Ada.Text_IO.Put (File, Text (Pos .. Last_Pos));
               end if;
            end if;
            Start := Last_Pos + 1;
         end if;
      end loop;
   end Write_Text;

   --  ------------------------------
   --  Write a line in the document.
   --  ------------------------------
   procedure Write_Line (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Line      : in String) is
   begin
      if Formatter.Need_Newline then
         Ada.Text_IO.New_Line (File);
         Formatter.Need_Newline := False;
      end if;
      if Formatter.Mode = L_START_CODE and then Line'Length > 2
        and then Line (Line'First .. Line'First + 1) = "  "
      then
         Ada.Text_IO.Put_Line (File, Line (Line'First + 2 .. Line'Last));
      else
         case Formatter.Mode is
            when L_TEXT =>
               Formatter.Write_Text (File, Line);
               Ada.Text_IO.New_Line (File);

            when L_LIST | L_LIST_ITEM =>
               --  When we are in a list, make sure the continuation lines
               --  start with two spaces.
               if Line'Length > 2 and then Line (Line'First) = ' '
                 and then Line (Line'First + 1) /= ' '
               then
                  Ada.Text_IO.Put (File, ' ');
               end if;
               Formatter.Write_Text (File, Line);

            when others =>
               Ada.Text_IO.Put_Line (File, Line);

         end case;
      end if;
   end Write_Line;

   --  ------------------------------
   --  Write a line in the target document formatting the line if necessary.
   --  ------------------------------
   overriding
   procedure Write_Line (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Line      : in Line_Type) is
   begin
      case Line.Kind is
         when L_LIST =>
            Ada.Text_IO.New_Line (File);
            --  Ada.Text_IO.Put (File, Line.Content);
            Formatter.Write_Line (File, Line.Content);
            if Ada.Text_IO.Col (File) /= 1 then
               Formatter.Need_Newline := True;
            end if;
            Formatter.Mode := Line.Kind;

         when L_LIST_ITEM =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, Line.Content);
            --  Ada.Text_IO.Put (File, Line.Content);
            --  Formatter.Need_Newline := True;
            if Ada.Text_IO.Col (File) /= 1 then
               Formatter.Need_Newline := True;
            end if;

         when L_START_CODE =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, "```Ada");

         when L_END_CODE =>
            Formatter.Mode := L_TEXT;
            Formatter.Write_Line (File, "```");

         when L_TEXT =>
            if Formatter.Mode /= L_START_CODE then
               Formatter.Mode := L_TEXT;
            end if;
            Formatter.Write_Line (File, Line.Content);

         when L_HEADER_1 =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, "# " & Line.Content);
            Formatter.Mode := L_TEXT;

         when L_HEADER_2 =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, "## " & Line.Content);
            Formatter.Mode := L_TEXT;

         when L_HEADER_3 =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, "### " & Line.Content);
            Formatter.Mode := L_TEXT;

         when L_HEADER_4 =>
            Formatter.Mode := Line.Kind;
            Formatter.Write_Line (File, "#### " & Line.Content);
            Formatter.Mode := L_TEXT;

         when others =>
            null;

      end case;
   end Write_Line;

   --  ------------------------------
   --  Finish the document.
   --  ------------------------------
   overriding
   procedure Finish_Document (Formatter : in out Document_Formatter;
                              Document  : in File_Document;
                              File      : in Ada.Text_IO.File_Type;
                              Source    : in String) is
      pragma Unreferenced (Formatter);
   begin
      Ada.Text_IO.New_Line (File);
      if Document.Print_Footer then
         Ada.Text_IO.Put_Line (File, "----");
         Ada.Text_IO.Put_Line (File,
                               "[Generated by Dynamo](https://github.com/stcarrez/dynamo) from *"
                               & Source & "*");
      end if;
   end Finish_Document;

end Gen.Artifacts.Docs.Markdown;
