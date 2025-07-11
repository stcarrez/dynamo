-----------------------------------------------------------------------
--  gen-artifacts-docs-googlecode -- Artifact for Googlecode documentation format
--  Copyright (C) 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Gen.Artifacts.Docs.Googlecode is

   --  ------------------------------
   --  Get the document name from the file document (ex: <name>.wiki or <name>.md).
   --  ------------------------------
   overriding
   function Get_Document_Name (Formatter : in Document_Formatter;
                               Document  : in File_Document) return String is
      pragma Unreferenced (Formatter);
   begin
      return Ada.Strings.Unbounded.To_String (Document.Name) & ".wiki";
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
      Ada.Text_IO.Put_Line (File, "#summary " & Ada.Strings.Unbounded.To_String (Document.Title));
      Ada.Text_IO.New_Line (File);
   end Start_Document;

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
      Ada.Text_IO.Put_Line (File, Line);
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
            Ada.Text_IO.Put (File, Line.Content);
            Formatter.Need_Newline := True;

         when L_LIST_ITEM =>
            Ada.Text_IO.Put (File, Line.Content);
            Formatter.Need_Newline := True;

         when L_START_CODE =>
            Formatter.Write_Line (File, "{{{");

         when L_END_CODE =>
            Formatter.Write_Line (File, "}}}");

         when L_TEXT =>
            Formatter.Write_Line (File, Line.Content);

         when L_HEADER_1 =>
            Formatter.Write_Line (File, "= " & Line.Content & " =");

         when L_HEADER_2 =>
            Formatter.Write_Line (File, "== " & Line.Content & " ==");

         when L_HEADER_3 =>
            Formatter.Write_Line (File, "=== " & Line.Content & " ===");

         when L_HEADER_4 =>
            Formatter.Write_Line (File, "==== " & Line.Content & " ====");

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
                               "[https://github.com/stcarrez/dynamo Generated by Dynamo] from _"
                               & Source & "_");
      end if;
   end Finish_Document;

end Gen.Artifacts.Docs.Googlecode;
