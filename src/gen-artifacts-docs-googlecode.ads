-----------------------------------------------------------------------
--  gen-artifacts-docs-googlecode -- Artifact for Googlecode documentation format
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

private package Gen.Artifacts.Docs.Googlecode is

   --  Format documentation for Google Wiki syntax.
   type Document_Formatter is new Gen.Artifacts.Docs.Document_Formatter with record
      Need_Newline : Boolean := False;
   end record;

   --  Get the document name from the file document (ex: <name>.wiki or <name>.md).
   overriding
   function Get_Document_Name (Formatter : in Document_Formatter;
                               Document  : in File_Document) return String;

   --  Start a new document.
   overriding
   procedure Start_Document (Formatter : in out Document_Formatter;
                             Document  : in File_Document;
                             File      : in Ada.Text_IO.File_Type);

   --  Write a line in the target document formatting the line if necessary.
   overriding
   procedure Write_Line (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Line      : in Line_Type);

   --  Finish the document.
   overriding
   procedure Finish_Document (Formatter : in out Document_Formatter;
                              Document  : in File_Document;
                              File      : in Ada.Text_IO.File_Type;
                              Source    : in String);

   --  Write a line in the document.
   procedure Write_Line (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Line      : in String);

end Gen.Artifacts.Docs.Googlecode;
