-----------------------------------------------------------------------
--  gen-artifacts-docs-markdown -- Artifact for GitHub Markdown documentation format
--  Copyright (C) 2015, 2018 Stephane Carrez
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
private package Gen.Artifacts.Docs.Markdown is

   --  Format documentation for GitHub Markdown syntax.
   type Document_Formatter is new Gen.Artifacts.Docs.Document_Formatter with record
      Need_Newline : Boolean := False;
      Mode         : Line_Kind := L_TEXT;
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

   --  Write a line doing some link transformation for Markdown.
   procedure Write_Text (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Text      : in String);

   --  Write a line doing some link transformation for Markdown.
   procedure Write_Text_Auto_Links (Formatter : in out Document_Formatter;
                                    File      : in Ada.Text_IO.File_Type;
                                    Text      : in String);

end Gen.Artifacts.Docs.Markdown;
