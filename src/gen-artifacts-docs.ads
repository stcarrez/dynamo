-----------------------------------------------------------------------
--  gen-artifacts-docs -- Artifact for documentation
--  Copyright (C) 2012, 2015, 2017, 2018, 2019, 2020, 2021 Stephane Carrez
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
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO;

with Gen.Model.Packages;
private with Util.Strings.Maps;

--  with Asis;
--  with Asis.Text;
--  with Asis.Elements;
--  with Asis.Exceptions;
--  with Asis.Errors;
--  with Asis.Implementation;
--  with Asis.Elements;
--  with Asis.Declarations;
--  The <b>Gen.Artifacts.Docs</b> package is an artifact for the generation of
--  application documentation.  Its purpose is to scan the project source files
--  and extract some interesting information for a developer's guide.  The artifact
--  scans the Ada source files, the XML configuration files, the XHTML files.
--
--  The generated documentation is intended to be published on a web site.
--  The Google Wiki style is generated by default.
--
--  1/ In the first step, the project files are scanned and the useful
--     documentation is extracted.
--
--  2/ In the second step, the documentation is merged and reconciled.  Links to
--     documentation pages and references are setup and the final pages are generated.
--
--  Ada
--  ---
--  The documentation starts at the first '== TITLE ==' marker and finishes before the
--  package specification.
--
--  XHTML
--  -----
--  Same as Ada.
--
--  XML Files
--  ----------
--  The documentation is part of the XML and the <b>documentation</b> or <b>description</b>
--  tags are extracted.
package Gen.Artifacts.Docs is

   --  Tag marker (same as Java).
   TAG_CHAR     : constant Character := '@';

   --  Specific tags recognized when analyzing the documentation.
   TAG_AUTHOR         : constant String := "author";
   TAG_TITLE          : constant String := "title";
   TAG_INCLUDE        : constant String := "include";
   TAG_INCLUDE_CONFIG : constant String := "include-config";
   TAG_INCLUDE_BEAN   : constant String := "include-bean";
   TAG_INCLUDE_QUERY  : constant String := "include-query";
   TAG_INCLUDE_PERM   : constant String := "include-permission";
   TAG_INCLUDE_DOC    : constant String := "include-doc";
   TAG_SEE            : constant String := "see";

   Unknown_Tag : exception;

   type Doc_Format is (DOC_MARKDOWN, DOC_WIKI_GOOGLE);

   --  ------------------------------
   --  Documentation artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with private;

   --  Prepare the model after all the configuration files have been read and before
   --  actually invoking the generation.
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Project : in out Gen.Model.Projects.Project_Definition'Class;
                      Context : in out Generator'Class);

   procedure Generate (Handler : in out Artifact;
                       Context : in out Generator'Class);

   --  Set the output document format to generate.
   procedure Set_Format (Handler : in out Artifact;
                         Format  : in Doc_Format;
                         Footer  : in Boolean);

   --  Load from the file a list of link definitions which can be injected in the generated doc.
   --  This allows to avoid polluting the Ada code with external links.
   procedure Read_Links (Handler : in out Artifact;
                         Path    : in String);

private

   type Line_Kind is (L_TEXT, L_LIST, L_LIST_ITEM, L_SEE, L_INCLUDE, L_INCLUDE_CONFIG,
                      L_INCLUDE_BEAN, L_INCLUDE_PERMISSION, L_INCLUDE_QUERY,
                      L_INCLUDE_DOC,
                      L_START_CODE, L_END_CODE,
                      L_HEADER_1, L_HEADER_2, L_HEADER_3, L_HEADER_4);

   subtype Line_Include_Kind is Line_Kind range L_INCLUDE .. L_INCLUDE_DOC;

   type Line_Type (Len : Natural) is record
      Kind    : Line_Kind := L_TEXT;
      Content : String (1 .. Len);
   end record;

   package Line_Vectors is
      new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                             Element_Type => Line_Type);

   type Line_Group_Vector is array (Line_Include_Kind) of Line_Vectors.Vector;

   type Doc_State is (IN_PARA, IN_SEPARATOR, IN_CODE, IN_CODE_SEPARATOR, IN_LIST);

   type Document_Formatter is abstract tagged record
      Links : Util.Strings.Maps.Map;
   end record;
   type Document_Formatter_Access is access all Document_Formatter'Class;

   type File_Document is record
      Name         : UString;
      Title        : UString;
      State        : Doc_State := IN_PARA;
      Line_Number  : Natural := 0;
      Lines        : Line_Group_Vector;
      Was_Included : Boolean := False;
      Print_Footer : Boolean := True;
      Formatter    : Document_Formatter_Access;
   end record;

   --  Get the document name from the file document (ex: <name>.wiki or <name>.md).
   function Get_Document_Name (Formatter : in Document_Formatter;
                               Document  : in File_Document) return String is abstract;

   --  Start a new document.
   procedure Start_Document (Formatter : in out Document_Formatter;
                             Document  : in File_Document;
                             File      : in Ada.Text_IO.File_Type) is abstract;

   --  Write a line in the target document formatting the line if necessary.
   procedure Write_Line (Formatter : in out Document_Formatter;
                         File      : in Ada.Text_IO.File_Type;
                         Line      : in Line_Type) is abstract;

   --  Finish the document.
   procedure Finish_Document (Formatter : in out Document_Formatter;
                              Document  : in File_Document;
                              File      : in Ada.Text_IO.File_Type;
                              Source    : in String) is abstract;

   package Doc_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => File_Document,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   --  Include the document extract represented by <b>Name</b> into the document <b>Into</b>.
   --  The included document is marked so that it will not be generated.
   procedure Include (Docs     : in out Doc_Maps.Map;
                      Source   : in String;
                      Into     : in out File_Document;
                      Name     : in String;
                      Mode     : in Line_Include_Kind;
                      Position : in Natural);

   --  Generate the project documentation that was collected in <b>Docs</b>.
   --  The documentation is merged so that the @include tags are replaced by the matching
   --  document extracts.
   procedure Generate (Handler : in out Artifact;
                       Docs    : in out Doc_Maps.Map;
                       Dir     : in String);

   --  Returns True if the line indicates a bullet or numbered list.
   function Is_List (Line : in String) return Boolean;

   --  Returns True if the line indicates a code sample.
   function Is_Code (Line : in String) return Boolean;

   --  Append a raw text line to the document.
   procedure Append_Line (Doc  : in out File_Document;
                          Line : in String);

   --  Look and analyze the tag defined on the line.
   procedure Append_Tag (Doc : in out File_Document;
                         Tag : in String);

   --  Analyse the documentation line and collect the documentation text.
   procedure Append (Doc   : in out File_Document;
                     Line  : in String);

   --  After having collected the documentation, terminate the document by making sure
   --  the opened elements are closed.
   procedure Finish (Doc : in out File_Document);

   --  Set the name associated with the document extract.
   procedure Set_Name (Doc  : in out File_Document;
                       Name : in String);

   --  Set the title associated with the document extract.
   procedure Set_Title (Doc   : in out File_Document;
                        Title : in String);

   --  Scan the files in the directory referred to by <b>Path</b> and collect the documentation
   --  in the <b>Docs</b> hashed map.
   procedure Scan_Files (Handler : in out Artifact;
                         Path    : in String;
                         Docs    : in out Doc_Maps.Map);

   --  Read the Ada specification/body file and collect the useful documentation.
   --  To keep the implementation simple, we don't use the ASIS packages to scan and extract
   --  the documentation.  We don't need to look at the Ada specification itself.  Instead,
   --  we assume that the Ada source follows some Ada style guidelines.
   procedure Read_Ada_File (Handler : in out Artifact;
                            File    : in String;
                            Result  : in out File_Document);

   procedure Read_Xml_File (Handler : in out Artifact;
                            File    : in String;
                            Result  : in out File_Document);

   --  Read some general purpose documentation files.  The documentation file
   --  can be integrated and merged by using the @include-doc tags and it may
   --  contain various @ tags.
   procedure Read_Doc_File (Handler : in out Artifact;
                            File    : in String;
                            Result  : in out File_Document);

   type Artifact is new Gen.Artifacts.Artifact with record
      Xslt_Command : UString;
      Format       : Doc_Format := DOC_WIKI_GOOGLE;
      Print_Footer : Boolean := True;
      Formatter    : Document_Formatter_Access;
   end record;

end Gen.Artifacts.Docs;
