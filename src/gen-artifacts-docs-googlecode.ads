-----------------------------------------------------------------------
--  gen-artifacts-docs-googlecode -- Artifact for Googlecode documentation format
--  Copyright (C) 2015 Stephane Carrez
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

private package Gen.Artifacts.Docs.Googlecode is

   --  Format documentation for Google Wiki syntax.
   type Document_Formatter is new Gen.Artifacts.Docs.Document_Formatter with null record;

   --  Get the document name from the file document (ex: <name>.wiki or <name>.md).
   overriding
   function Get_Document_Name (Formatter : in Document_Formatter;
                               Document  : in File_Document) return String;

end Gen.Artifacts.Docs.Googlecode;
