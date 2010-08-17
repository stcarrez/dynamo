-----------------------------------------------------------------------
--  gen-generator -- Code Generator
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with DOM.Core;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with ASF.Applications.Views;

package Gen.Generator is

   type Package_Type is (PACKAGE_MODEL, PACKAGE_FORMS);

   type Mapping_File is record
      Path   : Ada.Strings.Unbounded.Unbounded_String;
      Output : Ada.Text_IO.File_Type;
   end record;

   type Handler is limited private;

   --  Initialize the generator
   procedure Initialize (H : in out Handler);

   --  Read the XML model file
   procedure Read_Model (H    : in out Handler;
                         File : in String);

   --  Generate the code using the template file
   procedure Generate (H    : in out Handler;
                       File : in String);

   --  Set the directory where template files are stored.
   procedure Set_Template_Directory (Path : in String);

   --  Find the template path and return it
   function Get_Template_Path (H    : Handler;
                               Name : in String) return String;

private

   type Handler is new ASF.Applications.Views.View_Handler with record
      Model : DOM.Core.Node;
      Doc   : DOM.Core.Document;
      Root  : DOM.Core.Element;
   end record;

end Gen.Generator;
