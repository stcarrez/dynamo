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
with Ada.Command_Line;
with Ada.Strings.Unbounded;

with ASF.Applications.Main;
with Gen.Model.Tables;
package Gen.Generator is

   type Iteration_Mode is (ITERATION_PACKAGE, ITERATION_TABLE);

   type Package_Type is (PACKAGE_MODEL, PACKAGE_FORMS);

   type Mapping_File is record
      Path   : Ada.Strings.Unbounded.Unbounded_String;
      Output : Ada.Text_IO.File_Type;
   end record;

   type Handler is limited private;

   --  Initialize the generator
   procedure Initialize (H : in out Handler);

   --  Report an error and set the exit status accordingly
   procedure Error (H : in out Handler;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "");

   --  Read the XML model file
   procedure Read_Model (H    : in out Handler;
                         File : in String);

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       Mode  : in Iteration_Mode;
                       File  : in String);

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access);

   --  Generate all the code generation files stored in the directory
   procedure Generate_All (H    : in out Handler;
                           Mode : in Iteration_Mode;
                           Name : in String);

   --  Set the directory where template files are stored.
   procedure Set_Template_Directory (H    : in out Handler;
                                     Path : in String);

   --  Set the directory where results files are generated.
   procedure Set_Result_Directory (H    : in out Handler;
                                   Path : in String);

   --  Register a model mapping
   procedure Register_Mapping (H    : in out Handler;
                               Node : in DOM.Core.Node);

   --  Get the exit status
   --  Returns 0 if the generation was successful
   --  Returns 1 if there was a generation error
   function Get_Status (H : in Handler) return Ada.Command_Line.Exit_Status;

private

   type Handler is new ASF.Applications.Main.Application with record
      Conf   : ASF.Applications.Config;
      Model  : aliased Gen.Model.Tables.Model_Definition;
      Doc    : DOM.Core.Document;
      Root   : DOM.Core.Element;
      Status : Ada.Command_Line.Exit_Status := 0;
   end record;

end Gen.Generator;
