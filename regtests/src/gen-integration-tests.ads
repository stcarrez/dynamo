-----------------------------------------------------------------------
--  gen-integration-tests -- Tests for integration
--  Copyright (C) 2012, 2013, 2014, 2016, 2017, 2018, 2021, 2023 Stephane Carrez
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

with GNAT.Source_Info;
with Util.Tests;
package Gen.Integration.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   overriding
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Result  : out UString;
                      Status  : in Natural := 0;
                      Source  : String := GNAT.Source_Info.File;
                      Line    : Natural := GNAT.Source_Info.Line);

   --  Test dynamo create-project command.
   procedure Test_Create_Project (T : in out Test);

   --  Test dynamo create-project command --ado.
   procedure Test_Create_ADO_Project (T : in out Test);

   --  Test dynamo create-project command --gtk.
   procedure Test_Create_GTK_Project (T : in out Test);

   --  Test dynamo create-project command --lib.
   procedure Test_Create_Lib_Project (T : in out Test);

   --  Test project configure.
   procedure Test_Configure (T : in out Test);

   --  Test propset command.
   procedure Test_Change_Property (T : in out Test);

   --  Test add-module command.
   procedure Test_Add_Module (T : in out Test);

   --  Test add-model command.
   procedure Test_Add_Model (T : in out Test);

   --  Test add-module-operation command.
   procedure Test_Add_Module_Operation (T : in out Test);

   --  Test add-service command.
   procedure Test_Add_Service (T : in out Test);

   --  Test add-query command.
   procedure Test_Add_Query (T : in out Test);

   --  Test add-page command.
   procedure Test_Add_Page (T : in out Test);

   --  Test add-layout command.
   procedure Test_Add_Layout (T : in out Test);

   --  Test add-ajax-form command.
   procedure Test_Add_Ajax_Form (T : in out Test);

   --  Test generate command.
   procedure Test_Generate (T : in out Test);

   --  Test help command.
   procedure Test_Help (T : in out Test);

   --  Test dist command.
   procedure Test_Dist (T : in out Test);

   --  Test dist with exclude support command.
   procedure Test_Dist_Exclude (T : in out Test);

   --  Test dist command.
   procedure Test_Info (T : in out Test);

   --  Test build-doc command.
   procedure Test_Build_Doc (T : in out Test);

   --  Test build-doc command with -pandoc.
   procedure Test_Build_Pandoc (T : in out Test);

   --  Test generate command with Hibernate XML mapping files.
   procedure Test_Generate_Hibernate (T : in out Test);

   --  Test generate command (XMI enum).
   procedure Test_Generate_XMI_Enum (T : in out Test);

   --  Test generate command (XMI Ada Bean).
   procedure Test_Generate_XMI_Bean (T : in out Test);

   --  Test generate command (XMI Ada Bean with inheritance).
   procedure Test_Generate_XMI_Bean_Table (T : in out Test);

   --  Test generate command (XMI Ada Table).
   procedure Test_Generate_XMI_Table (T : in out Test);

   --  Test generate command (XMI Associations between Tables).
   procedure Test_Generate_XMI_Association (T : in out Test);

   --  Test generate command (XMI Datatype).
   procedure Test_Generate_XMI_Datatype (T : in out Test);

   --  Test generate command using the ArgoUML file directly (runs unzip -cq and parse the output).
   procedure Test_Generate_Zargo_Association (T : in out Test);

   --  Test UML with several tables that have dependencies between each of them (non circular).
   procedure Test_Generate_Zargo_Dependencies (T : in out Test);

   --  Test UML with several tables in several packages (non circular).
   procedure Test_Generate_Zargo_Packages (T : in out Test);

   --  Test UML with serialization code.
   procedure Test_Generate_Zargo_Serialization (T : in out Test);

   --  Test UML with several errors in the UML model.
   procedure Test_Generate_Zargo_Errors (T : in out Test);

   --  Test GNAT compilation of the final project.
   procedure Test_Build (T : in out Test);

   --  Test GNAT compilation of the generated model files.
   procedure Test_Build_Model (T : in out Test);

end Gen.Integration.Tests;
