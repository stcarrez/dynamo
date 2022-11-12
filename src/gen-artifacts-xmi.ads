-----------------------------------------------------------------------
--  gen-artifacts-xmi -- UML-XMI artifact for Code Generator
--  Copyright (C) 2012, 2013, 2014, 2016, 2021, 2022 Stephane Carrez
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
with Gen.Model.Packages;
with Gen.Model.XMI;
with Util.Strings.Sets;

--  The <b>Gen.Artifacts.XMI</b> package is an artifact for the generation of Ada code
--  from an UML XMI description.
package Gen.Artifacts.XMI is

   --  ------------------------------
   --  UML XMI artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with private;

   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class);

   --  Prepare the model after all the configuration files have been read and before
   --  actually invoking the generation.
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Project : in out Gen.Model.Projects.Project_Definition'Class;
                      Context : in out Generator'Class);

   --  Read the UML/XMI model file.
   procedure Read_Model (Handler       : in out Artifact;
                         File          : in String;
                         Profile       : in String;
                         Context       : in out Generator'Class;
                         Is_Predefined : in Boolean := False);

private

   --  Read the UML profiles that are referenced by the current models.
   --  The UML profiles are installed in the UML config directory for dynamo's installation.
   procedure Read_Profiles (Handler : in out Artifact;
                            Context : in out Generator'Class);

   type Artifact is new Gen.Artifacts.Artifact with record
      Nodes      : aliased Gen.Model.XMI.UML_Model;

      --  A set of profiles that are necessary for the model definitions.
      Profiles   : aliased Util.Strings.Sets.Set;

      Has_Config : Boolean := False;

      --  Stereotype which triggers the generation of database table.
      Table_Stereotype      : Gen.Model.XMI.Stereotype_Element_Access;
      PK_Stereotype         : Gen.Model.XMI.Stereotype_Element_Access;
      FK_Stereotype         : Gen.Model.XMI.Stereotype_Element_Access;
      Data_Model_Stereotype : Gen.Model.XMI.Stereotype_Element_Access;
      Nullable_Stereotype   : Gen.Model.XMI.Stereotype_Element_Access;
      Not_Null_Stereotype   : Gen.Model.XMI.Stereotype_Element_Access;
      Use_FK_Stereotype     : Gen.Model.XMI.Stereotype_Element_Access;
      Version_Stereotype    : Gen.Model.XMI.Stereotype_Element_Access;
      Auditable_Stereotype  : Gen.Model.XMI.Stereotype_Element_Access;

      --  Tag definitions which control the generation.
      Has_List_Tag          : Gen.Model.XMI.Tag_Definition_Element_Access;
      Table_Name_Tag        : Gen.Model.XMI.Tag_Definition_Element_Access;
      Sql_Type_Tag          : Gen.Model.XMI.Tag_Definition_Element_Access;
      Generator_Tag         : Gen.Model.XMI.Tag_Definition_Element_Access;
      Literal_Tag           : Gen.Model.XMI.Tag_Definition_Element_Access;
      Sql_Length_Tag        : Gen.Model.XMI.Tag_Definition_Element_Access;
      Assoc_Order_Tag       : Gen.Model.XMI.Tag_Definition_Element_Access;

      --  Stereotype which triggers the generation of AWA bean types.
      Bean_Stereotype         : Gen.Model.XMI.Stereotype_Element_Access;
      Limited_Bean_Stereotype : Gen.Model.XMI.Stereotype_Element_Access;

      --  Stereotype which triggers the generation of serialization.
      Serialize_Stereotype    : Gen.Model.XMI.Stereotype_Element_Access;
   end record;

   function Find_Profile (Name : in String;
                          Path : in String) return String;

end Gen.Artifacts.XMI;
