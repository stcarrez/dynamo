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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with Util.Beans.Objects;
with ASF.Applications.Main;
with ASF.Contexts.Faces;

with Gen.Model.Packages;
with Gen.Model.Projects;
with Gen.Artifacts.Hibernate;
with Gen.Artifacts.Query;
with Gen.Artifacts.Mappings;
package Gen.Generator is

   type Package_Type is (PACKAGE_MODEL, PACKAGE_FORMS);

   type Mapping_File is record
      Path   : Ada.Strings.Unbounded.Unbounded_String;
      Output : Ada.Text_IO.File_Type;
   end record;

   type Handler is new ASF.Applications.Main.Application and Gen.Artifacts.Generator with private;

   --  Initialize the generator
   procedure Initialize (H : in out Handler;
                         Config_Dir : in Ada.Strings.Unbounded.Unbounded_String);

   --  Report an error and set the exit status accordingly
   procedure Error (H : in out Handler;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "");

   --  Read the XML model file
   procedure Read_Model (H    : in out Handler;
                         File : in String);

   --  Read the XML project file
   procedure Read_Project (H    : in out Handler;
                           File : in String);

   --  Prepare the model by checking, verifying and initializing it after it is completely known.
   procedure Prepare (H : in out Handler);

   --  Tell the generator to activate the generation of the given template name.
   --  The name is a property name that must be defined in generator.properties to
   --  indicate the template file.  Several artifacts can trigger the generation
   --  of a given template.  The template is generated only once.
   procedure Add_Generation (H    : in out Handler;
                             Name : in String;
                             Mode : in Gen.Artifacts.Iteration_Mode);

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       Mode  : in Gen.Artifacts.Iteration_Mode;
                       File  : in String);

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access);

   --  Generate all the code for the templates activated through <b>Add_Generation</b>.
   procedure Generate_All (H    : in out Handler);

   --  Generate all the code generation files stored in the directory
   procedure Generate_All (H    : in out Handler;
                           Mode : in Gen.Artifacts.Iteration_Mode;
                           Name : in String);

   --  Set the directory where template files are stored.
   procedure Set_Template_Directory (H    : in out Handler;
                                     Path : in Ada.Strings.Unbounded.Unbounded_String);

   --  Set the directory where results files are generated.
   procedure Set_Result_Directory (H    : in out Handler;
                                   Path : in Ada.Strings.Unbounded.Unbounded_String);

   --  Get the result directory path.
   function Get_Result_Directory (H : in Handler) return String;

   --  Get the config directory path.
   function Get_Config_Directory (H : in Handler) return String;

   --  Get the exit status
   --  Returns 0 if the generation was successful
   --  Returns 1 if there was a generation error
   function Get_Status (H : in Handler) return Ada.Command_Line.Exit_Status;

   --  Get the configuration parameter.
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in String := "") return String;

   --  Set the force-save file mode.  When False, if the generated file exists already,
   --  an error message is reported.
   procedure Set_Force_Save (H  : in out Handler;
                             To : in Boolean);

   --  Set the project name.
   procedure Set_Project_Name (H    : in out Handler;
                               Name : in String);

   --  Save the project description and parameters.
   procedure Save_Project (H : in out Handler);

private

   use Ada.Strings.Unbounded;
   use Gen.Artifacts;

   package Template_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                     Element_Type    => Gen.Artifacts.Iteration_Mode,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");

   type Handler is new ASF.Applications.Main.Application and Gen.Artifacts.Generator with record
      Conf       : ASF.Applications.Config;

      --  Config directory.
      Config_Dir : Ada.Strings.Unbounded.Unbounded_String;

      --  Output base directory.
      Output_Dir : Ada.Strings.Unbounded.Unbounded_String;

      Model  : aliased Gen.Model.Packages.Model_Definition;
      Doc    : DOM.Core.Document;
      Root   : DOM.Core.Element;
      Status : Ada.Command_Line.Exit_Status := 0;
      File   : access Util.Beans.Objects.Object;

      --  The project document.
      Project : aliased Gen.Model.Projects.Project_Definition;
      Project_Doc : DOM.Core.Document;

      --  Hibernate XML artifact
      Hibernate : Gen.Artifacts.Hibernate.Artifact;

      --  Query generation artifact.
      Query     : Gen.Artifacts.Query.Artifact;

      --  Type mapping artifact.
      Mappings  : Gen.Artifacts.Mappings.Artifact;

      --  The list of templates that must be generated.
      Templates : Template_Map.Map;

      --  Force the saving of a generated file, even if a file already exist.
      Force_Save : Boolean := True;
   end record;

   --  Execute the lifecycle phases on the faces context.
   overriding
   procedure Execute_Lifecycle (App     : in Handler;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end Gen.Generator;
