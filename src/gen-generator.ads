-----------------------------------------------------------------------
--  gen-generator -- Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2018, 2022 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with Util.Beans.Objects;
with Util.Properties;

with ASF.Applications.Main;
with ASF.Contexts.Faces;
with ASF.Servlets;

with Gen.Model.Packages;
with Gen.Model.Projects;
with Gen.Artifacts.Hibernate;
with Gen.Artifacts.Query;
with Gen.Artifacts.Mappings;
with Gen.Artifacts.Distribs;
with Gen.Artifacts.XMI;
with Gen.Artifacts.Yaml;
package Gen.Generator is

   package UBO renames Util.Beans.Objects;

   --  A fatal error that prevents the generator to proceed has occurred.
   Fatal_Error : exception;

   G_URI : constant String := "http://code.google.com/p/ada-ado/generator";

   type Package_Type is (PACKAGE_MODEL, PACKAGE_FORMS);

   type Mapping_File is record
      Path   : UString;
      Output : Ada.Text_IO.File_Type;
   end record;

   type Handler is new ASF.Applications.Main.Application and Gen.Artifacts.Generator with private;

   --  Initialize the generator
   procedure Initialize (H : in out Handler;
                         Config_Dir : in UString;
                         Debug : in Boolean);

   --  Get the configuration properties.
   function Get_Properties (H : in Handler) return Util.Properties.Manager;

   --  Report an error and set the exit status accordingly
   procedure Error (H : in out Handler;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String := "");

   overriding
   procedure Error (H       : in out Handler;
                    Message : in String);

   --  Report an info message.
   procedure Info (H : in out Handler;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   --  Read the XML package file
   procedure Read_Package (H    : in out Handler;
                           File : in String);

   --  Read the model mapping types and initialize the hibernate artifact.
   procedure Read_Mappings (H    : in out Handler);

   --  Read the XML model file
   procedure Read_Model (H      : in out Handler;
                         File   : in String;
                         Silent : in Boolean);

   --  Read the model and query files stored in the application directory <b>db</b>.
   procedure Read_Models (H : in out Handler;
                          Dirname : in String);

   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   procedure Read_Project (H         : in out Handler;
                           File      : in String;
                           Recursive : in Boolean := False);

   --  Prepare the model by checking, verifying and initializing it after it is completely known.
   procedure Prepare (H : in out Handler);

   --  Finish the generation.  Some artifacts could generate other files that take into
   --  account files generated previously.
   procedure Finish (H : in out Handler);

   --  Tell the generator to activate the generation of the given template name.
   --  The name is a property name that must be defined in generator.properties to
   --  indicate the template file.  Several artifacts can trigger the generation
   --  of a given template.  The template is generated only once.
   procedure Add_Generation (H    : in out Handler;
                             Name : in String;
                             Mode : in Gen.Artifacts.Iteration_Mode;
                             Mapping : in String);

   --  Enable the generation of the Ada package given by the name.  By default all the Ada
   --  packages found in the model are generated.  When called, this enables the generation
   --  only for the Ada packages registered here.
   procedure Enable_Package_Generation (H    : in out Handler;
                                        Name : in String);

   --  Save the content generated by the template generator.
   procedure Save_Content (H       : in out Handler;
                           File    : in String;
                           Content : in UString);

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       Mode  : in Gen.Artifacts.Iteration_Mode;
                       File  : in String;
                       Save  : not null access
                         procedure (H       : in out Handler;
                                    File    : in String;
                                    Content : in UString));

   --  Generate the code using the template file
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access;
                       Save  : not null access
                         procedure (H       : in out Handler;
                                    File    : in String;
                                    Content : in UString));

   --  Generate all the code for the templates activated through <b>Add_Generation</b>.
   procedure Generate_All (H    : in out Handler);

   --  Generate all the code generation files stored in the directory
   procedure Generate_All (H    : in out Handler;
                           Mode : in Gen.Artifacts.Iteration_Mode;
                           Name : in String);

   --  Set the directory where template files are stored.
   procedure Set_Template_Directory (H    : in out Handler;
                                     Path : in UString);

   --  Set the directory where results files are generated.
   procedure Set_Result_Directory (H    : in out Handler;
                                   Path : in String);

   --  Get the result directory path.
   function Get_Result_Directory (H : in Handler) return String;

   --  Get the project plugin directory path.
   function Get_Plugin_Directory (H : in Handler) return String;

   --  Get the config directory path.
   function Get_Config_Directory (H : in Handler) return String;

   --  Get the dynamo installation directory path.
   function Get_Install_Directory (H : in Handler) return String;

   --  Return the search directories that the AWA application can use to find files.
   --  The search directories is built by using the project dependencies.
   function Get_Search_Directories (H : in Handler) return String;

   --  Get the exit status
   --  Returns 0 if the generation was successful
   --  Returns 1 if there was a generation error
   function Get_Status (H : in Handler) return Ada.Command_Line.Exit_Status;

   --  Get the configuration parameter.
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in String := "") return String;

   --  Get the configuration parameter.
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean;

   --  Set the force-save file mode.  When False, if the generated file exists already,
   --  an error message is reported.
   procedure Set_Force_Save (H  : in out Handler;
                             To : in Boolean);

   --  Set the project name.
   procedure Set_Project_Name (H    : in out Handler;
                               Name : in String);

   --  Get the project name.
   function Get_Project_Name (H : in Handler) return String;

   --  Set the project property.
   procedure Set_Project_Property (H     : in out Handler;
                                   Name  : in String;
                                   Value : in String);

   --  Get the project property identified by the given name.  If the project property
   --  does not exist, returns the default value.  Project properties are loaded
   --  by <b>Read_Project</b>.
   function Get_Project_Property (H       : in Handler;
                                  Name    : in String;
                                  Default : in String := "") return String;

   --  Set the global configuration identified by the name by pre-pending the
   --  environtment variable if it is defined.
   procedure Set_Configuration (H        : in out Handler;
                                Name     : in String;
                                Env_Name : in String);

   --  Save the project description and parameters.
   procedure Save_Project (H : in out Handler);

   --  Get the path of the last generated file.
   function Get_Generated_File (H : in Handler) return String;

   --  Update the project model through the <b>Process</b> procedure.
   procedure Update_Project (H : in out Handler;
                             Process : not null access
                               procedure (P : in out Model.Projects.Root_Project_Definition));

   --  Scan the dynamo directories and execute the <b>Process</b> procedure with the
   --  directory path.
   procedure Scan_Directories (H : in Handler;
                               Process : not null access
                                 procedure (Dir : in String));

private

   use Ada.Strings.Unbounded;
   use Gen.Artifacts;

   type Template_Context is record
      Mode    : Gen.Artifacts.Iteration_Mode;
      Mapping : UString;
   end record;

   package Template_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => UString,
                                     Element_Type    => Template_Context,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");

   type Object_Access is access all UBO.Object;

   type Handler is new ASF.Applications.Main.Application and Gen.Artifacts.Generator with record
      Conf       : ASF.Applications.Config;

      --  Config directory.
      Config_Dir : UString;

      --  Output base directory.
      Output_Dir : UString;

      Model  : aliased Gen.Model.Packages.Model_Definition;
      Status : Ada.Command_Line.Exit_Status := 0;

      --  The file that must be saved (the file attribute in <f:view>.
      File     : Object_Access;

      --  Indicates whether the file must be saved at each generation or only once.
      --  This is the mode attribute in <f:view>.
      Mode     : Object_Access;

      --  Indicates whether the file must be ignored after the generation.
      --  This is the ignore attribute in <f:view>.  It is intended to be used for the package
      --  body generation to skip that in some cases when it turns out there is no operation.
      Ignore   : Object_Access;

      --  Whether the AdaMappings.xml file was loaded or not.
      Type_Mapping_Loaded : Boolean := False;

      --  The root project document.
      Project : aliased Gen.Model.Projects.Root_Project_Definition;

      --  Hibernate XML artifact
      Hibernate : Gen.Artifacts.Hibernate.Artifact;

      --  Query generation artifact.
      Query     : Gen.Artifacts.Query.Artifact;

      --  Type mapping artifact.
      Mappings  : Gen.Artifacts.Mappings.Artifact;

      --  The distribution artifact.
      Distrib   : Gen.Artifacts.Distribs.Artifact;

      --  Ada generation from UML-XMI models.
      XMI       : Gen.Artifacts.XMI.Artifact;

      --  Yaml model files support.
      Yaml      : Gen.Artifacts.Yaml.Artifact;

      --  The list of templates that must be generated.
      Templates : Template_Map.Map;

      --  Force the saving of a generated file, even if a file already exist.
      Force_Save : Boolean := True;

      --  A fake servlet for template evaluation.
      Servlet    : ASF.Servlets.Servlet_Access;
   end record;

   --  Execute the lifecycle phases on the faces context.
   overriding
   procedure Execute_Lifecycle (App     : in Handler;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end Gen.Generator;
