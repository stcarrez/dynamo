-----------------------------------------------------------------------
--  gen-artifacts -- Artifacts for Code Generator
--  Copyright (C) 2011, 2012, 2018, 2021 Stephane Carrez
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
with Ada.Finalization;

with DOM.Core;
with Util.Log;
with Gen.Model;
with Gen.Model.Packages;
with Gen.Model.Projects;

--  The <b>Gen.Artifacts</b> package represents the methods and process to prepare,
--  control and realize the code generation.
package Gen.Artifacts is

   type Iteration_Mode is (ITERATION_PACKAGE, ITERATION_TABLE);

   type Generator is limited interface and Util.Log.Logging;

   --  Report an error and set the exit status accordingly
   procedure Error (Handler : in out Generator;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String := "") is abstract;

   --  Get the config directory path.
   function Get_Config_Directory (Handler : in Generator) return String is abstract;

   --  Get the result directory path.
   function Get_Result_Directory (Handler : in Generator) return String is abstract;

   --  Get the configuration parameter.
   function Get_Parameter (Handler : in Generator;
                           Name    : in String;
                           Default : in String := "") return String is abstract;

   --  Get the configuration parameter.
   function Get_Parameter (Handler : in Generator;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean is abstract;

   --  Tell the generator to activate the generation of the given template name.
   --  The name is a property name that must be defined in generator.properties to
   --  indicate the template file.  Several artifacts can trigger the generation
   --  of a given template.  The template is generated only once.
   procedure Add_Generation (Handler : in out Generator;
                             Name    : in String;
                             Mode    : in Iteration_Mode;
                             Mapping : in String) is abstract;

   --  Scan the dynamo directories and execute the <b>Process</b> procedure with the
   --  directory path.
   procedure Scan_Directories (Handler : in Generator;
                               Process : not null access
                                 procedure (Dir : in String)) is abstract;

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   type Artifact is abstract new Ada.Finalization.Limited_Controlled with private;

   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class);

   --  Prepare the model after all the configuration files have been read and before
   --  actually invoking the generation.
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Project : in out Gen.Model.Projects.Project_Definition'Class;
                      Context : in out Generator'Class) is null;

   --  After the generation, perform a finalization step for the generation process.
   procedure Finish (Handler : in out Artifact;
                     Model   : in out Gen.Model.Packages.Model_Definition'Class;
                     Project : in out Gen.Model.Projects.Project_Definition'Class;
                     Context : in out Generator'Class) is null;

   --  Check whether this artifact has been initialized.
   function Is_Initialized (Handler : in Artifact) return Boolean;

private

   type Artifact is abstract new Ada.Finalization.Limited_Controlled with record
      Initialized : Boolean := False;
   end record;

end Gen.Artifacts;
