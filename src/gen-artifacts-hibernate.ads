-----------------------------------------------------------------------
--  gen-artifacts-hibernate -- Hibernate artifact for Code Generator
--  Copyright (C) 2011, 2012, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with DOM.Core;
with Gen.Model.Packages;

--  The <b>Gen.Artifacts</b> package represents the methods and process to prepare,
--  control and realize the code generation.
package Gen.Artifacts.Hibernate is

   --  ------------------------------
   --  Model Definition
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

   --  After the generation, perform a finalization step for the generation process.
   --  For each database SQL mapping, collect the schema files from the different modules
   --  of the project and generate an SQL script that can be used to create the database tables.
   overriding
   procedure Finish (Handler : in out Artifact;
                     Model   : in out Gen.Model.Packages.Model_Definition'Class;
                     Project : in out Gen.Model.Projects.Project_Definition'Class;
                     Context : in out Generator'Class);

private

   type Artifact is new Gen.Artifacts.Artifact with null record;

end Gen.Artifacts.Hibernate;
