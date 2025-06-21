-----------------------------------------------------------------------
--  gen-artifacts-query -- Query artifact for Code Generator
--  Copyright (C) 2011, 2012, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with DOM.Core;
with Gen.Model.Packages;

--  The <b>Gen.Artifacts.Query</b> package is an artifact for the generation of
--  data structures returned by queries.
package Gen.Artifacts.Query is

   --  ------------------------------
   --  Query artifact
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

private

   type Artifact is new Gen.Artifacts.Artifact with null record;

end Gen.Artifacts.Query;
