-----------------------------------------------------------------------
--  gen-artifacts-mappings -- Type mapping artifact for Code Generator
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>Gen.Artifacts.Mappings</b> package is an artifact to map XML-based types
--  into Ada types.
package Gen.Artifacts.Mappings is

   --  ------------------------------
   --  Mappings artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with null record;

   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class);

end Gen.Artifacts.Mappings;
