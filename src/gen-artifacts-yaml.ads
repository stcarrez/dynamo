-----------------------------------------------------------------------
--  gen-artifacts-yaml -- Yaml database model files
--  Copyright (C) 2018, 2021, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Gen.Model.Packages;

package Gen.Artifacts.Yaml is

   --  ------------------------------
   --  Yaml artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with private;

   --  Read the UML/XMI model file.
   procedure Read_Model (Handler       : in out Artifact;
                         File          : in String;
                         Model         : in out Gen.Model.Packages.Model_Definition;
                         Context       : in out Generator'Class);

   --  Save the model in a YAML file.
   procedure Save_Model (Handler : in Artifact;
                         Path    : in String;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class);

private

   type Artifact is new Gen.Artifacts.Artifact with null record;

end Gen.Artifacts.Yaml;
