-----------------------------------------------------------------------
--  gen-artifacts-yaml -- Yaml database model files
--  Copyright (C) 2018, 2021 Stephane Carrez
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

with Gen.Model.Packages;

package Gen.Artifacts.Yaml is

   --  ------------------------------
   --  Yaml artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with private;

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
