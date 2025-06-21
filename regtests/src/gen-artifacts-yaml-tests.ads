-----------------------------------------------------------------------
--  gen-artifacts-yaml-tests -- Tests for YAML model files
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package Gen.Artifacts.Yaml.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading the YAML files defines in regression tests.
   procedure Test_Read_Yaml (T : in out Test);

end Gen.Artifacts.Yaml.Tests;
