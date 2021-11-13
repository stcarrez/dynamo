-----------------------------------------------------------------------
--  gen-artifacts-yaml-tests -- Tests for YAML model files
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

with Util.Test_Caller;

with Gen.Generator;
package body Gen.Artifacts.Yaml.Tests is

   package Caller is new Util.Test_Caller (Test, "Gen.Yaml");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Gen.Yaml.Read_Model",
                       Test_Read_Yaml'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading the YAML files defines in regression tests.
   --  ------------------------------
   procedure Test_Read_Yaml (T : in out Test) is

      A : Artifact;
      G : Gen.Generator.Handler;
      C : constant String := Util.Tests.Get_Parameter ("config_dir", "config");

      Path  : constant String := Util.Tests.Get_Path ("regtests/files/users.yaml");
      Model : Gen.Model.Packages.Model_Definition;
      Iter  : Gen.Model.Packages.Package_Cursor;
      Cnt   : Natural := 0;
   begin
      Gen.Generator.Initialize (G, To_UString (C), False);
      A.Read_Model (Path, Model, G);

      Iter := Model.First;
      while Gen.Model.Packages.Has_Element (Iter) loop
         Cnt := Cnt + 1;
         Gen.Model.Packages.Next (Iter);
      end loop;
      Util.Tests.Assert_Equals (T, 1, Cnt, "Missing some packages");
   end Test_Read_Yaml;

end Gen.Artifacts.Yaml.Tests;
