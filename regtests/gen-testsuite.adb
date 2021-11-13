-----------------------------------------------------------------------
--  gen-testsuite -- Testsuite for gen
--  Copyright (C) 2012, 2018, 2021 Stephane Carrez
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

with Ada.Directories;

with Gen.Artifacts.XMI.Tests;
with Gen.Artifacts.Yaml.Tests;
with Gen.Integration.Tests;
package body Gen.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   Dir   : UString;

   function Suite return Util.Tests.Access_Test_Suite is
      Result : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      Gen.Artifacts.XMI.Tests.Add_Tests (Result);
      Gen.Artifacts.Yaml.Tests.Add_Tests (Result);
      Gen.Integration.Tests.Add_Tests (Result);
      return Result;
   end Suite;

   --  ------------------------------
   --  Get the test root directory.
   --  ------------------------------
   function Get_Test_Directory return String is
   begin
      return To_String (Dir);
   end Get_Test_Directory;

   procedure Initialize (Props : in Util.Properties.Manager) is
      pragma Unreferenced (Props);
   begin
      Dir := To_UString (Ada.Directories.Current_Directory);
   end Initialize;

end Gen.Testsuite;
