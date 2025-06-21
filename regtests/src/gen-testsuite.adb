-----------------------------------------------------------------------
--  gen-testsuite -- Testsuite for gen
--  Copyright (C) 2012, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
