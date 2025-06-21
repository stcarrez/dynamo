-----------------------------------------------------------------------
--  gen-testsuite -- Testsuite for gen
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Util.Properties;
package Gen.Testsuite is

   function Suite return Util.Tests.Access_Test_Suite;

   --  Get the test root directory.
   function Get_Test_Directory return String;

   procedure Initialize (Props : in Util.Properties.Manager);

end Gen.Testsuite;
