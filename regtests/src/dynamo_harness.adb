-----------------------------------------------------------------------
--  Dynamo-test -- Unit tests
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Gen.Testsuite;

procedure Dynamo_Harness is

   procedure Harness is new Util.Tests.Harness (Gen.Testsuite.Suite,
                                                Gen.Testsuite.Initialize);

begin
   Harness ("dynamo-tests.xml");
end Dynamo_Harness;
