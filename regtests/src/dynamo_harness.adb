-----------------------------------------------------------------------
--  Dynamo-test -- Unit tests
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Tests;
with Gen.Testsuite;

procedure Dynamo_Harness is

   procedure Harness is new Util.Tests.Harness (Gen.Testsuite.Suite,
                                                Gen.Testsuite.Initialize);

begin
   Harness ("dynamo-tests.xml");
end Dynamo_Harness;
