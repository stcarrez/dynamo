-----------------------------------------------------------------------
--  gen-testsuite -- Testsuite for gen
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

with Gen.Artifacts.XMI.Tests;
package body Gen.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Result : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      Gen.Artifacts.XMI.Tests.Add_Tests (Result);
      return Result;
   end Suite;

   procedure Initialize (Props : in Util.Properties.Manager) is
   begin
      null;
   end Initialize;

end Gen.Testsuite;
