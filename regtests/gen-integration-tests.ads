-----------------------------------------------------------------------
--  gen-integration-tests -- Tests for integration
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

with Ada.Strings.Unbounded;
with Util.Tests;
package Gen.Integration.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Execute the command and get the output in a string.
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Result  : out Ada.Strings.Unbounded.Unbounded_String);

   --  Test dynamo create-project command.
   procedure Test_Create_Project (T : in out Test);

   --  Test project configure.
   procedure Test_Configure (T : in out Test);

   --  Test propset command.
   procedure Test_Change_Property (T : in out Test);

   --  Test add-module command.
   procedure Test_Add_Module (T : in out Test);

end Gen.Integration.Tests;
