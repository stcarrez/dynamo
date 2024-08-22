-----------------------------------------------------------------------
--  gen-xmi-tests -- Tests for xmi
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
package Gen.Artifacts.XMI.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading the XMI files defines in the Dynamo UML configuration repository.
   procedure Test_Read_XMI (T : in out Test);

   --  Test searching an XMI element by using a qualified name.
   procedure Test_Find_Element (T : in out Test);

   --  Test searching an XMI Tag definition element by using its name.
   procedure Test_Find_Tag_Definition (T : in out Test);

end Gen.Artifacts.XMI.Tests;
