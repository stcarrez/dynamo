-----------------------------------------------------------------------
--  gen-model-projects -- Projects meta data
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Util.Beans.Objects;

with Gen.Model.List;
with Gen.Model.Packages;
with Gen.Model.Mappings;
package Gen.Model.Projects is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Project Definition
   --  ------------------------------
   type Project_Definition is new Definition with record
      Name : Unbounded_String;
   end record;
   type Project_Definition_Access is access all Project_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Project_Definition;
                       Name : String) return Util.Beans.Objects.Object;

end Gen.Model.Projects;
