-----------------------------------------------------------------------
--  gen-model-enums -- Enum definitions
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

with Ada.Strings.Unbounded;

with Util.Beans.Objects;

with Gen.Model.List;
with Gen.Model.Mappings;
with Gen.Model.Packages;
package Gen.Model.Enums is

   use Ada.Strings.Unbounded;

   type Enum_Definition;
   type Enum_Definition_Access is access all Enum_Definition'Class;

   --  ------------------------------
   --  Enum value definition
   --  ------------------------------
   type Value_Definition is new Definition with record
      Number : Natural := 0;
      Enum   : Enum_Definition_Access;
   end record;
   type Value_Definition_Access is access all Value_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Value_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   package Value_List is new Gen.Model.List (T         => Value_Definition,
                                             T_Access  => Value_Definition_Access);

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Enum_Definition is new Mappings.Mapping_Definition with record
      Values         : aliased Value_List.List_Definition;
      Values_Bean    : Util.Beans.Objects.Object;
      Package_Def    : Gen.Model.Packages.Package_Definition_Access;
      Type_Name      : Unbounded_String;
      Pkg_Name       : Unbounded_String;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Enum_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Enum_Definition);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Enum_Definition);

end Gen.Model.Enums;
