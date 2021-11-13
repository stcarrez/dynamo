-----------------------------------------------------------------------
--  gen-model-enums -- Enum definitions
--  Copyright (C) 2011, 2012, 2018, 2021 Stephane Carrez
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

with Gen.Model.List;
with Gen.Model.Mappings;
with Gen.Model.Packages;
package Gen.Model.Enums is

   type Enum_Definition is tagged;
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
                       Name : String) return UBO.Object;

   --  Compare two enum literals.
   function "<" (Left, Right : in Value_Definition_Access) return Boolean;

   package Value_List is new Gen.Model.List (T         => Value_Definition,
                                             T_Access  => Value_Definition_Access);

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Enum_Definition is new Mappings.Mapping_Definition with record
      Values         : aliased Value_List.List_Definition;
      Values_Bean    : UBO.Object;
      Package_Def    : Gen.Model.Packages.Package_Definition_Access;
      Type_Name      : UString;
      Nullable_Type  : UString;
      Pkg_Name       : UString;
      Sql_Type       : UString;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Enum_Definition;
                       Name : String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Enum_Definition);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Enum_Definition);

   --  Add an enum value to this enum definition and return the new value.
   procedure Add_Value (Enum  : in out Enum_Definition;
                        Name  : in String;
                        Value : out Value_Definition_Access);

   --  Create an enum with the given name.
   function Create_Enum (Name : in UString) return Enum_Definition_Access;

end Gen.Model.Enums;
