-----------------------------------------------------------------------
--  gen-model-mappings -- Type mappings for Code Generator
--  Copyright (C) 2011, 2012 Stephane Carrez
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

--  The <b>Gen.Model.Mappings</b> package controls the mappings to convert an XML
--  type into the Ada type.
package Gen.Model.Mappings is

   ADA_MAPPING    : constant String := "Ada05";

   MySQL_MAPPING  : constant String := "MySQL";

   SQLite_MAPPING : constant String := "SQLite";

   type Basic_Type is (T_BOOLEAN, T_INTEGER, T_DATE, T_ENUM, T_IDENTIFIER, T_STRING, T_BLOB);

   --  ------------------------------
   --  Mapping Definition
   --  ------------------------------
   type Mapping_Definition is new Definition with record
      Target        : Ada.Strings.Unbounded.Unbounded_String;
      Kind          : Basic_Type := T_INTEGER;
   end record;
   type Mapping_Definition_Access is access all Mapping_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Mapping_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Find the mapping for the given type name.
   function Find_Type (Name : in Ada.Strings.Unbounded.Unbounded_String)
                       return Mapping_Definition_Access;

   procedure Register_Type (Name    : in String;
                            Mapping : in Mapping_Definition_Access;
                            Kind    : in Basic_Type);

   --  Register a type mapping <b>From</b> that is mapped to <b>Target</b>.
   procedure Register_Type (Target        : in String;
                            From          : in String;
                            Kind          : in Basic_Type);

   --  Setup the type mapping for the language identified by the given name.
   procedure Set_Mapping_Name (Name : in String);

end Gen.Model.Mappings;
