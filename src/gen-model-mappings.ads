-----------------------------------------------------------------------
--  gen-model-mappings -- Type mappings for Code Generator
--  Copyright (C) 2011, 2012, 2015, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded.Hash;

--  The <b>Gen.Model.Mappings</b> package controls the mappings to convert an XML
--  type into the Ada type.
package Gen.Model.Mappings is

   ADA_MAPPING    : constant String := "Ada05";

   MySQL_MAPPING  : constant String := "MySQL";

   SQLite_MAPPING : constant String := "SQLite";

   Postgresql_MAPPING : constant String := "Postgresql";

   type Basic_Type is (T_BOOLEAN,
                       T_INTEGER,
                       T_DATE,
                       T_ENUM,
                       T_IDENTIFIER,
                       T_STRING,
                       T_FLOAT,
                       T_BLOB,
                       T_ENTITY_TYPE,
                       T_BEAN,
                       T_TABLE);

   --  ------------------------------
   --  Mapping Definition
   --  ------------------------------
   type Mapping_Definition is tagged;
   type Mapping_Definition_Access is access all Mapping_Definition'Class;

   type Mapping_Definition is new Definition with record
      Target        : UString;
      Kind          : Basic_Type := T_INTEGER;
      Allow_Null    : Mapping_Definition_Access;
      Nullable      : Boolean := False;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Mapping_Definition;
                       Name : String) return UBO.Object;

   --  Find the mapping for the given type name.
   function Find_Type (Name       : in UString;
                       Allow_Null : in Boolean)
                       return Mapping_Definition_Access;

   --  Get the type name according to the mapping definition.
   function Get_Type_Name (Name : in UString) return String;

   --  Get the type name.
   function Get_Type_Name (From : Mapping_Definition) return String;

   --  Get the type name for the code template generator.
   --  (See Gen.Generator.To_Ada_Type)
   function To_Ada_Type (From : in Mapping_Definition;
                         Mode : in Natural) return UBO.Object;

   procedure Register_Type (Name    : in String;
                            Mapping : in Mapping_Definition_Access;
                            Kind    : in Basic_Type);

   --  Register a type mapping <b>From</b> that is mapped to <b>Target</b>.
   procedure Register_Type (Target        : in String;
                            From          : in String;
                            Kind          : in Basic_Type;
                            Allow_Null    : in Boolean);

   --  Setup the type mapping for the language identified by the given name.
   procedure Set_Mapping_Name (Name : in String);

   package Mapping_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => UString,
                                     Element_Type    => Mapping_Definition_Access,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => Ada.Strings.Unbounded."=");

   subtype Map is Mapping_Maps.Map;

   subtype Cursor is Mapping_Maps.Cursor;

end Gen.Model.Mappings;
