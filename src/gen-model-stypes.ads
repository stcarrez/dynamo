-----------------------------------------------------------------------
--  gen-model-stypes -- Simple data type definitions
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Gen.Model.Mappings;
with Gen.Model.Packages;
package Gen.Model.Stypes is

   --  ------------------------------
   --  Simple type definition
   --  ------------------------------
   type Stype_Definition is new Mappings.Mapping_Definition with record
      Package_Def    : Gen.Model.Packages.Package_Definition_Access;
      Parent_Type    : UString;
      Type_Name      : UString;
      Nullable_Type  : UString;
      Pkg_Name       : UString;
      Sql_Type       : UString;
   end record;
   type Stype_Definition_Access is access all Stype_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Stype_Definition;
                       Name : String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Stype_Definition);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Stype_Definition);

   --  Create an simple type with its parent type.
   function Create_Stype (Name   : in UString;
                          Parent : in UString) return Stype_Definition_Access;

end Gen.Model.Stypes;
