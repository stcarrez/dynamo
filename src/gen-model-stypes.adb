-----------------------------------------------------------------------
--  gen-model-stypes -- Simple data type definitions
--  Copyright (C) 2021, 2022 Stephane Carrez
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

package body Gen.Model.Stypes is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Stype_Definition;
                       Name : String) return UBO.Object is
   begin
      if Name = "parent" then
         return UBO.To_Object (From.Parent_Type);
      elsif Name = "name" then
         return UBO.To_Object (From.Type_Name);
      elsif Name = "isEnum" then
         return UBO.To_Object (False);
      elsif Name = "isDiscrete" or else Name = "isNewDiscrete" then
         return UBO.To_Object (True);
      elsif Name = "sqlType" then
         if Length (From.Sql_Type) > 0 then
            return UBO.To_Object (Mappings.Get_Type_Name (From.Sql_Type));
         else
            return UBO.To_Object (Mappings.Get_Type_Name (From.Parent_Type));
         end if;
      else
         return Mappings.Mapping_Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Stype_Definition) is
   begin
      O.Target := O.Type_Name;
   end Prepare;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Stype_Definition) is
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Create an simple type with its parent type.
   --  ------------------------------
   function Create_Stype (Name   : in UString;
                          Parent : in UString) return Stype_Definition_Access is
      use Ada.Strings.Unbounded;

      Stype : constant Stype_Definition_Access := new Stype_Definition;
   begin
      Stype.Set_Name (Name);
      declare
         Pos : constant Natural := Index (Stype.Name, ".", Ada.Strings.Backward);
      begin
         Stype.Parent_Type := Parent;
         if Pos > 0 then
            Stype.Pkg_Name := Unbounded_Slice (Stype.Name, 1, Pos - 1);
            Stype.Type_Name := Unbounded_Slice (Stype.Name, Pos + 1, Length (Stype.Name));
            Stype.Nullable_Type := "Nullable_" & Stype.Type_Name;
            --  Stype.Target := Stype.Name;
         else
            Stype.Pkg_Name := To_Unbounded_String ("ADO");
            Stype.Type_Name := Stype.Name;
            --  Stype.Target := Stype.Name;
         end if;
      end;
      return Stype;
   end Create_Stype;

end Gen.Model.Stypes;
