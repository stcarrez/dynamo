-----------------------------------------------------------------------
--  gen-model-beans -- Ada Bean declarations
--  Copyright (C) 2012, 2013, 2018, 2021, 2022 Stephane Carrez
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

with Gen.Model.Mappings;
package body Gen.Model.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Bean_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "members" or else Name = "columns" then
         return From.Members_Bean;

      elsif Name = "type" then
         return UBO.To_Object (From.Type_Name);

      elsif Name = "isBean" then
         return UBO.To_Object (True);

      else
         return Tables.Table_Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Create an attribute with the given name and add it to the bean.
   --  ------------------------------
   procedure Add_Attribute (Bean   : in out Bean_Definition;
                            Name   : in UString;
                            Column : out Gen.Model.Tables.Column_Definition_Access) is
   begin
      Column := new Gen.Model.Tables.Column_Definition;
      Column.Set_Name (Name);
      Column.Sql_Name := Name;
      Column.Number   := Bean.Members.Get_Count;
      Column.Table    := Bean'Unchecked_Access;
      Bean.Members.Append (Column);
   end Add_Attribute;

   --  ------------------------------
   --  Create a table with the given name.
   --  ------------------------------
   function Create_Bean (Name : in UString) return Bean_Definition_Access is
      use Ada.Strings.Unbounded;

      Bean : constant Bean_Definition_Access := new Bean_Definition;
   begin
      Bean.Kind := Mappings.T_BEAN;
      Bean.Set_Name (Name);
      declare
         Pos : constant Natural := Index (Bean.Name, ".", Ada.Strings.Backward);
      begin
         if Pos > 0 then
            Bean.Pkg_Name := Unbounded_Slice (Bean.Name, 1, Pos - 1);
            Bean.Type_Name := Unbounded_Slice (Bean.Name, Pos + 1, Length (Bean.Name));
         else
            Bean.Pkg_Name := To_UString ("ADO");
            Bean.Type_Name := Bean.Name;
         end if;
      end;
      return Bean;
   end Create_Bean;

end Gen.Model.Beans;
