-----------------------------------------------------------------------
--  gen-model-beans -- Ada Bean declarations
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

package body Gen.Model.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Bean_Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "members" or Name = "columns" then
         return From.Members_Bean;

      elsif Name = "type" then
         return Util.Beans.Objects.To_Object (From.Type_Name);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Bean_Definition) is
      Iter : Gen.Model.Tables.Column_List.Cursor := O.Members.First;
   begin
      while Gen.Model.Tables.Column_List.Has_Element (Iter) loop
         Gen.Model.Tables.Column_List.Element (Iter).Prepare;
         Gen.Model.Tables.Column_List.Next (Iter);
      end loop;
   end Prepare;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Bean_Definition) is
   begin
      O.Members_Bean := Util.Beans.Objects.To_Object (O.Members'Unchecked_Access,
                                                      Util.Beans.Objects.STATIC);
   end Initialize;

   --  ------------------------------
   --  Create an attribute with the given name and add it to the bean.
   --  ------------------------------
   procedure Add_Attribute (Bean   : in out Bean_Definition;
                            Name   : in Unbounded_String;
                            Column : out Gen.Model.Tables.Column_Definition_Access) is
   begin
      Column := new Gen.Model.Tables.Column_Definition;
      Column.Name     := Name;
      Column.Sql_Name := Name;
      Column.Number   := Bean.Members.Get_Count;
      Column.Table    := Bean'Unchecked_Access;
      Bean.Members.Append (Column);
   end Add_Attribute;

   --  ------------------------------
   --  Create a table with the given name.
   --  ------------------------------
   function Create_Bean (Name : in Unbounded_String) return Bean_Definition_Access is
      Bean : constant Bean_Definition_Access := new Bean_Definition;
   begin
      Bean.Name := Name;
      declare
         Pos : constant Natural := Index (Bean.Name, ".", Ada.Strings.Backward);
      begin
         if Pos > 0 then
            Bean.Pkg_Name := Unbounded_Slice (Bean.Name, 1, Pos - 1);
            Bean.Type_Name := Unbounded_Slice (Bean.Name, Pos + 1, Length (Bean.Name));
         else
            Bean.Pkg_Name := To_Unbounded_String ("ADO");
            Bean.Type_Name := Bean.Name;
         end if;
      end;
      return Bean;
   end Create_Bean;

end Gen.Model.Beans;
