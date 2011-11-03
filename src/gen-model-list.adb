-----------------------------------------------------------------------
--  gen-model-list -- List bean interface for model objects
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
package body Gen.Model.List is

   --  ------------------------------
   --  Compare the two definitions.
   --  ------------------------------
   function "<" (Left, Right : in T_Access) return Boolean is
      Left_Name  : constant String := Left.Get_Name;
      Right_Name : constant String := Right.Get_Name;
   begin
      return Left_Name < Right_Name;
   end "<";

   --  ------------------------------
   --  Get the first item of the list
   --  ------------------------------
   function First (Def : List_Definition) return Cursor is
   begin
      return Def.Nodes.First;
   end First;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   function Get_Count (From : List_Definition) return Natural is
      Count : constant Natural := Natural (From.Nodes.Length);
   begin
      return Count;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   procedure Set_Row_Index (From  : in out List_Definition;
                            Index : in Natural) is
   begin
      From.Row := Index;
      if Index > 0 then
         declare
            Current : constant T_Access := From.Nodes.Element (Index - 1);
            Bean    : constant Util.Beans.Basic.Readonly_Bean_Access := Current.all'Access;
         begin
            From.Value_Bean := Util.Beans.Objects.To_Object (Bean, Util.Beans.Objects.STATIC);
         end;
      else
         From.Value_Bean := Util.Beans.Objects.Null_Object;
      end if;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   function Get_Row (From  : List_Definition) return Util.Beans.Objects.Object is
   begin
      return From.Value_Bean;
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : List_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "size" then
         return Util.Beans.Objects.To_Object (From.Get_Count);

      elsif Name = "rowIndex" then
         return Util.Beans.Objects.To_Object (From.Row);

      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Append the item in the list
   --  ------------------------------
   procedure Append (Def  : in out List_Definition;
                     Item : in T_Access) is
   begin
      Def.Nodes.Append (Item);
   end Append;

   --  ------------------------------
   --  Sort the list of items on their names.
   --  ------------------------------
   procedure Sort (List : in out List_Definition) is
   begin
      Sorting.Sort (List.Nodes);
   end Sort;

   --  ------------------------------
   --  Find a definition given the name.
   --  Returns the definition object or null.
   --  ------------------------------
   function Find (Def  : in List_Definition;
                  Name : in String) return T_Access is
      Iter : Vectors.Cursor := Def.Nodes.First;
   begin
      while Vectors.Has_Element (Iter) loop
         if Vectors.Element (Iter).Get_Name = Name then
            return Vectors.Element (Iter);
         end if;
         Vectors.Next (Iter);
      end loop;
      return null;
   end Find;

end Gen.Model.List;
