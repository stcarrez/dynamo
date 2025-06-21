-----------------------------------------------------------------------
--  gen-model-list -- List bean interface for model objects
--  Copyright (C) 2009, 2010, 2011, 2012, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Gen.Model.List is

   --  ------------------------------
   --  Make an iterator for the list.
   --  ------------------------------
   function Iterate (Container : in List_Definition)
                     return List_Iterator.Forward_Iterator'Class is
   begin
      return Result : constant Iterator := (List => Container.Self);
   end Iterate;

   --  ------------------------------
   --  Make an iterator for the list.
   --  ------------------------------
   function Element_Value (Container : in List_Definition;
                           Pos       : in Cursor)
                           return T_Access is
      pragma Unreferenced (Container);
   begin
      return Element (Pos);
   end Element_Value;

   overriding
   function First (Object : in Iterator) return Cursor is
   begin
      return Object.List.First;
   end First;

   overriding
   function Next (Object : in Iterator;
                  Pos    : in Cursor) return Cursor is
      pragma Unreferenced (Object);

      C : Cursor := Pos;
   begin
      Next (C);
      return C;
   end Next;

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
   overriding
   function Get_Count (From : List_Definition) return Natural is
      Count : constant Natural := Natural (From.Nodes.Length);
   begin
      return Count;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out List_Definition;
                            Index : in Natural) is
   begin
      From.Row := Index;
      if Index > 0 then
         declare
            Current : constant T_Access := From.Nodes.Element (Index - 1);
            Bean    : constant Util.Beans.Basic.Readonly_Bean_Access := Current.all'Access;
         begin
            Current.Set_Index (Index);
            From.Value_Bean := UBO.To_Object (Bean, UBO.STATIC);
         end;
      else
         From.Value_Bean := UBO.Null_Object;
      end if;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : List_Definition) return UBO.Object is
   begin
      return From.Value_Bean;
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : List_Definition;
                       Name : String) return UBO.Object is
   begin
      if Name = "size" then
         return UBO.To_Object (From.Get_Count);
      else
         return UBO.Null_Object;
      end if;
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

   procedure Sort_On (List : in out List_Definition) is
      package Sorting is new Vectors.Generic_Sorting;
   begin
      Sorting.Sort (List.Nodes);
   end Sort_On;

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

   --  ------------------------------
   --  Iterate over the elements of the list executing the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (Def     : in List_Definition;
                      Process : not null access procedure (Item : in T_Access)) is
      Iter : Vectors.Cursor := Def.Nodes.First;
   begin
      while Vectors.Has_Element (Iter) loop
         Process (Vectors.Element (Iter));
         Vectors.Next (Iter);
      end loop;
   end Iterate;

end Gen.Model.List;
