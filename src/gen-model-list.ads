-----------------------------------------------------------------------
--  gen-model-list -- List bean interface for model objects
--  Copyright (C) 2009, 2010, 2011, 2012, 2018, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;

generic
   type T is new Gen.Model.Definition with private;
   type T_Access is access all T'Class;
package Gen.Model.List is

   --  Compare the two definitions.
   function "<" (Left, Right : in T_Access) return Boolean;

   package Vectors is
      new Ada.Containers.Vectors (Index_Type   => Natural,
                                  Element_Type => T_Access,
                                  "="          => "=");

   package Sorting is new Vectors.Generic_Sorting;

   subtype Cursor is Vectors.Cursor;
   subtype Vector is Vectors.Vector;

   function Has_Element (Position : Cursor) return Boolean
     renames Vectors.Has_Element;

   function Element (Position : Cursor) return T_Access
     renames Vectors.Element;

   procedure Next (Position : in out Cursor)
     renames Vectors.Next;

   type List_Definition is limited new Util.Beans.Basic.List_Bean with private
     with Default_Iterator  => Iterate,
     Iterator_Element  => T_Access,
     Constant_Indexing => Element_Value;

   package List_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   --  Make an iterator for the list.
   function Iterate (Container : in List_Definition)
      return List_Iterator.Forward_Iterator'Class;

   --  Get the iterator element.
   function Element_Value (Container : in List_Definition;
                           Pos       : in Cursor) return T_Access;

   --  Get the first item of the list
   function First (Def : List_Definition) return Cursor;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : List_Definition) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out List_Definition;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : List_Definition) return UBO.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : List_Definition;
                       Name : String) return UBO.Object;

   --  Append the item in the list
   procedure Append (Def  : in out List_Definition;
                     Item : in T_Access);

   --  Sort the list of items on their names.
   procedure Sort (List : in out List_Definition);

   generic
      with function "<" (Left, Right : T_Access) return Boolean is <>;
   procedure Sort_On (List : in out List_Definition);

   --  Find a definition given the name.
   --  Returns the definition object or null.
   function Find (Def  : in List_Definition;
                  Name : in String) return T_Access;

   --  Iterate over the elements of the list executing the <tt>Process</tt> procedure.
   procedure Iterate (Def     : in List_Definition;
                      Process : not null access procedure (Item : in T_Access));

private

   type List_Definition_Access is access all List_Definition;

   type List_Definition is limited new Util.Beans.Basic.List_Bean with record
      Self       : List_Definition_Access := List_Definition'Unchecked_Access;
      Nodes      : Vectors.Vector;
      Row        : Natural := 0;
      Value_Bean : UBO.Object;
   end record;

   type Iterator is limited new List_Iterator.Forward_Iterator with record
      List : List_Definition_Access;
   end record;

   overriding
   function First (Object : Iterator) return Cursor;

   overriding
   function Next (Object : Iterator;
                  Pos    : Cursor) return Cursor;

end Gen.Model.List;
