------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--       A S I S . D A T A _ D E C O M P O S I T I O N . S E T _ G E T      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2005, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences.  ASIS-for-GNAT is now maintained by  Ada Core Technologies Inc --
-- (http://www.gnat.com).                                                   --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains access and update routines for abstractions
--  declared in Asis.Data_Decomposition.
--
--  It also contains routines for creating lists of record and array
--  components

with A4G.Asis_Tables; use A4G.Asis_Tables;
with A4G.DDA_Aux;     use A4G.DDA_Aux;

with Table;

private package Asis.Data_Decomposition.Set_Get is

   --  Tables used to create query results which are of list types:

   package Record_Component_Table is new Table.Table (
     Table_Component_Type => Record_Component,
     Table_Index_Type     => Asis.ASIS_Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Record_Componnet_List");

   RC_Table : Record_Component_Table.Table_Ptr renames
      Record_Component_Table.Table;

   Def_N_Table : Asis_Element_Table.Table_Ptr renames
      Asis_Element_Table.Table;

   Nil_Record_Component_List : Record_Component_List (1 .. 0);
   --  Nil constant for Record_Component_List is not provided in the
   --  Asis.Data_Decomposition package.

   Parent_Type_Definition : Element;
   --  Global variable used to store the type definition from which componnets
   --  are extracted

   Record_Type_Entity : Entity_Id;
   --  Global variable used to store the type entity defining the type of
   --  a record. This is a type entity which actually defines the
   --  type, that is, it may be an implicit type as well

   procedure Set_Parent_Type_Definition (E : Element);
   --  Sets Parent_Type_Definition (Currently this is a trivial assignment,
   --  but we would better keep procedural interface in case if something
   --  more smart is really required here.

   procedure Set_Record_Type_Entity (RC : Record_Component);
   --  Sets Record_Type_Entity for RC, if Is_Record (RC)

   procedure Set_Record_Type_Entity (AC : Array_Component);
   --  Sets Record_Type_Entity for AC, if Is_Record (AC)

   procedure Set_Record_Type_Entity;
   --  Sets Record_Type_Entity by using the valuse of Parent_Type_Definition
   --  global variable

   --  Access functions to Record_Component fields:

   subtype RC is Record_Component;

   function Parent_Record_Type (Comp : RC) return Asis.Declaration;
   function Component_Name     (Comp : RC) return Asis.Defining_Name;
   function Is_Record_Comp     (Comp : RC) return Boolean;
   function Is_Array_Comp      (Comp : RC) return Boolean;
   function Parent_Discrims    (Comp : RC) return Discrim_List;

   function Get_Type_Entity    (Comp : RC) return Entity_Id;
   --  Returns type Entity describing the subtype of the component.
   --  It may be implicit type as well

   function Get_Comp_Entity (Comp : RC) return Entity_Id;
   --  Returns the Entity Id of the given record component

   function Get_Record_Entity (Comp : RC) return Entity_Id;
   --  Returns the Entity Id of the enclosing record type declaration
   --  for a given component (it may be a type derived from a record type)

   --  Array Components:

   subtype AC is Array_Component;

   function Parent_Array_Type     (Comp : AC) return Asis.Declaration;
   function Is_Record_Comp        (Comp : AC) return Boolean;
   function Is_Array_Comp         (Comp : AC) return Boolean;
   function Dimension             (Comp : AC) return ASIS_Natural;
   function Parent_Discrims       (Comp : AC) return Discrim_List;

   function Get_Array_Type_Entity (Comp : AC) return  Entity_Id;
   --  Returns the Entity_Id for the array type from which this array
   --  component is extracted. It may be the Id of some implicit array type
   --  as well

   procedure Set_Parent_Discrims (Comp : in out AC; Discs : Discrim_List);
   --  Sets Discs as Parent_Discrims for Comp. In case if Discs is
   --  Null_Discrims sets Parent_Discrims as null.

   type List_Kinds is (New_List, Append);
   --  The way of creating a list in Element or Component Table

   procedure Set_Named_Components (E : Element; List_Kind : List_Kinds);
   --  Stores all the A_Defining_Identifier components contained in E in
   --  Asis_Element_Table. If List_Kind is set to New_List, it resets
   --  Asis_Element_Table before putting any information in it. If List_Kind
   --  is set to Append, the Table is not reset, and the created list
   --  is appended to the list already stored in the table.

   procedure Set_All_Named_Components (E : Element);
   --  Provided that E is a record type definition or a definition of a
   --  derived type derived from some record type, this function
   --  stores all the A_Defining_Identifier Elements defining the
   --  componnets for this type in Asis_Element_Table. Before doing this,
   --  the procedure resets Asis_Element_Table.

   procedure Set_Record_Components_From_Names
     (Parent_First_Bit : ASIS_Natural  := 0;
      Data_Stream      : Portable_Data := Nil_Portable_Data;
      Discriminants    : Boolean       := False);
   --  Supposing that an appropriate list of component defining names is set
   --  in the Asis_Element_Table, this procedure converts them into
   --  the corresponding list of record componnets in Record_Component_Table.
   --
   --  Parent_First_Bit is needed to compute the first bit and position of
   --  the component that is extracted from another (record or array)
   --  component. Usually everything is alligned at least bytewise, but in case
   --  if representation clauses are used to create heavily packed data
   --  structure, we may need to know where to start to compute the component
   --  beginning
   --
   --  If Data_Stream parameter is set, it is used to define which
   --  components from the list set in Asis_Element_Table should be
   --  presented in the result.
   --
   --  Discriminants flag is used to indicate the case when (only)
   --  discriminant components should be constructed, in this case no
   --  discriminant constraint (explicit or default) should be taken into
   --  account (they may be dynamic in case of A_Complex_Dynamic_Model type).

   function Set_Array_Componnet
     (Array_Type_Definition      : Element;
      Enclosing_Record_Component : Record_Component := Nil_Record_Component;
      Parent_Indication          : Element          := Nil_Element;
      Parent_Discriminants       : Discrim_List     := Null_Discrims;
      Parent_First_Bit_Offset    : ASIS_Natural     := 0;
      Dynamic_Array              : Boolean          := False)
      return Array_Component;
   --  Sets and returns an Array_Component value. Array_Type_Definition
   --  parameter should represent a (constrained or unconstrained) array type
   --  definition or a derived type definition for which an ancestor type is
   --  an array type. The returned value represent the component of this array
   --  type. Enclosing_Record_Component is set when the array componnet to be
   --  created is a part of some Record_Component, in this case the
   --  corresponding component definition may contain index constraints.
   --  Parent_Indication is set when the array componnet to be created is a
   --  part of some other array component, in this case the corresponding
   --  component subtype indication may contain an index constraint
   --  ???? Documentation needs revising!!!

end Asis.Data_Decomposition.Set_Get;
