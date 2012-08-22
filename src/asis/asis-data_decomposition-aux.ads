------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--          A S I S . D A T A _ D E C O M P O S I T I O N . A U X           --
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
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains auxiliary routines needed by queries from
--  Asis.Data_Decomposition.

with Snames;  use Snames;

private package Asis.Data_Decomposition.Aux is

   --  If the documentation of a function which works on ASIS Element does
   --  not contain an explicit list of appropriate Element Kinds, this means,
   --  that the functon does not check the kind of its actual, and a caller
   --  is responsible for proving the correct argument.

   function Subtype_Model_Kind (S : Element) return Type_Model_Kinds;
   --  Defines Type_Model_Kind for A_Subtype_Indication Element.
   --  The current approach is:
   --
   --  - if the model kind of the type mark of S is A_Complex_Dynamic_Model
   --    or Not_A_Type_Model the result is also A_Complex_Dynamic_Model or
   --    Not_A_Type_Model respectively, regardless of the constraint imposed
   --    by S (if any); ???

   --  - if S does not contain an explicit constraint, its model kind is the
   --    same as of its type mark;
   --
   --  - if S contains an explicit constraint, then:
   --    o  if the constraint is static, the result is A_Simple_Static_Model;
   --
   --    o  if the constraint is dynamic, but the only dynamic components
   --       in this constraint are discriminants from the enclosing record
   --       type definition, then the result is A_Simple_Dynamic_Model;
   --
   --    o  otherwise the result is A_Complex_Dynamic_Model;

   type Constraint_Model_Kinds is (
      Not_A_Constraint_Model,
      --  non-defined
      Static_Constraint,
      --  constraint is defined by static expressions
      Discriminated,
      --  all the dynamic expressions in the constrain are discriminants of
      --  the enclosing record type definition
      External);
      --  the constraint contains external dynamic expressions

   function Constraint_Model_Kind (C : Element) return Constraint_Model_Kinds;
   --  Supposing that C is either index or discriminant constraint element,
   --  this function checks the model kind of this constraint.

   function Record_Model_Kind (R : Element) return Type_Model_Kinds;
   --  Provided that R is of A_Record_Type_Definition kind, defines
   --  the type model kind for it.

   function Type_Definition_From_Subtype_Mark (S : Element) return Element;
   --  Taking an Element which is a subtype mark defining a given component,
   --  this query computes the type definition for the (base) type denoted
   --  by this type mark.

   function Discriminant_Part_From_Type_Definition
     (T : Element)
      return Element;
   --  Provided that T is A_Type_Definition Element, it returns its
   --  known_discriminant_part (Nil_Element if there is no
   --  known_discriminant_part). In case of a derived type,
   --  known_discriminant_part of the parent type is returned in case if
   --  the declaration of this derived type does not have its own
   --  known_discriminant_part

   function Is_Derived_From_Record (TD : Element) return Boolean;
   function Is_Derived_From_Array  (TD : Element) return Boolean;
   --  Check if TD is A_Derived_Type_Definition element defining a type which
   --  is (directly or undirectly) derived from a record/array type.

   function Root_Record_Definition (Type_Def : Element) return Element;
   function Root_Array_Definition  (Type_Def : Element) return Element;
   --  If Is_Derived_From_Record/Is_Derived_From_Array (Type_Def), then this
   --  function returns the definition of a record type/array from which this
   --  type is derived (directly or indirectly). Otherwise it returns its
   --  argument unchanged.

   function Component_Type_Definition (E : Element) return Element;
   --  Provided that E is of A_Component_Declaration (Record_Component
   --  case) or A_Subtype_Indication (Array_Component case) kind (the function
   --  does not check this, a caller is responsible for providing the right
   --  argument), this function returns the type definition for this component
   --  (it unwinds all the subtypings, but not derivations)

   function Subtype_Entity (E : Element) return Entity_Id;
   --  Given E as A_Subtype_Indication element, this function computes the
   --  Entity Id of the corresponding type or subtype (which may be an
   --  implicit type created by the compiler as well)

   function Linear_Index
     (Inds        : Dimension_Indexes;
      Ind_Lengths : Dimention_Length;
      Conv        : Convention_Id := Convention_Ada)
      return Asis.ASIS_Natural;

   function De_Linear_Index
     (Index       : Asis.ASIS_Natural;
      D           : ASIS_Natural;
      Ind_Lengths : Dimention_Length;
      Conv        : Convention_Id := Convention_Ada)
      return Dimension_Indexes;
   --  These two functions perform index linearizetion-delinearization
   --  for DDA queries working with array componnets and indexes
   --  D is the number of dimention of an array componnet, Ind_Lengths is a
   --  list of lengths for each dimention, both these parameters should come
   --  from the caller, they should be extracted from the corresponding array
   --  component. The length of Dimension_Indexes returned by De_Linear_Index
   --  is D.
   --
   --  ???  What about checks that indexes and length are in right ranges???
   --  ???  Or should it be done in the calling context???

   function Max_Len (Component : Array_Component) return Asis.ASIS_Natural;
   --  Computes linearized length of array componnet

   function Wrong_Indexes
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return Boolean;
   --  Supposing that Componnet is not-null, this function checks if Indexes
   --  are in the expected ranges for this component (this check starts from
   --  checking that the dimentions are the same). Returns True if Indexes
   --  are NOT the appropriate indexes for the component, because this
   --  function is supposed to use as a check to detect an inappropriate
   --  argument of a query

   function Build_Discrim_List_If_Data_Presented
     (Rec          : Entity_Id;
      Data         : Asis.Data_Decomposition.Portable_Data;
      Ignore_Discs : Boolean := False)
      return         Discrim_List;
   --  This is a wrapper function for A4G.DDA_Aux.Build_Discrim_List.
   --  In case is Data is not Nil_Portable_Data, it behaves as
   --  A4G.DDA_Aux.Build_Discrim_List, otherwise if Ignore_Discs is not set
   --  ON, it tries to get the discriminant data from the discriminant
   --  constraint or default discriminant values, otherwise it returns
   --  Null_Discrims,

end Asis.Data_Decomposition.Aux;
