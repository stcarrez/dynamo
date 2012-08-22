------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--   A S I S . D A T A _ D E C O M P O S I T I O N . E X T E N S I O N S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2008, Free Software Foundation, Inc.       --
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

--  This package contains queries yielding various representation information
--  which may be useful for ASIS applications, and which can not be obtained
--  through the Asis.Data_Decomposition package as defined by the ASIS
--  Standard.

package Asis.Data_Decomposition.Extensions is

   generic
      type Constrained_Subtype is private;
   function Portable_Data_Value
     (Value : Constrained_Subtype)
      return Portable_Data;
   --  This is the inverse function for
   --  Asis.Data_Decomposition.Portable_Constrained_Subtype. Instantiated with
   --  an appropriate scalar type, (e.g., System.Integer, can be used to
   --  convert value to a data stream that can be used by ASIS Data
   --  Decomposition queries, in particular, for passing as an actual for the
   --  Value parameter of
   --  Asis.Data_Decomposition.Construct_Artificial_Data_Stream.
   --
   --  Instantiated with a record type, can be used to convert a value to a
   --  data stream that can be analyzed by ASIS Data Decomposition queries.

   function Component_Name_Definition
     (Component : Record_Component)
      return      Asis.Declaration;
   --  For the argument Compononent, returns the corresponding
   --  A_Defining_Identified Element.
   --  Asis.Data_Decomposition.Component_Declaration query can not be used to
   --  determine the name of the component in case if two or more record
   --  components are defined by the same component declaration or discriminant
   --  specification, that's why this query may be needed.
   --
   --  All non-Nil component values are appropriate.
   --
   --  Returns Defining_Name_Kinds:
   --       A_Defining_Identifier

   ----------------------------
   -- Static type attributes --
   ----------------------------

   --  Queries defined in this section returns static representation
   --  attributes of types and subtypes. Some, but not all of the attributes
   --  defined in RM 95 which return representation information about types
   --  and subtypes and which are not functions are mapped onto
   --  Asis.Data_Decomposition.Extensions queries.

   --  --|AN Application Note:
   --
   --  In contrast to Asis.Data_Decomposition queries which operates on type
   --  definitions, these queries operates on type and subtype defining names.
   --  The reason is that type and its subtypes may have different values of
   --  the same representation attribute, and this difference may be
   --  important for application.

   --------------------------------------------------------
   -- Floating Point Types and Decimal Fixed Point Types --
   --------------------------------------------------------

   function Digits_Value
     (Floating_Point_Subtype : Asis.Element)
      return                   ASIS_Natural;
   --  Provided that Floating_Point_Subtype is the defining name of some
   --  floating point or decimal fixed point type or subtype (including types
   --  derived from floating point or decimal fixed point types), this
   --  function returns the requested decimal precision for this type or
   --  subtype, as defined in RM 95, 3.5.8(2), 3.5.10(7)
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some floating
   --                            point or decimal fixed point type
   --                            or subtype

   -----------------------------------------------------
   -- Fixed Point Types and Decimal Fixed Point Types --
   -----------------------------------------------------

   --  --|AN Application Note:
   --
   --  For fixed point types, it is important to return the precise values of
   --  'small' and 'delta'. The corresponding Ada attributes 'Small and 'Delta
   --  are defined in RM95 as being of universal_real type. But in this unit,
   --  we can not use universal_real as a return type of a query, and using
   --  any explicitly defined real type can not guarantee that the exact
   --  values of 'small' and 'delta' will be returned by the corresponding
   --  queries, since these values have arbitrary exact precision.
   --
   --  To represent the value of universal_real type, the Small_Value
   --  and Delta_Value functions return a pair of integers representing
   --  the normalized fraction with integer numerator and denominator
   --  ("normalized means that the fraction is reduced to lowest terms),
   --  or alternatively a string is returned that contains the fraction
   --  represented in this manner. The latter form is the only one that
   --  can be used if the numerator or denominator is outside the range
   --  of ASIS_Integer.

   type Fraction is record
      Num   : ASIS_Integer;
      Denum : ASIS_Positive;
   end record;

   function Small_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                String;
   --  Provided that Fixed_Point_Subtype is the defining name of some
   --  fixed point type or subtype (including types derived from fixed
   --  point types), this function returns the string image of 'small' of
   --  this type or subtype, as defined in RM 95, 3.5.10(2) as a string
   --  value representing a fraction (numerator / denominator), with no
   --  spaces, and both numerator and denominator represented in decimal.
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some fixed point
   --                            or decimal fixed point type or subtype

   function Small_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                Fraction;
   --  Provided that Fixed_Point_Subtype is the defining name of some
   --  fixed point type or subtype (including types derived from fixed
   --  point types), this function returns the fraction representation of
   --  'small' of this type or subtype, as defined in RM 95, 3.5.10(2)
   --  ASIS_Failed is raised and the corresponding Diagnosis tring is set
   --  if the numerator or denominator is outside the representable range.
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some fixed point
   --                            or decimal fixed point type or subtype

   function Delta_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                String;
   --  Provided that Fixed_Point_Subtype is the defining name of some
   --  fixed point type or subtype (including types derived from fixed
   --  point types), this function returns the string image of 'delta' of
   --  this type or subtype, as defined in RM 95, 3.5.10(2) as a string
   --  value representing a fraction (numerator / denominator), with no
   --  spaces, and both numerator and denominator represented in decimal.
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some fixed point
   --                            or decimal fixed point type or subtype

   function Delta_Value
     (Fixed_Point_Subtype : Asis.Element)
      return               Fraction;
   --  Provided that Fixed_Point_Subtype is the defining name of some
   --  fixed point type or subtype (including types derived from fixed
   --  point types), this function returns the fraction representation of
   --  'delta' of this type or subtype, as defined in RM 95, 3.5.10(3)
   --  ASIS_Failed is raised and the corresponding Diagnosis tring is set
   --  if the numerator or denominator is outside the representable range.
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some fixed point
   --                            or decimal fixed point type or subtype

   ----------------------
   -- Other Attributes --
   ----------------------

   --  The current version of Asis.Data_Decomposition.Extensions does not
   --  provide queries for the 'Fore or 'Aft attributes of fixed point types.

   -------------------------------
   -- Decimal Fixed Point Types --
   -------------------------------

   function Scale_Value
     (Desimal_Fixed_Point_Subtype : Asis.Element)
      return                        ASIS_Natural;
   --  Provided that Desimal_Fixed_Point_Subtype is the defining name of some
   --  decimal fixed point type or subtype (including types derived from
   --  decimal fixed point types), this function returns the scale of
   --  this type or subtype, as defined in RM 95 3.5.10(11).
   --
   --  Appropriate Defining_Name_Kinds
   --     A_Defining_Identifier, provided that it defines some decimal fixed
   --                            point type or subtype

end Asis.Data_Decomposition.Extensions;
