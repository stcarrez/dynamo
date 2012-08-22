------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . K N D _ C O N V                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Asis;

with A4G.Int_Knds; use A4G.Int_Knds;

package A4G.Knd_Conv is

   --------------------------------------
   -- Element Classification Functions --
   --------------------------------------

   --  The following functions convert the Internal Element Kind value
   --  given as their argument into the corresponding value of the
   --  corresponding Asis Element Classification subordinate kind.
   --  Not_A_XXX is returned if the argument does not belong to the
   --  corresponding internal classification subtype.

   function Asis_From_Internal_Kind
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Element_Kinds;

   function Pragma_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Pragma_Kinds;

   function Defining_Name_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Defining_Name_Kinds;

   function Declaration_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Declaration_Kinds;
   function Definition_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Definition_Kinds;

   function Type_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Type_Kinds;

   function Formal_Type_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Formal_Type_Kinds;

   function Access_Type_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Access_Type_Kinds;

   function Root_Type_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Root_Type_Kinds;

--  --|A2005 start

   function Access_Definition_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Access_Definition_Kinds;

   function Interface_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Interface_Kinds;

--  --|A2005 end

   function Constraint_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Constraint_Kinds;

   function Discrete_Range_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Discrete_Range_Kinds;

   function Expression_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Expression_Kinds;

   function Operator_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Operator_Kinds;

   function Attribute_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Attribute_Kinds;

   function Association_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Association_Kinds;

   function Statement_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Statement_Kinds;

   function Path_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Path_Kinds;

   function Clause_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Clause_Kinds;

   function Representation_Clause_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Representation_Clause_Kinds;

   -------------------------------------
   -- Additional Classification items --
   -------------------------------------

      function Def_Operator_Kind
        (Op_Kind : Internal_Element_Kinds)
         return Internal_Element_Kinds;
      --  this function "converts" the value of Internal_Operator_Symbol_Kinds
      --  into the corresponding value of Internal_Defining_Operator_Kinds
      --  It is an error to call it to an Internal_Element_Kinds value which
      --  does not belong to Internal_Operator_Symbol_Kinds

end A4G.Knd_Conv;
