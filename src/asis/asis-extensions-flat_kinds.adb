------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . K N D _ C O N V                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 15351 $
--                                                                          --
--            Copyright (c) 1995-2002, Free Software Foundation, Inc.       --
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

with Asis.Set_Get;
with A4G.Knd_Conv; use A4G.Knd_Conv;
with A4G.Vcheck;   use A4G.Vcheck;

package body Asis.Extensions.Flat_Kinds is

   use Asis;

   -----------------------
   -- Flat_Element_Kind --
   -----------------------

   function Flat_Element_Kind
     (Element : Asis.Element)
      return    Flat_Element_Kinds
   is
   begin
      Check_Validity (Element, "Asis.Extensions.Flat_Kinds.Flat_Element_Kind");

      return Flat_Element_Kinds (Asis.Set_Get.Int_Kind (Element));
   end Flat_Element_Kind;

   -------------------------------------------------
   -- Flat Element Kinds Conversion Functions --
   -------------------------------------------------

   function Asis_From_Flat_Kind
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Element_Kinds
   is
   begin
      return Asis_From_Internal_Kind (Internal_Element_Kinds (Flat_Kind));
   end Asis_From_Flat_Kind;

   function Pragma_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Pragma_Kinds
   is
   begin
      return Pragma_Kind_From_Internal (Internal_Element_Kinds (Flat_Kind));
   end Pragma_Kind_From_Flat;

   function Defining_Name_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Defining_Name_Kinds
   is
   begin
      return Defining_Name_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Defining_Name_Kind_From_Flat;

   function Declaration_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Declaration_Kinds
   is
   begin
      return Declaration_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Declaration_Kind_From_Flat;

   function Definition_Kind_From_Flat
      (Flat_Kind : Flat_Element_Kinds)
       return      Asis.Definition_Kinds
   is
   begin
      return Definition_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Definition_Kind_From_Flat;

   function Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Type_Kinds
   is
   begin
      return Type_Kind_From_Internal (Internal_Element_Kinds (Flat_Kind));
   end Type_Kind_From_Flat;

   function Formal_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Formal_Type_Kinds
   is
   begin
      return Formal_Type_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Formal_Type_Kind_From_Flat;

   function Access_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Access_Type_Kinds
   is
   begin
      return Access_Type_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Access_Type_Kind_From_Flat;

   function Root_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Root_Type_Kinds
   is
   begin
      return Root_Type_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Root_Type_Kind_From_Flat;

   function Constraint_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Constraint_Kinds
   is
   begin
      return Constraint_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Constraint_Kind_From_Flat;

   function Discrete_Range_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Discrete_Range_Kinds
   is
   begin
      return Discrete_Range_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Discrete_Range_Kind_From_Flat;

   function Expression_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Expression_Kinds
   is
   begin
      return Expression_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Expression_Kind_From_Flat;

   function Operator_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Operator_Kinds
   is
   begin
      return Operator_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Operator_Kind_From_Flat;

   function Attribute_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Attribute_Kinds
   is
   begin
      return Attribute_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Attribute_Kind_From_Flat;

   function Association_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Association_Kinds
   is
   begin
      return Association_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Association_Kind_From_Flat;

   function Statement_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Statement_Kinds
   is
   begin
      return Statement_Kind_From_Internal (Internal_Element_Kinds (Flat_Kind));
   end Statement_Kind_From_Flat;

   function Path_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Path_Kinds
   is
   begin
      return Path_Kind_From_Internal (Internal_Element_Kinds (Flat_Kind));
   end Path_Kind_From_Flat;

   function Clause_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Clause_Kinds
   is
   begin
      return Clause_Kind_From_Internal (Internal_Element_Kinds (Flat_Kind));
   end Clause_Kind_From_Flat;

   function Representation_Clause_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return      Asis.Representation_Clause_Kinds
   is
   begin
      return Representation_Clause_Kind_From_Internal
        (Internal_Element_Kinds (Flat_Kind));
   end Representation_Clause_Kind_From_Flat;

   -------------------------------------
   -- Additional Classification items --
   -------------------------------------

   -----------------------
   -- Def_Operator_Kind --
   -----------------------

   function Def_Operator_Kind
     (Op_Kind : Flat_Element_Kinds)
      return    Flat_Element_Kinds
   is
   begin
      return Flat_Element_Kinds (Def_Operator_Kind
               (Internal_Element_Kinds (Op_Kind)));
   end Def_Operator_Kind;

end Asis.Extensions.Flat_Kinds;
