------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                             A 4 G . N O R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
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

--  This package defines routines needed for yielding and processing
--  normalized associations and their components

with Asis;

with Types; use Types;

package A4G.Norm is

   ---------------------------------------
   -- Obtaining normalized associations --
   ---------------------------------------

   function Normalized_Param_Associations
     (Call_Elem : Asis.Element)
      return      Asis.Association_List;
   --  Creates the list of normalized associations for a given call to an
   --  entry, a procedure or to a function (is intended to be used in the
   --  implementation of Asis.Expressions.Function_Call_Parameters and
   --  Asis.Statements.Call_Statement_Parameters). This function assumes that
   --  the association list to be returned is not empty. It is an error to call
   --  it when Sinfo.Parameter_Assoccciations function gives No_List for
   --  the node representing the call in question

   function Normalized_Discriminant_Associations
     (Constr_Elem : Asis.Element;
      Constr_Node : Node_Id)
      return Asis.Association_List;
   --  creates the list of normalized associations for a given discriminant
   --  constraint; is intended to be used in the implementation of
   --  Asis.Definitions.Discriminant_Associations. This function assumes,
   --  that Constr_Node is of N_Index_Or_Discriminant_Constraint,
   --  it is an error to call it for other nodes.

   function Normalized_Generic_Associations
     (Inst_Elem          : Asis.Element;
      Templ_Node         : Node_Id)
      return Asis.Association_List;
   --  Creates the list of normalized associations for a given generic
   --  instantiation (is intended to be used in the implementation of
   --  Asis.Decalarations.Generic_Actual_Part. Templ_Node should be the
   --  node representing the corresponding generic template declaration.
   --  This function is supposed to be called if it is known that the list
   --  of normalized associations is not empty
   --
   --  See the documentation of the body for the description of the
   --  representation of the normalized generic associations.

   function Normalized_Record_Component_Associations
     (Aggregate : Asis.Element)
      return      Asis.Element_List;
   --  Creates a list of normalized associations for a record aggregate

   function Defining_Gen_Parameter (Gen_Form_Par : Node_Id) return Node_Id;
   --  Assuming that Gen_Form_Par is a node representing a
   --  generic_formal_parameter_SELECTOR_NAME (it is an error to call this
   --  function for another actual!!!), this function finds the node
   --  representing the defining occurrence of this generic formal
   --  parameter.
   --
   --  ??? Is here a really good place for this function?
   --
   --  ???  And do we really need it???
   --
   --  For now this function is PARTIALLY IMPLEMENTED - it can work only
   --  with a generic_formal_parameter_SELECTOR_NAME which is
   --  operator_symbol "+" or "-"

   ----------------------------------------
   -- Processing normalized associations --
   ----------------------------------------

   function Discr_Def_Name
     (Association : Asis.Discriminant_Association)
      return        Asis.Defining_Name;
   --  from a normalized discriminant association  this function creates
   --  the ASIS Element representing the defining occurrence of the
   --  discriminant. (Is intended to be used in
   --  Asis.Expressions.Discriminant_Selector_Names).
   --
   --  !!!NOTE: for now the implementation is unstable and definitely
   --           contains holes.
end A4G.Norm;
