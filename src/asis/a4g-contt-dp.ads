------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C O N T T . D P                         --
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

--  This package defines routines for computing and setting semantic
--  dependencies between ASIS Compilation Units

with Asis;            use Asis;
with Asis.Extensions; use Asis.Extensions;

package A4G.Contt.Dp is              --  Context_Table.DePendencies

   --  All the subprograms defined in this package are supposed, that
   --  (1) they are called at most once (depending on the unit kind) for any
   --      unit stored in the Context Unit table
   --  (2) the caller is responsible, that these subprograms are called for
   --      the actuals of appropriate kinds

   --  OPEN PROBLEMS:
   --
   --  1. DOCUMENTATION!!!
   --
   --  2. The idea is to *compute from the tree* only direct dependencies,
   --     and for all the indirect dependencies, to compute them from
   --     the direct dependencies using their transitive nature.

   procedure Set_Supporters (C : Context_Id; U : Unit_Id; Top : Node_Id);
   --  This procedure sets the **direct** supporters for U. For now,
   --  the idea is to set *from the tree* only the direct dependencies,
   --  as being the function of the source text of a Unit (???). All
   --  the indirect dependencies should be set from direct ones using
   --  the transitive nature of the dependencies.
   --
   --  So, for now, only Direct_Supporters, Direct_Dependants and
   --  Implicit_Supporters (representing only direct implicit dependencies
   --  imposed by implicit with clauses) are set by this procedure.
   --
   --  OPEN PROBLEM: If we set only **direct* dependencies, may be,
   --  we need only ONE function to do this? do we really need
   --  Set_Ancestors, Set_Childs, ????   ?

   procedure Set_Subunits (C : Context_Id; U : Unit_Id; Top : Node_Id);
   --  Sets the full list of the subunit for a given body (that is, adds
   --  nonexistent units for missed subunits)

   procedure Process_Stub (C : Context_Id; U : Unit_Id; Stub : Node_Id);
   --  Taking the node for a body stub, this function checks if the
   --  corresponding subunit exists in the Context C. If it does not exist,
   --  a unit of A_Nonexistent_Body kind is allocated in the Context Unit
   --  table and appended to the list of subunits of U.
   --
   --  This procedure supposes, that before it is called, the normalized
   --  name of U has been already set in A_Name_Buffer. When returning from
   --  this procedure, A_Name_Buffer and A_Name_Len are remained in the
   --  same state as before the call.

   procedure Append_Subunit_Name (Def_S_Name : Node_Id);
   --  Supposing that A_Name_Buf contains the name of the parent body, and
   --  Def_S_Name points to the defining identifier obtained from the body
   --  stub, this procedure forms in A_Name_Buffer the name of the subunit

   procedure Set_Withed_Units (C : Context_Id; U : Unit_Id; Top : Node_Id);
   --  This procedure sets the Direct_Supporters and Implicit_Supporters
   --  dependency lists on the base of with clauses explicicitly presented
   --  in unit's source and generated by the compiler respectively.

   procedure Set_Direct_Dependents (U : Unit_Id);
   --  This procedure is supposed to be called for U just after
   --  Set_Withed_Units has done its work. For any unit U1 included
   --  in the list of direct supporters for U, U is included in the list
   --  of direct dependers of U1.

   procedure Add_To_Parent (C : Context_Id; U : Unit_Id);
   --  Adds U to the list of children for its parent unit declaration.
   --  U is added to the list only it is consistent with the parent

   procedure Set_All_Dependencies (Use_First_New_Unit : Boolean := False);
   --  Sets all supportiers and all dependants for units contained in the
   --  argument Context. Should be called when all the units are already set.
   --  If Use_First_New_Unit is set ON (this may happen for Incremental
   --  Context only), completes the dependencies only for new units from the
   --  new tree (see the body of A4G.Get_Unit.Fetch_Unit_By_Ada_Name)

   procedure Set_All_Ancestors
     (Compilation_Units :         Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Ancestors and computes the
   --  consistent part of the result.(???)

   procedure Set_All_Descendants
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Descendants and computes the
   --  consistent part of the result.(???)

   procedure Set_All_Supporters
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Supporters and
   --  computes the consistent part of the result.(???)

   procedure Set_All_Dependents
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Dependent_Units   :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Dependents and computes the
   --  consistent part of the result.(???)

   procedure Set_All_Families
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Family and
   --  computes the consistent part of the result.(???)

   procedure Set_All_Needed_Units
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access;
      Missed            : in out Compilation_Unit_List_Access);
   --  This procedure takes the arguments of
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order query in
   --  case when Relation parameter is set to Needed_Units and
   --  computes the consistent part of the result and missed units.(???)

end A4G.Contt.Dp;
