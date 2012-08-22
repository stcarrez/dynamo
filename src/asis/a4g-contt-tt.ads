------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C O N T T . T T                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore.                  --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines for each ASIS Context the corresponding Tree Table,
--  which contains the information about the tree output files needed for
--  handling and swapping the ASTs represented by the tree output files
--  accessed by ASIS.

with Asis;

package A4G.Contt.TT is              --  Context_Table.Tree_Tables

   ----------------
   -- Tree Table --
   ----------------

   --  The tree table has an entry for each AST ( = tree output file)
   --  created and read at least once for this run of ASIS application.

   --  The entries in the table are accessing using a Tree_Id which
   --  ranges from Nil_Tree (a special value using for initializing
   --  ASIS Nil_Element and ASIS Nil_Compilation_Unit) to Last_Tree.
   --  Each entry has the following fields:

   ---------------------
   -- Tree Name Table --
   ---------------------

   procedure Get_Name_String (C : Context_Id; Id : Tree_Id);
   --  Get_Name_String is used to retrieve the string associated with
   --  an entry in the name table. The resulting string is stored in
   --  Name_Buffer and Name_Len is set.

   function Get_Tree_Name (C : Context_Id; Id : Tree_Id) return String;
   --  Returns the full name of the tree file.

   function Allocate_Tree_Entry return Tree_Id; --  #####
   --  Allocates the new entry in the Tree Table for the tree output file
   --  name stored in the A_Name_Buffer (A_Name_Len should be set
   --  in a proper way).

   ------------------------------
   -- Internal Tree Attributes --
   ------------------------------

   --  Each Tree entry contains the following fields, representing the Tree
   --  attributes needed to organize tree processing inside ASIS
   --  implementation:

   --  Enclosing_Lib : Context_Id;                          --##
   --    Context Id of the ASIS Context for which the tree has been
   --    created.

   --  Main_Unit_Id : Unit_Id;
   --    The ASIS Compilation Unit, corresponding to the main unit in
   --    the tree

   --  Main_Top : Node_Id;
   --    The top node (having N_Compilation_Unit Node Kind) of Main_Unit

   --   Units : Elist_Id;
   --     The list of all the Units (or all the Units except Main_Unit?)
   --     which may be processed on the base of this tree, [each Unit
   --     is accompanied by its top node, which it has in the given tree
   --     ??? Not implemented for now!]

   ---------------------------------------------------------------
   --  Internal Tree Unit Attributes Access and Update Routines --
   ---------------------------------------------------------------

   function Main_Unit_Id (T : Tree_Id) return Unit_Id;
   function Main_Unit_Id               return Unit_Id;
   --  Returns the Id of the main unit in Current_Tree

   procedure Set_Main_Unit_Id (T : Tree_Id; U : Unit_Id);
   procedure Set_Main_Top     (T : Tree_Id; N : Node_Id);
   --  Do we really need Set procedures having a Tree (and its "enclosing"
   --  Context) as a parameter? Now it seems, that all settings will be
   --  done for the currently accessing Tree only.

   procedure Set_Main_Unit_Id (U : Unit_Id);
   procedure Set_Main_Top     (N : Node_Id);

   -----------------------------------
   -- Subprograms for Tree Swapping --
   -----------------------------------

   function Unit_In_Current_Tree (C : Context_Id; U : Unit_Id) return Boolean;
   --  Checks if the subtree for a given Unit defined by C and U, is
   --  contained in the currently accessed tree.

   procedure Reset_Tree (Context : Context_Id; Tree : Tree_Id);
   --  Resets the currently accessed tree to the tree identified by
   --  the Context and Tree parameters

   procedure Reset_Tree_For_Unit (C : Context_Id; U : Unit_Id);
   procedure Reset_Tree_For_Unit (Unit : Asis.Compilation_Unit);
   --  Resets the currently accessed tree to some tree containing
   --  the subtree for a given unit. For now, there is no special
   --  strategy for choosing the tree among all the trees containing
   --  the given unit

   procedure Reset_Tree_For_Element (E : Asis.Element);
   --  Resets the currently accessed tree to the tree containing the node(s)
   --  of the argument Element.

   procedure Reset_Instance_Tree
     (Lib_Level_Instance : Asis.Compilation_Unit;
      Decl_Node          : in out Node_Id);
   --  Given Lib_Level_Instance as ASIS Compilation Unit being a library-level
   --  instantiation, or a package or generic package containing
   --  an instantiation of some library-level generic unit, and Decl_Node as
   --  the node representing some declaration in the corresponding spec (which
   --  can be either expanded generics spec if Lib_Level_Instance is a library-
   --  level instantiation or a normal spec in case of a (generic) package);
   --  it is an error to call this procedure with other arguments), this
   --  procedure resets the currently accessed tree to the main tree for
   --  Lib_Level_Instance (it may be the tree created for the body of
   --  Lib_Level_Instance in case if Lib_Level_Instance is a package
   --  declaration) and resets Decl_Node to point to the same construct in
   --  this tree.
   --
   --  If the corresponding ASIS Context does not contain the main tree for
   --  this library-level instantiation, the procedure does nothing.
   --  Also does nothing if Lib_Level_Instance is a package body

   function Restore_Node_From_Trace
     (In_Body : Boolean               := False;
      CU      : Asis.Compilation_Unit := Asis.Nil_Compilation_Unit)
      return    Node_Id;
   --  Taking the node trace stored in Node_Trace table, tries to find the
   --  construct corresponding to the beginning of the trace in the currently
   --  accessed tree. By default we consider that we are in the package spec,
   --  unless In_Body is set ON.

   procedure Append_Full_View_Tree_To_Unit    (C : Context_Id; U : Unit_Id);
   procedure Append_Limited_View_Tree_To_Unit (C : Context_Id; U : Unit_Id);
   --  Appends the currently accessed tree to the list of the (consistent)
   --  trees containing a given Unit (this tree list belongs to the unit U).

   procedure Reorder_Trees (C : Context_Id);
   --  This procedure is called in the very end of opening the context C, when
   --  all the information is already set in the Context Unit table. It
   --  reorders the tree lists associated with units according to the
   --  following rules (note, that currently the first tree in the tree list
   --  is used by Element gateway queries to get into the unit structure:
   --
   --  (1) for a subunit, the tree for its ancestor body is moved into the
   --      first position in the tree list;
   --
   --  (2) for a package declaration or generic package declaration, if this
   --      package requires a body, the tree for the body is moved into the
   --      first position in the tree list;
   --
   --  (3) for package or generic package declaration which does not require a
   --      body, the tree created for the given (generic) package is moved
   --      into the first position in the tree list;
   --
   --  (4) for a library-level instantiation, the tree created for the
   --      instantiation is moved into the first position in the tree list;
   --
   --  (5) for a (generic) subprogram declaration, the tree for the
   --      corresponding body is moved into the first position in the tree
   --      list;
   --
   --  (6) for the bodies, we may also need to set the main tree first, because
   --      according to Lib (h), the body may be compiled as being needed for
   --      some spec (or other body unit)
   --
   --  For -CA Context, if the tree to be moved into the first position in
   --  the tree list does not exist, the corresponding warning is generated,
   --  except if the corresponding unit is of A_Predefined_Unit or
   --  An_Implementation_Unit origin

   ---------------------------------
   -- General-Purpose Subprograms --
   ---------------------------------

   function Present (Tree : Tree_Id) return Boolean;
   --  Tests given Tree Id for non-equality with No_Tree_Name.
   --  This allows  notations like "if Present (Tree)" as opposed to
   --  "if Tree /= No_Tree_Name"

   function No (Tree : Tree_Id) return Boolean;
   --  Tests given Tree Id for equality with No_Tree_Name. This allows
   --  notations like "if No (Tree)" as opposed to
   --  "if Tree = No_Tree_Name"

   function Last_Tree (C : Context_Id) return Tree_Id;
   --  Returns the Tree_Id of the last tree which has been allocated
   --  in the Tree Table.

   procedure Output_Tree (C : Context_Id; Tree : Tree_Id);
   --  Produces the debug output of the Tree Table entry corresponding
   --  to Tree

   procedure Print_Trees (C : Context_Id);
   --  Produces the debug output from the Tree table for the Context C.

   function Tree_Consistent_With_Sources
     (E :    Asis.Element)
      return Boolean;

   function Tree_Consistent_With_Sources
     (CU :   Asis.Compilation_Unit)
      return Boolean;
   --  These functions are supposed to be used for Incremental Context mode.
   --  They check that the tree from which their argument Element or Unit has
   --  been obtained is still consistent with all the sources from which
   --  the tree was generated (and that all these sources are available)
   --  This function supposes that its argument is not null and that  the tree
   --  to check is available.

   function Current_Tree_Consistent_With_Sources return Boolean;
   --  Checks that for the current tree all the sources from which it has been
   --  obtained are still available and that the tree is consistent with
   --  these sources. The caller is responsible for setting as the current
   --  tree the tree he would like to check

end A4G.Contt.TT;
