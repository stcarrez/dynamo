------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A 4 G . T R E E _ R E C                           --
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

--  This package defines the Tree_Rec type, which is used as an actual for
--  the instantiation of the GANT Table creating the type for the Context tree
--  table. This table stores information about trees (tree output files)
--  cerating the Ada environment processed by a given Context

with A4G.A_Types; use A4G.A_Types;

with Types;       use Types;

package A4G.Tree_Rec is

   --  See the A4G.Contt.TT package for the details of maintaining
   --  the tree tables

   ------------------------------
   -- Tree_Rec type definition --
   ------------------------------

   --  Similar to the Contexts' Unit tabl‚es and Source File tables,
   --  Tree tables are organised as Name tables (for the names of the
   --  tree files), and each entry in such a Name table has additional
   --  fields for information related to tree files handling in ASIS.

   type Tree_Record is record

      --------------------------------
      -- Fields for Tree Name Table --
      --------------------------------

      Tree_Name_Chars_Index      : Int;
      --  Starting locations of characters in the Name_Chars table minus
      --  one (i.e. pointer to character just before first character). The
      --  reason for the bias of one is that indexes in Name_Buffer are
      --  one's origin, so this avoids unnecessary adds and subtracts of 1.

      Tree_Name_Len      : Short;
      --  Lengths of the names in characters

      Int_Info : Int;
      --  Int Value associated with this tree

      ---------------------
      -- Tree attributes --
      ---------------------

      Main_Unit : Unit_Id;
      --    The ASIS Compilation Unit, correspondig to the main unit in
      --    the tree

      Main_Top : Node_Id;
      --    The top node (having N_Compilation_Unit Node Kind) of Main_Unit
      --    DO WE REALLY NEED IT?

      Units : Elist_Id;
      --    The list of all the Units (or all the Units except Main_Unit?)
      --    which may be processed on the base of this tree, [each Unit
      --    is accompanied by its top node, which it has in the given tree
      --    ??? Not implemented for now!]

   end record;

end A4G.Tree_Rec;
