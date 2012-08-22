------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                      A 4 G . A S I S _ T A B L E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
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

--  This package contains definitions of tables and related auxilary resources
--  needed in more than one ASIS implementation package

with Asis;

with Sinfo; use Sinfo;
with Table;
with Types; use Types;

package A4G.Asis_Tables is

   package Internal_Asis_Element_Table is new Table.Table (
     Table_Component_Type => Asis.Element,
     Table_Index_Type     => Asis.ASIS_Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Internal Element_List");

   --  This table contains ASIS Elements. It is supposed to be used only for
   --  creating the result Element lists in ASIS structural queries. Note that
   --  many ASIS queries use instantiations of Traverse_Elements to create
   --  result lists, so we have to make sure that ASIS structural queries
   --  used in the implementation of Traverse_Element use another table to
   --  create result lists

   package Asis_Element_Table is new Table.Table (
     Table_Component_Type => Asis.Element,
     Table_Index_Type     => Asis.ASIS_Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Element_List");
   --  This table contains ASIS Elements. It is supposed to be used for any
   --  purpose except creating the result Element lists in ASIS structural
   --  queries.

   procedure Add_New_Element (Element : Asis.Element);
   --  Differs from Asis_Element_Table.Append that checks if the argument
   --  Element already is in the table, and appends the new element only if the
   --  check fails. Note that the implementation is based on a simple array
   --  search, so it can result in performance penalties if there are too
   --  many elements in the table.

   type Node_Trace_Rec is record
      Kind          : Node_Kind;
      Node_Line     : Physical_Line_Number;
      Node_Col      : Column_Number;
   end record;
   --  This record represents a Node in the node trace used to find the same
   --  construct in another tree

   package Node_Trace is new Table.Table (
     Table_Component_Type => Node_Trace_Rec,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Node_Trace");
   --  This table is used to create the node trace needed to compare elements
   --  from nested instances

   function Is_Equal
     (N         : Node_Id;
      Trace_Rec : Node_Trace_Rec)
      return      Boolean;
   --  Checks if N (in the currently accessed tree corresponds to the node
   --  for which Trace_Rec was created

   procedure Create_Node_Trace (N : Node_Id);
   --  Creates the Node trace which is supposed to be used to find the node
   --  representing the same construct in another tree. The trace is also used
   --  to check is two nodes from different trees, each belonging to expanded
   --  generics both denote the same thing. This trace contains the record
   --  about N itself and all the enclosing constructs such as package bodies
   --  and package specs. For the package which is an expanded generic, the
   --  next element in the trace is the corresponding instantiation node.

   function Enclosing_Scope (N : Node_Id) return Node_Id;
   --  Given a node somewhere from expanded generic, returnes its enclosing
   --  "scope" which can be N_Package_Declaration, N_Package_Body or
   --  N_Generic_Declaration node. The idea is to use this function to create
   --  the node trace either for storing it in the Note Trace table or for
   --  creating the trace on the fly to compare it with the stored trace.

end A4G.Asis_Tables;
