------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . I T E S T S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-1999, Free Software Foundation, Inc.       --
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

with Sinput;       use Sinput;
with Atree;        use Atree;

package body A4G.Itests is

   ----------------------
   -- Is_From_Instance --
   ----------------------

   function Is_From_Instance (N : Node_Id) return Boolean is
   begin
      return "/=" (No_Location,
                   Instantiation (Get_Source_File_Index (Sloc (N))));
   end Is_From_Instance;

   -------------------------------
   -- Is_Inherited_Discriminant --
   -------------------------------

   function Is_Inherited_Discriminant (N : Node_Id) return Boolean is
      Type_Decl_Node : Node_Id;
   begin
      --  the idea of the test is to test if the node corresponding to the
      --  enclosing type declaration has been inserted (namely inserted,
      --  because a rewritten node corresponds to the situation when
      --  a derived type has its own known_discriminant part; and we cannot
      --  use Comes_From_Source flag, because it is set ON (???!!!) for
      --  such incerted nodes (at least, in 3.05)

      Type_Decl_Node := Parent (Parent (N));

      return Is_Rewrite_Insertion (Type_Decl_Node);
   end Is_Inherited_Discriminant;

end A4G.Itests;
