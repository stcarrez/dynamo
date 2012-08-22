------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . I T E S T S                            --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines tests for defining if an Element based on the
--  given node should test Is_Implicit, Is_Inherited and Is_From_Instance

with Types; use Types;

package A4G.Itests is

   function Is_Inherited_Discriminant (N : Node_Id) return Boolean;
   --  Tests if N corresponds to the defining occurrence of an inherited
   --  discriminant. N should be of N_Defining_Identifier kind
   --
   --  INCOMPLETE and INRELIABLE for now!

   function Is_From_Instance (N : Node_Id) return Boolean;
   --  Tests if N corresponds to an implicit or explicit construction from
   --  the result of some generic instantiation (that is, from an expanded
   --  generic template)

end A4G.Itests;
