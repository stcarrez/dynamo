------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . A _ A L L O C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                              Version : 1.00                              --
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

package A4G.A_Alloc is

--  This package contains definitions for initial sizes and growth increments
--  for the various dynamic arrays used for principle ASIS Context
--  Model data strcutures. The indicated initial size is allocated for the
--  start of each file, and the increment factor is a percentage used
--  to increase the table size when it needs expanding
--  (e.g. a value of 100 = 100% increase = double)

--  This package is the ASIS implementation's analog of the GNAT Alloc package

   Alloc_ASIS_Units_Initial : constant := 1_000;
   --  Initial allocation for unit tables

   Alloc_ASIS_Units_Increment : constant := 150;
   --  Incremental allocation factor for unit tables

   Alloc_Contexts_Initial : constant := 20;
   --  Initial allocation for Context table (A4G.Contt)

   Alloc_Contexts_Increment : constant := 150;
   --  Incremental allocation factor for Context table (Contt)

   Alloc_ASIS_Trees_Initial : constant := 1_000;
   --  Initial allocation for tree tables

   Alloc_ASIS_Trees_Increment : constant := 150;
   --  Incremental allocation factor for tree tables

end A4G.A_Alloc;
