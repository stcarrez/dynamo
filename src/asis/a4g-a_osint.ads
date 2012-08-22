------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . A _ O S I N T                          --
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

--  The original idea of this package was to be an ASIS analog of the GNAT
--  Osint package and to contain the low-level routines needed by different
--  components of the ASIS implementation. But its current version contains a
--  very few routines, so probably we should merge this package with some
--  other ASIS implementation utility package.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Types;       use Types;

package A4G.A_Osint is

   function Normalize_Directory_Name
     (Directory : String)
      return String;
   --  Verify and normalize a directory name. If directory name is invalid,
   --  this will return an empty string (not implemented for now -  all the
   --  checks should be made by a caller). Otherwise it will insure a
   --  trailing directory separator and make other normalizations.

   function Get_Max_File_Name_Length return Int;
   --  yields the maximum file name length for system. Returns Int'Last,
   --  if the system does not limit the maximum file name length.

   procedure Free_Argument_List (List : in out Argument_List_Access);
   --  if List is not null, frees the memory occupied by its content

end A4G.A_Osint;
