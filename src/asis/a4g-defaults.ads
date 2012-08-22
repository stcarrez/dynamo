------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . D E F A U L T S                         --
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

--  This package defines the default directory source paths which are the same
--  for any ASIS context.
--
--  In many aspects this package loks like the corresponding part of the
--  GNAT Osint package (see, in particular the code of Osint.Initialize
--  and Osint routines for locating files. But we cannot use Osint directly.

with A4G.A_Types; use A4G.A_Types;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Table;

package A4G.Defaults is

   ----------------------------------------------
   -- Data Structures for Default Search Paths --
   ----------------------------------------------

   package ASIS_Src_Search_Directories is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Dir_Id,
     Table_Low_Bound      => First_Dir_Id,
     Table_Initial        => 12,
     Table_Increment      => 100,
     Table_Name           => "A4G.Defaults.Src_Search_Directories");
   --  Table of the names of the directories listed as the value of the
   --  ADA_INCLUDE_PATH environment variable

   package ASIS_Lib_Search_Directories is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Dir_Id,
     Table_Low_Bound      => First_Dir_Id,
     Table_Initial        => 12,
     Table_Increment      => 100,
     Table_Name           => "A4G.Defaults.Lib_Search_Directories");
   --  Table of the names of the directories listed as the value of the
   --  ADA_OBJECT_PATH environment variable. We are considering object
   --  and ALI files coming together, so we call them both as library
   --  files.

   package ASIS_Tree_Search_Directories is new Table.Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Dir_Id,
     Table_Low_Bound      => First_Dir_Id,
     Table_Initial        => 12,
     Table_Increment      => 100,
     Table_Name           => "A4G.Defaults.Lib_Search_Directories");
   --  Table of the names of the directories for dfault trees. Currently
   --  contains exactly the same information as the table defined by
   --  ASIS_Lib_Search_Directories, because we consider, that
   --  the ADA_INCLUDE_PATH environment variable also defines the
   --  "default" location for tree files. This may be changed if we decide
   --  to use a separate environment variable for trees

   procedure Initialize;
   --  Initialises the tables containing the default search paths by
   --  examining the environment variables. This procedure should
   --  be called when ASIS is initialized, and ontly in this situation.

   procedure Finalize;
   --  reclames all the storage used by strings which store the default
   --  source paths.
   --  This procedure should be called when ASIS is finalized, and only
   --  in this situation.

   function Locate_Default_File
     (File_Name : String_Access;
      Dir_Kind  : Search_Dir_Kinds)
      return String_Access;
   --  Tries to locate the given file in the default directories, following
   --  the order in which these directories are listed in the values of
   --  the corresponding environment variables. Returns the full file name,
   --  if the fle has been located, or returns a null access value otherwise.

   procedure Print_Source_Defaults;
   procedure Print_Lib_Defaults;
   procedure Print_Tree_Defaults;
   --  these procedures produce the debug output for the tables storing the
   --  default source paths.
end A4G.Defaults;
