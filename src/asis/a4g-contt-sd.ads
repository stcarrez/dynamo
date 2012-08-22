------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . C O N T T . S D                          --
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
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
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

--  This package defines the procedures which scans the tree search paths for
--  a given Context and analyses the availible tree files

with Namet; use Namet;

package A4G.Contt.SD is

   First_Tree_File : Name_Id := First_Name_Id;
   Last_Tree_File  : Name_Id := First_Name_Id - 1;
   --  Indexes of the first and the last tree file candidates, which were
   --  found during the last scanning of the tree search path of some
   --  directory. Are set by Scan_Tree_Files below. These variables are
   --  undefinite
   --  SHOULD WE MOVE THESE VARIABLES IN THE BODY
   --  AND EVEN MORE - DO WE REALLY NEED THEM AT ALL??!!

   procedure Scan_Tree_Files_New (C : Context_Id);
   --  Stores the names of the tree files making up the Context C in the Tree
   --  table for C. Every tree file name is stored only once.
   --  In All_Trees Context mode it scans the tree search path, using the same
   --  approach for the tree files with the same name as GNAT does for source
   --  files in the source search path. In N_Trees mode it scans the Parametes
   --  string set when C was associated. In this case, if the name of the same
   --  tree file is given more than once, but in diffrent forms (for example
   --  ../my_dir/foo.ats and ../../my_home_dir/my_dir/foo.ats), all these
   --  different names of the same tree file will be stored in the tree table

   procedure Investigate_Trees_New (C : Context_Id);
   --  This procedure implements the second step of opening a Context. It uses
   --  the names of the tree files in the Context Tree Table. For every tree
   --  file, it reads it in and extracts some information about compilation
   --  units presented by this file. It also makes the consistency check.
   --  Checks which are made by this procedure depend on the context options
   --  which were set when C was associated.
   --
   --  Is this package the right location for this procedure???

   procedure Scan_Units_New;
   --  Scans the currently accessed tree which was readed in by the
   --  immediately preceding call to Read_and_Check_New. If a unit is "new"
   --  (that is, if it has not already been encountered during opening a
   --  Context), all the black-box information is computed and stored in the
   --  Context table. Otherwise (that is, if the unit is already "known")
   --  the consistency check is made.
   --
   --  When this procedure raises ASIS_Failed, it forms the Diagnosis string
   --  on befalf on Asis.Ada_Environments.Open

end A4G.Contt.SD;
