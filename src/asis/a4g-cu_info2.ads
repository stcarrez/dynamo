------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C U _ I N F O 2                         --
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

with Asis;        use Asis;

with A4G.A_Types; use A4G.A_Types;

with Types;       use Types;

--  This package contains the routines used to compute the unit attributes
--  during the second pass through the tree files, when ASIS operates in
--  Use_Pre_Created_Trees mode. There is some duplication of the
--  functionalities provided by this package and by A4G.C_U_Info and
--  A4G.S_U_Info; the last two packages were coded before for
--  Compile_On_The_Fly ASIS operation mode. The intent is to get rid of
--  A4G.C_U_Info and A4G.S_U_Info in the final ASIS version, so we do not
--  bother about these duplications for now.

--  ???THIS COMMENT HEADER SHOULD BE REVISED!!!???

package A4G.CU_Info2 is

   procedure Set_Kind_and_Class
     (C   : Context_Id;
      U   : Unit_Id;
      Top : Node_Id);
   --  Taking the unit's subtree top node, this procedure computes and sets
   --  the Unit Kind and the Unit Class for U. Because of some technical ,
   --  reasons, it is more easy to define the Unit Kind and the Unit Class
   --  in the same routine

   procedure Get_Ada_Name (Top : Node_Id);
   --  Computes (by traversing the tree) the fully expanded Ada name
   --  of a compilation unit whose subtree contained as having Top as
   --  its top node in the full tree currently being accessed. This name
   --  then is set in A_Name_Buffer, and A_Name_Len is set as its length

   procedure Set_S_F_Name_and_Origin
     (Context : Context_Id;
      Unit    : Unit_Id;
      Top     : Node_Id);
   --  This procedure obtains the source file name from the GNAT tree and
   --  stores it in the Unit_Table. By analyzing the file name (this analysis
   --  is based on the Fname.Is_Predefined_File_Name GNAt function, the
   --  Unit_Origin for the Unit is defined and stored in the Unit table.

   function Is_Main (Top : Node_Id; Kind : Unit_Kinds) return Boolean;
   --  Defines if the Unit having Top as its N_Compilation_Unit node
   --  can be a main subprogram for a partition. Asis Unit Kind is
   --  used to optimize this computation

   procedure Set_Dependencies
     (C   : Context_Id;
      U   : Unit_Id;
      Top : Node_Id);
   --  Taking the unit's subtree top node, this procedure computes and sets
   --  all the dependency information needed for semantic queries from the
   --  Asis.Compilation_Units package. This information is stored as unit
   --  lists (see A4G.Unit_Rec). For now, we do not compute the lists of
   --  *direct* supporters and *direct* dependents, because we think, that
   --  these ASIS notions are ill-defined and cannot be mapped onto RM95
   --  in a natural way.

end A4G.CU_Info2;
