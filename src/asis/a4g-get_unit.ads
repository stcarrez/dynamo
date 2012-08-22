------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . G E T _ U N I T                         --
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

with A4G.A_Types; use A4G.A_Types;

package A4G.Get_Unit is

--  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  !!                                                                 !!
--  !!  This package should be completely revised (and very likely -   !!
--  !!  removed), when migration to using pre-created trees as to the  !!
--  !!  *ONLY* ASIS operation mode is completed                        !!
--  !!                                                                 !!
--  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

--  This paskage contains routines for obtaining ASIS Compilation Units
--  according to the requests arosen during processing the queries from
--  Asis.Compilation_Units package which get Units and Unit Lists from an
--  ASIS Context. As the first step, these routines try to find the
--  reqiested unit(s) by making the corresponding search among Units
--  which are already known to ASIS by looking for the Units in the internal
--  tables implementing the ASIS Context Model. If this first step does not
--  give the required resuil, the attempt to fing the Unit(s) by processing
--  the Ada sources or pre-created tree output files is undertaken. To obtain
--  information about the Unit which has never been known to ASIS, ASIS has,
--  depending on the options set for a context, either to compile it "on the
--  fly" in order to check its correctness and to produce the tree output
--  file, if the Unit is correct, or to try to locate the pre-created tree
--  output file; then the tree output file has to be read in and analysed.
--  If the tree contains the subtrees for other Ada units which have not
--  been known to ASIS so far, ASIS stores the information about these units
--  in its internal tables (even the ASIS query being processed has no
--  relation to these units).

   -----------------
   -- Subprograms --
   -----------------

   function Get_One_Unit
     (Name    : Wide_String;
      Context : Context_Id;
      Spec    : Boolean)
      return    Unit_Id;
   --  This function defines the top-level control flow for the two
   --  functions from the Asis_Compilation_Unit package which yields
   --  one ASIS Compilation Unit: Library_Unit_Declaration and
   --  Compilation_Unit_Body. Spec is used to make the diffference
   --  between these two functions: it is set True for
   --  Library_Unit_Declaration and False for Compilation_Unit_Body.

   function Fetch_Unit_By_Ada_Name
     (Name      : String;
      Norm_Name : String;
      Context   : Context_Id;
      Spec      : Boolean)
      return      Unit_Id;
   --  This function is supposed to be called in the following situation:
   --
   --  -  Name is non-empty string which can be interpreted as an Ada unit
   --     name;
   --
   --  -  Norm_Name is non-empty string representing the normalised version
   --     of the Ada unit name passed as the actual for Name. This means
   --     that Norm_Name'Legth = Name'Length + 2, and Norm_name ends with
   --     "%s" or "%b" DEPENDING ON THE VALUE OF SPEC;
   --
   --  The Context parameter indicates the ASIS Context to look for a Unit
   --  indicated by Name and Norm_Name. Spec indicates whether the library
   --  unit declaration or the compilation unit body should be fetched
   --  (See the documentation for the functions Library_Unit_Declaration
   --  and Compilation_Unit_Body from the Asis_Compilation_Units package
   --  for the definition of the ASIS meaning of "library unit declaration"
   --  and "compilation unit body").
   --
   --  This function returns the Unit_Id value indicating the desired
   --  Unit. Nil_Unit is returned if Context does not contain the   -- ##
   --  Unit having Name as its unit name.
   --
   --  If ASIS already knows this Unit, the returned Unit_Id value
   --  points to some pre-existing entry in the Unit Table for the given
   --  Context. Otherwise ASIS tries to compile Unit from its cource.
   --  If the compilation is successful, ASIS retrieves the newly
   --  created tree and it obtains and stores in the Unit Table
   --  information about the Unit AND ABOUT ALL IS SUPPORTERS, WHICH
   --  ARE NOT KNOWN TO ASIS!
   --
   --  !!! THIS FUNCTION SHOULD ANALYZE EXISTING TREES BEFORE TRYING
   --  !!! TO CREATE THE NEW TREE, THIS IS NOT IMPLEMENTED YET
   --
   --  IMPLEMENTATION LIMITATION : if the file name of an Ada unit is
   --  defined by the Source_File_Name pragma, ASIS cannot call GNAT
   --  to compile this unit!

   function Get_Main_Unit_Tree_On_The_Fly
     (Start_Unit : Unit_Id;
      Cont       : Context_Id;
      Spec       : Boolean)
      return       Unit_Id;
   --  This function is supposed to be used for the Incremental Context mode
   --  only. It purpose is to try to create the main tree for Start_Unit
   --  (if Spec is set ON) or for the body of Start_Unit (if Spec is set OFF).
   --  If the tree is successfully created, this function returns either
   --  Start_From or the Id of the body of Start_From respectively, otherwise
   --  Nil_Unit is returned.
   --
   --  If this function creates the new tree, this tree is added to the set
   --  of trees making up the current incremental context, and this tree
   --  becomes the tree currently accessed by ASIS.
   --
   --  This function is supposed to be called with Start_Unit representing
   --  some spec unit in the Cont. It is also supposed that the tree which
   --  should be created by the call does not exist in Cont. The caller is
   --  responsible to insure these conditions.
   --  condition

end A4G.Get_Unit;
