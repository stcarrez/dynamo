------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . G N A T _ I N T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2008, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the utility routines used for calling the GNAT
--  compiler from inside the ASIS implementation routines to create a tree
--  file.  These routines may be used by ASIS-based tools as well. The idea is
--  to call GNAT in a black-box manner. The current version of this package
--  borrows most of the ideas and the code patterns from the body of the
--  GNAT Make package (which defines the gnatmake-related routines).
--  Unfortunately, GNAT do not provide the public interface to these
--  routines, so we simply have copied the code from make.adb with some
--  modifications.
--
--  This package also contains the routine which reads the tree file with
--  checking the GNAT-ASIS versions compartibility.

with Ada.Calendar; use Ada.Calendar;

with A4G.A_Types;  use A4G.A_Types;
with A4G.A_Debug;

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Types;        use Types;

package A4G.GNAT_Int is

   -----------------------------------
   -- Compiler Variables & Routines --
   -----------------------------------

   Nul_Argument_List : constant Argument_List (1 .. 0) := (others => null);

   --  The flags listed below are used to form the appropriate GNAT or
   --  gnatmake call to create the tree file

   Comp_Flag    : constant String_Access := new String'("-c");

   GNAT_Flag    : constant String_Access := new String'("-gnatg");
   GNAT_Flag_ct : constant String_Access := new String'("-gnatct");
   GNAT_Flag_t  : constant String_Access := new String'("-gnatt");
   GNAT_Flag_ws : constant String_Access := new String'("-gnatws");
   GNAT_Flag_yN : constant String_Access := new String'("-gnatyN");
   GNAT_Flag_05 : constant String_Access := new String'("-gnat05");

   GCC_Flag_X   : constant String_Access := new String'("-x");
   GCC_Par_Ada  : constant String_Access := new String'("ada");
   GCC_Flag_o   : constant String_Access := new String'("-o");

   GNATMAKE_Flag_q     : constant String_Access := new String'("-q");
   GNATMAKE_Flag_u     : constant String_Access := new String'("-u");
   GNATMAKE_Flag_f     : constant String_Access := new String'("-f");
   GNATMAKE_Flag_cargs : constant String_Access := new String'("-cargs");

--   Display_Executed_Programs : Boolean renames A4G.A_Debug.Debug_Mode;
   --  Set to True if name of commands should be output on stderr.
   --  Now this flag is toughtly binded with the flag setting the
   --  ASIS Debug Mode. Is it a good decision?

   function Execute
     (Program      : String_Access;
      Args         : Argument_List;
      Compiler_Out : String := "";
      Display_Call : Boolean := A4G.A_Debug.Debug_Mode)
      return         Boolean;
   --  Executes Program. If the program is not set (the actual for Program is
   --  null), executes the gcc command Args contains the arguments to be passed
   --  to Program. If the program is executed successfully True is returned.
   --
   --  If Compiler_Out is a non-empty string, this string is treated as the
   --  name of a text file to redirect the compiler output into (if the file
   --  does not exist, it is created). Othervise the compiler output is
   --  sent to Stderr.
   --
   --  If Display_Call is ON, outputs into Stderr the command used to execure
   --  Program.

   procedure Create_Tree (Source_File   :     String_Access;
                          Context       :     Context_Id;
                          Is_Predefined :     Boolean;
                          Success       : out Boolean);
   --  Tries to create the tree output file for the given source file
   --  in the context of a given Context. Uses the "standard" GNAT
   --  installation to do this

   procedure Tree_In_With_Version_Check
     (Desc    : File_Descriptor;
      Cont    : Context_Id;
      Success : out Boolean);
   --  Desc is the file descriptor for the file containing the tree file
   --  created by the compiler, Cont is the Id of the Context this tree is
   --  supposed to belong to. This routine reads in the content of the tree
   --  file and makes the GNAT-ASIS version check as a part of tree reading.
   --  If the version check fails or if any error corresponding to the problems
   --  with the expected tree format is detected, Program_Error is raised with
   --  the exception message "Inconsistent versions of GNAT and ASIS". If
   --  the tree can not be read in because of any other reason (for example,
   --  it is not compile-only), the Success parameter is set OFF and the
   --  continuation depends on the Context parameters. If the tree has been
   --  read in successfully and if it is compile-only, Success is set ON.
   --
   --  Before calling this procedure, a caller should put the name of the tree
   --  file to read into A_Name_Buffer.
   --
   --  NOTE: the procedure always closes Desc before returning. Closing it
   --  the second time is erroneous.

   function A_Time (T : Time_Stamp_Type)
      return Time;
   --  Converts GNAT file time stamp into the corresponding value
   --  of Asis_Time.

end A4G.GNAT_Int;
