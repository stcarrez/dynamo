------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . U _ C O N V                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

package A4G.U_Conv is

--  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  !!                                                                 !!
--  !!  This package should be completely revised (and very likely -   !!
--  !!  removed), when migration to using pre-created trees as to the  !!
--  !!  *ONLY* ASIS operation mode is completed                        !!
--  !!                                                                 !!
--  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

--  This paskage contain routines supporting the ASIS and ASIS-GNAT conventions
--  for file and unit names.

   -----------------
   -- Subprograms --
   -----------------

   procedure Get_Norm_Unit_Name (U_Name           :     String;
                                 N_U_Name         : out String;
                                 Spec             :     Boolean;
                                 May_Be_Unit_Name : out Boolean);
   --  Having got U_Name as supposetely being an Ada unit name, this procedure
   --  checks if it satisfies the Ada unit name syntax structure and tries to
   --  convert it into "normalized" form by folding all the letters to
   --  lower case and appending the "%s" or "%b" suffix depending on the
   --  value of Spec (See also unitt.ads). If U_Name can be treated as Ada
   --  unit name, its "normalized" version is returned as the value of
   --  N_U_Name, and May_Be_Unit_Name is set True. If the structure of
   --  U_Name does not satisfy the syntax rules imposed on the Ada unit
   --  names, May_Be_Unit_Name is set False, and the value of N_U_Name is
   --  undefined.
   --
   --  The caller is responcible for the fact, that the length of the actual
   --  for N_U_Name is the length of the actual for U_Name plus 2
   --  (for "%s" or "%b") ).  The caller is also responsible for the
   --  fact that the lower bounds of U_Name and N_U_Name are the same.
   --  (Otherwise CONSTRAINT_ERROR will be raised). It is also supposed
   --  that this procedure will  never be called for the empty string as an
   --  actual for U_Name.
   --
   --  Note, that the "normalized" Unit name returned by this routine
   --  does not contain the prefix indicating the enclosing Context.

   function Source_From_Unit_Name
     (S    : String;
      Spec : Boolean)
      return String_Access;
   --  Converts S into the ada source file name according to the file name
   --  rules specified in GNAT DOCUMENT INTRO. The suffix of the file name
   --  is set as ".ads" if Spec is True or ".adb" otherwise.
   --
   --  It is supposed that this function is called for S that is non-empty
   --  string having a structure of the fully expynded Ada unit unit name.
   --  This function does not check the content of S leaving it to the
   --  caller.
   --
   --  The result of this function is NUL-terminated

   function Tree_From_Source_Name (S : String_Access) return String_Access;
   --  Returns the reference to a newly created NUL-terminated string.
   --  It is supposed to be called only for the (references to the) Ada
   --  source file names obtained as the results of the Locate_In_Context
   --  function. It is also supposed that S points to a NUL-terminated
   --  string. The content of the returned string is obtained by
   --  transforming the content of the string pointed by S and interpreted
   --  as Ada source file name into the name of the corresponding tree
   --  output file.
   --
   --  This function requires revising if the effect of Source_File_Name
   --  pragma is implemented!

   function Is_Predefined_File_Name (S : String_Access) return Boolean;
   --  This function is the full analog of the GNAT function
   --  Fname.Is_Predefined_File_Name, but it works with the string
   --  value which is not stored in the GNAT name buffer.

   function To_String (S : String_Access) return String;
   --  Should be applied only to the references to NUL terminated
   --  strings (usially - file names). Returns the content of the
   --  referenced string without the trailing NUL character.

end A4G.U_Conv;
