------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--        A S I S . D A T A _ D E C O M P O S I T I O N . V C H E C K       --
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

--  This package contains validity checks for abstractions declared in
--  Asis.Data_Decomposition (see 22.1, 22.3)

private package Asis.Data_Decomposition.Vcheck is

   type Component_Kinds is (Not_A_Component, Arr, Rec);

   procedure Check_Validity
     (Comp  : Record_Component;
      Query : String);
   --  Checks if Comp is valid in a sense as defined in 22.1. Raises
   --  Asis_Failed and sets the corresponding error status and diagnosis in
   --  case if the check fails. The Query parameter is supposed to be the name
   --  of the query where the check is performad.

   procedure Check_Validity
     (Comp  : Array_Component;
      Query : String);
   --  Checks if Comp is valid in a sense as defined in 22.3. Raises
   --  Asis_Failed and sets the corresponding error status and diagnosis in
   --  case if the check fails. The Query parameter is supposed to be the name
   --  of the query where the check is performad.

   procedure Raise_ASIS_Inappropriate_Component
     (Diagnosis      : String;
      Component_Kind : Component_Kinds);
   --  Raises ASIS_Inappropriate_Element with Value_Error Error Status.
   --  Diagnosis usially is the name of the query where the exception is
   --  raised. Component_Kind, if not equal to Not_A_Component, is used to put
   --  in the ASIS diagnosis string some information to distinguish the
   --  queries with the same name which are defined for both Record_Component
   --  and Array_Component.

end Asis.Data_Decomposition.Vcheck;
