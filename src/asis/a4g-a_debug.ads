------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . A _ D E B U G                         --
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

package A4G.A_Debug is

--  This package contains global flags used to control the inclusion
--  of debugging code in various phases of the ASIS-for-GNAT. It is
--  an almost complete analog of the GNAT Debug package

   -------------------------
   -- Dynamic Debug Flags --
   -------------------------

   --  Thirty six flags that can be used to activate various specialized
   --  debugging output information. The flags are preset to False, which
   --  corresponds to the given output being suppressed. The individual
   --  flags can be turned on using the undocumented switch /dxxx where
   --  xxx is a string of letters for flags to be turned on. Documentation
   --  on the current usage of these flags is contained in the body of Debug
   --  rather than the spec, so that we don't have to recompile the world
   --  when a new debug flag is added

   Debug_Flag_A : Boolean := False;
   Debug_Flag_B : Boolean := False;
   Debug_Flag_C : Boolean := False;
   Debug_Flag_D : Boolean := False;
   Debug_Flag_E : Boolean := False;
   Debug_Flag_F : Boolean := False;
   Debug_Flag_G : Boolean := False;
   Debug_Flag_H : Boolean := False;
   Debug_Flag_I : Boolean := False;
   Debug_Flag_J : Boolean := False;
   Debug_Flag_K : Boolean := False;
   Debug_Flag_L : Boolean := False;
   Debug_Flag_M : Boolean := False;
   Debug_Flag_N : Boolean := False;
   Debug_Flag_O : Boolean := False;
   Debug_Flag_P : Boolean := False;
   Debug_Flag_Q : Boolean := False;
   Debug_Flag_R : Boolean := False;
   Debug_Flag_S : Boolean := False;
   Debug_Flag_T : Boolean := False;
   Debug_Flag_U : Boolean := False;
   Debug_Flag_V : Boolean := False;
   Debug_Flag_W : Boolean := False;
   Debug_Flag_X : Boolean := False;
   Debug_Flag_Y : Boolean := False;
   Debug_Flag_Z : Boolean := False;

   Debug_Flag_1 : Boolean := False;
   Debug_Flag_2 : Boolean := False;
   Debug_Flag_3 : Boolean := False;
   Debug_Flag_4 : Boolean := False;
   Debug_Flag_5 : Boolean := False;
   Debug_Flag_6 : Boolean := False;
   Debug_Flag_7 : Boolean := False;
   Debug_Flag_8 : Boolean := False;
   Debug_Flag_9 : Boolean := False;

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True);
   --  Where C is 0-9 or a-z, sets the corresponding debug flag to the
   --  given value. In the checks off version of debug, the call to
   --  Set_Debug_Flag is always a null operation.

   procedure Set_Off;
   --  Sets all the debug flags OFF (except Debug_Lib_Model for now),
   --  is to be called by Asis_Environment.Finalize

   procedure Set_On;  -- TEMPORARY SOLUTION!!!
   --  Sets all the debug flags ON.

   ------------------------
   -- TEMPORARY SOLUTION --
   ------------------------

   Debug_Mode      : Boolean := False;
   --  Flag indicating if the debugging information should be output by the
   --  routines from the A4G.A_Output package

   Debug_Lib_Model : Boolean := False;
   --  Flag forcing the debug output of the tables implementing the ASIS
   --  Context Model to be performed when finalizing the ASIS Environment.
   --  Currently should be set by hand. The debug output is produced only if
   --  Debug_Mode is set ON.

end A4G.A_Debug;
