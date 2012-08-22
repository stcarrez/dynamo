------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . A _ O U T P U T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 15311 $
--                                                                          --
--            Copyright (c) 1995-2002, Free Software Foundation, Inc.       --
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

--  This package contains the utility routines used for providing ASIS
--  warnings, debug images for ASIS types and internal debugging information.

with Asis;        use Asis;
with Asis.Errors; use Asis.Errors;

with Types;       use Types;

package A4G.A_Output is

   Max_Debug_Buffer_Len : Natural := 8 * 1024;
   Debug_Buffer : String (1 .. Max_Debug_Buffer_Len);
   Debug_Buffer_Len : Natural range 0 .. Max_Debug_Buffer_Len;
   --  buffer to form debug image strings

   procedure Add (Phrase : String);
   --  Adds Phrase to Debug_Buffer and resets Debug_Buffer_Len

   procedure ASIS_Warning
     (Message : String;
      Error   : Asis.Errors.Error_Kinds := Not_An_Error);
   --  Produces a warning message (the text of the message is the string
   --  passed as an actual for the Message parameter. The effect of calling
   --  this procedure depends on which ASIS warning mode was set when ASIS was
   --  initialized. In case of Suppress nothing happens, in case of Normal
   --  Message is sent to Stderr, and in case of Treat_As_Error the warning
   --  is converted into raising ASIS_Failed, Message is sent to ASIS diagnosis
   --  and the value of the Error parameter is set to the ASIS Error Status

   function Debug_String (CUnit : Compilation_Unit) return String;
   --  Produces the string containing debug information about CUnit

   function Debug_String (Cont : Context) return String;
   --  Produces the string containing debug information about Cont

   procedure  Debug_String
     (CUnit    : Compilation_Unit;
      No_Abort : Boolean := False);
   --  Produces the string containing debug information about CUnit
   --  Forms in Debug_Buffer the string containing debug information about
   --  the argument unit. If No_Abort is set ON, then any exception raised
   --  inside this procedure is suppressed and the message about suppressed
   --  exception is added to the result string. This is needed to avoid
   --  circularity during reporting of ASIS implementation bug.

   procedure Debug_String
     (E        : Element;
      No_Abort : Boolean := False);
   --  Forms in Debug_Buffer the string containing debug information about E
   --  If No_Abort is set ON, then any exception raised inside this procedure
   --  is suppressed and the message about suppressed exception is added to
   --  the result string. This is needed to avoid circularity during reporting
   --  of ASIS implementation bug.

   procedure Write_Node (N : Node_Id; Prefix : String := "");
   --  outputs the tree node attributes without using any facilities
   --  from the GNAT Treepr package. The string passed as an actual for
   --  Prefix is outputted in the beginning of every string

end A4G.A_Output;
