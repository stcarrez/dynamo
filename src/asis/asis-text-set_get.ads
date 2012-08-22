------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A S I S . T E X T . S E T _ G E T                 --
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
-- CHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
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

package Asis.Text.Set_Get is

   ---------
   -- Get --
   ---------

   function Line_Length (L : Line) return Character_Position;
   --  Returns line length. Note, that this is the length in the original
   --  source, counted in wide characters, but not the length in the internal
   --  representation counted in one-byte characters

   function Line_Location (L : Line) return Source_Ptr;
   --  when Line Location is obtained, the tree is resetted if needed

   function Valid (L : Line) return Boolean;
   --  chechs, if the argument is valid, that is, if the Context
   --  from which this line was obtained is still opened

   function Line_Wide_Image (L : Line) return Wide_String;
   --  Returns the line image as the line is represented in the original
   --  source text

   function Debug_Image (The_Span : Span) return String;
   --  Produces the debug output for its argument

   ---------
   -- Set --
   ---------

   procedure Set_Line_Length   (L : in out Line; N : Character_Position);
   procedure Set_Line_Location (L : in out Line; S : Source_Ptr);
   --  this procedure is intended to correct the Sloc field in
   --  the first line from a line list covering a given Span.
   --  This means, that all the fields of the Line to be corrected
   --  have been already set as pointing to the beginning of
   --  a given line. Together with setting the Sloc field, this
   --  procedure adjust the Rel_Sloc field.

   procedure Set_Lines (LList : in out Line_List; El : Element);
   --  This procedure creates LList as a list of lines
   --  accessable through El. It gets LList as a list
   --  of Nil_Lines and makes the proper settings for
   --  the components of each line, making them non-nil
   --  It takes all the characteristics of these lines from El,
   --  that is, from the tree on which El is based. This is
   --  "tree-swapping-safe" procedure.
   --  The only call to this procedure is in the third
   --  Asis.Text.Lines function, which explicitly specifies
   --  the bounds of the Line_List to be returned (that is, LList).
   --  The caller makes all the checks needed to make sure, that
   --  the compilation enclosing El really contains lines with numbers
   --  LList'First and LList'Last
end Asis.Text.Set_Get;
