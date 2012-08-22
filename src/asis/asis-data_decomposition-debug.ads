------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--           A S I S . D A T A _ D E C O M P O S I T I O N . D E B U G      --
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

--  This package contains routines forming debug images for bstractions
--  declared in Asis.Data_Decomposition.

package Asis.Data_Decomposition.Debug is

   function Debug_Image (RC : Record_Component) return Wide_String;
   function Debug_Image (AC : Array_Component) return Wide_String;

   --  Returns a string value containing implementation-defined debug
   --  information associated with the element.
   --
   --  The return value uses Asis.Text.Delimiter_Image to separate the lines
   --  of multi-line results.  The return value does not end with
   --  Asis.Text.Delimiter_Image.

   function Is_Derived_From_Record (TD : Element) return Boolean;
   function Is_Derived_From_Array  (TD : Element) return Boolean;
   --  The public renaming of
   --  Asis.Data_Decomposition.Aux.Is_Derived_From_Record/
   --  Asis.Data_Decomposition.Aux.Is_Derived_From_Array
   --  May be, we should have it in Asis.Extensions

   function Dimension (Comp : Array_Component) return ASIS_Natural;
   --  The public renaming of
   --  Asis.Data_Decomposition.Set_Get.Dimension
   --  May be, we should have it in Asis.Extensions

--   function Linear_Index
--     (Inds        : Dimension_Indexes;
--      D           : ASIS_Natural;
--      Ind_Lengths : Dimention_Length;
--      Conv        : Convention_Id := Convention_Ada)
--      return Asis.ASIS_Natural;

--   function De_Linear_Index
--     (Index       : Asis.ASIS_Natural;
--      D           : ASIS_Natural;
--      Ind_Lengths : Dimention_Length;
--      Conv        : Convention_Id := Convention_Ada)
--      return Dimension_Indexes;
   --  The public renaming of
   --  Asis.Data_Decomposition.Aux.Linear_Index and
   --  Asis.Data_Decomposition.Aux.De_Linear_Index

end Asis.Data_Decomposition.Debug;
