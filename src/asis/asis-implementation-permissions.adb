------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--       A S I S . I M P L E M E N T A T I O N . P E R M I S S I O N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2007, Free Software Foundation, Inc.       --
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

package body Asis.Implementation.Permissions is

   ------------------------------
   -- Attributes_Are_Supported --
   ------------------------------

   function Attributes_Are_Supported return Boolean  is
   begin
      return False;
   end Attributes_Are_Supported;

   ------------------------------------------
   -- Call_Statement_Parameters_Normalized --
   ------------------------------------------

   function Call_Statement_Parameters_Normalized return Boolean is
   begin
      return False;
   end Call_Statement_Parameters_Normalized;

   -------------------------------
   -- Default_In_Mode_Supported --
   -------------------------------

   function Default_In_Mode_Supported return Boolean is
   begin
      return True;
   end Default_In_Mode_Supported;

   ------------------------------------------
   -- Discriminant_Associations_Normalized --
   ------------------------------------------

   function Discriminant_Associations_Normalized return Boolean  is
   begin
      return False;
   end Discriminant_Associations_Normalized;

   -----------------------------------------
   -- Function_Call_Parameters_Normalized --
   -----------------------------------------

   function Function_Call_Parameters_Normalized return Boolean is
   begin
      return False;
   end Function_Call_Parameters_Normalized;

   ------------------------------------
   -- Generic_Actual_Part_Normalized --
   ------------------------------------

   function Generic_Actual_Part_Normalized return Boolean is
   begin
      return False;
   end Generic_Actual_Part_Normalized;

   ---------------------------------------
   -- Generic_Macro_Expansion_Supported --
   ---------------------------------------

   function Generic_Macro_Expansion_Supported return Boolean is
   begin
      return True;
   end Generic_Macro_Expansion_Supported;

   -----------------------------------
   -- Implicit_Components_Supported --
   -----------------------------------

   function Implicit_Components_Supported return Boolean is
   begin
      return False;
   end Implicit_Components_Supported;

   --------------------------------------
   -- Inherited_Declarations_Supported --
   --------------------------------------

   function Inherited_Declarations_Supported return Boolean is
   begin
      return True;
   end Inherited_Declarations_Supported;

   -------------------------------------
   -- Inherited_Subprograms_Supported --
   -------------------------------------

   function Inherited_Subprograms_Supported return Boolean is
   begin
      return True;
   end Inherited_Subprograms_Supported;

   -----------------------------
   -- Is_Commentary_Supported --
   -----------------------------

   function Is_Commentary_Supported return Boolean is
   begin
      return True;
   end Is_Commentary_Supported;

   --------------------------------------------------
   -- Is_Formal_Parameter_Named_Notation_Supported --
   --------------------------------------------------

   function Is_Formal_Parameter_Named_Notation_Supported return Boolean is
   begin
      return True;
   end Is_Formal_Parameter_Named_Notation_Supported;

   ------------------------------
   -- Is_Line_Number_Supported --
   ------------------------------

   function Is_Line_Number_Supported return Boolean is
   begin
      return True;
   end Is_Line_Number_Supported;

   ------------------------------
   -- Is_Prefix_Call_Supported --
   ------------------------------

   function Is_Prefix_Call_Supported return Boolean is
   begin
      return True;
   end Is_Prefix_Call_Supported;

   ---------------------------------------
   -- Is_Span_Column_Position_Supported --
   ---------------------------------------

   function Is_Span_Column_Position_Supported return Boolean is
   begin
      return True;
   end Is_Span_Column_Position_Supported;

   ------------------------------------
   -- Object_Declarations_Normalized --
   ------------------------------------

   function Object_Declarations_Normalized return Boolean is
   begin
      return False;
   end Object_Declarations_Normalized;

   -------------------------------------
   -- Predefined_Operations_Supported --
   -------------------------------------

   function Predefined_Operations_Supported return Boolean is
   begin
      return False;
   end Predefined_Operations_Supported;

   ----------------------------------------------
   -- Record_Component_Associations_Normalized --
   ----------------------------------------------

   function Record_Component_Associations_Normalized return Boolean is
   begin
      return False;
   end Record_Component_Associations_Normalized;

end Asis.Implementation.Permissions;
