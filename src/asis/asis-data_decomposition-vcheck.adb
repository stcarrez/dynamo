------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--        A S I S . D A T A _ D E C O M P O S I T I O N . V C H E C K       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License  distributed with ASIS-for-GNAT; see   file --
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
-- (http://www.adaccore.com).                                               --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains validity checks for abstractions declared in
--  Asis.Data_Decomposition (see 22.1, 22.3)

with Asis.Errors;     use Asis.Errors;
with Asis.Exceptions; use Asis.Exceptions;

with A4G.Vcheck;      use A4G.Vcheck;
with A4G.Contt;       use A4G.Contt;

package body Asis.Data_Decomposition.Vcheck is

   S_Record : constant String := "Record";
   S_Array  : constant String := "Array ";
   --  Substrings needed to form the diagnosis in
   --  Raise_ASIS_Inappropriate_Component

   -----------------------
   -- Local subprograms --
   -----------------------

   function Valid (Comp : Record_Component) return Boolean;
   function Valid (Comp : Array_Component)  return Boolean;
   --  Checks if the argument component is valid, that is, it's enclosing
   --  Context is still opened and it was not closed and then re-opened
   --  after obtaining the given component

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity
     (Comp  : Record_Component;
      Query : String)
   is
   begin

      if not Is_Nil (Comp) and then not Valid (Comp) then
         Set_Error_Status
           (Status    => Value_Error,
            Diagnosis => "Invalid Record Component value in " & Query);

         raise ASIS_Inappropriate_Element;

      end if;

   end Check_Validity;

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity
     (Comp  : Array_Component;
      Query : String)
   is
   begin

      if not Is_Nil (Comp) and then not Valid (Comp) then
         Set_Error_Status
           (Status    => Value_Error,
            Diagnosis => "Invalid Array Component value in " & Query);

         raise ASIS_Inappropriate_Element;

      end if;

   end Check_Validity;

   ----------------------------------------
   -- Raise_ASIS_Inappropriate_Component --
   ----------------------------------------

   procedure Raise_ASIS_Inappropriate_Component
     (Diagnosis      : String;
      Component_Kind : Component_Kinds)
   is
      Comp : String (S_Record'Range);
   begin
      if Component_Kind = Rec then
         Comp := S_Record;
      else
         Comp := S_Array;
      end if;

      Set_Error_Status
        (Status    => Value_Error,
         Diagnosis => "Inappropriate " & Comp & " Component in "
                                    & Diagnosis);

      raise ASIS_Inappropriate_Element;

   end Raise_ASIS_Inappropriate_Component;

   -----------
   -- Valid --
   -----------

   function Valid (Comp : Record_Component) return Boolean is
   begin
      return Is_Opened (Comp.Parent_Context) and then
             Later (Opened_At (Comp.Parent_Context), Comp.Obtained);
   end Valid;

   function Valid (Comp : Array_Component)  return Boolean is
   begin
      return Is_Opened (Comp.Parent_Context) and then
             Later (Opened_At (Comp.Parent_Context), Comp.Obtained);
   end Valid;

end Asis.Data_Decomposition.Vcheck;
