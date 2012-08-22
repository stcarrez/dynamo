------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--             A S I S . E X T E N S I O N S . I T E R A T O R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (c) 2003-2006, Free Software Foundation, Inc.          --
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

with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Elements;           use Asis.Elements;
with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Iterator;           use Asis.Iterator;

with A4G.Vcheck;              use A4G.Vcheck;

package body Asis.Extensions.Iterator is

   Package_Name : constant String := "Asis.Extensions.Iterator.";

   -------------------
   -- Traverse_Unit --
   -------------------

   procedure Traverse_Unit
     (Unit    :        Asis.Compilation_Unit;
      Control : in out Traverse_Control;
      State   : in out State_Information)
   is
      Arg_Kind : constant Unit_Kinds := Unit_Kind (Unit);

      procedure Process_Element is new Asis.Iterator.Traverse_Element
        (State_Information => State_Information,
         Pre_Operation     => Pre_Operation,
         Post_Operation    => Post_Operation);

   begin
      Check_Validity (Unit, Package_Name & "Control");

      if not (Arg_Kind in A_Procedure .. A_Protected_Body_Subunit) then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Traverse_Unit");
      end if;

      declare
         Cont_Clause_Elements : constant Element_List :=
            Asis.Elements.Context_Clause_Elements
              (Compilation_Unit => Unit,
               Include_Pragmas  => True);

         Unit_Element : constant Asis.Element :=
           Asis.Elements.Unit_Declaration (Unit);

      begin

         for I in Cont_Clause_Elements'Range loop
            Process_Element (Cont_Clause_Elements (I), Control, State);
         end loop;

         Process_Element (Unit_Element, Control, State);
      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Inappropriate_Context     |
           ASIS_Inappropriate_Container   |
           ASIS_Inappropriate_Element     |
           ASIS_Inappropriate_Line        |
           ASIS_Inappropriate_Line_Number =>

         Add_Call_Information (Outer_Call => Package_Name & "Traverse_Unit");
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Traverse_Unit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Traverse_Unit",
            Ex            => Ex,
            Arg_CU        => Unit);
   end Traverse_Unit;

end Asis.Extensions.Iterator;
