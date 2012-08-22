------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--          A S I S . C O M P I L A T I O N _ U N I T S . T I M E S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2008, Free Software Foundation, Inc.       --
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

with Asis.Exceptions; use Asis.Exceptions;
with Asis.Errors;     use Asis.Errors;

with Asis.Set_Get;    use Asis.Set_Get;

with A4G.Vcheck;      use A4G.Vcheck;

package body Asis.Compilation_Units.Times is

   Package_Name : constant String := "Asis.Compilation_Units.Times.";

   Standard_Time : constant Ada.Calendar.Time :=
      Ada.Calendar.Time_Of (1994, 12, 21, 0.0);
   --  used as Time_Of_Last_Update for the predefined Standard package

   --------------------
   -- Attribute_Time --
   --------------------

   function Attribute_Time
     (Compilation_Unit : Asis.Compilation_Unit;
      Attribute        : Wide_String)
      return             Ada.Calendar.Time
   is
   begin
      pragma Unreferenced (Attribute);

      Check_Validity (Compilation_Unit, Package_Name & "Attribute_Time");

      return Nil_ASIS_Time;
   end Attribute_Time;

   ------------------------------
   -- Compilation_CPU_Duration --
   ------------------------------

   function Compilation_CPU_Duration
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Standard.Duration
   is
   begin
      Check_Validity
        (Compilation_Unit, Package_Name & "Compilation_CPU_Duration");

      return 0.0;
   end Compilation_CPU_Duration;

   -------------------------
   -- Time_Of_Last_Update --
   -------------------------

   function Time_Of_Last_Update
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Ada.Calendar.Time
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Time_Of_Last_Update");

      if Get_Unit_Id (Compilation_Unit) = Standard_Id then

         return Standard_Time;
      else

         return Time_Stamp (Compilation_Unit);
      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Time_Of_Last_Update");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Time_Of_Last_Update",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Time_Of_Last_Update;

end Asis.Compilation_Units.Times;
