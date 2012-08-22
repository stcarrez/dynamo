------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--      A S I S . C O M P I L A T I O N _ U N I T S . R E L A T I O N S     --
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

with Asis.Errors;     use Asis.Errors;
with Asis.Exceptions; use Asis.Exceptions;
with Asis.Extensions; use Asis.Extensions;

with Asis.Set_Get;    use  Asis.Set_Get;

with A4G.Contt.Dp;    use A4G.Contt.Dp;
with A4G.Vcheck;      use A4G.Vcheck;

package body Asis.Compilation_Units.Relations is

   Package_Name : constant String := "Asis.Compilation_Units.Relations.";

   -----------------------
   -- Elaboration_Order --
   -----------------------
   --  NOT  IMPLEMENTED --

   function Elaboration_Order
     (Compilation_Units : Asis.Compilation_Unit_List;
      The_Context       : Asis.Context)
      return              Relationship
   is
   begin

      Check_Validity (The_Context,
               Package_Name & "Semantic_Dependence_Order");

      if Is_Nil (Compilation_Units) then
         return Nil_Relationship;
      end if;

      Not_Implemented_Yet (Diagnosis =>
                Package_Name & "Semantic_Dependence_Order");
      --  ASIS_Failed is raised, Not_Implemented_Error status is setted

      return Nil_Relationship; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Semantic_Dependence_Order");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Semantic_Dependence_Order",
            Ex         => Ex);
   end Elaboration_Order;

   -------------------------------
   -- Semantic_Dependence_Order --
   -------------------------------
   --   PARTIALLY IMPLEMENTED   --

   function Semantic_Dependence_Order
     (Compilation_Units : Asis.Compilation_Unit_List;
      Dependent_Units   : Asis.Compilation_Unit_List;
      The_Context       : Asis.Context;
      Relation          : Asis.Relation_Kinds)
      return              Relationship
   is
      Res_Cont_Id  : Context_Id;
      Arg_Kind     : Asis.Unit_Kinds;
      Result_List  : Compilation_Unit_List_Access;
      Missing_List : Compilation_Unit_List_Access;
      Missing_Len  : ASIS_Natural := 0;
   begin
      Check_Validity (The_Context, Package_Name & "Semantic_Dependence_Order");

      Res_Cont_Id := Get_Cont_Id (The_Context);
      --  The current implementation limitation is that all the units from
      --  Compilation_Units list and from Dependent_Units should be from
      --  The_Context

      for I in Compilation_Units'Range loop
         Check_Validity (Compilation_Units (I),
                         Package_Name & "Semantic_Dependence_Order");

         Arg_Kind := Kind (Compilation_Units (I));

         if Arg_Kind = Not_A_Unit                or else
            Arg_Kind = A_Nonexistent_Declaration or else
            Arg_Kind = A_Nonexistent_Body        or else
            Arg_Kind = A_Configuration_Compilation
         then
            Raise_ASIS_Inappropriate_Compilation_Unit (Diagnosis =>
               Package_Name & "Semantic_Dependence_Order");
         end if;

         if Res_Cont_Id /= Encl_Cont_Id (Compilation_Units (I)) then
            Not_Implemented_Yet (Diagnosis =>
               Package_Name &
               "Semantic_Dependence_Order (multi-context processing");
         end if;

      end loop;

      for I in Dependent_Units'Range loop
         Check_Validity (Dependent_Units (I),
                         Package_Name & "Semantic_Dependence_Order");

         Arg_Kind := Kind (Dependent_Units (I));

         if Arg_Kind = Not_A_Unit                or else
            Arg_Kind = A_Nonexistent_Declaration or else
            Arg_Kind = A_Nonexistent_Body        or else
            Arg_Kind = A_Configuration_Compilation
         then
            Raise_ASIS_Inappropriate_Compilation_Unit (Diagnosis =>
               Package_Name & "Semantic_Dependence_Order");
         end if;

         if Res_Cont_Id /= Encl_Cont_Id (Dependent_Units (I)) then
            Not_Implemented_Yet (Diagnosis =>
               Package_Name &
               "Semantic_Dependence_Order (multi-context processing");
         end if;

      end loop;

      if Is_Nil (Compilation_Units) then
         return Nil_Relationship;
      end if;

      case Relation is
         when Ancestors =>

            Set_All_Ancestors (Compilation_Units, Result_List);

         when Descendants =>

            Set_All_Descendants (Compilation_Units, Result_List);

         when Supporters =>

            Set_All_Supporters (Compilation_Units, Result_List);

         when Dependents =>

            Set_All_Dependents
              (Compilation_Units, Dependent_Units, Result_List);

         when Family =>

            Set_All_Families (Compilation_Units, Result_List);

         when Needed_Units =>

            Set_All_Needed_Units
              (Compilation_Units, Result_List, Missing_List);

      end case;

      if Missing_List /= null then
         Missing_Len := Missing_List'Length;
      end if;

      declare
         Result : Relationship
                    (Consistent_Length   => Result_List'Length,
                     Inconsistent_Length => 0,
                     Missing_Length      => Missing_Len,
                     Circular_Length     => 0);
      begin
         Result.Consistent := Result_List.all;

         if Missing_List /= null then
            Result.Missing := Missing_List.all;
         end if;

         Free (Result_List);
         Free (Missing_List);

         return Result;
      end;

   exception
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Semantic_Dependence_Order");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Semantic_Dependence_Order",
            Ex         => Ex);
   end Semantic_Dependence_Order;

end Asis.Compilation_Units.Relations;
