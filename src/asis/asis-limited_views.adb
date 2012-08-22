------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                   A S I S . L I M I T E D _ V I E W S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- This specification  is  added to be used together with the Ada  Semantic --
-- Interface Specification Standard (ISO/IEC 15291) for use with GNAT.      --                        --
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

with Asis.Compilation_Units;    use Asis.Compilation_Units;
with Asis.Elements;             use Asis.Elements;
with Asis.Errors;               use Asis.Errors;
with Asis.Exceptions;           use Asis.Exceptions;

with Asis.Set_Get;              use  Asis.Set_Get;

with A4G.Asis_Tables;
with A4G.Contt.TT;
with A4G.Contt.UT;
with A4G.Mapping;               use A4G.Mapping;
with A4G.Vcheck;                use A4G.Vcheck;

with Atree;                     use Atree;
with Nlists;                    use Nlists;
with Sinfo;                     use Sinfo;

package body Asis.Limited_Views is

   Package_Name : constant String := "Asis.Limited_Views.";

   -------------------------
   -- Get_Nonlimited_View --
   -------------------------

   function Get_Nonlimited_View (D : Asis.Element) return Asis.Element is
      Encl_Unit     :          Asis.Compilation_Unit;
      Arg_Node      :          Node_Id;
      Res_Node      :          Node_Id;
      Def_Name_Case : constant Boolean := Element_Kind (D) = A_Defining_Name;
   begin
      if not Is_From_Limited_View (D) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Is_From_Limited_View (non-limited view as actual)",
            Wrong_Kind => Int_Kind (D));
      end if;

      Encl_Unit := Enclosing_Compilation_Unit (D);

      if Has_Limited_View_Only (Encl_Unit) then
         return Nil_Element;
      end if;

      Arg_Node := Node (D);

      if Def_Name_Case then
         while not (Is_List_Member (Arg_Node)
             or else
               Nkind (Arg_Node) = N_Package_Declaration)
         loop
            Arg_Node := Parent (Arg_Node);
         end loop;

      end if;

      A4G.Asis_Tables.Create_Node_Trace (Arg_Node);
      A4G.Contt.TT.Reset_Tree_For_Unit (Encl_Unit);
      Res_Node := A4G.Contt.TT.Restore_Node_From_Trace (CU => Encl_Unit);

      if Def_Name_Case then
         if Nkind (Res_Node) = N_Package_Declaration then
            if Is_List_Member (Res_Node) then
               Res_Node := Defining_Unit_Name (Sinfo.Specification (Res_Node));
            else
               Res_Node := Defining_Identifier (Res_Node);
            end if;
         end if;
      end if;

      return Node_To_Element_New
               (Node    => Res_Node,
                In_Unit => Encl_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => D,
               Outer_Call => Package_Name & "Get_Nonlimited_View");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Get_Nonlimited_View",
            Ex            => Ex,
            Arg_Element   => D);
   end Get_Nonlimited_View;

   ---------------------------
   -- Has_Limited_View_Only --
   ---------------------------

   function Has_Limited_View_Only
     (Right : Asis.Compilation_Unit)
     return Boolean
   is
      Result : Boolean := False;
   begin
      if Unit_Kind (Right) = A_Package
        and then
         not Is_Standard (Right)
      then
         Result := A4G.Contt.UT.Has_Limited_View_Only
                     (Encl_Cont_Id (Right),
                      Get_Unit_Id (Right));
      end if;

      return Result;
   exception
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Has_Limited_View_Only");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Has_Limited_View_Only",
            Ex         => Ex,
            Arg_CU     => Right);
   end Has_Limited_View_Only;

   --------------------------
   -- Is_From_Limited_View --
   --------------------------

   function Is_From_Limited_View (D : Asis.Element) return Boolean is
   begin
      return Special_Case (D) = From_Limited_View;
   end Is_From_Limited_View;

end Asis.Limited_Views;
