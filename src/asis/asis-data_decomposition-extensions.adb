------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--   A S I S . D A T A _ D E C O M P O S I T I O N . E X T E N S I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
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

with Ada.Unchecked_Conversion;
with System;                          use System;

with Asis.Data_Decomposition.Vcheck;  use Asis.Data_Decomposition.Vcheck;
with Asis.Data_Decomposition.Set_Get; use Asis.Data_Decomposition.Set_Get;
with Asis.Errors;                     use Asis.Errors;
with Asis.Exceptions;                 use Asis.Exceptions;
with Asis.Set_Get;                    use Asis.Set_Get;

with A4G.DDA_Aux;                     use A4G.DDA_Aux;
with A4G.Vcheck;                      use A4G.Vcheck;

with Einfo;                           use Einfo;
with Namet;                           use Namet;
with Uintp;                           use Uintp;
with Urealp;                          use Urealp;

package body Asis.Data_Decomposition.Extensions  is

   Package_Name : constant String := "Asis.Data_Decomposition.Extensions.";

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Write_Real_To_Buffer (U : Ureal);
   --  Puts nnn/ddd to Name_Buffer, where nnn and ddd are integer values of
   --  the normalized numerator and denominator of the given real value.
   --  This procedure is supposed to be used to output images of positive
   --  real values only, so it assumes, that nnn is always positive.

   procedure Add_Uint_To_Buffer (U : Uint);
   --  Add image of universal integer to Name_Buffer, updating Name_Len
   --  (This procedure is the simplified version of the local procedure
   --  Exp_Dbug.Add_Uint_To_Buffer - it always consider its argument
   --  being positive)

   ------------------------
   -- Add_Uint_To_Buffer --
   ------------------------

   procedure Add_Uint_To_Buffer (U : Uint) is
   begin
      UI_Image (U, Decimal);
      Add_Str_To_Name_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
   end Add_Uint_To_Buffer;

   -------------------------------
   -- Component_Name_Definition --
   -------------------------------

   function Component_Name_Definition
     (Component : Record_Component)
      return      Asis.Declaration
   is
      Result : Asis.Element;
   begin
      Check_Validity (Component, Package_Name & "Component_Name_Definition");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Component
           (Diagnosis      => Package_Name & "Component_Name_Definition",
            Component_Kind => Rec);
      end if;

      Result := Component_Name (Component);

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Name_Definition");

         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Name_Definition",
            Ex          => Ex);
   end Component_Name_Definition;

   --------------------------
   -- Delta_Value (String) --
   --------------------------

   function Delta_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                String
   is
      Arg_Node      : Node_Id;
      Res_Ureal     : Ureal;
   begin
      Check_Validity
        (Fixed_Point_Subtype, Package_Name & "Delta_Value (String)");

      Arg_Node := R_Node (Fixed_Point_Subtype);

      if not (Int_Kind (Fixed_Point_Subtype) = A_Defining_Identifier and then
              Is_Fixed_Point_Type (Arg_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Delta_Value (String)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype));
      end if;

      Res_Ureal := Einfo.Delta_Value (Arg_Node);

      Write_Real_To_Buffer (Res_Ureal);

      return Namet.Name_Buffer (1 .. Namet.Name_Len);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Delta_Value (String)",
               Argument   => Fixed_Point_Subtype);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Delta_Value (String)",
           Ex          => Ex,
           Arg_Element => Fixed_Point_Subtype);
   end Delta_Value;

   ----------------------------
   -- Delta_Value (Fraction) --
   ----------------------------

   function Delta_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                Fraction
   is
      Arg_Node  : Node_Id;
      Res_Ureal : Ureal;

      Result    : Fraction;

   begin
      Check_Validity
        (Fixed_Point_Subtype, Package_Name & "Delta_Value (Fraction)");

      Arg_Node := R_Node (Fixed_Point_Subtype);

      if not (Int_Kind (Fixed_Point_Subtype) = A_Defining_Identifier and then
              Is_Fixed_Point_Type (Arg_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Delta_Value (Fraction)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype));
      end if;

      Res_Ureal := Einfo.Delta_Value (Arg_Node);

      Result.Num   := UI_To_Aint (Norm_Num (Res_Ureal));
      Result.Denum := UI_To_Aint (Norm_Den (Res_Ureal));

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Delta_Value (Fraction)",
               Argument   => Fixed_Point_Subtype);
         end if;

         raise;
      when Invalid_Data =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Delta_Value (Fraction)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype),
            Status     => Data_Error);
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Delta_Value (Fraction)",
           Ex          => Ex,
           Arg_Element => Fixed_Point_Subtype);
   end Delta_Value;

   ------------------
   -- Digits_Value --
   ------------------

   function Digits_Value
     (Floating_Point_Subtype : Asis.Element)
      return                   ASIS_Natural
   is
      Arg_Node      : Node_Id;
   begin
      Check_Validity (Floating_Point_Subtype, Package_Name & "Digits_Value");

      Arg_Node := R_Node (Floating_Point_Subtype);

      if not (Int_Kind (Floating_Point_Subtype) = A_Defining_Identifier
            and then
             (Is_Decimal_Fixed_Point_Type (Arg_Node) or else
              Is_Floating_Point_Type (Arg_Node)))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Digits_Value",
            Wrong_Kind => Int_Kind (Floating_Point_Subtype));
      end if;

      return ASIS_Natural (UI_To_Int (Einfo.Digits_Value (Arg_Node)));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Digits_Value",
               Argument   => Floating_Point_Subtype);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Digits_Value",
           Ex          => Ex,
           Arg_Element => Floating_Point_Subtype);
   end Digits_Value;

   --------------------------
   --  Portable_Data_Value --
   --------------------------

   function Portable_Data_Value
     (Value : Constrained_Subtype)
      return  Portable_Data
   is

      Local_Value : aliased constant Constrained_Subtype := Value;
      for Local_Value'Alignment use Standard'Maximum_Alignment;
      --  We force the maximum alignment for Local_Value to make it
      --  compatible with maximum alignment set for Portable_Data
      --  in the spec of Asis.Data_Decomposition

      subtype Result_Portable_Data is
         Portable_Data (1 .. (Constrained_Subtype'Object_Size + 7) / 8);

      type Result_Portable_Data_Access is access Result_Portable_Data;

      function To_Result_Portable_Data_Access is new
         Ada.Unchecked_Conversion (Address, Result_Portable_Data_Access);

      Result : constant Result_Portable_Data_Access :=
         To_Result_Portable_Data_Access (Local_Value'Address);

   begin

      return Result.all;
   exception
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Portable_Data_Value");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Portable_Data_Value",
           Ex          => Ex);
   end Portable_Data_Value;

   -----------------
   -- Scale_Value --
   -----------------

   function Scale_Value
     (Desimal_Fixed_Point_Subtype : Asis.Element)
      return                        ASIS_Natural
   is
      Arg_Node      : Node_Id;

   begin
      Check_Validity
        (Desimal_Fixed_Point_Subtype, Package_Name & "Scale_Value");

      Arg_Node := R_Node (Desimal_Fixed_Point_Subtype);

      if not (Int_Kind (Desimal_Fixed_Point_Subtype) = A_Defining_Identifier
            and then
              Is_Decimal_Fixed_Point_Type (Arg_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Scale_Value",
            Wrong_Kind => Int_Kind (Desimal_Fixed_Point_Subtype));
      end if;

      return ASIS_Natural (UI_To_Int (Einfo.Scale_Value (Arg_Node)));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Scale_Value",
               Argument   => Desimal_Fixed_Point_Subtype);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Scale_Value",
           Ex          => Ex,
           Arg_Element => Desimal_Fixed_Point_Subtype);
   end Scale_Value;

   --------------------------
   -- Small_Value (String) --
   --------------------------

   function Small_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                String
   is
      Arg_Node  : Node_Id;
      Res_Ureal : Ureal;

   begin
      Check_Validity
        (Fixed_Point_Subtype, Package_Name & "Small_Value (String)");

      Arg_Node := R_Node (Fixed_Point_Subtype);

      if not (Int_Kind (Fixed_Point_Subtype) = A_Defining_Identifier and then
              Is_Fixed_Point_Type (Arg_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Small_Value (String)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype));
      end if;

      Res_Ureal := Einfo.Small_Value (Arg_Node);

      Write_Real_To_Buffer (Res_Ureal);

      return Namet.Name_Buffer (1 .. Namet.Name_Len);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Small_Value (String)",
               Argument   => Fixed_Point_Subtype);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Small_Value (String)",
           Ex          => Ex,
           Arg_Element => Fixed_Point_Subtype);
   end Small_Value;

   ----------------------------
   -- Small_Value (Fraction) --
   ----------------------------

   function Small_Value
     (Fixed_Point_Subtype : Asis.Element)
      return                Fraction
   is
      Arg_Node  : Node_Id;
      Res_Ureal : Ureal;

      Result    : Fraction;
   begin
      Check_Validity
        (Fixed_Point_Subtype, Package_Name & "Small_Value (Fraction)");

      Arg_Node := R_Node (Fixed_Point_Subtype);

      if not (Int_Kind (Fixed_Point_Subtype) = A_Defining_Identifier and then
              Is_Fixed_Point_Type (Arg_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Small_Value (Fraction)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype));
      end if;

      Res_Ureal := Einfo.Small_Value (Arg_Node);

      Result.Num   := UI_To_Aint (Norm_Num (Res_Ureal));
      Result.Denum := UI_To_Aint (Norm_Den (Res_Ureal));

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Small_Value (Fraction)",
               Argument   => Fixed_Point_Subtype);
         end if;

         raise;

      when Invalid_Data =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Small_Value (Fraction)",
            Wrong_Kind => Int_Kind (Fixed_Point_Subtype),
            Status     => Data_Error);
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Small_Value (Fraction)",
           Ex          => Ex,
           Arg_Element => Fixed_Point_Subtype);
   end Small_Value;

   --------------------------
   -- Write_Real_To_Buffer --
   --------------------------

   procedure Write_Real_To_Buffer (U : Ureal) is
   begin
      Namet.Name_Len := 0;

      Add_Uint_To_Buffer (Norm_Num (U));
      Add_Str_To_Name_Buffer ("/");
      Add_Uint_To_Buffer (Norm_Den (U));
   end Write_Real_To_Buffer;

end Asis.Data_Decomposition.Extensions;
