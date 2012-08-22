------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          A 4 G . D D A _ A U X                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 1999-2012, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by AdaCore (http://www.adacore.com).                --
--                                                                          --
------------------------------------------------------------------------------

with Asis;
with Asis.Data_Decomposition;

use type Asis.Data_Decomposition.Portable_Value;

with Atree;   use Atree;
with Einfo;   use Einfo;
with Sem_Aux; use Sem_Aux;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Nlists;  use Nlists;
with System;  use System;

with Unchecked_Conversion;

package body A4G.DDA_Aux is

   pragma Warnings (Off, Default_Bit_Order);
   --  This pragma is needed to suppress warnings (generated in -gnatwa mode)
   --  for the conditions like
   --
   --    if Default_Bit_Order = High_Order_First then
   --
   --  Conditions like this includes two constants, so they are always True (or
   --  always False), but the value of Default_Bit_Order is different on
   --  different platforms

   -------------------------------------------
   -- Renamed Entities in Imported Packages --
   -------------------------------------------

   --  These are simply renamed to avoid the need for qualification

   subtype ASIS_Integer  is Asis.ASIS_Integer;
   subtype ASIS_Natural  is Asis.ASIS_Natural;
   subtype ASIS_Positive is Asis.ASIS_Positive;

   subtype Portable_Value is Asis.Data_Decomposition.Portable_Value;
   subtype Portable_Positive is Asis.Data_Decomposition.Portable_Positive;
   subtype Portable_Data is Asis.Data_Decomposition.Portable_Data;

   subtype Dimension_Indexes is Asis.Data_Decomposition.Dimension_Indexes;

   subtype Discrim_List is Repinfo.Discrim_List;

   ------------------------
   -- Local Declarations --
   ------------------------

   SU : constant := 8;
   --  Size of storage unit, basically we assume this throughout, but we
   --  still try to use this symbolic value everywhere, both for clarity
   --  and to assist anyone undertaking the (rather large) task of dealing
   --  with non-byte addressable machines.

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

   type Bit_String is array (0 .. ASIS_Natural'Last - 1) of Bit;
   pragma Pack (Bit_String);
   --  Type used for interpreting Portable_Data values as bit strings

   type Bit_String_Ptr is access all Bit_String;
   --  The actual access is via a bit string pointer, obtained by the
   --  use of unchecked conversion on the portable data value.

   function To_Bit_String_Ptr is
     new Unchecked_Conversion (Address, Bit_String_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Check (U : Node_Ref_Or_Val) return Uint;
   --  This function checks if the given value is a constant, and if so
   --  returns it, otherwise the exception Variable_Rep_Info is raised.

   function Check (U : Node_Ref_Or_Val) return ASIS_Natural;
   --  Like above function, but value is returned as ASIS_Natural. The
   --  exception Invalid_Data is raised if the value is not in the range
   --  of this type.

   function Check_And_Eval
     (U     : Node_Ref_Or_Val;
      Discs : Discrim_List)
      return  Uint;
   --  This function checks if the given value is a constant, or is a value
   --  that depends on the discriminants of its containing record. In the
   --  former case, the value is returned, in the latter case, the list of
   --  discriminants is used to evaluate the value. If U is No_Uint on
   --  entry, then the exception Variable_Rep_Info is raised.

   function Check_And_Eval
     (U     : Node_Ref_Or_Val;
      Discs : Discrim_List)
      return  ASIS_Natural;
   --  Like above function, but value is returned as ASIS_Natural. The
   --  exception Invalid_Data is raised if the value is not in the range
   --  of this type.

   function Extract_Field
     (Data  : Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id)
      return  Portable_Data;
   --  Given a portable data value, Data, takes the bit slice starting at
   --  offset Start, with length Len bits and returns a Portable_Value that
   --  is interpretable as a value of the given type Typ. In the case of
   --  a scalar value, the result will be 1,2,4, or 8 bytes long with proper
   --  sign or zero extension as required.

   function Set_Field
     (Data  : Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id;
      Val   : Portable_Data)
      return  Portable_Data;
   --  Given a portable data value, sets the bit slice in this value to
   --  contain the value corresponding to the value given in Val. The value
   --  returned is the resulting Portable_Data value, extended if necessary
   --  to be long enough to accomodate the slice, and with the new value
   --  set in place.

   procedure Set_Field
     (Data  : in out Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id;
      Val   : Portable_Data);
   --  This is similar in effect, except that the assignment is done in place
   --  to the supplied Data value, which must be long enough to accomodate the
   --  given slice (if it is not, then the Invalid_Data exception is raised.

   ------------------------
   -- Build_Discrim_List --
   ------------------------

   function Build_Discrim_List
     (Rec  : Entity_Id;
      Data : Portable_Data)
      return Discrim_List
   is
   begin
      if Is_Record_Type (Rec) and then Has_Discriminants (Rec) then
         declare
            D   : Entity_Id;
            Dis : Discrim_List (1 .. Number_Discriminants (Rec));

         begin
            D := First_Discriminant (Rec);
            for J in Dis'Range loop
               Dis (J) := Extract_Discriminant (Data, D);
               D := Next_Discriminant (D);
            end loop;

            return Dis;
         end;

      else
         return Null_Discrims;
      end if;
   end Build_Discrim_List;

   -----------
   -- Check --
   -----------

   function Check (U : Node_Ref_Or_Val) return Uint is
   begin
      if U = No_Uint or else U < 0 then
         raise Variable_Rep_Info;
      else
         return U;
      end if;
   end Check;

   function Check (U : Node_Ref_Or_Val) return ASIS_Natural is
   begin
      if U = No_Uint or else U < 0 then
         raise Variable_Rep_Info;

      elsif not UI_Is_In_Aint_Range (U) then
         raise Invalid_Data;

      else
         return UI_To_Aint (U);
      end if;
   end Check;

   --------------------
   -- Check_And_Eval --
   --------------------

   function Check_And_Eval
     (U     : Node_Ref_Or_Val;
      Discs : Discrim_List)
      return  Uint
   is
   begin
      if U = No_Uint then
         raise Variable_Rep_Info;
      else
         return Rep_Value (U, Discs);
      end if;
   end Check_And_Eval;

   function Check_And_Eval
     (U     : Node_Ref_Or_Val;
      Discs : Discrim_List)
      return  ASIS_Natural
   is
      V : Uint;

   begin
      if U = No_Uint then
         raise Variable_Rep_Info;

      else
         V := Rep_Value (U, Discs);

         if not UI_Is_In_Aint_Range (V) then
            raise Invalid_Data;
         else
            return UI_To_Aint (V);
         end if;
      end if;
   end Check_And_Eval;

   -----------------------
   -- Component_Present --
   -----------------------

   function Component_Present
     (Comp  : Entity_Id;
      Discs : Discrim_List)
      return  Boolean
   is
      Decl : constant Node_Id := Declaration_Node (Comp);
      Var  : Node_Id;

      function Variant_Present (V : Node_Id) return Boolean;
      --  Given the N_Variant node and using Discs as the global object
      --  representing the discriminant list, determines if the given
      --  variant is present for the given list of discriminant values.
      --  This includes checking the existance of enclosing variants in case
      --  if V is a nesed variant, or checking the presence of other
      --  variants in case if V is 'when OTHERS' variant

      function Variant_Present (V : Node_Id) return Boolean is

         Next_Var : Node_Id;
         --  Needed to iterate throurg the preceding variants in the
         --  same variant part

         Result   : Boolean := True;

         function Enclosing_Variant (V : Node_Id) return Node_Id;
         --  Implements Sinfo.Enclosing_Variant, which because of some
         --  unknown reeason always returns Empty ???

         function Enclosing_Variant (V : Node_Id) return Node_Id is
            Result : Node_Id := Empty;
         begin
            Result := Parent (Parent (Parent (V)));

            if Nkind (Result) /= N_Variant then
               Result := Empty;
            end if;

            return Result;
         end Enclosing_Variant;

      begin

         if No (V) then
            --  To stop the recursion in case of nested variants
            return True;

         else

            Next_Var := First_Non_Pragma (List_Containing (V));

            while Next_Var /= V loop

               --  Checking that all the preceding variants (if any) do not
               --  present

               if Rep_Value (Present_Expr (Next_Var), Discs) /= Uint_0 then
                  Result := False;
                  exit;
               end if;

               Next_Var := Next_Non_Pragma (Next_Var);
            end loop;

            if Result then
               --  Checking that the given variant presents "locally"
               Result := Rep_Value (Present_Expr (V), Discs) /= Uint_0;
            end if;

            return Result and then Variant_Present (Enclosing_Variant (V));

         end if;
      end Variant_Present;

   begin
      --  If not a component, assume must be present

      if Nkind (Decl) /= N_Component_Declaration then
         return True;

      --  If not in variant part, assume must be present

      else
         Var := Parent (Parent (Decl));

         if Nkind (Var) /= N_Variant then
            return True;

         --  Otherwise evaluate to see if present

         else
            return Variant_Present (Var);
         end if;
      end if;
   end Component_Present;

   -------------------------
   -- Decode_Scalar_Value --
   -------------------------

   function Decode_Scalar_Value
     (Typ  : Entity_Id;
      Data : Portable_Data)
      return Uint
   is
      U   : Uint;
      Neg : Boolean;

   begin
      U := Uint_0;

      --  Determine if input value is negative

      if Is_Unsigned_Type (Typ) or else Has_Biased_Representation (Typ) then
         Neg := False;

      elsif Default_Bit_Order = High_Order_First then
         Neg := Data (Data'First) >= 16#80#;
      else
         Neg := Data (Data'Last) >= 16#80#;
      end if;

      --  Negative values of a signed type

      if Neg then

         if Default_Bit_Order = Low_Order_First then
            for J in reverse Data'Range loop
               U := U * 256 + Int ((not Data (J)));
            end loop;

         else
            for J in Data'Range loop
               U := U * 256 + Int ((not Data (J)));
            end loop;
         end if;

         return -(U + 1);

      --  Non-negative values

      else
         if Default_Bit_Order = Low_Order_First then
            for J in reverse Data'Range loop

               U := U * 256 + Int (Data (J));
            end loop;

         else
            for J in Data'Range loop
               U := U * 256 + Int (Data (J));
            end loop;
         end if;

         --  Remove bias if biased type

         if Has_Biased_Representation (Typ) then
            return U + Eval_Scalar_Node
                         (Type_Low_Bound (First_Subtype (Typ)));
         else
            return U;
         end if;
      end if;
   end Decode_Scalar_Value;

   -------------------------
   -- Encode_Scalar_Value --
   -------------------------

   function Encode_Scalar_Value
     (Typ  : Entity_Id;
      Val  : ASIS_Integer)
      return Portable_Data
   is
   begin
      return Encode_Scalar_Value (Typ, UI_From_Aint (Val));
   end Encode_Scalar_Value;

   function Encode_Scalar_Value
     (Typ  : Entity_Id;
      Val  : Uint)
      return Portable_Data
   is
      V : Uint := Val;
      L : Portable_Positive;

      Lo : constant Uint :=
             Eval_Scalar_Node (Type_Low_Bound (Base_Type (Typ)));

      Hi : constant Uint :=
             Eval_Scalar_Node (Type_High_Bound (Base_Type (Typ)));

   begin
      if Val < Lo or else Val > Hi then
         raise Invalid_Data;
      end if;

      --  If biased type, then introduce bias

      if Has_Biased_Representation (Typ) then
         V := V - Eval_Scalar_Node (Type_Low_Bound (First_Subtype (Typ)));
      end if;

      --  Negative values (type must be signed). In these cases we adjust
      --  to get the corresponding unsigned value (which will look to be
      --  appropriately sign extended when it is stored in the output)

      if V < 0 then
         if V >= -(Uint_2 ** 7) then
            V := Uint_2 ** 8 + V;
            L := 1;

         elsif V >= -(Uint_2 ** 15) then
            V := Uint_2 ** 16 + V;
            L := 2;

         elsif V >= -(Uint_2 ** 31) then
            V := Uint_2 ** 32 + V;
            L := 4;

         elsif V >= -(Uint_2 ** 63) then
            V := Uint_2 ** 64 + V;
            L := 8;

         else
            raise Invalid_Data;
         end if;

      --  Non-negative values of unsigned types

      elsif Is_Unsigned_Type (Typ)
        or else Has_Biased_Representation (Typ)
      then
         if V < Uint_2 ** 8 then
            L := 1;

         elsif V < Uint_2 ** 16 then
            L := 2;

         elsif V < Uint_2 ** 32 then
            L := 4;

         elsif V < Uint_2 ** 64 then
            L := 8;

         else
            raise Invalid_Data;
         end if;

      --  Non-negative values of signed types

      else
         if V < Uint_2 ** 7 then
            L := 1;

         elsif V < Uint_2 ** 15 then
            L := 2;

         elsif V < Uint_2 ** 31 then
            L := 4;

         elsif V < Uint_2 ** 63 then
            L := 8;

         else
            raise Invalid_Data;
         end if;

      end if;

      declare
         Data : Portable_Data (1 .. L);

      begin
         if Default_Bit_Order = High_Order_First then
            for J in reverse Data'Range loop
               Data (J) := Portable_Value (UI_To_Int (V mod 256));
               V := V / 256;
            end loop;

         else
            for J in Data'Range loop
               Data (J) := Portable_Value (UI_To_Int (V mod 256));
               V := V / 256;
            end loop;
         end if;

         return Data;
      end;
   end Encode_Scalar_Value;

   ----------------------
   -- Eval_Scalar_Node --
   ----------------------

   function Eval_Scalar_Node
     (N     : Node_Id;
      Discs : Discrim_List := Null_Discrims)
      return  Uint
   is
      Dnum : Uint;
      Ent  : Entity_Id;

   begin
      --  Case of discriminant reference

      if Nkind (N) = N_Identifier
        and then Ekind (Entity (N)) = E_Discriminant
      then
         Dnum := Discriminant_Number (Entity (N));

         if Dnum > Discs'Last then
            raise Constraint_Error;
         else
            return Discs (UI_To_Int (Dnum));
         end if;

      --  Case of static expression, note that we cannot use Expr_Value
      --  here, since we cannot afford to drag in all of Sem_Eval.

      elsif Is_Static_Expression (N) then

         --  Identifier case

         if Nkind (N) = N_Identifier then
            Ent := Entity (N);

            --  Enumeration literal, we need the Pos value

            if Ekind (Ent) = E_Enumeration_Literal then
               return Enumeration_Pos (Ent);

            --  A user defined static constant

            else
               return Eval_Scalar_Node (Constant_Value (Ent), Discs);
            end if;

         --  Integer literal

         elsif Nkind (N) = N_Integer_Literal then
            return Intval (N);

         --  Only other possibility is a character literal

         else
            Ent := Entity (N);

            --  Since Character literals of type Standard.Character don't
            --  have any defining character literals built for them, they
            --  do not have their Entity set, so just use their Char
            --  code. Otherwise for user-defined character literals use
            --  their Pos value as usual.

            if No (Ent) then
               return Char_Literal_Value (N);

            --  Enumeration literal other than a character literal defined in
            --  Standard, we need the Pos value

            elsif Ekind (Ent) = E_Enumeration_Literal then
               return Enumeration_Pos (Ent);

            --  A user defined static constant

            else
               return Eval_Scalar_Node (Constant_Value (Ent), Discs);

            --  ??? There is at least one more case which can not handled
            --  properly yet: N is a reference to a component of a static
            --  record object

            end if;

         end if;

      --  If not static expression, or discriminant, cannot get bounds

      else
         raise Variable_Rep_Info;
      end if;
   end Eval_Scalar_Node;

   -----------------------------
   -- Extract_Array_Component --
   -----------------------------

   function Extract_Array_Component
     (Typ   : Entity_Id;
      Data  : Portable_Data;
      Subs  : Dimension_Indexes;
      Discs : Discrim_List := Null_Discrims)
      return  Portable_Data
   is
      N : constant ASIS_Natural := Linear_Index (Typ, Subs, Discs);
      S : constant ASIS_Natural := UI_To_Aint (Get_Component_Size (Typ));
      F : constant ASIS_Natural := N * S;

   begin
      return Extract_Field (Data, F, S, Component_Type (Typ));
   end Extract_Array_Component;

   -------------------
   -- Extract_Field --
   -------------------

   function Extract_Field
     (Data  : Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id)
      return  Portable_Data
   is
      P  : constant Bit_String_Ptr := To_Bit_String_Ptr (Data'Address);
      RL : ASIS_Natural;
      L  : ASIS_Natural;

      Uns : constant Boolean := Is_Unsigned_Type (Typ)
                                  or else Has_Biased_Representation (Typ);

   begin
      --  Here for non-scalar case, in this case, we simply build a
      --  portable data value that is the right length, rounded up to
      --  the next byte as needed, and then copy the bits to the target
      --  padding at the end with zero bits.

      if not Is_Scalar_Type (Typ) then
         declare
            Res : aliased Portable_Data (1 .. (Len + (SU - 1)) / SU);
            RP  : constant Bit_String_Ptr := To_Bit_String_Ptr (Res'Address);

         begin
            RP (0 .. Len - 1) := P (Start .. Start + Len - 1);
            RP (Len .. Res'Length * SU - 1) := (others => 0);
            return Res;
         end;

      --  For scalar types, things are more complex, since we have to deal
      --  with proper endian handling and proper sign/zero extension.

      else
         --  First job is to find length of result

         L := Len;

         if L <= 8 then
            RL := 1;

         elsif L <= 16 then
            RL := 2;

         elsif L <= 32 then
            RL := 4;

         else
            RL := 8;

            --  Deal with case where there are unused bits

            if L > 64 then
               L := 64;
            end if;
         end if;

         declare
            Res : aliased Portable_Data (1 .. RL);
            RP  : constant Bit_String_Ptr := To_Bit_String_Ptr (Res'Address);
            Ptr : ASIS_Integer;
            SX  : Bit;

         begin
            --  Big-endian case. In this case we fill the result from right
            --  to left, since we want the result right justified, and then
            --  zero/sign fill on the left (i.e. at low numbered bits).

            if Default_Bit_Order = High_Order_First then
               Ptr := RL * SU - 1;

               for J in reverse Start .. Start + Len - 1 loop
                  RP (Ptr) := P (J);
                  Ptr := Ptr - 1;
               end loop;

               if Uns then
                  SX := 0;
               else
                  SX := P (Start);
               end if;

               for J in reverse 0 .. Ptr loop
                  RP (J) := SX;
               end loop;

            --  Little-endian case. In this case, we fill the result from
            --  the left to right, since we want the result left justified,
            --  and then zero/sign on the right (i.e. at high numbered bits)

            else
               Ptr := 0;

               for J in Start .. Start + Len - 1 loop
                  RP (Ptr) := P (J);
                  Ptr := Ptr + 1;
               end loop;

               if Uns then
                  SX := 0;
               else
                  SX := P (Start + Len - 1);
               end if;

               for J in Ptr .. RL * SU - 1 loop
                  RP (J) := SX;
               end loop;
            end if;

            return Res;
         end;
      end if;
   end Extract_Field;

   ------------------------------
   -- Extract_Record_Component --
   ------------------------------

   function Extract_Record_Component
     (Data  : Portable_Data;
      Comp  : Entity_Id;
      Discs : Discrim_List := Null_Discrims)
      return Portable_Data
   is
   begin
      if Component_Present (Comp, Discs) then
         return
           Extract_Field
             (Data  => Data,
              Start => Check_And_Eval (Component_Bit_Offset (Comp), Discs),
              Len   => Check_And_Eval (Esize (Comp), Discs),
              Typ   => Etype (Comp));

      else
         raise No_Component;
      end if;
   end Extract_Record_Component;

   --------------------------
   -- Extract_Discriminant --
   --------------------------

   function Extract_Discriminant
     (Data : Portable_Data;
      Disc : Entity_Id)
      return Uint
   is
   begin
      return
        Decode_Scalar_Value
          (Etype (Disc),
           Extract_Field
            (Data  => Data,
             Start => Check (Component_Bit_Offset (Disc)),
             Len   => Check (Esize (Disc)),
             Typ   => Etype (Disc)));
   end Extract_Discriminant;

   ------------------------------
   -- Get_Component_Bit_Offset --
   ------------------------------

   function Get_Component_Bit_Offset
     (Comp  : Entity_Id;
      Discs : Discrim_List := Null_Discrims)
      return  Uint
   is
   begin
      if Component_Present (Comp, Discs) then
         return Check_And_Eval (Component_Bit_Offset (Comp), Discs);
      else
         raise No_Component;
      end if;
   end Get_Component_Bit_Offset;

   ------------------------
   -- Get_Component_Size --
   ------------------------

   function Get_Component_Size (Typ  : Entity_Id) return Uint is
   begin
      return Check (Component_Size (Typ));
   end Get_Component_Size;

   ---------------
   -- Get_Esize --
   ---------------

   function Get_Esize
     (Comp  : Entity_Id;
      Discs : Discrim_List := Null_Discrims)
      return  Uint
   is
   begin
      if Component_Present (Comp, Discs) then
         return Check_And_Eval (Esize (Comp), Discs);
      else
         raise No_Component;
      end if;
   end Get_Esize;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Typ   : Entity_Id;
      Sub   : ASIS_Positive;
      Discs : Discrim_List := Null_Discrims)
      return  ASIS_Natural
   is
      N : Node_Id;
      T : Entity_Id;
      L : Node_Id;
      U : Node_Id;

   begin
      N := First_Index (Typ);
      for J in 1 .. Sub - 1 loop
         N := Next_Index (N);
      end loop;

      T := Etype (N);

      L := Type_Low_Bound (T);

      U := Type_High_Bound (T);

      return
        UI_To_Aint
          (UI_Max (0, Eval_Scalar_Node (U, Discs)
                        - Eval_Scalar_Node (L, Discs)
                            + 1));
   end Get_Length;

   ------------------
   -- Linear_Index --
   ------------------

   function Linear_Index
     (Typ   : Entity_Id;
      Subs  : Dimension_Indexes;
      Discs : Discrim_List := Null_Discrims)
      return  ASIS_Natural
   is
      Indx : ASIS_Natural;
      Len  : ASIS_Positive;

   begin
      Indx := 0;

      --  For the normal case, we are row major

      if Convention (Typ) /= Convention_Fortran then
         for J in Subs'Range loop
            Len := Get_Length (Typ, J, Discs);

            if Subs (J) > Len then
               raise No_Component;
            else
               Indx := Indx * Len + Subs (J) - 1;
            end if;
         end loop;

      --  For Fortran, we are column major

      else
         for J in reverse Subs'Range loop
            Len := Get_Length (Typ, J, Discs);

            if Subs (J) > Len then
               raise No_Component;
            else
               Indx := Indx * Len + Subs (J) - 1;
            end if;
         end loop;
      end if;

      return Indx;
   end Linear_Index;

   -------------------------
   -- Set_Array_Component --
   -------------------------

   function Set_Array_Component
     (Typ   : Entity_Id;
      Data  : Portable_Data;
      Subs  : Dimension_Indexes;
      Val   : Portable_Data;
      Discs : Discrim_List := Null_Discrims)
      return  Portable_Data
   is
      N : constant ASIS_Natural := Linear_Index (Typ, Subs, Discs);
      S : constant ASIS_Natural := UI_To_Aint (Get_Component_Size (Typ));
      F : constant ASIS_Natural := N * S;

   begin
      return Set_Field (Data, F, S, Component_Type (Typ), Val);
   end Set_Array_Component;

   procedure Set_Array_Component
     (Typ   : Entity_Id;
      Data  : in out Portable_Data;
      Subs  : Dimension_Indexes;
      Val   : Portable_Data;
      Discs : Discrim_List := Null_Discrims)
   is
      N : constant ASIS_Natural := Linear_Index (Typ, Subs, Discs);
      S : constant ASIS_Natural := UI_To_Aint (Get_Component_Size (Typ));
      F : constant ASIS_Natural := N * S;

   begin
      Set_Field (Data, F, S, Component_Type (Typ), Val);
   end Set_Array_Component;

   ----------------------
   -- Set_Discriminant --
   ----------------------

   function Set_Discriminant
     (Data : Portable_Data;
      Disc : Entity_Id;
      Val  : Uint)
      return Portable_Data
   is
      F : constant ASIS_Natural := Check (Component_Bit_Offset (Disc));
      S : constant ASIS_Natural := Check (Esize (Disc));
      T : constant Entity_Id    := Etype (Disc);

   begin
      return Set_Field (Data, F, S, T, Encode_Scalar_Value (T, Val));
   end Set_Discriminant;

   procedure Set_Discriminant
     (Data : in out Portable_Data;
      Disc : Entity_Id;
      Val  : Uint)
   is
      F : constant ASIS_Natural := Check (Component_Bit_Offset (Disc));
      S : constant ASIS_Natural := Check (Esize (Disc));
      T : constant Entity_Id    := Etype (Disc);

   begin
      Set_Field (Data, F, S, T, Encode_Scalar_Value (T, Val));
   end Set_Discriminant;

   function Set_Discriminant
     (Data : Portable_Data;
      Disc : Entity_Id;
      Val  : ASIS_Integer)
      return Portable_Data
   is
      F : constant ASIS_Natural := Check (Component_Bit_Offset (Disc));
      S : constant ASIS_Natural := Check (Esize (Disc));
      T : constant Entity_Id    := Etype (Disc);

   begin
      return Set_Field (Data, F, S, T, Encode_Scalar_Value (T, Val));
   end Set_Discriminant;

   procedure Set_Discriminant
     (Data : in out Portable_Data;
      Disc : Entity_Id;
      Val  : ASIS_Integer)
   is
      F : constant ASIS_Natural := Check (Component_Bit_Offset (Disc));
      S : constant ASIS_Natural := Check (Esize (Disc));
      T : constant Entity_Id    := Etype (Disc);

   begin
      Set_Field (Data, F, S, T, Encode_Scalar_Value (T, Val));
   end Set_Discriminant;

   ---------------
   -- Set_Field --
   ---------------

   function Set_Field
     (Data  : Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id;
      Val   : Portable_Data)
      return  Portable_Data
   is
      Req_Bytes : constant ASIS_Natural := (Start + Len + (SU - 1)) / SU;

   begin
      if Data'Length >= Req_Bytes then
         declare
            Result : Portable_Data := Data;

         begin
            Set_Field (Result, Start, Len, Typ, Val);
            return Result;
         end;

      --  Extension of the value is needed

      else
         declare
            Result : Portable_Data (1 .. Req_Bytes);

         begin
            Result (1 .. Data'Length) := Data;

            for J in Data'Length + 1 .. Result'Length loop
               Result (J) := 0;
            end loop;

            Set_Field (Result, Start, Len, Typ, Val);
            return Result;
         end;
      end if;
   end Set_Field;

   procedure Set_Field
     (Data  : in out Portable_Data;
      Start : ASIS_Natural;
      Len   : ASIS_Natural;
      Typ   : Entity_Id;
      Val   : Portable_Data)
   is
      Req_Bytes : constant ASIS_Natural := (Start + Len + (SU - 1)) / SU;
      Val_Bits  : constant ASIS_Natural := Val'Length * SU;
      Min_Size  : constant ASIS_Natural := ASIS_Natural'Min (Len, Val_Bits);

      D : constant Bit_String_Ptr := To_Bit_String_Ptr (Data'Address);
      V : constant Bit_String_Ptr := To_Bit_String_Ptr (Val'Address);

      SX : Bit;
      --  0 or 1 for zero or sign extension

      Uns : constant Boolean := Is_Unsigned_Type (Typ)
                                  or else Has_Biased_Representation (Typ);

   begin
      --  Error if length of data not sufficient to accomodate new field

      if Data'Length < Req_Bytes then
         raise Constraint_Error;
      end if;

      --  Case of non-scalar type, in this case, we simply copy the data
      --  from the start of Val into place in the target, filling in only
      --  those bits corresponding to the actual field in the target.

      if not Is_Scalar_Type (Typ) then

         --  Error if supplied value is too short

         if Val_Bits < Len then
            raise Invalid_Data;
         end if;

         --  Otherwise copy in the required bits. Note that we do not
         --  check uncopied bits of the original field in this case.

         for J in 0 .. Len - 1 loop
            D (J + Start) := V (J);
         end loop;

         return;

      --  For a scalar type, things are more complicated, since we need
      --  to store the right set of bits, and then zero or sign extend.
      --  We also need to check that the value being stored is not too
      --  large, i.e. any unstored bits are zero or sign bits as required.

      --  For the little endian case, we store bits from the left end,
      --  low numbered bit first, i.e. low order bit first)

      elsif Default_Bit_Order = Low_Order_First then

         pragma Warnings (On, Default_Bit_Order);

         for J in 0 .. Min_Size - 1 loop
            D (J + Start) := V (J);
         end loop;

         --  Find proper extension bit

         if Uns or else V (Min_Size - 1) = 0 then
            SX := 0;
         else
            SX := 1;
         end if;

         --  If unstored bits, they must all be sign/zero extension bits

         if Len < Val_Bits then
            for J in Len .. Val_Bits - 1 loop
               if V (J) /= SX then
                  raise Invalid_Data;
               end if;
            end loop;

         --  Otherwise, store sign/zero extension bits in rest of target

         else -- Len >= Val_Bits
            for J in Val_Bits .. Len - 1 loop
               D (J + Start) := SX;
            end loop;
         end if;

         return;

      --  For the little endian case, we store bits from the right end,
      --  high numbered bit first, i.e. low order bit first)

      else -- Default_Bit_Order = High_Order_First then

         for J in 0 .. Min_Size - 1 loop
            D (Start + Len - 1 - J) := V (Val_Bits - 1 - J);
         end loop;

         --  Find proper extension bit

         if Uns or else V (Val_Bits - Min_Size) = 0 then
            SX := 0;
         else
            SX := 1;
         end if;

         --  If unstored bits, they must all be sign/zero extension bits

         if Len < Val_Bits then
            for J in Len  .. Val_Bits - 1 loop
               if V (Val_Bits - 1 - J) /= SX then
                  raise Invalid_Data;
               end if;
            end loop;

         --  Otherwise, store sign/zero extension bits in rest of target

         else -- Len >= Val_Bits
            for J in Val_Bits .. Len - 1 loop
               D (Start + Len - 1 - J) := SX;
            end loop;
         end if;

         return;
      end if;
   end Set_Field;

   --------------------------
   -- Set_Record_Component --
   --------------------------

   function Set_Record_Component
     (Data  : Portable_Data;
      Comp  : Entity_Id;
      Val   : Portable_Data;
      Discs : Discrim_List := Null_Discrims)
      return  Portable_Data
   is
      F : constant ASIS_Natural :=
            Check_And_Eval (Component_Bit_Offset (Comp), Discs);

      S : constant ASIS_Natural :=
            Check_And_Eval (Esize (Comp), Discs);

   begin
      if Component_Present (Comp, Discs) then
         return Set_Field (Data, F, S, Etype (Comp), Val);
      else
         raise No_Component;
      end if;
   end Set_Record_Component;

   procedure Set_Record_Component
     (Data  : in out Portable_Data;
      Comp  : Entity_Id;
      Val   : Portable_Data;
      Discs : Discrim_List := Null_Discrims)
   is
      F : constant ASIS_Natural :=
            Check_And_Eval (Component_Bit_Offset (Comp), Discs);

      S : constant ASIS_Natural :=
            Check_And_Eval (Esize (Comp), Discs);

   begin
      if Component_Present (Comp, Discs) then
         Set_Field (Data, F, S, Etype (Comp), Val);
      else
         raise No_Component;
      end if;
   end Set_Record_Component;

   ------------------
   -- UI_From_Aint --
   ------------------

   --  Due to the somewhat unfortunate choice of ASIS_Integer to be Integer
   --  rather than Int, there is no very simple way of doing this accurately.
   --  In fact, on all targets so far Integer and Int are the same type so
   --  we can simply assume that this test is OK.

   --  The following static assertions verify this assumption:

--   Assert_1 : constant := 1 / Boolean'Pos
--     (Int'Pos (Int'First) = ASIS_Integer'Pos (ASIS_Integer'First));

--   Assert_2 : constant := 1 / Boolean'Pos
--     (Int'Pos (Int'Last) = ASIS_Integer'Pos (ASIS_Integer'Last));

   function UI_From_Aint (A : ASIS_Integer) return Uint is
   begin
      return UI_From_Int (Int (A));
   end UI_From_Aint;

   -------------------------
   -- UI_Is_In_Aint_Range --
   -------------------------

   --  See comment and assertions for UI_From_Aint which also apply here

   function UI_Is_In_Aint_Range (U : Uint) return Boolean is
   begin
      return UI_Is_In_Int_Range (U);
   end UI_Is_In_Aint_Range;

   ----------------
   -- UI_To_Aint --
   ----------------

   function UI_To_Aint (U : Uint) return ASIS_Integer is
   begin
      if UI_Is_In_Aint_Range (U) then
         return ASIS_Integer (UI_To_Int (U));
      else
         raise Invalid_Data;
      end if;
   end UI_To_Aint;

end A4G.DDA_Aux;
