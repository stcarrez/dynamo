------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--          A S I S . D A T A _ D E C O M P O S I T I O N . A U X           --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Declarations; use Asis.Declarations;
with Asis.Definitions;  use Asis.Definitions;
with Asis.Elements;     use Asis.Elements;
with Asis.Expressions;  use Asis.Expressions;
with Asis.Extensions;   use Asis.Extensions;
with Asis.Iterator;     use Asis.Iterator;

with Asis.Set_Get;      use Asis.Set_Get;

with A4G.DDA_Aux;       use A4G.DDA_Aux;

with Atree;             use Atree;
with Einfo;             use Einfo;
with Elists;            use Elists;
with Sinfo;             use Sinfo;
with Uintp;             use Uintp;

package body Asis.Data_Decomposition.Aux is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Build_Discrim_List_From_Constraint
     (Rec :  Entity_Id)
      return Discrim_List;
   --  Given a record entiti which has discriminants, returns a list
   --  of discriminant values obtained from teh discriminant constraint or,
   --  if there is no constraint, from default discriminant values.
   --  Null_Discrims is returned in case if there is no constraint and no
   --  default values for discriminants. Raises Variable_Rep_Info
   --  if there is a default value which is not a static expression.

   function Elist_Len (List : Elist_Id) return Int;
   --  Computes the length of Entity list. 0 i sreturned in case if No (List).

   function Is_Static_Constraint (Discr_Constr : Elist_Id) return Boolean;
   --  Provided that Discr_Constr is obtained as Discriminant_Constraint
   --  attribute of some type entity, checks if all the constraints are static

   function Is_Empty_Record (Rec_Type_Def : Element) return Boolean;
   --  Supposes Rec_Type_Def to be of A_Record_Type_Definition kind. Checks
   --  if it defines a record type with no non-discriminant components

   ----------------------------------------
   -- Build_Discrim_List_From_Constraint --
   ----------------------------------------

   function Build_Discrim_List_From_Constraint
     (Rec :  Entity_Id)
      return Discrim_List
   is
      Constraint  : constant Elist_Id := Discriminant_Constraint (Rec);
      Res_Len     : constant Int      := Elist_Len (Constraint);
      Result      : Discrim_List (1 .. Res_Len);
      Next_Constr : Elmt_Id;
      Next_Expr   : Node_Id;
   begin

      Next_Constr := First_Elmt (Constraint);

      for J in 1 .. Res_Len loop
         Next_Expr := Node (Next_Constr);

         if Nkind (Next_Expr) = N_Discriminant_Association then
            Next_Expr := Sinfo.Expression (Next_Expr);
         end if;

         Result (J) := Eval_Scalar_Node (Next_Expr);
         Next_Constr := Next_Elmt (Next_Constr);
      end loop;

      return Result;

   end Build_Discrim_List_From_Constraint;

   ------------------------------------------
   -- Build_Discrim_List_If_Data_Presented --
   ------------------------------------------

   function Build_Discrim_List_If_Data_Presented
     (Rec          : Entity_Id;
      Data         : Asis.Data_Decomposition.Portable_Data;
      Ignore_Discs : Boolean := False)
      return         Discrim_List
   is
   begin
      if Data /= Nil_Portable_Data then
         return Build_Discrim_List (Rec, Data);

      elsif not Ignore_Discs and then
            Has_Discriminants (Rec)
      then
         --  Here we have the situation when the record type has
         --  discriminants, but no data stream is provided. Thit means, that
         --  we have to use the default values for discriminants

         return Build_Discrim_List_From_Constraint (Rec);
      else
         return Null_Discrims;
      end if;

   end Build_Discrim_List_If_Data_Presented;

   -------------------------------
   -- Component_Type_Definition --
   -------------------------------

   function Component_Type_Definition (E : Element) return Element is
      Result : Element := Nil_Element;
   begin
      case Int_Kind (E) is
         when A_Component_Declaration =>
            Result := Object_Declaration_View (E);
            Result := Component_Subtype_Indication (Result);
         when A_Subtype_Indication =>
            Result := E;
         when others =>
            pragma Assert (False);
            null;
      end case;

      Result := Asis.Definitions.Subtype_Mark (Result);

      if Int_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      Result := Corresponding_Name_Declaration (Result);

      Result := Corresponding_First_Subtype (Result);

      Result := Type_Declaration_View (Result);

      return Result;

   end Component_Type_Definition;

   ---------------------------
   -- Constraint_Model_Kind --
   ---------------------------

   function Constraint_Model_Kind
     (C    : Element)
      return Constraint_Model_Kinds
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (C);
      Result   : Constraint_Model_Kinds := Static_Constraint;
      --  We start from the most optimistic assumption

      Control : Traverse_Control := Continue;

      procedure Analyze_Constraint
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Constraint_Model_Kinds);
      --  Checks the individual constraint and its components. Used as
      --  Pre-Operation

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Constraint_Model_Kinds);
      --  Placeholder for Post-Operation

      procedure Traverse_Constraint is new Traverse_Element (
         State_Information => Constraint_Model_Kinds,
         Pre_Operation     => Analyze_Constraint,
         Post_Operation    => No_Op);

      ------------------------
      -- Analyze_Constraint --
      -------------------------

      procedure Analyze_Constraint
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Constraint_Model_Kinds)
      is
         Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
         Tmp_El   : Asis.Element;
         Tmp_Node : Node_Id;
      begin

         if Arg_Kind = An_Identifier
           and then
            Int_Kind (Enclosing_Element (Element)) =
               A_Discriminant_Association
           and then
            not Is_Equal (Element,
                          Discriminant_Expression
                            (Enclosing_Element (Element)))
         then
            --  If we are here, Element is from discriminant selector names
            return;
         end if;

         case Arg_Kind is

            when A_Discrete_Subtype_Indication =>

               if Is_Nil (Subtype_Constraint (Element)) then
                  --  If we are here then we have a range constraint. Let's try
                  --  to get the information directly from the tree (starting
                  --  from the overpessimistic assumption).

                  State  := External;
                  Tmp_El := Asis.Definitions.Subtype_Mark (Element);

                  if Expression_Kind (Tmp_El) = An_Attribute_Reference then
                     --  We are overpessimistic in this rather unusial case:
                     Control := Terminate_Immediately;
                     return;
                  end if;

                  Tmp_Node := R_Node (Tmp_El);
                  Tmp_Node := Entity (Tmp_Node);

                  if Present (Tmp_Node) then
                     Tmp_Node := Scalar_Range (Tmp_Node);

                     if Is_Static_Expression (Low_Bound (Tmp_Node))
                       and then
                        Is_Static_Expression (High_Bound (Tmp_Node))
                     then
                        State := Static_Constraint;
                     end if;

                  end if;

                  Control := Terminate_Immediately;
               end if;

            when A_Discrete_Range_Attribute_Reference =>

               if Is_Static (Element) then
                  Control := Abandon_Children;
               else
                  State   := External;
                  Control := Terminate_Immediately;
               end if;

            when Internal_Expression_Kinds =>
               if Is_True_Expression (Element) then

                  if Is_Static (Element) then
                     --  Nothing to do, no need to change State
                     Control := Abandon_Children;
                  else

                     if Arg_Kind = An_Identifier and then
                        Int_Kind (Corresponding_Name_Declaration (Element)) =
                           A_Discriminant_Specification
                     then
                        --  See RM 95 3.8(12)
                        State   := Discriminated;
                        Control := Abandon_Children;
                     else
                        --  Completely dinamic situation for sure
                        State   := External;
                        Control := Terminate_Immediately;
                     end if;

                  end if;

               else
                  --  The only possibility for those Elements which are in
                  --  Internal_Expression_Kinds, but are not
                  --  Is_True_Expression is a type mark, and we do not have to
                  --  analyze it
                  Control := Abandon_Children;
               end if;

            when An_Index_Constraint                |
                 A_Discriminant_Constraint          |
                 A_Discrete_Simple_Expression_Range |
                 A_Discriminant_Association         |
                 A_Simple_Expression_Range =>

               --  Just go down:
               null;

            when others =>
               pragma Assert (False);
               null;
         end case;

      end Analyze_Constraint;

      -----------
      -- No_Op --
      -----------

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Constraint_Model_Kinds)
      is
      begin
         pragma Unreferenced (Element);
         pragma Unreferenced (Control);
         pragma Unreferenced (State);

         null;
      end No_Op;

   begin

      if Arg_Kind = An_Index_Constraint or else
         Arg_Kind = A_Discriminant_Constraint
      then
         Traverse_Constraint
           (Element => C,
            Control => Control,
            State   => Result);
      end if;

      return Result;

   end Constraint_Model_Kind;

   ---------------------
   -- De_Linear_Index --
   ---------------------

   function De_Linear_Index
     (Index       : Asis.ASIS_Natural;
      D           : ASIS_Natural;
      Ind_Lengths : Dimention_Length;
      Conv        : Convention_Id := Convention_Ada)
      return        Dimension_Indexes
   is
      Len     : Asis.ASIS_Natural := 1;
      Tmp_Ind : Asis.ASIS_Natural := Index;
      Tmp_Res : Asis.ASIS_Natural;
      Result  : Dimension_Indexes (1 .. D);
   begin

      for J in 1 .. D loop
         Len := Len * Ind_Lengths (J);
      end loop;

      --   Len can never be 0, because this function can never be called
      --   for an empty array

      --  For the normal case, we are row major

      if Conv /= Convention_Fortran then

         for J in Result'Range loop
            Len := Len / Ind_Lengths (J);

            Tmp_Res := Tmp_Ind / Len;

            if Tmp_Res * Len < Tmp_Ind then
               Tmp_Res := Tmp_Res + 1;
            end if;

            Result (J) := Tmp_Res;

            Tmp_Ind := Tmp_Ind - Len * (Result (J) - 1);
         end loop;

      --  For Fortran, we are column major

      else

         for J in reverse Result'Range loop
            Len := Len / Ind_Lengths (J);

            Tmp_Res := Tmp_Ind / Len;

            if Tmp_Res * Len < Tmp_Ind then
               Tmp_Res := Tmp_Res + 1;
            end if;

            Result (J) := Tmp_Res;

            Tmp_Ind := Tmp_Ind - Len * (Result (J) - 1);
         end loop;

      end if;

      return Result;
   end De_Linear_Index;

   --------------------------------------------
   -- Discriminant_Part_From_Type_Definition --
   --------------------------------------------

   function Discriminant_Part_From_Type_Definition
     (T    : Element)
      return Element
   is
      Type_Entity   : Node_Id;
      Tmp_Element   : Element;
      Result        : Element := Nil_Element;
   begin

      Type_Entity := R_Node (T);

      if Nkind (Type_Entity) /= N_Private_Type_Declaration then
         Type_Entity := Parent (Type_Entity);
      end if;

      Type_Entity := Sinfo.Defining_Identifier (Type_Entity);

      if Einfo.Has_Discriminants (Type_Entity) then

         Result := Enclosing_Element (T);

         Result := Discriminant_Part (Result);

         if Is_Nil (Result) then
            --  Here we already know, that the type defined by T has
            --  discriminants. The only possibility is that it is derived
            --  from a type with known discriminant part. So we have to
            --  traverse backward the derivation chain and return the first
            --  known discriminant part found
            Tmp_Element := Corresponding_Parent_Subtype (T);
            Tmp_Element := Corresponding_First_Subtype (Tmp_Element);

            loop
               Result := Discriminant_Part (Tmp_Element);

               exit when not Is_Nil (Result);

               Tmp_Element := Type_Declaration_View (Tmp_Element);
               Tmp_Element := Corresponding_Parent_Subtype (Tmp_Element);
               Tmp_Element := Corresponding_First_Subtype (Tmp_Element);

            end loop;

         end if;

      end if;

      return Result;

   end Discriminant_Part_From_Type_Definition;

   ---------------
   -- Elist_Len --
   ---------------

   function Elist_Len (List : Elist_Id) return Int is
      Result  : Int := 0;
      Next_El : Elmt_Id;
   begin

      if Present (List) then
         Next_El := First_Elmt (List);

         while Present (Next_El) loop
            Result  := Result + 1;
            Next_El := Next_Elmt (Next_El);
         end loop;

      end if;

      return Result;
   end Elist_Len;

   ----------------------------
   -- Is_Derived_From_Record --
   ----------------------------

   function Is_Derived_From_Record (TD : Element) return Boolean is
      Result           : Boolean := False;
      Type_Entity_Node : Node_Id;
   begin

      if Int_Kind (TD) = A_Derived_Type_Definition then
         Type_Entity_Node := R_Node (TD);
         Type_Entity_Node := Defining_Identifier (Parent (Type_Entity_Node));
         Result := Is_Record_Type (Type_Entity_Node);
      end if;

      return Result;
   end Is_Derived_From_Record;

   ---------------------------
   -- Is_Derived_From_Array --
   ---------------------------

   function Is_Derived_From_Array (TD : Element) return Boolean is
      Result           : Boolean := False;
      Type_Entity_Node : Node_Id;
   begin

      if Int_Kind (TD) = A_Derived_Type_Definition then
         Type_Entity_Node := R_Node (TD);
         Type_Entity_Node := Defining_Identifier (Parent (Type_Entity_Node));
         Result := Is_Array_Type (Type_Entity_Node);
      end if;

      return Result;
   end Is_Derived_From_Array;

   ---------------------
   -- Is_Empty_Record --
   ---------------------

   function Is_Empty_Record (Rec_Type_Def : Element) return Boolean is
      Result   : Boolean          := False;
      Arg_Node : constant Node_Id := R_Node (Rec_Type_Def);

   begin
      if Null_Present (Arg_Node) or else
         Null_Present (Component_List (Arg_Node))
      then
         Result := True;
      end if;

      return Result;
   end Is_Empty_Record;

   --------------------------
   -- Is_Static_Constraint --
   --------------------------

   function Is_Static_Constraint (Discr_Constr : Elist_Id) return Boolean is
      Result  : Boolean := True;
      Next_DA : Elmt_Id;
   begin
      Next_DA := First_Elmt (Discr_Constr);

      while Present (Next_DA) loop

         if not Is_Static_Expression (Node (Next_DA)) then
            Result := False;
            exit;
         end if;

         Next_DA := Next_Elmt (Next_DA);
      end loop;

      return Result;

   end Is_Static_Constraint;

   ------------------
   -- Linear_Index --
   ------------------

   function Linear_Index
     (Inds        : Dimension_Indexes;
      Ind_Lengths : Dimention_Length;
      Conv        : Convention_Id := Convention_Ada)
      return        Asis.ASIS_Natural
   is
      Indx : Asis.ASIS_Natural := 0;
   begin
      --  For the normal case, we are row major

      if Conv /= Convention_Fortran then
         for J in Inds'Range loop
            Indx := Indx * Ind_Lengths (J) + Inds (J) - 1;
         end loop;

      --  For Fortran, we are column major

      else
         for J in reverse Inds'Range loop
            Indx := Indx * Ind_Lengths (J) + Inds (J) - 1;
         end loop;
      end if;

      return Indx + 1;
   end Linear_Index;

   -------------
   -- Max_Len --
   -------------

   function Max_Len (Component : Array_Component) return Asis.ASIS_Natural is
      Result : Asis.ASIS_Natural;
   begin
      if Component.Length (1) = 0 then
         --  the case of an empty array
         return 0;
      else
         Result := 1;
      end if;

      for J in Component.Length'Range loop
         exit when Component.Length (J) = 0;
         Result := Result * Component.Length (J);
      end loop;

      return Result;

   end Max_Len;

   -----------------------
   -- Record_Model_Kind --
   -----------------------

   function Record_Model_Kind (R : Element) return Type_Model_Kinds is
      Type_Entity : Node_Id;
      Result      : Type_Model_Kinds := Not_A_Type_Model;

      Control : Traverse_Control := Continue;

      procedure Analyze_Component_Definition
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Type_Model_Kinds);
      --  Checks the individual component definition. Used as Pre-Operation.

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Type_Model_Kinds);
      --  Placeholder for Post-Operation

      procedure Traverse_Record_Definition is new Traverse_Element (
         State_Information => Type_Model_Kinds,
         Pre_Operation     => Analyze_Component_Definition,
         Post_Operation    => No_Op);

      ----------------------------------
      -- Analyze_Component_Definition --
      ----------------------------------

      procedure Analyze_Component_Definition
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Type_Model_Kinds)
      is
      begin

         pragma Unreferenced (State);

         if Int_Kind (Element) = A_Component_Declaration then

            case Subtype_Model_Kind
                   (Component_Subtype_Indication
                      (Object_Declaration_View (Element)))
            is

               when A_Simple_Static_Model | A_Simple_Dynamic_Model =>
                  Control := Abandon_Children;

               when A_Complex_Dynamic_Model =>
                  Result  := A_Complex_Dynamic_Model;
                  Control := Terminate_Immediately;

               when Not_A_Type_Model =>
                  Result  := Not_A_Type_Model;
                  Control := Terminate_Immediately;

            end case;

         end if;

      end Analyze_Component_Definition;

      -----------
      -- No_Op --
      -----------

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Type_Model_Kinds)
      is
      begin
         pragma Unreferenced (Element);
         pragma Unreferenced (Control);
         pragma Unreferenced (State);

         null;
      end No_Op;

   begin
      Type_Entity := R_Node (Enclosing_Element (R));
      Type_Entity := Sinfo.Defining_Identifier (Type_Entity);

      if Is_Empty_Record (R)
        or else
         (not (Has_Discriminants (Type_Entity)
            and then
             not Is_Constrained (Type_Entity))
        and then
          (RM_Size (Type_Entity) > 0))
      then
         Result := A_Simple_Static_Model;

      elsif RM_Size (Type_Entity) = Uint_0 or else
            RM_Size (Type_Entity) = No_Uint
      then
         --  This is the case when the front-end consider the type to be
         --  essentially dynamic and therefore it can not set Esize field
         --  at all
         Result := A_Complex_Dynamic_Model;

      else
         --  We start from the most optimistic assumption
         Result := A_Simple_Dynamic_Model;

         Traverse_Record_Definition
           (Element => R,
            Control => Control,
            State   => Result);

      end if;

      return Result;
   end Record_Model_Kind;

   ---------------------------
   -- Root_Array_Definition --
   ---------------------------

   function Root_Array_Definition  (Type_Def : Element) return Element is
      Result : Element := Type_Def;
   begin

      if Is_Derived_From_Array (Type_Def) then
         Result := Corresponding_Root_Type (Type_Def);
         Result := Type_Declaration_View (Result);
      end if;

      return Result;

   end Root_Array_Definition;

   ----------------------------
   -- Root_Record_Definition --
   ----------------------------

   function Root_Record_Definition (Type_Def : Element) return Element is
      Result : Element := Type_Def;
   begin

      if Is_Derived_From_Record (Type_Def) then
         Result := Corresponding_Root_Type (Type_Def);
         Result := Type_Declaration_View (Result);
      end if;

      return Result;

   end Root_Record_Definition;

   --------------------
   -- Subtype_Entity --
   --------------------

   function Subtype_Entity (E : Element) return Entity_Id is
      Result   : Node_Id   := Node (E);
      Res_Kind : Node_Kind := Nkind (Result);
   begin

      while Present (Result) and then
            not (Res_Kind = N_Subtype_Declaration     or else
                 Res_Kind = N_Object_Declaration      or else
                 Res_Kind = N_Derived_Type_Definition or else
                 Res_Kind = N_Full_Type_Declaration   or else
                 Res_Kind = N_Component_Declaration)
      loop
         Result   := Parent (Result);
         Res_Kind := Nkind (Result);
      end loop;

      pragma Assert (Present (Result));

      case Res_Kind is
         when N_Subtype_Declaration =>
            Result := Defining_Identifier (Result);

         when N_Object_Declaration | N_Component_Declaration =>
            Result := Etype (Defining_Identifier (Result));

         when N_Derived_Type_Definition =>
            Result := Defining_Identifier (Parent (Result));

         when N_Full_Type_Declaration =>
            --  Here we are expecting the component subtype definition from
            --  array type declaration as the only possible case
            Result := Defining_Identifier (Result);

            pragma Assert (Is_Array_Type (Result));

            Result := Component_Type (Result);

         when others =>
            null;
      end case;

      return Result;

   end Subtype_Entity;

   ------------------------
   -- Subtype_Model_Kind --
   ------------------------

   function Subtype_Model_Kind (S : Element) return Type_Model_Kinds is
      Result         : Type_Model_Kinds := Not_A_Type_Model;

      Type_Mark_Elem : Element;
      Type_Mark_Def  : Element;
      Constr_Elem    : Element;

      Constraint_Model : Constraint_Model_Kinds := Not_A_Constraint_Model;

      Type_Entity    : Entity_Id;

   begin

      pragma Assert (Int_Kind (S) = A_Subtype_Indication);

      Type_Mark_Elem := Asis.Definitions.Subtype_Mark (S);
      Constr_Elem    := Subtype_Constraint (S);

      if Int_Kind (Type_Mark_Elem) = A_Selected_Component then
         Type_Mark_Elem := Selector (Type_Mark_Elem);
      end if;

      Type_Mark_Def := Corresponding_Name_Declaration (Type_Mark_Elem);

      if Int_Kind (Type_Mark_Def) = A_Private_Type_Declaration then
         Type_Mark_Def := Corresponding_Type_Declaration (Type_Mark_Def);
      end if;

      Type_Entity   := Sinfo.Defining_Identifier (R_Node (Type_Mark_Def));

      --  Type_Mark_Def can only be either type or subtype declaration

      Type_Mark_Def := Type_Declaration_View (Type_Mark_Def);

      case Int_Kind (Type_Mark_Def) is
         when Internal_Type_Kinds =>
            Result := Type_Model_Kind (Type_Mark_Def);

         when A_Subtype_Indication =>
            Result := Subtype_Model_Kind (Type_Mark_Def);

         when others =>
            Result := A_Complex_Dynamic_Model;
      end case;

      if Result in A_Simple_Static_Model .. A_Simple_Dynamic_Model then
         --  Here we have to chech if the constraint (if any) affects the
         --  result
         case Int_Kind (Constr_Elem) is

            when An_Index_Constraint |
                 A_Discriminant_Constraint =>
               Constraint_Model := Constraint_Model_Kind (Constr_Elem);

            when Not_An_Element =>

               if Has_Discriminants (Type_Entity)
                 and then
                 not Is_Empty_Elmt_List (Discriminant_Constraint (Type_Entity))
               then
                  --  This is the case, when a subtype indication to analyze
                  --  does not contain any explicit constraint, but
                  --  the corresponding discriminanted subtype might be
                  --  constrained by explicit constraint or default values
                  --  somewhere before in subtyping and/or derivation chain

                  if Is_Static_Constraint
                       (Discriminant_Constraint (Type_Entity))
                  then
                     Constraint_Model := Static_Constraint;
                  end if;

               end if;

            when others =>
               --  We consider, that other kinds of the constraint can not
               --  affect the result
               null;
         end case;

         case Constraint_Model is
            when External =>
               Result := A_Complex_Dynamic_Model;

            when Discriminated =>
               Result := A_Simple_Dynamic_Model;

            when Static_Constraint =>
               Result := A_Simple_Static_Model;

            when others =>
               null;
         end case;

      end if;

      return Result;

   end Subtype_Model_Kind;

   ---------------------------------------
   -- Type_Definition_From_Subtype_Mark --
   ---------------------------------------

   function Type_Definition_From_Subtype_Mark (S : Element) return Element is
      Result : Element;
   begin

      if Int_Kind (S) = A_Selected_Component then
         Result := Selector (S);
      else
         Result := S;
      end if;

      Result := Corresponding_Name_Declaration (Result);
      Result := Corresponding_First_Subtype    (Result);
      Result := Type_Declaration_View          (Result);

      return Result;
   end Type_Definition_From_Subtype_Mark;

   -------------------
   -- Wrong_Indexes --
   -------------------

   function Wrong_Indexes
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return Boolean
   is
      D      : constant ASIS_Natural := Component.Dimension;
      Result : Boolean               := True;
   begin

      if D = Indexes'Length then
         Result := False;

         for J in 1 .. D loop
            if Indexes (J) > Component.Length (J) then
               Result := True;
               exit;
            end if;
         end loop;
      end if;

      return Result;
   end Wrong_Indexes;

end Asis.Data_Decomposition.Aux;
