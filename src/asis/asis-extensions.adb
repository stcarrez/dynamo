------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                      A S I S . E X T E N S I O N S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Asis.Compilation_Units;    use Asis.Compilation_Units;
with Asis.Declarations;         use Asis.Declarations;
with Asis.Definitions;          use Asis.Definitions;
with Asis.Errors;               use Asis.Errors;
with Asis.Exceptions;           use Asis.Exceptions;
with Asis.Expressions;          use Asis.Expressions;
with Asis.Statements;           use Asis.Statements;

with Asis.Set_Get;              use  Asis.Set_Get;

with A4G.A_Debug;               use A4G.A_Debug;
with A4G.A_Opt;                 use A4G.A_Opt;
with A4G.A_Sem;                 use A4G.A_Sem;
with A4G.A_Sinput;              use A4G.A_Sinput;
with A4G.Contt;                 use A4G.Contt;
with A4G.Contt.TT;              use A4G.Contt.TT;
with A4G.Contt.UT;              use A4G.Contt.UT;
with A4G.DDA_Aux;               use A4G.DDA_Aux;
with A4G.Decl_Sem;              use A4G.Decl_Sem;
with A4G.Asis_Tables;           use A4G.Asis_Tables;
with A4G.Expr_Sem;              use A4G.Expr_Sem;
with A4G.GNAT_Int;              use A4G.GNAT_Int;
with A4G.Mapping;               use A4G.Mapping;
with A4G.Queries;               use A4G.Queries;
with A4G.Vcheck;                use A4G.Vcheck;

with Atree;                     use Atree;
with Einfo;                     use Einfo;
with Elists;                    use Elists;
with Namet;                     use Namet;
with Nlists;                    use Nlists;
with Output;                    use Output;
with Sinfo;                     use Sinfo;
with Sinput;                    use Sinput;
with Snames;                    use Snames;
with Stand;                     use Stand;
with Stringt;                   use Stringt;
with Uintp;                     use Uintp;
with Urealp;                    use Urealp;

package body Asis.Extensions is

   Package_Name : constant String := "Asis.Extensions.";

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Typeless_Subaggregate (Aggr : Node_Id) return Boolean;
   --  Checks if Aggr represents an inner typeless subaggregate of
   --  multi-dimensional array aggregate. A caller is responsible for providing
   --  only nodes that represents components of array aggregates as actuals.

   function Is_Expanded_Subprogram (N : Node_Id) return Boolean;
   --  Checks if N corresponds to the spec of an expanded generic
   --  subprogram. Is needed because Comes_From_Source in this case is
   --  set OFF (opposite to expanded packages)

   function Is_Type_Operator
     (Op_Decl   : Asis.Element;
      Type_Decl : Asis.Element)
      return    Boolean;
   --  Checks if Op_Decl declares an operator function having a parameter
   --  or a result of the type Type_Decl (Type_Decl is supposed to be a type
   --  declaration name). Returns False for a function body if the body has
   --  the separate spec

   function Overrides_Type_Operator
     (Op_Decl   : Asis.Element;
      Type_Decl : Asis.Element)
      return      Boolean;
   --  Provided that Is_Type_Operator (Op_Decl, Type_Decl) is True (note,
   --  that this function does not check this, it should be checked by the
   --  caller), checks if Op_Decl overrides a predefined or inherited
   --  operator function that exists for Type_Decl

   function Is_From_Import_Procedure_Pragma (N : Node_Id) return Boolean;
   --  Checks a specific situation for an identifier specific to a pragma for
   --  GNAT-specific pragmas Import_Procedure and  Import_Valued_Procedure -
   --  for components of MECHANISM_NAME having the form of A (B).

   function Get_LF_From_Ureal (U : Ureal) return Long_Long_Float;
   --  Converts universal real into Long_Float. This is a quick-and-dirty
   --  solution for extending Static_Expression_Value_Image for real image,
   --  it may blow up in case if numerator or denominator is too big. The
   --  conversion does some arbitrary rounding (I believe this rounding is
   --  reasonable, but I have no proof of this)

   pragma Unreferenced (Get_LF_From_Ureal);

   function Get_Implemented_Op
     (Op_Decl  : Asis.Element;
      Type_Def : Asis.Element)
      return     Asis.Element;
   pragma Unreferenced (Get_Implemented_Op);
   --  Op_Decl is supposed to be a declaration of a dispatching operation for
   --  that Is_Overriding_Operation is true. Type_Def is supposed to be an
   --  interface type definition for some interface type that is included in
   --  the interface list of the definition of the type that is the type
   --  of dispatching operand(s) of Op_Decl. This function checks if Op_Decl
   --  may implement some operation of this interface, and if it may returns
   --  the declaration of this interface operation as a result, otherwise it
   --  returns Nil_Element.

   function Is_Procedure (Decl : Asis.Element) return Boolean;
   pragma Unreferenced (Is_Procedure);
   --  Checks that Decl declares a procedure

   ------------------
   -- Acts_As_Spec --
   ------------------

   function Acts_As_Spec (Declaration : Asis.Element) return Boolean is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node  : Node_Id;
      Name_Node : Node_Id;
      Spec_Node : Node_Id;
      Arg_Ekind : Entity_Kind;

      Result : Boolean := False;
   begin
      Check_Validity (Declaration, Package_Name & "Acts_As_Spec");

      Arg_Node := Node (Declaration);

      case Arg_Kind is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  =>
            Result := Acts_As_Spec (Arg_Node);

            --  The problem here is that for some subprogram bodies the
            --   front-end creates artificial specs and sets OFF the
            --  Acts_As_Spec flag for the body. At the moment we have detected
            --  two such situations (and we exclude the case of expanded
            --  subprogram body not to mix up with the similar situation in
            --  the tree, see :

            if not Result
              and then
               Special_Case (Declaration) /= Expanded_Subprogram_Instantiation
            then

               --  (1) Bodies declared immediately within protected bodies

               if Nkind (Parent (Arg_Node)) = N_Protected_Body then
                  Spec_Node := Corresponding_Spec (Arg_Node);

                  if Is_Artificial_Protected_Op_Item_Spec (Spec_Node) then
                     Result := True;
                  end if;

               else

                  --  (2) child subprogram bodies with no separate spec

                  Name_Node := Defining_Unit_Name (Specification (Arg_Node));

                  if Nkind (Name_Node) = N_Defining_Program_Unit_Name then

                     Arg_Node := Corresponding_Spec (Arg_Node);

                     if Present (Arg_Node) then
                        while not
                          (Nkind (Arg_Node) = N_Subprogram_Declaration or else
                           Nkind (Arg_Node) = N_Generic_Subprogram_Declaration)
                        loop
                           Arg_Node := Parent (Arg_Node);
                        end loop;

                        Result := not Comes_From_Source (Arg_Node);
                     end if;

                  end if;

               end if;

            end if;

         when A_Procedure_Body_Stub |
              A_Function_Body_Stub =>
            Arg_Ekind := Ekind (Defining_Unit_Name (Specification (Arg_Node)));

            Result := Arg_Ekind = E_Function
              or else Arg_Ekind = E_Procedure;

         when An_Expression_Function_Declaration =>

            if Is_Part_Of_Inherited (Declaration) then
               Result := True;
            else
               Result :=
                 Nkind (R_Node (Declaration)) = N_Subprogram_Declaration;
            end if;

         when others => null;
      end case;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Acts_As_Spec");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Acts_As_Spec",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Acts_As_Spec;

   ------------------------------
   -- Compilation_Dependencies --
   ------------------------------

   function Compilation_Dependencies
     (Main_Unit : Asis.Compilation_Unit)
      return      Asis.Compilation_Unit_List
   is
      Arg_Kind    : constant Asis.Unit_Kinds := Kind (Main_Unit);
      Arg_Unit_Id : Unit_Id;
      Res_Cont_Id : Context_Id;
   begin
      Check_Validity (Main_Unit, Package_Name & "Compilation_Dependencies");

      if Arg_Kind not in A_Procedure .. A_Protected_Body_Subunit then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Compilation_Dependencies");
      end if;

      Res_Cont_Id := Encl_Cont_Id (Main_Unit);
      Reset_Context (Res_Cont_Id);
      Arg_Unit_Id := Get_Unit_Id  (Main_Unit);

      declare
         Result_Id_List : constant Unit_Id_List :=
           GNAT_Compilation_Dependencies (Arg_Unit_Id);

         Result_List : constant Compilation_Unit_List :=
           Get_Comp_Unit_List (Result_Id_List, Res_Cont_Id);
      begin

         if Is_Nil (Result_List) then
            Raise_ASIS_Inappropriate_Compilation_Unit
              (Diagnosis => Package_Name & "Compilation_Dependencies");
         else
            return Result_List;
         end if;

      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Dependencies");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Compilation_Dependencies",
            Ex            => Ex,
            Arg_CU        => Main_Unit);
   end Compilation_Dependencies;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Source_File      :     String_Access;
      Args             :     Argument_List;
      Success          : out Boolean;
      GCC              :     String_Access := null;
      Use_GNATMAKE     :     Boolean       := False;
      Use_Temp_Prj     :     Boolean       := False;
      Compiler_Out     :     String        := "";
      All_Warnings_Off :     Boolean       := True;
      Display_Call     :     Boolean       := False)
   is
      Comp_Args : Argument_List (Args'First .. Args'Last + 12 + 1);

      First_Idx : constant Integer := Comp_Args'First;
      Last_Idx  : Integer := First_Idx;

      Obj_Name : String_Access;
      Dot_Idx  : Natural;

      Is_GNAAMP_Call : Boolean := False;
      --  In case of the call to GNAAMP we should not set '-x ada' flags

      Is_GNATMAKE_Call : Boolean := Use_GNATMAKE;

   begin

      if Is_GNATMAKE_Call and then GCC = null then
         --  We can not set gnatmake-specific parameters in this case
         Is_GNATMAKE_Call := False;
      end if;

      if  GCC /= null then
         declare  --  ??? What an awful code!
            Name    : constant String   := To_Lower (Base_Name (GCC.all));
            Dot_Idx : Positive          := Name'Last;
         begin
            for J in reverse Name'Range loop
               if Name (J) = '.' then
                  Dot_Idx := J - 1;
                  exit;
               end if;
            end loop;

            if Name (Name'First .. Dot_Idx) = "gnaamp" then
               Is_GNAAMP_Call := True;
            end if;
         end;
      end if;

      Comp_Args (Last_Idx) := Comp_Flag;
      Last_Idx := Last_Idx + 1;

      Comp_Args (Last_Idx) := GNAT_Flag_ct;
      Last_Idx := Last_Idx + 1;

      if Is_GNATMAKE_Call then
         Comp_Args (Last_Idx) := GNATMAKE_Flag_u;
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GNATMAKE_Flag_f;
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GNATMAKE_Flag_q;
         Last_Idx := Last_Idx + 1;
      elsif not Is_GNAAMP_Call then
         Comp_Args (Last_Idx) := GCC_Flag_X;
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GCC_Par_Ada;
         Last_Idx := Last_Idx + 1;
      end if;

      for J in Args'Range loop
         Comp_Args (Last_Idx) := Args (J);
         Last_Idx := Last_Idx + 1;
      end loop;

      if All_Warnings_Off then
         Comp_Args (Last_Idx) := GNAT_Flag_ws;
         Last_Idx := Last_Idx + 1;

         Comp_Args (Last_Idx) := GNAT_Flag_yN;
         Last_Idx := Last_Idx + 1;

      end if;

      Comp_Args (Last_Idx) := Source_File;

      if Is_GNATMAKE_Call
       and then
         not Use_Temp_Prj
      then
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GNATMAKE_Flag_cargs;
         Last_Idx := Last_Idx + 1;
         Comp_Args (Last_Idx) := GCC_Flag_o;
         Last_Idx := Last_Idx + 1;

         Obj_Name := new String'(Base_Name (Source_File.all));
         Dot_Idx  := Obj_Name'Last;

         for J in reverse Obj_Name'Range loop

            if Obj_Name (J) = '.' then
               Dot_Idx := J - 1;
               exit;
            end if;

         end loop;

         Comp_Args (Last_Idx) := new String'
           (Get_Current_Dir                      &
            Directory_Separator                  &
            Obj_Name (Obj_Name'First .. Dot_Idx) &
            ".o");
      end if;

      Success :=
        Execute
          (GCC, Comp_Args (Args'First .. Last_Idx), Compiler_Out,
           Display_Call => Display_Call);
   end Compile;

   ----------------
   -- Components --
   ----------------

   function Components (E : Asis.Element) return Asis.Element_List is
      Child_Access  : constant Query_Array := Appropriate_Queries (E);
      Result_Length : Integer := 0;
   begin
      Check_Validity (E, Package_Name & "Components");

      if Is_Nil (E) then
         return Nil_Element_List;
      end if;

      --  first, we compute the result's length:

      for Each_Query in Child_Access'Range loop
         case Child_Access (Each_Query).Query_Kind is
            when Bug =>
               null;
            when Single_Element_Query =>
               if not Is_Nil (Child_Access (Each_Query).Func_Simple (E)) then
                  Result_Length := Result_Length + 1;
               end if;
            when Element_List_Query =>
               declare
                  Child_List : constant Asis.Element_List :=
                     Child_Access (Each_Query).Func_List (E);
               begin
                  Result_Length := Result_Length + Child_List'Length;
               end;
            when Element_List_Query_With_Boolean =>
               declare
                  Child_List : constant Asis.Element_List :=
                     Child_Access (Each_Query).Func_List_Boolean
                        (E, Child_Access (Each_Query).Bool);
               begin
                  Result_Length := Result_Length + Child_List'Length;
               end;
         end case;
      end loop;

      --  and now, we define the result element list of Result_Length
      --  length and fill it in by repeating the same loop. This is
      --  not effective, and this will have to be revised.

      if Result_Length = 0 then
         return Nil_Element_List;
      end if;

      declare
         Result_List : Asis.Element_List (1 .. Result_Length);
         Next_Element : Integer := 1;
      begin

         for Each_Query in Child_Access'Range loop
            case Child_Access (Each_Query).Query_Kind is
               when Bug =>
                  null;
               when Single_Element_Query =>
                  if not Is_Nil
                    (Child_Access (Each_Query).Func_Simple (E)) then
                     Result_List (Next_Element) :=
                        Child_Access (Each_Query).Func_Simple (E);
                     Next_Element := Next_Element + 1;
                  end if;
               when Element_List_Query =>
                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List (E);
                  begin
                     for I in Child_List'First .. Child_List'Last loop
                        Result_List (Next_Element) := Child_List (I);
                        Next_Element := Next_Element + 1;
                     end loop;
                  end;
               when Element_List_Query_With_Boolean =>
                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List_Boolean
                           (E, Child_Access (Each_Query).Bool);
                  begin
                     for I in Child_List'First .. Child_List'Last loop
                        Result_List (Next_Element) := Child_List (I);
                        Next_Element := Next_Element + 1;
                     end loop;
                  end;
            end case;
         end loop;
         return Result_List;
      end;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => E,
               Outer_Call => Package_Name & "Components");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Components",
            Ex            => Ex,
            Arg_Element   => E);
   end Components;

   -----------------------------------------------
   -- Corresponding_Body_Parameter_Definition --
   -----------------------------------------------

   function Corresponding_Body_Parameter_Definition
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Defining_Name);

      Encl_Constr      : Asis.Element;
      Encl_Constr_Kind : Internal_Element_Kinds;
      Result           : Asis.Element := Nil_Element;

   begin
      Check_Validity
        (Defining_Name,
         Package_Name & "Corresponding_Body_Parameter_Definition");

      if Arg_Kind /= A_Defining_Identifier then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Body_Parameter_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Encl_Constr := Enclosing_Element (Defining_Name);

      if Declaration_Kind (Encl_Constr) not in A_Formal_Declaration then
         Encl_Constr := (Enclosing_Element (Encl_Constr));
      end if;

      Encl_Constr_Kind := Int_Kind (Encl_Constr);

      case Encl_Constr_Kind is

         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration =>

            Result := Defining_Name;

         when A_Procedure_Body_Stub |
              A_Function_Body_Stub =>

            Encl_Constr := Corresponding_Subunit (Encl_Constr);

         when A_Procedure_Declaration        |
              A_Function_Declaration         |
              A_Generic_Function_Declaration |
              A_Generic_Procedure_Declaration =>

            Encl_Constr := Corresponding_Body (Encl_Constr);
            Encl_Constr_Kind := Int_Kind (Encl_Constr);

            if Encl_Constr_Kind = A_Procedure_Body_Stub or else
               Encl_Constr_Kind = A_Function_Body_Stub
            then
               Encl_Constr := Corresponding_Subunit (Encl_Constr);
            elsif Encl_Constr_Kind = An_Import_Pragma then
               Encl_Constr := Nil_Element;
            end if;

         when others =>
            --  For all the other situations we can not return a parameter
            --  definition in the body
            Encl_Constr := Nil_Element;
      end case;

      if not Is_Nil (Result)
        or else
         Is_Nil (Encl_Constr)
        or else
         Declaration_Kind (Encl_Constr) = Not_A_Declaration
      then

         return Result;
      end if;

      Process_Parameter_Specifications : declare

         Def_Name_Image : constant String
            := To_Lower (To_String (Defining_Name_Image (Defining_Name)));

         Param_Specs : constant Asis.Element_List
            := Parameter_Profile (Encl_Constr);

      begin

         Through_Parameter_Specs : for I in Param_Specs'Range loop

            Process_Parameter_Names : declare
               Par_Names : constant Asis.Element_List :=
                 Names (Param_Specs (I));
            begin

               Through_Parameter_Names : for J in Par_Names'Range loop
                  if Def_Name_Image =
                     To_Lower (To_String (Defining_Name_Image
                       (Par_Names (J))))
                  then
                     Result := Par_Names (J);
                     exit Through_Parameter_Specs;
                  end if;

               end loop Through_Parameter_Names;

            end Process_Parameter_Names;

         end loop Through_Parameter_Specs;

      end Process_Parameter_Specifications;

      pragma Assert (not Is_Nil (Result));

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Defining_Name,
               Outer_Call => Package_Name &
                             "Corresponding_Body_Parameter_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Body_Parameter_Definition",
            Ex            => Ex,
            Arg_Element   => Defining_Name);
   end Corresponding_Body_Parameter_Definition;

   -----------------------------------------
   -- Corresponding_Called_Entity_Unwound --
   -----------------------------------------

   function Corresponding_Called_Entity_Unwound
     (Statement : Asis.Statement)
      return      Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
      Arg_Node_Kind : Node_Kind;
      Result_Node   : Node_Id;
      Result_Unit   : Compilation_Unit;
      Res_Spec_Case : Special_Cases := Not_A_Special_Case;
   begin
      Check_Validity
        (Statement, Package_Name & "Corresponding_Called_Entity_Unwound");

      if not (Arg_Kind = An_Entry_Call_Statement or else
              Arg_Kind = A_Procedure_Call_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Called_Entity_Unwound",
            Wrong_Kind => Arg_Kind);
      end if;

      --  the implementation approach is similar to the approach taken for
      --  Asis.Expressions.Corresponding_Called_Function

      Arg_Node := R_Node (Statement);
      --  To be on the safe side, we use R_Node instead of Node, but it looks
      --  like in this case R_Node and Node should be the same
      Arg_Node_Kind := Nkind (Arg_Node);

      case Arg_Node_Kind is
         when  N_Attribute_Reference =>
            return Nil_Element;
            --  call to a procedure-attribute
         when  N_Entry_Call_Statement | N_Procedure_Call_Statement =>
            --  here we have to filter out the case when Nil_Element
            --  should be returned for a call through access-to-function:
            if Nkind (Sinfo.Name (Arg_Node)) = N_Explicit_Dereference then
               return Nil_Element;
            end if;
--  ??? <tree problem 4>
--  this fragment should be revised when the problem is fixed (as it should)
            if Arg_Node_Kind = N_Entry_Call_Statement then
               Result_Node := Sinfo.Name (Arg_Node);
               --  Result_Node points to the name of the called entry
               if Nkind (Result_Node) = N_Indexed_Component then
                  --  this is the case for a call to an entry from an
                  --  entry family
                  Result_Node := Prefix (Result_Node);
               end if;
               Result_Node := Entity (Selector_Name (Result_Node));
            else
               Result_Node := Entity (Sinfo.Name (Arg_Node));
               --  only this assignment is needed if tree problem 4 is
               --  fixed
            end if;
--  ??? <tree problem 4>  - end
         when others =>
            pragma Assert (False);
            null;
      end case;

      Result_Node := Unwind_Renaming (Result_Node);

      if No (Result_Node) then
         --  renaming of a procedure-attribute
         return Nil_Element;
      end if;

      if not Comes_From_Source (Result_Node) then
         return Nil_Element;
      end if;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Statement), Result_Node);

--      if not Is_Consistent (Result_Unit, Encl_Unit (Statement)) then
--         return Nil_Element;
--      end if;

      --  And now - from a defining name to a declaration itself
      Result_Node := Parent (Result_Node);

      if Nkind (Result_Node) in
           N_Function_Specification .. N_Procedure_Specification
      then
         Result_Node := Parent (Result_Node);
      end if;

      if Is_Expanded_Subprogram (Result_Node) then
         Res_Spec_Case := Expanded_Subprogram_Instantiation;
      end if;

      return Node_To_Element_New
        (Node      => Result_Node,
         Spec_Case => Res_Spec_Case,
         In_Unit   => Result_Unit);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name &
                             "Corresponding_Called_Entity_Unwound");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Called_Entity_Unwound",
            Ex            => Ex,
            Arg_Element   => Statement);
   end Corresponding_Called_Entity_Unwound;

   -------------------------------------------
   -- Corresponding_Called_Function_Unwound --
   -------------------------------------------

   function Corresponding_Called_Function_Unwound
     (Expression : Asis.Expression)
      return       Asis.Declaration
   is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node      : Node_Id;
      Arg_Node_Kind : Node_Kind;
      Result_Node   : Node_Id;
      Result_Unit   : Compilation_Unit;
      Res_Spec_Case : Special_Cases := Not_A_Special_Case;
   begin
      Check_Validity
        (Expression, Package_Name & "Corresponding_Called_Function_Unwound");

      if not (Arg_Kind = A_Function_Call) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Called_Function_Unwound",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we have to filter out the cases when a Nil_Element
      --  should be returned. For now, these cases include:
      --
      --  - calls to functions-attributes;
      --  - all forms of calls to predefined operators;
      --  - all forms of calls to inherited functions
      --
      --  We hope to implement the last case in future...

      --  First, we try the simplest approach, and then we will add patches
      --  if needed:

      Arg_Node      := R_Node (Expression);
      Arg_Node_Kind := Nkind (Arg_Node);
      --  Rewritten node should know everything. But if this node is the
      --  result of compile-time optimization, we have to work with
      --  original node only:
      if Arg_Node_Kind = N_String_Literal    or else
         Arg_Node_Kind = N_Integer_Literal   or else
         Arg_Node_Kind = N_Real_Literal      or else
         Arg_Node_Kind = N_Character_Literal or else
         Arg_Node_Kind = N_Raise_Constraint_Error or else
         Arg_Node_Kind = N_Identifier
      then
         Arg_Node      := Node (Expression);
         Arg_Node_Kind := Nkind (Arg_Node);

      elsif Arg_Node_Kind = N_Explicit_Dereference then
         --  See F727-023
         Arg_Node      := Sinfo.Prefix (Arg_Node);
         Arg_Node_Kind := Nkind (Arg_Node);

      end if;

      case Arg_Node_Kind is
         when  N_Attribute_Reference =>
            return Nil_Element;
         when  N_Function_Call            |
               N_Procedure_Call_Statement =>

            --  The second choice here corresponds to a procedure that is an
            --  argument of Debug pragma

            --  here we have to filter out the case when Nil_Element
            --  should be returned for a call through access-to-function:
            if Nkind (Sinfo.Name (Arg_Node)) = N_Explicit_Dereference then
               return Nil_Element;
            else
               Result_Node := Entity (Sinfo.Name (Arg_Node));
            end if;
         when N_Op =>
            --  all the predefined operations (??)
            Result_Node := Entity (Arg_Node);
         when others =>
            pragma Assert (False);
            null;
      end case;

      --  here we have Result_Node pointed to the defining occurrence of
      --  the corresponding called function. Three things should be done:
      --  1. If Result_Node is defined in a renaming definition, we have
      --     to unwind all the renamings till the defining occurrence of
      --     the corresponding callable entity will be reached;
      --  2. If a given callable entity is implicitly defined, Nil_Element

      --     should be returned;
      --  3. We have to come from a defining name to the corresponding
      --     declaration and then we should return the Element
      --     corresponding to this declaration

      Result_Node := Unwind_Renaming (Result_Node);

      if No (Result_Node) then
         --  renaming of a function-attribute
         return Nil_Element;
      end if;

      --  here we have Result_Node pointing to the defining occurrence of the
      --  name of the corresponding called function. First, we have to
      --  filter out implicitly declared functions:

      if not Comes_From_Source (Result_Node) then
         return Nil_Element;
      end if;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Expression), Result_Node);

      Result_Node := Parent (Result_Node);

      if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
         Result_Node := Parent (Result_Node);
      end if;

      Result_Node := Parent (Result_Node);
      --  to go from a defining name to a declaration itself

      if Is_Expanded_Subprogram (Result_Node) then
         Res_Spec_Case := Expanded_Subprogram_Instantiation;
      end if;

      return Node_To_Element_New
        (Node      => Result_Node,
         Spec_Case => Res_Spec_Case,
         In_Unit   => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Corresponding_Called_Function_Unwound");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Called_Function_Unwound",
            Ex            => Ex,
            Arg_Element   => Expression);
   end Corresponding_Called_Function_Unwound;

   ------------------------------------
   -- Corresponding_First_Definition --
   ------------------------------------

   function Corresponding_First_Definition
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Defining_Name);

      Is_Parameter      : Boolean := False;
      Encl_Constr       : Asis.Element;
      Encl_Constr_Kind  : Internal_Element_Kinds;
      First_Declaration : Asis.Element;

      Result            : Asis.Element := Nil_Element;

   begin
      Check_Validity
        (Defining_Name, Package_Name & "Corresponding_First_Definition");

      if Arg_Kind not in Internal_Defining_Name_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_First_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Encl_Constr := Enclosing_Element (Defining_Name);

      if Int_Kind (Encl_Constr) = A_Parameter_Specification then
         Encl_Constr := Enclosing_Element (Encl_Constr);
         Is_Parameter := True;
      end if;

      if Is_Subunit (Encl_Constr) then
         Encl_Constr := Corresponding_Body_Stub (Encl_Constr);
      end if;

      Encl_Constr_Kind := Int_Kind (Encl_Constr);

      case Encl_Constr_Kind is

         when A_Procedure_Body_Declaration     |
              A_Function_Body_Declaration      |
              A_Function_Renaming_Declaration  |
              A_Procedure_Renaming_Declaration |
              A_Procedure_Body_Stub            |
              A_Function_Body_Stub               =>

            if ((Encl_Constr_Kind = A_Procedure_Body_Declaration  or else
                 Encl_Constr_Kind = A_Function_Body_Declaration   or else
                 Encl_Constr_Kind = A_Procedure_Body_Stub         or else
                 Encl_Constr_Kind = A_Function_Body_Stub)
                and then (not (Acts_As_Spec (Encl_Constr))))
              or else
               ((Encl_Constr_Kind = A_Function_Renaming_Declaration or else
                 Encl_Constr_Kind = A_Procedure_Renaming_Declaration)
                 and then Is_Renaming_As_Body (Encl_Constr))
            then
               --  there should be a corresponding spec where the first
               --  definition should be:

               if Is_Subunit (Encl_Constr) then
                  Encl_Constr := Corresponding_Body_Stub (Encl_Constr);
               end if;

               First_Declaration := Corresponding_Declaration (Encl_Constr);

               if not Is_Parameter then
                  --  just returning a defining name from a declaration,
                  --  otherwise Result will remain nil, and we will have
                  --  to process the case of a formal parameter after this
                  --  case statement
                  Result := Names (First_Declaration) (1);
               end if;
            else
               Result := Defining_Name;
            end if;

         when A_Package_Body_Declaration      |
              A_Task_Body_Declaration         |
              A_Protected_Body_Declaration    |
              A_Package_Body_Stub             |
              A_Task_Body_Stub                |
              A_Protected_Body_Stub           |
              An_Entry_Body_Declaration       =>

            First_Declaration := Corresponding_Declaration (Encl_Constr);

            if not Is_Parameter then
               Result := Names (First_Declaration) (1);
            end if;

         when An_Accept_Statement =>

            First_Declaration := Corresponding_Entry (Encl_Constr);

         when An_Ordinary_Type_Declaration =>
            Result := Corresponding_Type_Declaration (Encl_Constr);

            if Is_Nil (Result) then
               --  Encl_Constr is not a completion of an incomplete or
               --  private type declaration
               Result := Defining_Name;
            else
               Result := Names (Result) (1);
            end if;

         when others =>
            Result := Defining_Name;
      end case;

      if Is_Nil (Result) then
         --  here we have to compute the first definition of the formal
         --  parameter in a subprogram spec/entry declaration

         Process_Parameter_Specifications : declare

            Def_Name_Image : constant String
               := To_Lower (To_String (Defining_Name_Image (Defining_Name)));

            Param_Specs : constant Asis.Element_List
               := Parameter_Profile (First_Declaration);

         begin

            Through_Parameter_Specs : for I in Param_Specs'Range loop

               Process_Parameter_Names : declare
                  Par_Names : constant Asis.Element_List :=
                    Names (Param_Specs (I));
               begin

                  Through_Parameter_Names : for J in Par_Names'Range loop
                     if Def_Name_Image =
                        To_Lower (To_String (Defining_Name_Image
                          (Par_Names (J))))
                     then
                        Result := Par_Names (J);
                        exit Through_Parameter_Specs;
                     end if;

                  end loop Through_Parameter_Names;

               end Process_Parameter_Names;

            end loop Through_Parameter_Specs;

         end Process_Parameter_Specifications;
      end if;

      pragma Assert (not Is_Nil (Result));

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Defining_Name,
               Outer_Call => Package_Name & "Corresponding_First_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Corresponding_First_Definition",
            Ex            => Ex,
            Arg_Element   => Defining_Name);
   end Corresponding_First_Definition;

   ----------------------------------------
   -- Corresponding_Overridden_Operation --
   ----------------------------------------

   function Corresponding_Overridden_Operation
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Result   : Asis.Element := Nil_Element;

      Result_Unit : Compilation_Unit;
      Result_Node : Node_Id;

      Inherited        : Boolean := False;
      Association_Type : Node_Id;
   begin
      Check_Validity (Declaration,
                      Package_Name & "Corresponding_Overridden_Operation");

      case Declaration_Kind (Declaration) is
         when A_Procedure_Declaration          |
              A_Function_Declaration           |
              A_Procedure_Instantiation        |
              A_Function_Instantiation         |
              A_Procedure_Body_Declaration     |
              A_Null_Procedure_Declaration     |
              A_Function_Body_Declaration      |
              A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>
            null;
         when others =>
            Raise_ASIS_Inappropriate_Element
             (Diagnosis => Package_Name &
                           "Corresponding_Overridden_Operation",
              Wrong_Kind => Int_Kind (Declaration));
      end case;

      if Is_Overriding_Operation (Declaration) then

         if Declaration_Kind (Declaration) in
              A_Procedure_Instantiation .. A_Function_Instantiation
         then
            Result_Node := Specification (Instance_Spec (Node (Declaration)));
            Result_Node :=
              Related_Instance (Defining_Unit_Name (Result_Node));
         else
            Result_Node :=
              Defining_Unit_Name (Specification (Node (Declaration)));
         end if;

         Result_Node := Overridden_Operation (Result_Node);

         Inherited := not Comes_From_Source (Result_Node);

         if Inherited then
            Association_Type := Result_Node;
            Result_Node := Explicit_Parent_Subprogram (Result_Node);

            Result_Unit :=
              Enclosing_Unit (Encl_Cont_Id (Declaration), Association_Type);

            Result := Node_To_Element_New (Node          => Result_Node,
                                           Node_Field_1  => Association_Type,
                                           Inherited     => True,
                                           In_Unit       => Result_Unit);

            if Is_From_Instance (Association_Type) then
               Set_From_Instance (Result, True);
            else
               Set_From_Instance (Result, False);
            end if;

         else
            Result_Unit :=
              Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

            Result := Node_To_Element_New (Node    => Result_Node,
                                           In_Unit => Result_Unit);
         end if;

         Result := Enclosing_Element (Result);

         if Special_Case (Result) = Expanded_Subprogram_Instantiation then
            Result := Enclosing_Element (Result);
         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name &
                             "Corresponding_Overridden_Operation");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Overridden_Operation",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Corresponding_Overridden_Operation;

   -----------------------------------------
   -- Corresponding_Overridden_Operations --
   -----------------------------------------

   --  UNDER CONSTRUCTION!!!

   pragma Warnings (Off);

   function Corresponding_Overridden_Operations
     (Declaration : Asis.Declaration)
      return        Asis.Element_List
   is
      Type_Def : Asis.Element;
      Tmp_El   : Asis.Element;
      Result   : Asis.Element := Nil_Element;

      Arg_Node  : Entity_Id;
      Prim_Elmt : Elmt_Id;
      Prim_Node : Entity_Id;
      Res_Node  : Node_Id;
   begin
      Check_Validity (Declaration,
                      Package_Name & "Corresponding_Overridden_Operations");

      case Declaration_Kind (Declaration) is
         when A_Procedure_Declaration          |
              A_Function_Declaration           |
              A_Procedure_Instantiation        |
              A_Function_Instantiation         |
              A_Procedure_Body_Declaration     |
              A_Function_Body_Declaration      |
              A_Null_Procedure_Declaration     |
              A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>
            null;
         when others =>
            Raise_ASIS_Inappropriate_Element
             (Diagnosis => Package_Name &
                           "Corresponding_Overridden_Operations",
              Wrong_Kind => Int_Kind (Declaration));
      end case;

      if not Is_Overriding_Operation (Declaration) then
         return Nil_Element_List;
      end if;

      --  Simple case: single inheritance:

      Type_Def := Primitive_Owner (Declaration);

      if Is_Nil (Definition_Interface_List (Type_Def)) then
         return (1 => Corresponding_Overridden_Operation (Declaration));
      end if;

      --  General case - multiple inheritance
      Asis_Element_Table.Init;

      Tmp_El    := First_Name (Declaration);
      Arg_Node  := R_Node (Tmp_El);

      Tmp_El    := First_Name (Enclosing_Element (Type_Def));
      Prim_Elmt := First_Elmt (Primitive_Operations (R_Node (Tmp_El)));
      Prim_Node := Node (Prim_Elmt);

      while Present (Prim_Elmt) loop
         --  Check if Prim_Node corresponds to overridden primitive:

         if Present (Interface_Alias (Prim_Node))
           and then
            Alias (Prim_Node) = Arg_Node
         then
            Res_Node := Interface_Alias (Prim_Node);

            --  ???
            --  !!! Here we have to form the element representing overridden
            --  subprogram and to add it to Asis_Element_Table
         end if;

         Prim_Elmt := Next_Elmt (Prim_Elmt);
         Prim_Node := Node (Prim_Elmt);
      end loop;

      --  ???
      Asis_Element_Table.Append
        (Corresponding_Overridden_Operation (Declaration));

      return Asis.Declaration_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name &
                             "Corresponding_Overridden_Operations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Overridden_Operations",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Corresponding_Overridden_Operations;

   pragma Warnings (On);

--   function Corresponding_Overridden_Operations
--     (Declaration : Asis.Declaration)
--      return        Asis.Element_List
--   is
--      Type_Def : Asis.Element;
--      Result   : Asis.Element := Nil_Element;
--   begin
--      Check_Validity (Declaration,
--                      Package_Name & "Corresponding_Overridden_Operations");

--      case Declaration_Kind (Declaration) is
--         when A_Procedure_Declaration          |
--              A_Function_Declaration           |
--              A_Procedure_Instantiation        |
--              A_Function_Instantiation         |
--              A_Procedure_Body_Declaration     |
--              A_Function_Body_Declaration      |
--              A_Procedure_Renaming_Declaration |
--              A_Function_Renaming_Declaration  =>
--            null;
--         when others =>
--            Raise_ASIS_Inappropriate_Element
--             (Diagnosis => Package_Name &
--                           "Corresponding_Overridden_Operations");
--      end case;

--      if not Is_Overriding_Operation (Declaration) then
--         return Nil_Element_List;
--      end if;

--      --  Simple case: single inheritance:

--      Type_Def := Primitive_Owner (Declaration);

--      if Is_Nil (Definition_Interface_List (Type_Def)) then
--         return (1 => Corresponding_Overridden_Operation (Declaration));
--      end if;

--      --  General case - multiple inheritance

--      declare
--         Interfaces : Asis.Element_List :=
--           Definition_Interface_List (Type_Def);

--         Start_From : Positive := Interfaces'First;
--      begin
--         Asis_Element_Table.Init;
--         Result := Corresponding_Overridden_Operation (Declaration);
--         Asis_Element_Table.Append (Result);
--         Type_Def := Primitive_Owner (Declaration);

--         --  First, replace each interface name in Interfaces with the
--         --  corresponding type definition and check if we may start further
--         --  processing not from the first interface in the list

--         for J in Interfaces'Range loop
--            Interfaces (J) :=
--              Type_Declaration_View
--                (Corresponding_Name_Definition
--                  (Normalize_Reference (Interfaces (J))));

--            if Is_Equal (Interfaces (J), Type_Def) then
--               Start_From := J + 1;
--            end if;
--         end loop;

--         for J in Start_From .. Interfaces'Last loop
--            Result := Get_Implemented_Op (Declaration, Interfaces (J));

--            if not Is_Nil (Result) then
--               Asis_Element_Table.Append (Result);
--            end if;
--         end loop;

--         return Asis.Declaration_List
--                  (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));
--      end;

--   exception
--      when ASIS_Inappropriate_Element =>
--         raise;
--      when ASIS_Failed =>

--         if Status_Indicator = Unhandled_Exception_Error then
--            Add_Call_Information
--              (Argument   => Declaration,
--               Outer_Call => Package_Name &
--                             "Corresponding_Overridden_Operations");
--         end if;

--         raise;
--      when Ex : others =>
--         Report_ASIS_Bug
--           (Query_Name    => Package_Name &
--                             "Corresponding_Overridden_Operations",
--            Ex            => Ex,
--            Arg_Element   => Declaration);
--   end Corresponding_Overridden_Operations;

   ----------------------------------------------
   -- Corresponding_Parent_Subtype_Unwind_Base --
   ----------------------------------------------

   function Corresponding_Parent_Subtype_Unwind_Base
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Arg_Elem : Asis.Element := Type_Definition;
      Result   : Asis.Element := Nil_Element;
   begin
      Check_Validity (Type_Definition,
                     Package_Name &
                     "Corresponding_Parent_Subtype_Unwind_Base");

      if not (Arg_Kind = A_Derived_Type_Definition or else
              Arg_Kind = A_Derived_Record_Extension_Definition)
      then
         Raise_ASIS_Inappropriate_Element
          (Diagnosis => Package_Name &
                        "Corresponding_Parent_Subtype_Unwind_Base",
           Wrong_Kind => Arg_Kind);
      end if;

      Result := Corresponding_Parent_Subtype (Arg_Elem);

      if Is_Nil (Result) then
         --  The only possible case for this - we have a 'Base attribute
         --  reference as a parent subtype mark
         Arg_Elem := Parent_Subtype_Indication (Arg_Elem);
         Arg_Elem := Asis.Definitions.Subtype_Mark (Arg_Elem);

         while Attribute_Kind (Arg_Elem) = A_Base_Attribute loop
            Arg_Elem := Prefix (Arg_Elem);
         end loop;

         if Expression_Kind (Arg_Elem) = A_Selected_Component then
            Arg_Elem := Selector (Arg_Elem);
         end if;

         Arg_Elem := Corresponding_Name_Declaration (Arg_Elem);

         if Declaration_Kind (Result) = A_Subtype_Declaration then
            Result := Corresponding_First_Subtype (Arg_Elem);
         else
            Result := Arg_Elem;
         end if;

      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Corresponding_Parent_Subtype_Unwind_Base");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Corresponding_Parent_Subtype_Unwind_Base",
            Ex            => Ex,
            Arg_Element   => Type_Definition);
   end Corresponding_Parent_Subtype_Unwind_Base;

   ----------------------
   -- CU_Requires_Body --
   ----------------------

   function CU_Requires_Body (Right : Asis.Compilation_Unit) return Boolean is
      Unit_Kind : constant Asis.Unit_Kinds := Kind (Right);
      Result    :          Boolean         := False;
   begin
      Check_Validity (Right, Package_Name & "CU_Requires_Body");
      Reset_Context (Encl_Cont_Id (Right));

      case Unit_Kind is
         when A_Generic_Procedure |
              A_Generic_Function  |
              A_Procedure         |
              A_Function          |
              A_Package           |
              A_Generic_Package   =>

            Result := Asis.Set_Get.Is_Body_Required (Right);
         when others =>
            null;
      end case;

      return Result;

   end CU_Requires_Body;

   ---------------------------
   -- Elements_Hash_Wrapper --
   ---------------------------

   function Elements_Hash_Wrapper
     (E    : Asis.Element)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      Asis_Hash : constant Asis.ASIS_Integer := abs Asis.Elements.Hash (E);
      Result    :          Ada.Containers.Hash_Type;
   begin
      Result := Ada.Containers.Hash_Type (Asis_Hash);
      return Result;
   exception
      when Constraint_Error =>
         return 0;
   end Elements_Hash_Wrapper;

   -------------------------------
   -- Element_Image_In_Template --
   -------------------------------

   function Element_Image_In_Template
     (Element : Asis.Element)
      return    Program_Text
   is
      Tmp_Element : Asis.Element := Element;
   begin

      Check_Validity (Element, Package_Name & "Element_Image_In_Template");

      if Is_Part_Of_Implicit (Element) or else
         not Is_Part_Of_Instance (Element)
      then
         return "";
      else
         --  What we are doing is tricky, but it gives the fast and
         --  easy-to-maintain solution: we consider the argument as if it is
         --  NOT from the expanded template, and we use the normal ASIS
         --  Element_Span function for it. The idea is to use Sloc fields
         --  from the element node which point to the corresponding positions
         --  in the template.
         Set_From_Instance (Tmp_Element, False);
         return Element_Image (Tmp_Element);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Element_Image_In_Template");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Element_Image_In_Template",
            Ex            => Ex,
            Arg_Element   => Element);
   end Element_Image_In_Template;

   ------------------------------
   -- Element_Span_In_Template --
   ------------------------------

   function Element_Span_In_Template
     (Element : Asis.Element)
      return    Asis.Text.Span
   is
      Tmp_Element : Asis.Element := Element;
   begin

      Check_Validity (Element, Package_Name & "Element_Span_In_Template");

      if Is_Part_Of_Implicit (Element) or else
         not Is_Part_Of_Instance (Element)
      then
         return Nil_Span;
      else
         --  What we are doing is tricky, but it gives the fast and
         --  easy-to-maintain solution: we consider the argument as if it is
         --  NOT from the expanded template, and we use the normal ASIS
         --  Element_Span function for it. The idea is to use Sloc fields
         --  from the element node which point to the corresponding positions
         --  in the template.
         Set_From_Instance (Tmp_Element, False);
         return Element_Span (Tmp_Element);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Element_Span_In_Template");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Element_Span_In_Template",
            Ex            => Ex,
            Arg_Element   => Element);
   end Element_Span_In_Template;

   -----------------------------
   -- Explicit_Type_Operators --
   -----------------------------

   function Explicit_Type_Operators
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Parent_El        : Asis.Element;
      --  The construct where the argument type is defined

      Type_Decl      : Asis.Element;
      --  Declaration of the argument type

      In_Package_Spec : Boolean;
      --  If the argument type is declared not in a package spec, but it is
      --  a derived type, we have to count all the explicit overridings of
      --  inherited operators, but if we are in the package spec, we just
      --  collect all the explicitly declared type operators

      Is_Formal_Type : Boolean;
   begin
      Check_Validity (Type_Definition,
               Package_Name & "Explicit_Type_Operators");

      if not (Arg_Kind in Internal_Type_Kinds        or else
              Arg_Kind in Internal_Formal_Type_Kinds or else
              Arg_Kind in A_Private_Type_Definition ..
                          A_Protected_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Explicit_Type_Operators",
            Wrong_Kind => Arg_Kind);
      end if;

      Type_Decl := Enclosing_Element (Type_Definition);
      Parent_El := Enclosing_Element (Type_Decl);

      Is_Formal_Type := Arg_Kind in Internal_Formal_Type_Kinds;

      In_Package_Spec :=
         Declaration_Kind (Parent_El) = A_Package_Declaration  or else
         (not Is_Formal_Type and then
          Declaration_Kind (Parent_El) = A_Formal_Package_Declaration);

      declare
         All_Comp   : constant Asis.Element_List := Components (Parent_El);
         Start_From :           Natural;
         Result     :           Asis.Element_List (All_Comp'Range);
         Res_First  : constant Natural := Result'First;
         Res_Last   :           Natural := Res_First - 1;
      begin
         for J in All_Comp'Range loop

            if Is_Equal (Type_Decl, All_Comp (J)) then
               Start_From := J + 1;
               exit;
            end if;

         end loop;

         for J in Start_From  .. All_Comp'Last loop

            if Is_Formal_Type
             and then
               Declaration_Kind (All_Comp (J)) not in A_Formal_Declaration
            then
               exit;
            end if;

            if Is_Type_Operator (All_Comp (J), Type_Decl)
              and then
               (In_Package_Spec
               or else
                Overrides_Type_Operator (All_Comp (J), Type_Decl))
            then
               Res_Last          := Res_Last + 1;
               Result (Res_Last) := All_Comp (J);

               if Is_Bool_Eq_Declaration (All_Comp (J)) then
                  Res_Last          := Res_Last + 1;
                  Result (Res_Last) :=
                    Corresponding_Equality_Operator (All_Comp (J));
               end if;

            end if;

         end loop;

         return Result (Res_First .. Res_Last);
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Explicit_Type_Operators");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Explicit_Type_Operators",
            Ex            => Ex,
            Arg_Element   => Type_Definition);
   end Explicit_Type_Operators;

   ----------------
   -- First_Name --
   ----------------

   function First_Name (Dcl : Asis.Element) return Asis.Element is
      Name_List : constant Asis.Element_List := Names (Dcl);
   begin
      return Name_List (Name_List'First);
   end First_Name;

   -------------------------------
   -- Formal_Subprogram_Default --
   -------------------------------

   function Formal_Subprogram_Default
     (Declaration : Asis.Generic_Formal_Parameter)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Arg_Node := Node (Declaration);

      Check_Validity (Declaration, Package_Name & "Formal_Subprogram_Default");

      if not (Arg_Kind = A_Formal_Procedure_Declaration or else
              Arg_Kind = A_Formal_Function_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Formal_Subprogram_Default",
            Wrong_Kind => Arg_Kind);
      end if;

      if not Present (Default_Name (Arg_Node)) then
         return Nil_Element;
      end if;

      return Node_To_Element_New (Node             => Default_Name (Arg_Node),
                                  Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Formal_Subprogram_Default");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Formal_Subprogram_Default",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Formal_Subprogram_Default;

   ---------------------
   -- Full_Name_Image --
   ---------------------

   function Full_Name_Image
     (Expression : Asis.Expression)
      return       Program_Text
   is
   begin
      case Expression_Kind (Expression) is
         when An_Identifier .. An_Enumeration_Literal =>
            return Asis.Expressions.Name_Image (Expression);
         when A_Selected_Component =>
            return Full_Name_Image (Prefix (Expression)) & '.' &
                   Asis.Expressions.Name_Image (Selector (Expression));
         when others =>
            Raise_ASIS_Inappropriate_Element
              (Package_Name & "Full_Name_Image",
               Wrong_Kind => Int_Kind (Expression));
      end case;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Full_Name_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Full_Name_Image",
            Ex            => Ex,
            Arg_Element   => Expression);
   end Full_Name_Image;

   -------------------------
   -- Get_Call_Parameters --
   -------------------------

   function Get_Call_Parameters
     (Call       : Asis.Element;
      Normalized : Boolean := False)
      return       Asis.Element_List
   is
   begin

      if Expression_Kind (Call) = A_Function_Call then
         return Function_Call_Parameters (Call, Normalized);
      else
         return Call_Statement_Parameters (Call, Normalized);
      end if;

   end Get_Call_Parameters;

   ------------------------
   -- Get_Implemented_Op --
   ------------------------

   --  Under construction!

   function Get_Implemented_Op
     (Op_Decl  : Asis.Element;
      Type_Def : Asis.Element)
      return     Asis.Element
   is
      pragma Unreferenced (Op_Decl, Type_Def);
   begin
      return Nil_Element;
   end Get_Implemented_Op;

--   function Get_Implemented_Op
--     (Op_Decl  : Asis.Element;
--      Type_Def : Asis.Element)
--      return     Asis.Element
--   is
--      Result        : Asis.Element               := Nil_Element;
--      Look_For_Proc : constant Boolean           := Is_Procedure (Op_Decl);
--     Primitives    : constant Asis.Element_List := Get_Primitives (Type_Def);

--      Arg_Chars  : Name_Id;
--      Res_Chars  : Name_Id;
--      Tmp_Node   : Node_Id;
--      Tmp_El1    : Asis.Element;
--      Tmp_El2    : Asis.Element;
--      Success    : Boolean;
--      Is_Controlling : Boolean;

--      Arg_Params : constant Asis.Element_List := Parameter_Profile (Op_Decl);
--   begin

--      Tmp_El    := First_Name (Decl);
--      Arg_Chars := Chars (R_Node (Tmp_El));

--      Scan_Primitives : for J in Primitives'Range loop

--         if Look_For_Proc xor Is_Procedure (Primitives (J)) then
--            Res_Chars := Chars (R_Node (First_Name (Primitives (J))));

--            if Res_Chars = Arg_Chars
--              and then
--               Arg_Params'Length = Parameter_Profile (Primitives (J))'Length
--            then
--               --  Check parameter profiles:
--               Success := True;

--               if not Look_For_Proc then
--                  --  Check for the result type
--                  Tmp_El1 := First_Name (Op_Decl);
--                  Tmp_El2 := First_Name (Primitives (J));

--                  if Has_Controlling_Result (R_Node (Tmp_El1)) xor
--                     Has_Controlling_Result (R_Node (Tmp_El2))
--                  then
--                     Success := False;
--                  else
--                     Is_Controlling :=
--                       Has_Controlling_Result (R_Node (Tmp_El1));

--                     Tmp_El1 := Result_Profile (Op_Decl);
--                     Tmp_El2 := Result_Profile (Primitives (J));

--                     if Definition_Kind (Tmp_El1) = An_Access_Definition
--                       xor
--                        Definition_Kind (Tmp_El2) = An_Access_Definition
--                     then
--                        Success := False;

--                        elsif not Is_Controlling then
--                           Succes := Are_Type_Conforming (Tmp_El1, Tmp_El2);
--                        end if;

--                     end if;
--                  end if;

--               end if;

--               if Success then

--                  declare
--                     Res_Params : constant Asis.Element_List :=
--                       Parameter_Profile (Primitives (J));
--                  begin
--                     Scan_Params : for P in Arg_Params'Range loop
--                        if not Are_Conformant
--                                 (Arg_Params (P), Res_Params (P))
--                        then
--                           Success := False;
--                           exit Scan_Params;
--                        end if;
--                     end loop;
--                  end;

--               end if;

--               if Success then
--                  Result := Primitives (J)
--                  exit Scan_Primitives;
--               end if;

--            end if;

--         end if;

--      end loop Scan_Primitives;

--      return Result;
--   end Get_Implemented_Op;

   ------------------------
   -- Get_Last_Component --
   ------------------------

   function Get_Last_Component (E : Asis.Element) return Asis.Element is
      Child_Access : constant Query_Array := Appropriate_Queries (E);
      Child        : Asis.Element         := Asis.Nil_Element;
   begin
      Check_Validity (E, Package_Name & "Get_Last_Component");

      if Is_Nil (E) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Get_Last_Component",
            Wrong_Kind => Not_An_Element);
      end if;

      if Debug_Flag_X then
         Write_Str ("   Get_Last_Component - called for ");
         Write_Str (Internal_Element_Kinds'Image (Int_Kind (E)));
         Write_Eol;
      end if;

      for Each_Query in reverse Child_Access'Range loop
         case Child_Access (Each_Query).Query_Kind is
            when Bug =>
               null;
            when Single_Element_Query =>
               Child := Child_Access (Each_Query).Func_Simple (E);
            when Element_List_Query =>
               declare
                  Child_List : constant Asis.Element_List :=
                     Child_Access (Each_Query).Func_List (E);
               begin
                  if not Is_Nil (Child_List) then
                     Child := Child_List (Child_List'Last);
                  end if;
               end;
            when Element_List_Query_With_Boolean =>
               declare
                  Child_List : constant Asis.Element_List :=
                     Child_Access (Each_Query).Func_List_Boolean
                        (E, Child_Access (Each_Query).Bool);
               begin
                  if not Is_Nil (Child_List) then
                     Child := Child_List (Child_List'Last);
                  end if;
               end;
         end case;

         exit when not Is_Nil (Child);

      end loop;

      if Debug_Flag_X then
         Write_Str ("   Get_Last_Component - returns ");
         Write_Str (Internal_Element_Kinds'Image (Int_Kind (Child)));
         Write_Eol;
      end if;
      return Child;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => E,
               Outer_Call => Package_Name & "Get_Last_Component");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Get_Last_Component",
            Ex            => Ex,
            Arg_Element   => E);
   end Get_Last_Component;

   -----------------------
   -- Get_LF_From_Ureal --
   -----------------------

   function Get_LF_From_Ureal (U : Ureal) return Long_Long_Float is
      Result  :          Long_Long_Float;
      Base    : constant Nat  := Rbase (U);
      U_Num   : constant Uint := Numerator (U);
      U_Denum : constant Uint := Denominator (U);
      Num     :          Long_Long_Integer;
      Denum   :          Long_Long_Integer;
   begin
      UI_Image (U_Num, Format => Decimal);
      Num := Long_Long_Integer'Value (UI_Image_Buffer (1 .. UI_Image_Length));

      UI_Image (U_Denum, Format => Decimal);
      Denum :=
        Long_Long_Integer'Value (UI_Image_Buffer (1 .. UI_Image_Length));

      if Base /= 0 then
         Denum := Long_Long_Integer (2 ** Natural (Denum));
      end if;

      Result := Long_Long_Float (Num) / Long_Long_Float (Denum);

      if UR_Is_Negative (U) then
         Result := -Result;
      end if;

      return Result;
   end Get_LF_From_Ureal;

   --------------------------
   -- Has_Enumeration_Type --
   --------------------------

   function Has_Enumeration_Type
     (Expression : Asis.Expression)
      return       Boolean
   is
      Result : Boolean := False;
   begin
      Check_Validity (Expression, Package_Name & "Has_Enumeration_Type");

      if Ekind (Etype (R_Node (Expression))) in Enumeration_Kind then
         Result := True;
      end if;

      return Result;
   end Has_Enumeration_Type;

   ----------------------
   -- Has_Integer_Type --
   ----------------------

   function Has_Integer_Type (Expression : Asis.Expression) return Boolean is
      Result : Boolean := False;
   begin
      Check_Validity (Expression, Package_Name & "Has_Integer_Type");

      if Ekind (Etype (R_Node (Expression))) in Integer_Kind then
         Result := True;
      end if;

      return Result;
   end Has_Integer_Type;

   ------------------------------
   -- Inherited_Type_Operators --
   ------------------------------

   function Inherited_Type_Operators
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Declaration_List
   is
      Arg_Kind  : constant Internal_Element_Kinds :=
        Int_Kind (Type_Definition);
      Type_Decl : Asis.Element;
   begin
      Check_Validity (Type_Definition,
               Package_Name & "Inherited_Type_Operators");

      if not (Arg_Kind in Internal_Type_Kinds        or else
              Arg_Kind in Internal_Formal_Type_Kinds or else
              Arg_Kind in A_Private_Type_Definition ..
                          A_Protected_Definition)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Inherited_Type_Operators",
            Wrong_Kind => Arg_Kind);
      end if;

      if not (Arg_Kind = A_Private_Extension_Definition        or else
              Arg_Kind = A_Derived_Type_Definition             or else
              Arg_Kind = A_Derived_Record_Extension_Definition or else
              Arg_Kind = A_Formal_Derived_Type_Definition)
      then
         return Nil_Element_List;
      end if;

      declare
         All_Inherited_Ops : constant Asis.Declaration_List :=
           Implicit_Inherited_Subprograms (Type_Definition);
         Result : Asis.Declaration_List (All_Inherited_Ops'Range);
         Res_First : constant Natural :=  Result'First;
         Res_Last  :           Natural :=  Res_First - 1;
      begin

         Type_Decl := Enclosing_Element (Type_Definition);

         for J in All_Inherited_Ops'Range loop

            if Is_Type_Operator (All_Inherited_Ops (J), Type_Decl) then
               Res_Last          := Res_Last + 1;
               Result (Res_Last) := All_Inherited_Ops (J);
            end if;

         end loop;

         return Result (Res_First .. Res_Last);

      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Inherited_Type_Operators");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Inherited_Type_Operators",
            Ex            => Ex,
            Arg_Element   => Type_Definition);
   end Inherited_Type_Operators;

   --------------------
   -- Is_Aspect_Mark --
   --------------------

   function Is_Aspect_Mark (Element : Asis.Element) return Boolean is
      Result : Boolean := False;
      Tmp    : Node_Id;
   begin
      if Expression_Kind (Element) = An_Identifier or else
         Attribute_Kind (Element) = A_Class_Attribute
      then
         Tmp    := R_Node (Element);
         Result := Nkind (Parent (Tmp)) = N_Aspect_Specification and then
                   Tmp = Sinfo.Identifier (Parent (Tmp));
      end if;

      return Result;
   end Is_Aspect_Mark;

   -----------------------------------
   -- Is_Aspect_Specific_Identifier --
   -----------------------------------

   function Is_Aspect_Specific_Identifier
     (Element : Asis.Element)
      return    Boolean
   is
      Result : Boolean := False;
      Tmp    : Node_Id;
   begin
      if Expression_Kind (Element) = An_Identifier then
         Tmp := R_Node (Element);

         if Nkind (Tmp) = N_Identifier
           and then
            not Present (Entity (Tmp))
         then
            Tmp := Parent (Tmp);

            if Present (Tmp) then
               case Nkind (Tmp) is
                  when N_Component_Association =>
                     if Nkind (Parent (Parent (Tmp))) =
                        N_Aspect_Specification
                     then
                        Result := True;
                     end if;
                  when N_Aspect_Specification =>
                     Result := R_Node (Element) /= Sinfo.Identifier (Tmp);

                  --  ... to be continued...
                  when others =>
                     null;
               end case;
            end if;

         end if;
      end if;

      return Result;
   end Is_Aspect_Specific_Identifier;

   ----------------------------
   -- Is_Bool_Eq_Declaration --
   ----------------------------

   function Is_Bool_Eq_Declaration
     (Declaration : Asis.Element)
      return        Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Result   :          Boolean := False;
      Op_Node  : Node_Id;
      Op_Etype : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Bool_Eq_Declaration");

      if Arg_Kind = A_Function_Declaration
        or else
         (Arg_Kind = A_Function_Body_Declaration
         and then
          Acts_As_Spec (Declaration))
        or else
         Arg_Kind = A_Function_Renaming_Declaration
      then
         Op_Node  := Defining_Unit_Name (Specification (Node (Declaration)));
         Op_Etype := Etype (Op_Node);

         if Is_Generic_Instance (Op_Node) then
            Op_Node :=
              Defining_Unit_Name (Node (Enclosing_Element (Declaration)));
         end if;

         if Nkind (Op_Node) = N_Defining_Program_Unit_Name then
            Op_Node := Defining_Identifier (Op_Node);
         end if;

         if Nkind (Op_Node) = N_Defining_Operator_Symbol
           and then
            Chars (Op_Node) = Name_Op_Eq
           and then
            Op_Etype = Standard_Boolean
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Bool_Eq_Declaration;

   -------------------
   -- Is_Class_Wide --
   -------------------

   function Is_Class_Wide
     (Declaration : Asis.Declaration)
      return        Boolean
   is
      Result         : Boolean := False;
      Subtype_Entity : Entity_Id;
   begin
      if Declaration_Kind (Declaration) = A_Subtype_Declaration then
         Subtype_Entity := R_Node (Declaration);
         Subtype_Entity := Defining_Identifier (Subtype_Entity);

         Result := Ekind (Subtype_Entity) = E_Class_Wide_Subtype;
      end if;

      return Result;
   end Is_Class_Wide;

   ------------------
   -- Is_Completed --
   ------------------

   function Is_Completed (Declaration : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
      Result   : Boolean := False;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Completed");

      --  JUNK IMPLEMENTATION!!!
      if not (Arg_Kind = A_Procedure_Declaration or else
              Arg_Kind = A_Function_Declaration)
        or else
          Is_Part_Of_Inherited (Declaration)
      then
         return False;
      end if;

      Arg_Node := Defining_Unit_Name (Specification (Node (Declaration)));

      Result := Has_Completion (Arg_Node);

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Is_Completed");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Completed",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Is_Completed;

   -----------------------------------
   -- Is_Default_For_Null_Procedure --
   -----------------------------------

   function Is_Default_For_Null_Procedure
     (Reference : Asis.Element)
      return      Boolean
   is
      Result : Boolean := False;
      Tmp    : Node_Id;
   begin

      if Expression_Kind (Reference) = An_Identifier
        and then
         Is_Part_Of_Instance (Reference)
      then
         Tmp := R_Node (Reference);

         if Nkind (Tmp) in N_Has_Entity then
            Tmp := Entity (Tmp);

            if Present (Tmp) and then  Ekind (Tmp) = E_Procedure then
               Tmp := Parent (Parent (Tmp));

               Result :=
                 Nkind (Tmp) = N_Subprogram_Body
                and then
                 Nkind (Parent (Tmp)) = N_Package_Specification;
            end if;

         end if;

      end if;

      return Result;
   end Is_Default_For_Null_Procedure;

   ----------------------------
   -- Is_Expanded_Subprogram --
   ----------------------------

   function Is_Expanded_Subprogram (N : Node_Id) return Boolean is
      Result : Boolean := False;
      Tmp    : Node_Id;
   begin
      if Nkind (N) = N_Subprogram_Declaration then
         Tmp := Defining_Unit_Name (Specification (N));

         if Nkind (Tmp) = N_Defining_Program_Unit_Name then
            Tmp := Defining_Identifier (Tmp);
         end if;

         if Is_Generic_Instance (Tmp) then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Expanded_Subprogram;

   -----------------
   -- Is_Exported --
   -----------------

   function Is_Exported (Defining_Name : Asis.Defining_Name) return Boolean is
      Arg_Node : Node_Id;
      Tmp      : Node_Id;
      Result   : Boolean := False;
   begin

      Check_Validity (Defining_Name, Package_Name & "Is_Exported");

      if Int_Kind (Defining_Name) not in Internal_Defining_Name_Kinds then
         return False;
      end if;

      Arg_Node := R_Node (Defining_Name);

      if Nkind (Arg_Node) = N_Defining_Program_Unit_Name then
         Arg_Node := Defining_Identifier (Arg_Node);
      end if;

      if Ekind (Arg_Node) = E_Subprogram_Body then
         --  Go to the corresponding spec entity

         Tmp := Parent (Arg_Node);

         while Nkind (Tmp) not in N_Subprogram_Specification loop
            Tmp := Parent (Tmp);
         end loop;

         Tmp := Parent (Tmp);
         Tmp := Corresponding_Decl_Node (Tmp);

         Arg_Node := Defining_Unit_Name (Specification (Tmp));

         if Nkind (Arg_Node) = N_Defining_Program_Unit_Name then
            Arg_Node := Defining_Identifier (Arg_Node);
         end if;
      end if;

      Result := Is_Exported (Arg_Node);

      return Result;
   end Is_Exported;

   -------------------------------------
   -- Is_From_Import_Procedure_Pragma --
   -------------------------------------

   function Is_From_Import_Procedure_Pragma (N : Node_Id) return Boolean is
      Tmp    : Node_Id := Parent (N);
      Result : Boolean := False;
   begin

      if Nkind (Tmp) = N_Indexed_Component then
         Tmp := Parent (Tmp);

         if Nkind (Tmp) = N_Aggregate then
            Tmp := Parent (Tmp);

            if Nkind (Tmp) = N_Pragma_Argument_Association then
               Tmp := Pragma_Identifier (Parent (Tmp));
               Result := Chars (Tmp) = Name_Import_Procedure
                        or else
                         Chars (Tmp) = Name_Import_Valued_Procedure;
            end if;
         end if;

      end if;

      return Result;
   end Is_From_Import_Procedure_Pragma;

   ---------------------------------
   -- Is_Implicit_Neq_Declaration --
   ---------------------------------

   function Is_Implicit_Neq_Declaration
     (Declaration : Asis.Element)
      return        Boolean
   is
   begin
      return
        Declaration_Kind (Declaration) = A_Function_Declaration
       and then
        Special_Case (Declaration) = Is_From_Imp_Neq_Declaration;
   end Is_Implicit_Neq_Declaration;

   --------------
   -- Is_Label --
   --------------

   function Is_Label (Defining_Name : Asis.Defining_Name) return Boolean is
      N      : constant Node_Id := Node (Defining_Name);
      Result :          Boolean := False;
   begin

      if Int_Kind (Defining_Name) = A_Defining_Identifier then

         if Nkind (N) = N_Label then
            Result := True;
         elsif Nkind (N) = N_Identifier
            and then
               Nkind (Parent (N)) = N_Loop_Statement
            and then
               Nkind (Original_Node (Parent (N))) = N_Goto_Statement
         then
            --  An infinite loop is implemented with goto statement
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Label;

   --------------------------
   -- Is_Main_Unit_In_Tree --
   --------------------------

   function Is_Main_Unit_In_Tree
     (Right : Asis.Compilation_Unit)
      return  Boolean
   is
      Arg_Kind     : constant Unit_Kinds := Kind (Right);
      Arg_Unit_Id  : Unit_Id;
      Arg_Cont_Id  : Context_Id;
   begin
      Check_Validity (Right, Package_Name & "Is_Main_Unit_In_Tree");

      Arg_Cont_Id := Encl_Cont_Id (Right);
      Reset_Context (Arg_Cont_Id);

      Arg_Unit_Id := Get_Unit_Id  (Right);

      if Arg_Kind in A_Procedure .. A_Protected_Body_Subunit then

         return GNAT_Compilation_Dependencies (Arg_Unit_Id) /=
                Nil_Unit_Id_List;

      else
         return False;
      end if;
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Main_Unit_In_Tree");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Main_Unit_In_Tree",
            Ex            => Ex,
            Arg_CU        => Right);
   end Is_Main_Unit_In_Tree;

   -----------------
   -- Is_Obsolete --
   -----------------

   function Is_Obsolete (Right : Asis.Compilation_Unit) return Boolean is
      Arg_Kind : constant Unit_Kinds := Kind (Right);
      Arg_Id   : Unit_Id;
      Result   : Boolean := True;
   begin
      Check_Validity (Right, Package_Name & "Is_Obsolete");

      case Arg_Kind is
         when Not_A_Unit                |
              A_Nonexistent_Declaration |
              A_Nonexistent_Body        |
              An_Unknown_Unit =>

            null;
         when others =>
            Arg_Id := Get_Unit_Id (Right);

            if Arg_Id = Standard_Id then
               Result := False;
            else
               Result := not (Source_Status (Right) = Up_To_Date);
            end if;

      end case;

      return Result;
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Obsolete");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Obsolete",
            Ex            => Ex,
            Arg_CU        => Right);
   end Is_Obsolete;

   -----------------------------
   -- Is_Overriding_Operation --
   -----------------------------

   function Is_Overriding_Operation
     (Declaration : Asis.Element)
      return        Boolean
   is
      Result   : Boolean   := False;
      Entity_N : Entity_Id := Empty;
   begin

      case Declaration_Kind (Declaration) is
         when A_Procedure_Instantiation |
              A_Function_Instantiation  =>
            Entity_N := Specification (Instance_Spec (Node (Declaration)));
            Entity_N := Related_Instance (Defining_Unit_Name (Entity_N));

         when A_Procedure_Declaration          |
              A_Function_Declaration           |
              A_Procedure_Body_Declaration     |
              A_Function_Body_Declaration      |
              A_Null_Procedure_Declaration     |
              A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>

            if not Is_Part_Of_Implicit (Declaration) then
               Entity_N := Specification (Node (Declaration));
               Entity_N := Defining_Unit_Name (Entity_N);
            end if;
         when others =>
            null;
      end case;

      if Present (Entity_N)
        and then
          Nkind (Entity_N) in
            N_Defining_Identifier .. N_Defining_Operator_Symbol
      then
         Result := Present (Overridden_Operation (Entity_N));
      end if;

      return Result;
   end Is_Overriding_Operation;

   ----------------------------
   -- Is_Predefined_Operator --
   ----------------------------

   function Is_Predefined_Operator
     (Operator : Asis.Element)
      return     Boolean
   is
      Result    : Boolean := False;
      Entity_N  : Entity_Id;
   begin

      if Expression_Kind (Operator) = An_Operator_Symbol then
         Entity_N := Entity (Node (Operator));

         Result := Present (Entity_N) and then Is_Predefined (Entity_N);
      end if;

      return Result;
   end Is_Predefined_Operator;

   ----------------
   -- Is_Private --
   ----------------

   function Is_Private (Declaration : Asis.Element) return Boolean is
      Arg_Element : Element := Declaration;

      Result    : Boolean := False;
      Next_Node : Node_Id;

      Enclosing_List : List_Id;
      Enclosing_Node : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Private");

      if Declaration_Kind (Declaration) = Not_A_Declaration or else
         Declaration_Kind (Declaration) in
           A_Loop_Parameter_Specification .. An_Element_Iterator_Specification
      then
         return False;
      end if;

      --  In case of an implicit Element we go to the "enclosing" explicit
      --  Element to get the node stored in R_Node field which can safely be
      --  used for tree traversal (for implicit Elements R_Node may be of
      --  special use and it may have the Parent field set to Empty
      while Is_Part_Of_Implicit (Arg_Element)
          and then
            Special_Case (Arg_Element) /= From_Limited_View
      loop
         Arg_Element := Enclosing_Element (Arg_Element);
      end loop;

      Next_Node := R_Node (Arg_Element);

      while Nkind (Next_Node) /= N_Compilation_Unit and then
            not Is_List_Member (Next_Node)
      loop
         Next_Node := Parent (Next_Node);
      end loop;

      while Nkind (Next_Node) /= N_Compilation_Unit loop

         --  If we are here, we have Next_Node being a list member

         Enclosing_List := List_Containing (Next_Node);

         Enclosing_Node := Parent (Enclosing_List);

         case Nkind (Enclosing_Node) is
            when N_Statement_Other_Than_Procedure_Call =>
               --  We can not be in any private part
               exit;
            when N_Package_Specification |
                 N_Task_Definition       |
                 N_Protected_Definition  =>

               if Enclosing_List = Private_Declarations (Enclosing_Node) then
                  Result := True;
                  exit;
               end if;

            when others =>
               null;
         end case;

         Next_Node := Parent (Next_Node);

         while Nkind (Next_Node) /= N_Compilation_Unit and then
               not Is_List_Member (Next_Node)
         loop
            Next_Node := Parent (Next_Node);
         end loop;

      end loop;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Is_Private");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Private",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Is_Private;

   ------------------
   -- Is_Procedure --
   ------------------

   function Is_Procedure (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Declaration_Kind (Decl) is
         when A_Procedure_Declaration          |
              A_Procedure_Instantiation        |
              A_Procedure_Body_Declaration     |
              A_Null_Procedure_Declaration     |
              A_Procedure_Renaming_Declaration =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Procedure;

   -----------------
   -- Is_RCI_Unit --
   -----------------

   function Is_RCI_Unit (C : Asis.Compilation_Unit) return Boolean is
      Arg_Node : Node_Id;
      Result   : Boolean := False;
   begin
      if Is_Standard (C) then
         return False;
      end if;

      case Unit_Kind (C) is
         when A_Package         |
              A_Procedure_Body  |
              A_Function_Body   |
              A_Generic_Package =>

            Arg_Node := Unit (Top (C));
            Arg_Node := Defining_Unit_Name (Specification (Arg_Node));

            if Nkind (Arg_Node) = N_Defining_Program_Unit_Name then
               Arg_Node := Defining_Identifier (Arg_Node);
            end if;

            Result := Is_Remote_Call_Interface (Arg_Node);

         when others => null;
      end case;

      return Result;
   end Is_RCI_Unit;

   -------------------------
   -- Is_Renaming_As_Body --
   -------------------------

   function Is_Renaming_As_Body (Declaration : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;

      Result : Boolean := False;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Renaming_As_Body");

      if Arg_Kind = A_Procedure_Renaming_Declaration or else
         Arg_Kind = A_Function_Renaming_Declaration
      then
         Arg_Node := R_Node (Declaration);

         if Nkind (Arg_Node) /= N_Subprogram_Declaration then
            Result := Present (Corresponding_Spec (Arg_Node));
         end if;
      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Is_Renaming_As_Body");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Renaming_As_Body",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Is_Renaming_As_Body;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node : Node_Id;
      Result   : Boolean := False;
   begin
      Check_Validity (Element, Package_Name & "Is_Static");

      if Arg_Kind in Internal_Expression_Kinds and then
         Is_True_Expression (Element)
      then
         Result := Sinfo.Is_Static_Expression (R_Node (Element));

      elsif Arg_Kind = A_Range_Attribute_Reference or else
            Arg_Kind =
               A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
                                                                        or else
            Arg_Kind = A_Discrete_Range_Attribute_Reference
      then
         Arg_Node := R_Node (Element);

         if Nkind (Arg_Node) = N_Range_Constraint then
            Arg_Node := Range_Expression (Arg_Node);
         end if;

         if Nkind (Arg_Node) = N_Range                   and then
            Is_Static_Expression (Low_Bound  (Arg_Node)) and then
            Is_Static_Expression (High_Bound (Arg_Node))
         then
            Result := True;
         end if;

      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Is_Static");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Static",
            Ex            => Ex,
            Arg_Element   => Element);
   end Is_Static;

   ------------------------
   -- Is_True_Expression --
   ------------------------

   function Is_True_Expression
     (Expression : Asis.Expression)
      return       Boolean
   is
      Arg_Node    : Node_Id                         := Node   (Expression);
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Expression);
      Expr_Chars  : Name_Id;
      Entity_Node : Entity_Id;

      Result      : Boolean                := True;
      --  the idea of the implementation is to find out the cases when
      --  Expression is NOT a true exception, so we initialize Result
      --  as True
   begin
      Check_Validity (Expression, Package_Name & "Is_True_Expression");

      if Arg_Kind not in Internal_Expression_Kinds then
         return False;
      end if;

      if Nkind (Arg_Node) = N_Identifier             and then
         Nkind (Parent (Arg_Node)) = N_Expanded_Name and then
         Arg_Node = Selector_Name (Parent (Arg_Node))
      then
         --  selector in an expanded name - all the semantic fields
         --  are set for the whole name, but not for this selector.
         --  So:
         Arg_Node := Parent (Arg_Node);
      end if;

      if Nkind (Arg_Node) not in N_Has_Etype          or else
         No (Etype (Arg_Node))                        or else
         Is_Anonymous (Ekind (Etype (Arg_Node)))      or else
         Ekind (Etype (Arg_Node)) = E_Subprogram_Type
      then
         --  Expression may be a true expression, but it may have a type which
         --  cannot be represented in ASIS (such as an anonymous access type),
         --  in such cases we also classify it as being not true expression
         Result := False;

      else
         --  in some cases more detailed analysis is required.
         --  ???  This part may require some more analysis - it may be
         --  somewhat redundant

         case Arg_Kind is
            when An_Identifier | A_Selected_Component =>
               --  and here we have to investigate whether or not this
               --  Expression is a "naming expression"

               if Special_Case (Expression) = Rewritten_Named_Number then
                  return True;
               end if;

--  ??? <tree problem 1>
--  this fragment should be revised when the problem is fixed (as it should)
               if Nkind (Arg_Node) = N_Selected_Component and then
                  Etype (Arg_Node) = Any_Type
                  --  for now (GNAT 3.05) this means, that Expression is an
                  --  expanded name of the character literal of ether a
                  --  predefined character type or of the type derived from a
                  --  predefined character type; the problem is that the
                  --  Entity field is not set for such a node
               then
                  return True;
               end if;
--  ??? <tree problem 1> - end

               --  now taking the Entity field (if any) and looking,
               --  what we have:

               if Nkind (Arg_Node) = N_Selected_Component then
                  Entity_Node := Entity (Selector_Name (Arg_Node));
               elsif Nkind (Arg_Node) = N_Attribute_Definition_Clause then
                  --  the attribute designator in an attribute definition
                  --  clause
                  Entity_Node := Empty;
               else
                  Entity_Node := Entity (Arg_Node);
               end if;

               if No (Entity_Node) then
                  Result := False;
               elsif Ekind (Entity_Node) = E_Enumeration_Literal then
                  null;
               else
                  case Ekind (Entity_Node) is
                     --  the first choice in this case statement should
                     --  filter in entities which *ARE* expressions in Ada
                     --  sense
                     when E_Variable =>
                        --  tasks and protected objects declared by _single_
                        --  task/protected declarations do not have
                        --  corresponding type declarations which can be
                        --  represented in ASIS
                        Result := Comes_From_Source (Parent (Entity_Node));
                     when E_Component                |
                          E_Constant                 |
                          E_Discriminant             |
                          E_Loop_Parameter           |
                          E_In_Out_Parameter         |
                          E_In_Parameter             |
                          E_Out_Parameter            |
                          E_Generic_In_Out_Parameter |
                          E_Generic_In_Parameter     |
                          E_Named_Integer            |
                          E_Named_Real               |
                          E_Enumeration_Literal      |
                           --  ??? (see elsif path)
                           --  enumeration literals are not treated as
                           --  functions in ASIS
                          E_Entry_Index_Parameter    |
                          E_Protected_Object         =>
                        null;
                        --  simply keeping the initialization of Result
                     when others =>
                        Result := False;
                  end case;
               end if;

            when Internal_Operator_Symbol_Kinds =>
               Result := False;
            when Internal_Attribute_Reference_Kinds =>

               case Internal_Attribute_Reference_Kinds (Arg_Kind) is
                  when An_Adjacent_Attribute          |
                       A_Base_Attribute               |
                       A_Ceiling_Attribute            |
                       A_Class_Attribute              |
                       A_Compose_Attribute            |
                       A_Copy_Sign_Attribute          |
                       An_Exponent_Attribute          |
                       A_Floor_Attribute              |
                       A_Fraction_Attribute           |
                       An_Image_Attribute             |
                       An_Input_Attribute             |
                       A_Leading_Part_Attribute       |
                       A_Machine_Attribute            |
                       A_Max_Attribute                |
                       A_Min_Attribute                |
                       A_Model_Attribute              |
                       An_Output_Attribute            |
                       A_Pos_Attribute                |
                       A_Pred_Attribute               |
                       A_Range_Attribute              |
                       A_Read_Attribute               |
                       A_Remainder_Attribute          |
                       A_Round_Attribute              |
                       A_Rounding_Attribute           |
                       A_Scaling_Attribute            |
                       A_Succ_Attribute               |
                       A_Truncation_Attribute         |
                       An_Unbiased_Rounding_Attribute |
                       A_Val_Attribute                |
                       A_Value_Attribute              |
                       A_Wide_Image_Attribute         |
                       A_Wide_Value_Attribute         |
                       A_Write_Attribute              =>

                     Result := False;
                  when An_Implementation_Defined_Attribute =>
                     Expr_Chars := Attribute_Name (Arg_Node);
                     if Expr_Chars = Name_Abort_Signal or else
                        Expr_Chars = Name_Elab_Body    or else
                        Expr_Chars = Name_Elab_Spec
                     then
                        Result := False;
                     end if;
                  when others =>
                     null;
               end case;

            when A_Positional_Array_Aggregate | A_Named_Array_Aggregate =>

               if Nkind (Parent (Arg_Node)) =
                     N_Enumeration_Representation_Clause
                 or else
                  Is_Typeless_Subaggregate (Arg_Node)
               then
                  Result := False;
               end if;

            when others =>
               null;
         end case;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Is_True_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_True_Expression",
            Ex            => Ex,
            Arg_Element   => Expression);
   end Is_True_Expression;

   ----------------------
   -- Is_Type_Operator --
   ----------------------

   function Is_Type_Operator
     (Op_Decl   : Asis.Element;
      Type_Decl : Asis.Element)
      return      Boolean
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Op_Decl);
      Result    :          Boolean                := False;
      Next_Type :          Asis.Element;
   begin

      if (Arg_Kind = A_Function_Declaration
         or else

          ((Arg_Kind = A_Function_Body_Declaration
           or else
            Arg_Kind = A_Function_Body_Stub
           or else
            Arg_Kind = A_Function_Renaming_Declaration)
           and then
            not (Is_Equal (Corresponding_Declaration (Op_Decl), Op_Decl)))

         or else
          Arg_Kind = A_Function_Instantiation
         or else
          Arg_Kind = A_Formal_Function_Declaration)

         and then
          Int_Kind (Names (Op_Decl) (1)) in Internal_Defining_Operator_Kinds
      then
         --  First, check the result type
         Next_Type := Result_Profile (Op_Decl);

         if Int_Kind (Next_Type) = A_Selected_Component then
            Next_Type := Selector (Next_Type);
         end if;

         if Int_Kind (Next_Type) = An_Identifier then
            Next_Type := Corresponding_Name_Declaration (Next_Type);
            Next_Type := Corresponding_First_Subtype (Next_Type);

            if Is_Equal (Next_Type, Type_Decl) then
               Result := True;
            end if;

         end if;

         if not Result then
            --  check parameter types
            declare
               Params : constant Asis.Element_List :=
                 Parameter_Profile (Op_Decl);
            begin

               for J in Params'Range loop
                  Next_Type := Object_Declaration_View (Params (J));

                  if Int_Kind (Next_Type) = A_Selected_Component then
                     Next_Type := Selector (Next_Type);
                  end if;

                  if Int_Kind (Next_Type) = An_Identifier then
                     Next_Type := Corresponding_Name_Declaration (Next_Type);
                     Next_Type := Corresponding_First_Subtype (Next_Type);

                     if Is_Equal (Next_Type, Type_Decl) then
                        Result := True;
                        exit;
                     end if;

                  end if;

               end loop;

            end;

         end if;

      end if;

      return Result;
   end Is_Type_Operator;

   ------------------------------
   -- Is_Typeless_Subaggregate --
   ------------------------------

   function Is_Typeless_Subaggregate (Aggr : Node_Id) return Boolean is
      Parent_Node : Node_Id := Parent (Aggr);
      Result      : Boolean := False;
      Arg_Type    : Entity_Id;
      Parent_Type : Entity_Id;
   begin

      if Nkind (Parent_Node) = N_Component_Association then
         Parent_Node := Parent (Parent_Node);
      end if;

      if Nkind (Parent_Node) = N_Aggregate then

         Arg_Type := Etype (Aggr);
         while Present (Arg_Type) and then Etype (Arg_Type) /= Arg_Type loop
            Arg_Type := Etype (Arg_Type);
         end loop;

         Parent_Type := Etype (Parent_Node);
         while Present (Parent_Type)
           and then Etype (Parent_Type) /= Parent_Type
         loop
            Parent_Type := Etype (Parent_Type);
         end loop;

         Result := Arg_Type = Parent_Type;

      end if;

      return Result;

   end Is_Typeless_Subaggregate;

   -------------------------
   -- Is_Uniquely_Defined --
   -------------------------

   function Is_Uniquely_Defined (Reference : Asis.Expression) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Reference);
      Arg_Node : Node_Id;

      Result : Boolean := False;

   begin
      Check_Validity (Reference, Package_Name & "Is_Uniquely_Defined");

      if Arg_Kind =  An_Identifier                  or else
         Arg_Kind in Internal_Operator_Symbol_Kinds or else
         Arg_Kind =  A_Character_Literal            or else
         Arg_Kind =  An_Enumeration_Literal
      then

         Result := True;
         --  We suppose, that in general case we have a unique declaration,
         --  and now let's try to detect if we have a special case:

         Arg_Node := Node (Reference);
         --  first, the situation when "passed a portion of a pragma that
         --  was "ignored" by the compiler", it relates to pragma arguments
         --  only, but not to pragma element identifiers:

         --  GNAT rewrites the tree structure for non-recognized pragma as
         --  if it is a null statement, so:

         if Nkind (Parent (Parent (Arg_Node))) = N_Null_Statement then
            Result := False;
         end if;

         if Arg_Kind = An_Identifier then
            --  There are three checks specific to arguments of An_Identifier
            --  kind only: a pragma_argument_identifier, an identifier specific
            --  to a pragma and a reference to an attribute_designator:
            if    Nkind (Arg_Node) = N_Pragma_Argument_Association
                  --  a reference to a pragma_argument_identifier
               or else
                  (Nkind (Arg_Node) in N_Has_Entity
                  and then
                   No (Entity (Arg_Node))
                  and then
                   (Nkind (Parent (Arg_Node)) = N_Pragma_Argument_Association
                   or else
                   Is_From_Import_Procedure_Pragma (Arg_Node)))
                  --  an identifier specific to a pragma, we make a guess that
                  --  any identifier on the place of a pragma argument is
                  --  specific to the pragma, if the Entity field is not set
                  --  for this identifier. Is it really true???
               or else
                  Nkind (Arg_Node) = N_Attribute_Reference
               or else
                  Special_Case (Reference) = Dummy_Class_Attribute_Designator
                  --  a reference to an attribute_designator
               or else
                  Nkind (Arg_Node) = N_Attribute_Definition_Clause
                  --  attribute designator from an attribute definition clause
            then
               Result := False;
            end if;

         end if;

         --  One more check for pragma argument. It corresponds to the
         --  situation when the identifier is specific for a pragma, but in
         --  the same time it is a part of other expression. This check is
         --  specific to extended Import and Export pragmas applying to
         --  subprograms.

         if Result                                        and then
            Special_Case (Reference) = Not_A_Special_Case and then
            Arg_Kind = An_Identifier                      and then
            No (Entity (Arg_Node))
         then
            --  The first possibility:
            --
            --    pragma Import_Function (Internal => Unix_Code_Mappings,
            --             External => "unix_code_mappings",
            --             Result_Type => Integer,
            --             Mechanism => (Value));
            --
            --  Value is rewritten into N_Aggregate
            --
            --  The second possibility:
            --
            --    pragma Import_Procedure (Internal => Ignore_Signal,
            --              External => "ignore_signal",
            --              Mechanism => (Value, Value));
            --
            --  Value is not rewritten and it is represented as a "normal"
            --  aggregate component
            --
            --  And the third possibility:
            --
            --    pragma Export_Procedure
            --      (Internal        => Reset,
            --       External        => "",
            --       Parameter_Types => (File_Type, File_Mode),
            --       Mechanism       => (File => Reference));
            --
            --  Here we have an aggregate with named associations:

            if (Nkind (R_Node (Reference)) = N_Aggregate and then
                (Nkind (Parent (R_Node (Reference)))) =
                 N_Pragma_Argument_Association)
              or else
                (Nkind (R_Node (Reference)) = N_Identifier
                and then
                 not (Is_Rewrite_Substitution (R_Node (Reference)))
                and then
                 ((Nkind (Parent (R_Node (Reference))) = N_Aggregate
                and then
                 Nkind (Parent (Parent (R_Node (Reference)))) =
                   N_Pragma_Argument_Association)
                  or else
                   (Nkind (Parent (R_Node (Reference))) =
                     N_Component_Association
                    and then
                    Nkind (Parent (Parent (R_Node (Reference)))) =
                     N_Aggregate
                    and then
                    Nkind (Parent (Parent (Parent ((R_Node (Reference)))))) =
                      N_Pragma_Argument_Association)
                   )
                )
            then
               Result := False;
            end if;

         end if;

         --  Then check for the situation when if passed a portion of a pragma
         --  that may be an ambiguous reference to more than one entity.

         if Result                                                    and then
            Nkind (Parent (Arg_Node)) = N_Pragma_Argument_Association and then
            Needs_List (Reference)
         then
            declare
               Res_List : constant Asis.Element_List :=
                  Corresponding_Name_Definition_List (Reference);
            begin

               if Res_List'Length /= 1 then
                  Result := False;
               end if;

            end;

         end if;

      end if;

      --  Case when the argument is a parameter of Source_File_Name pragma or
      --  component thereof

      if Result then

         while not Is_List_Member (Arg_Node) and then
               Present (Arg_Node)
         loop
            Arg_Node := Parent (Arg_Node);
         end loop;

         if Nkind (Arg_Node) = N_Pragma_Argument_Association
           and then
             Pragma_Name (Parent (Arg_Node)) = Name_Source_File_Name
         then
            Result := False;
         end if;

      end if;

      --  Case when the argument is the (component of the) prefix of the
      --  GNAT-specific attribute 'Elab_Body or 'Elab_Spec

      if Result then
         Arg_Node := Parent (R_Node (Reference));

         while Nkind (Arg_Node) = N_Selected_Component loop
            Arg_Node := Parent (Arg_Node);
         end loop;

         if Nkind (Arg_Node) = N_Attribute_Reference
           and then
            Attribute_Name (Arg_Node) in Name_Elab_Body .. Name_Elab_Spec
         then
            Result := False;
         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Reference,
               Outer_Call => Package_Name & "Is_Uniquely_Defined");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Uniquely_Defined",
            Ex            => Ex,
            Arg_Element   => Reference);
   end Is_Uniquely_Defined;

   -------------------------------
   -- Main_Unit_In_Current_Tree --
   -------------------------------

   function Main_Unit_In_Current_Tree
     (The_Context : Asis.Context)
      return        Asis.Compilation_Unit
   is
      Curr_Tree_Id : Tree_Id;
      Curr_Cont_Id : Context_Id;

      Res_Unit_Id  : Unit_Id := Nil_Unit;
   begin
      Check_Validity (The_Context, Package_Name & "Main_Unit_In_Current_Tree");

      Curr_Cont_Id := Get_Current_Cont;
      Curr_Tree_Id := Get_Current_Tree;

      if Tree_Processing_Mode (Get_Cont_Id (The_Context)) = GNSA then
         --  Note, that for GNSA Context no check is made! This works correctly
         --  only for -GNSA -C1 Context and if only this Context Is_Open
         --  at the moment

         Res_Unit_Id := Config_Comp_Id + 1;  --  ???
         --  Not a good approach!!!

      elsif Get_Cont_Id (The_Context) = Curr_Cont_Id and then
         Curr_Cont_Id /= Nil_Context_Id              and then
         Present (Curr_Tree_Id)
      then
         Res_Unit_Id := Main_Unit_Id;

      elsif Get_Cont_Id (The_Context) /= Nil_Context_Id then
         Reset_Context (Get_Cont_Id (The_Context));

         if Tree_Processing_Mode (Get_Cont_Id (The_Context)) = GNSA then
            --  Note, that for GNSA Context no check is made! This works
            --  correctly only for -GNSA -C1 Context and if only this Context
            --  Is_Open at the moment

            Res_Unit_Id := Config_Comp_Id + 1;  --  ???
            --  Not a good approach!!!

         elsif Last_Tree (Get_Cont_Id (The_Context)) >= First_Tree_Id then
            Res_Unit_Id := Main_Unit_Id (First_Tree_Id);
         end if;

      end if;

      if Present (Res_Unit_Id) then
         return Get_Comp_Unit (Res_Unit_Id, Get_Cont_Id (The_Context));
      else
         return Nil_Compilation_Unit;
      end if;

   exception
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Main_Unit_In_Current_Tree");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Main_Unit_In_Current_Tree",
            Ex            => Ex);
   end Main_Unit_In_Current_Tree;

   -----------
   -- No_Op --
   -----------

   procedure No_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out No_State)
   is
   begin
      pragma Unreferenced (Element);
      pragma Unreferenced (Control);
      pragma Unreferenced (State);

      null;
   end No_Op;

   -------------------------
   -- Normalize_Reference --
   -------------------------

   function Normalize_Reference (Ref : Asis.Element) return Asis.Element is
      Result : Asis.Element := Ref;
   begin
      case Expression_Kind (Ref) is
         when A_Selected_Component =>
            Result := Selector (Ref);
         when An_Attribute_Reference =>
            Result := Normalize_Reference (Prefix (Ref));
         when others =>
            null;
      end case;

      return Result;
   end Normalize_Reference;

   --------------------------
   -- Original_Line_Number --
   --------------------------

   function Original_Line_Number
     (Element       : Asis.Element;
      Compiled_Line : Line_Number_Positive)
      return Line_Number
   is
      SFI    : Source_File_Index;
      Result : Line_Number := 0;
   begin
      Check_Validity (Element, Package_Name & "Original_Line_Number");

      if Is_Text_Available (Element) then

         if Compiled_Line > Line_Number (Number_Of_Lines (Element)) then
            Raise_ASIS_Inappropriate_Line_Number
              (Package_Name & "Original_Line_Number");
         end if;

         SFI := Get_Source_File_Index (Location (Element));

         Result :=
            Line_Number (Sinput.Physical_To_Logical
              (Physical_Line_Number (Compiled_Line), SFI));
      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element | ASIS_Inappropriate_Line_Number =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Original_Line_Number");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Original_Line_Number",
            Ex            => Ex,
            Arg_Element   => Element);
   end Original_Line_Number;

   ------------------------
   -- Original_Text_Name --
   ------------------------

   function Original_Text_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin

      Check_Validity (Compilation_Unit, Package_Name & "Original_Text_Name");

      if not Exists (Compilation_Unit) then
         return Nil_Asis_Wide_String;
      else
         --  Exists resets the Context!
         return To_Program_Text (Ref_File (Compilation_Unit));
      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Original_Text_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Original_Text_Name",
            Ex            => Ex,
            Arg_CU        => Compilation_Unit);
   end Original_Text_Name;

   -----------------------------
   -- Overrides_Type_Operator --
   -----------------------------

   function Overrides_Type_Operator
     (Op_Decl   : Asis.Element;
      Type_Decl : Asis.Element)
      return      Boolean
   is
   pragma Unreferenced (Type_Decl);
      Op_Entity : Entity_Id;
      Result    : Boolean := False;
   begin
      --  We assume that Is_Type_Operator (Op_Decl, Type_Decl) is True

      --  !!! The implementation is incomplete!!!

      Op_Entity := Node (Names (Op_Decl) (1));

      if Present (Overridden_Operation (Op_Entity)) then
         Result := True;
      end if;

      return Result;
   end Overrides_Type_Operator;

   ---------------------
   -- Primitive_Owner --
   ---------------------

   function Primitive_Owner
     (Declaration : Asis.Declaration)
      return        Asis.Type_Definition
   is
      Arg_Kind   : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node   : Node_Id := Empty;
      Par_Node   : Node_Id := Empty;
      Res_Node   : Node_Id := Empty;
      Result     : Element := Nil_Element;

      Res_Kind   : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity (Declaration, Package_Name & "Primitive_Owner");

      if not (Arg_Kind = A_Procedure_Declaration          or else
              Arg_Kind = A_Null_Procedure_Declaration     or else
              Arg_Kind = A_Function_Declaration           or else
              Arg_Kind = A_Procedure_Renaming_Declaration or else
              Arg_Kind = A_Function_Renaming_Declaration  or else
              Arg_Kind = A_Procedure_Body_Declaration     or else
              Arg_Kind = A_Function_Body_Declaration      or else
              Arg_Kind = A_Procedure_Body_Stub            or else
              Arg_Kind = A_Function_Body_Stub)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Primitive_Owner",
            Wrong_Kind => Arg_Kind);
      end if;

      if not Is_From_Implicit                       (Declaration) and then
         Asis.Declarations.Is_Dispatching_Operation (Declaration)
      then

         Arg_Node := Specification (Node (Declaration));

         if Nkind (Arg_Node) = N_Function_Specification then

            if Has_Controlling_Result (Defining_Unit_Name (Arg_Node)) then
               Res_Node := Defining_Unit_Name (Arg_Node);
               Res_Node := Parent (Res_Node);
               Res_Node := Sinfo.Result_Definition (Res_Node);

               if Nkind (Res_Node) = N_Access_Definition then
                  Res_Node := Sinfo.Subtype_Mark (Res_Node);
               end if;

               Res_Node := Entity (Res_Node);
            end if;

         end if;

         if No (Res_Node) then
            --  This means that we do not have a function with controlling
            --  result, so we have to go through the formal parameter list,
            --  and it can not be No_List or empty

            Par_Node := First (Parameter_Specifications (Arg_Node));

            while Present (Par_Node) loop

               if Is_Controlling_Formal
                    (Defining_Identifier (Par_Node))
               then

                  if Nkind (Parameter_Type (Par_Node)) =
                     N_Access_Definition
                  then
                     Res_Node :=
                        Sinfo.Subtype_Mark (Parameter_Type (Par_Node));
                  else
                     Res_Node := Defining_Identifier (Par_Node);
                  end if;

                  Res_Node := Etype (Res_Node);

                  exit;
               end if;

               Par_Node := Next (Par_Node);
            end loop;

         end if;

         pragma Assert (Present (Res_Node));

         if Nkind (Parent (Res_Node)) = N_Subtype_Declaration then
            Res_Node := Etype (Res_Node);
         end if;

         Res_Node := Parent (Res_Node);

         case Nkind (Res_Node) is

            when N_Private_Type_Declaration =>
               if Tagged_Present (Res_Node) then
                  Res_Kind := A_Tagged_Private_Type_Definition;
               else
                  --  It can be non-tagged, if the full view is tagged
                  Res_Kind := A_Private_Type_Definition;
               end if;

            when N_Private_Extension_Declaration =>
               Res_Kind := A_Private_Extension_Definition;

            when N_Full_Type_Declaration =>
               Res_Node := Sinfo.Type_Definition (Res_Node);

            when others =>
               pragma Assert (False);
               null;

         end case;

         Result := Node_To_Element_New (Node             => Res_Node,
                                        Internal_Kind    => Res_Kind,
                                        Starting_Element => Declaration);

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Primitive_Owner");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Primitive_Owner",
            Ex            => Ex,
            Arg_Element   => Declaration);
   end Primitive_Owner;

   ------------------------
   -- Source_File_Status --
   ------------------------

   function Source_File_Status
     (Right : Asis.Compilation_Unit)
      return  Source_File_Statuses
   is
      Arg_Kind : constant Unit_Kinds := Kind (Right);
      Result   : Source_File_Statuses;
   begin
      Check_Validity (Right, Package_Name & "Source_File_Status");

      case Arg_Kind is
         when Not_A_Unit                |
              A_Nonexistent_Declaration |
              A_Nonexistent_Body        |
              An_Unknown_Unit =>

            Result := Absent;
         when others =>
            Result := Source_Status (Right);
      end case;

      return Result;
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Source_File_Status");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Source_File_Status",
            Ex            => Ex,
            Arg_CU        => Right);
   end Source_File_Status;

   -----------------------------------
   -- Static_Expression_Value_Image --
   -----------------------------------

   function Static_Expression_Value_Image
     (Expression : Asis.Expression)
      return       Wide_String
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id := Empty;
      Result   : Uint;
      Tmp_El   : Asis.Element;
   begin
      Check_Validity
        (Expression, Package_Name & "Static_Expression_Value_Image");

      if Arg_Kind not in Internal_Expression_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Static_Expression_Value_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      if not (Is_True_Expression   (Expression) and then
              Is_Static            (Expression))
      then
         return "";
      end if;

      Arg_Node  := R_Node (Expression);

      if Nkind (Arg_Node) = N_String_Literal then
         String_To_Name_Buffer (Strval (Arg_Node));
         return To_Wide_String (Name_Buffer (1 .. Name_Len));

--      elsif Nkind (Arg_Node) = N_Real_Literal then
--         begin
--            return Long_Long_Float'Wide_Image
--                     (Get_LF_From_Ureal (Realval (Arg_Node)));
--         exception
--            when others => return "";
--         end;
      elsif Has_Enumeration_Type (Expression) or else
            Has_Integer_Type     (Expression)
      then
         Result    := Eval_Scalar_Node (Arg_Node);
         UI_Image (Result, Format => Decimal);
         return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));

      else
         if Expression_Kind (Expression) = A_Selected_Component then
            Tmp_El := Selector (Expression);
         else
            Tmp_El := Expression;
         end if;

         if Expression_Kind (Tmp_El) = An_Identifier then
            begin
               Tmp_El := Corresponding_Name_Declaration (Tmp_El);
            exception
               when ASIS_Inappropriate_Element =>
                  Tmp_El := Nil_Element;
            end;

            if Declaration_Kind (Tmp_El) = A_Constant_Declaration then
               Tmp_El := Initialization_Expression (Tmp_El);
               return Static_Expression_Value_Image (Tmp_El);
            end if;
         end if;
      end if;

      return "";

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Static_Expression_Value_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Static_Expression_Value_Image",
            Ex            => Ex,
            Arg_Element   => Expression);
   end Static_Expression_Value_Image;

   -----------------------------------------
   -- Static_Range_High_Bound_Value_Image --
   -----------------------------------------

   function Static_Range_High_Bound_Value_Image
     (Range_Element : Asis.Range_Constraint)
      return          Wide_String
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Range_Element);
      Arg_Node  : Node_Id := Empty;
      Arg_Ekind : Entity_Kind;
      Result    : Uint;
   begin
      Check_Validity
        (Range_Element, Package_Name & "Static_Range_High_Bound_Value_Image");

      if not (Arg_Kind = A_Range_Attribute_Reference or else
              Arg_Kind =
                 A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
                                                                        or else
              Arg_Kind = A_Discrete_Range_Attribute_Reference)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Static_Range_High_Bound_Value_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      if not (Is_Static (Range_Element)) then
         return "";
      end if;

      Arg_Node := R_Node (Range_Element);

      if Nkind (Arg_Node) = N_Range_Constraint then
         Arg_Node := Range_Expression (Arg_Node);
      end if;

      Arg_Ekind := Ekind (Etype (Arg_Node));

      if not (Arg_Ekind in Discrete_Kind) then
         --  Implementation limitation!!!
         return "";
      end if;

      Result := Eval_Scalar_Node (High_Bound (Arg_Node));

      UI_Image (Result, Format => Decimal);

      return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Range_Element,
               Outer_Call => Package_Name &
                             "Static_Range_High_Bound_Value_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Static_Range_High_Bound_Value_Image",
            Ex            => Ex,
            Arg_Element   => Range_Element);
   end Static_Range_High_Bound_Value_Image;

   ----------------------------------------
   -- Static_Range_Low_Bound_Value_Image --
   ----------------------------------------

   function Static_Range_Low_Bound_Value_Image
     (Range_Element : Asis.Range_Constraint)
      return          Wide_String
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Range_Element);
      Arg_Node  : Node_Id := Empty;
      Arg_Ekind : Entity_Kind;
      Result    : Uint;
   begin
      Check_Validity
        (Range_Element, Package_Name & "Static_Range_Low_Bound_Value_Image");

      if not (Arg_Kind = A_Range_Attribute_Reference or else
              Arg_Kind =
                 A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
                                                                        or else
              Arg_Kind = A_Discrete_Range_Attribute_Reference)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Static_Range_Low_Bound_Value_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      if not (Is_Static (Range_Element)) then
         return "";
      end if;

      Arg_Node := R_Node (Range_Element);

      if Nkind (Arg_Node) = N_Range_Constraint then
         Arg_Node := Range_Expression (Arg_Node);
      end if;

      Arg_Ekind := Ekind (Etype (Arg_Node));

      if not (Arg_Ekind in Discrete_Kind) then
         --  Implementation limitation!!!
         return "";
      end if;

      Result := Eval_Scalar_Node (Low_Bound (Arg_Node));

      UI_Image (Result, Format => Decimal);

      return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Range_Element,
               Outer_Call => Package_Name &
                             "Static_Range_Low_Bound_Value_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name &
                             "Static_Range_Low_Bound_Value_Image",
            Ex            => Ex,
            Arg_Element   => Range_Element);
   end Static_Range_Low_Bound_Value_Image;

end Asis.Extensions;
