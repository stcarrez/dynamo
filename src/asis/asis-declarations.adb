------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                                                                          --
--                    A S I S . D E C L A R A T I O N S                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Elements;           use Asis.Elements;
with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Expressions;
with Asis.Extensions;         use Asis.Extensions;
with Asis.Limited_Views;      use Asis.Limited_Views;

with Asis.Set_Get;            use  Asis.Set_Get;

with A4G.A_Sem;               use A4G.A_Sem;
with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.Asis_Tables;         use A4G.Asis_Tables;
with A4G.Contt.TT;            use A4G.Contt.TT;
with A4G.Contt.UT;            use A4G.Contt.UT; use A4G.Contt;
with A4G.Decl_Sem;            use A4G.Decl_Sem;
with A4G.Mapping;             use A4G.Mapping;
with A4G.Norm;                use A4G.Norm;
with A4G.Span_End;            use A4G.Span_End;
with A4G.Stand;               use A4G.Stand;
with A4G.Vcheck;              use A4G.Vcheck;

with Aspects;                 use Aspects;
with Atree;                   use Atree;
with Einfo;                   use Einfo;
with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Sem_Aux;                 use Sem_Aux;
with Sinfo;                   use Sinfo;
with Snames;                  use Snames;
with Stand;                   use Stand;
with Uintp;                   use Uintp;

package body Asis.Declarations is

   Package_Name : constant String := "Asis.Declarations.";

   -----------------------
   -- ASIS 2012 queries --
   -----------------------

   ---------------------------
   -- Aspect_Specifications --
   ---------------------------

   function Aspect_Specifications
     (Declaration : Asis.Element)
      return        Asis.Element_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node :          Node_Id;
   begin
      Check_Validity
        (Declaration, Package_Name & "Aspect_Specifications");

      if not (Arg_Kind = An_Ordinary_Type_Declaration             or else
              Arg_Kind = A_Task_Type_Declaration                  or else
              Arg_Kind = A_Protected_Type_Declaration             or else
              Arg_Kind = A_Single_Task_Declaration                or else
              Arg_Kind = A_Single_Protected_Declaration           or else
              Arg_Kind = A_Variable_Declaration                   or else
              Arg_Kind = A_Constant_Declaration                   or else
              Arg_Kind = A_Component_Declaration                  or else
              Arg_Kind = A_Procedure_Declaration                  or else
              Arg_Kind = A_Null_Procedure_Declaration             or else
              Arg_Kind = A_Function_Declaration                   or else
              Arg_Kind = An_Expression_Function_Declaration       or else
              Arg_Kind = A_Package_Declaration                    or else
              Arg_Kind = A_Package_Body_Declaration               or else
              Arg_Kind = An_Object_Renaming_Declaration           or else
              Arg_Kind = An_Exception_Renaming_Declaration        or else
              Arg_Kind = A_Package_Renaming_Declaration           or else
              Arg_Kind = A_Procedure_Renaming_Declaration         or else
              Arg_Kind = A_Function_Renaming_Declaration          or else
              Arg_Kind = A_Generic_Package_Renaming_Declaration   or else
              Arg_Kind = A_Generic_Procedure_Renaming_Declaration or else
              Arg_Kind = A_Generic_Function_Renaming_Declaration  or else
              Arg_Kind = A_Task_Body_Declaration                  or else
              Arg_Kind = A_Protected_Body_Declaration             or else
              Arg_Kind = An_Exception_Declaration                 or else
              Arg_Kind = A_Generic_Procedure_Declaration          or else
              Arg_Kind = A_Generic_Function_Declaration           or else
              Arg_Kind = A_Generic_Package_Declaration            or else
              Arg_Kind = A_Package_Instantiation                  or else
              Arg_Kind = A_Procedure_Instantiation                or else
              Arg_Kind = A_Function_Instantiation                 or else
              Arg_Kind = A_Formal_Object_Declaration              or else
              Arg_Kind = A_Formal_Type_Declaration                or else
              Arg_Kind = A_Formal_Procedure_Declaration           or else
              Arg_Kind = A_Formal_Function_Declaration            or else
              Arg_Kind = A_Formal_Package_Declaration             or else
              Arg_Kind = A_Formal_Package_Declaration_With_Box)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Aspect_Specifications",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return N_To_E_List_New
        (List             => Aspect_Specifications (Arg_Node),
         Internal_Kind    => An_Aspect_Specification,
         Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Aspect_Specifications");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Aspect_Specifications",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Aspect_Specifications;

   -----------------------
   -- Result_Expression --
   -----------------------

   function Result_Expression
     (Declaration :  Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Res_Node :          Node_Id;
   begin
      Check_Validity
        (Declaration, Package_Name & "Result_Expression");

      if Arg_Kind /= An_Expression_Function_Declaration  then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Result_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Res_Node := R_Node (Declaration);

      if Nkind (Res_Node) = N_Subprogram_Declaration then
         Res_Node := Parent (Parent (Corresponding_Body (Res_Node)));
      end if;

      Res_Node := First (Statements (Handled_Statement_Sequence (Res_Node)));

      while Nkind (Res_Node) /= N_Return_Statement loop
         Res_Node := Next (Res_Node);
      end loop;

      Res_Node := Sinfo.Expression (Res_Node);

      return Node_To_Element_New
               (Node             => Res_Node,
                Starting_Element => Declaration);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Result_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Result_Expression",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Result_Expression;

   -----------------------
   -- ASIS 2005 queries --
   -----------------------

   --------------------------------
   -- Declaration_Interface_List --
   --------------------------------

   function Declaration_Interface_List
     (Declaration : Asis.Definition)
      return        Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node :          Node_Id;
   begin

      Check_Validity
        (Declaration, Package_Name & "Declaration_Interface_List");

      if not (Arg_Kind = A_Task_Type_Declaration         or else
              Arg_Kind = A_Protected_Type_Declaration    or else
              Arg_Kind = A_Single_Task_Declaration       or else
              Arg_Kind = A_Single_Protected_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Declaration_Interface_List",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return N_To_E_List_New (List             => Interface_List (Arg_Node),
                              Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Declaration_Interface_List");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Declaration_Interface_List",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Declaration_Interface_List;

   -------------------------------
   -- Is_Overriding_Declaration --
   -------------------------------

   function Is_Overriding_Declaration
    (Declaration : Asis.Declaration)
     return        Boolean
   is
      Arg_Kind   : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Check_Node : Node_Id;
      Result     :          Boolean                := False;
   begin

      Check_Validity (Declaration, Package_Name & "Is_Overriding_Declaration");

      if Arg_Kind = A_Procedure_Declaration            or else
         Arg_Kind = A_Function_Declaration             or else
         Arg_Kind = An_Expression_Function_Declaration or else
         Arg_Kind = A_Procedure_Body_Declaration       or else
         Arg_Kind = A_Function_Body_Declaration        or else
         Arg_Kind = A_Null_Procedure_Declaration       or else
         Arg_Kind = A_Procedure_Renaming_Declaration   or else
         Arg_Kind = A_Function_Renaming_Declaration    or else
         Arg_Kind = An_Entry_Declaration               or else
         Arg_Kind = A_Procedure_Body_Stub              or else
         Arg_Kind = A_Function_Body_Stub               or else
         Arg_Kind = A_Procedure_Instantiation          or else
         Arg_Kind = A_Function_Instantiation
      then
         Check_Node := Node (Declaration);

         case Arg_Kind is
            when A_Procedure_Declaration            |
                 A_Function_Declaration             |
                 An_Expression_Function_Declaration |
                 A_Procedure_Body_Declaration       |
                 A_Function_Body_Declaration        |
                 A_Procedure_Body_Stub              |
                 A_Function_Body_Stub               |
                 A_Procedure_Renaming_Declaration   |
                 A_Function_Renaming_Declaration    |
                 A_Null_Procedure_Declaration     =>
               Check_Node := Specification (Check_Node);
            when others =>
               null;
         end case;

         Result := Must_Override (Check_Node);
      end if;

      return Result;
   end Is_Overriding_Declaration;

   -----------------------------------
   -- Is_Not_Overriding_Declaration --
   -----------------------------------

   function Is_Not_Overriding_Declaration
     (Declaration : Asis.Declaration)
      return        Boolean
   is
      Arg_Kind   : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Check_Node : Node_Id;
      Result     : Boolean                         := False;
   begin

      Check_Validity
        (Declaration, Package_Name & "Is_Not_Overriding_Declaration");

      if Arg_Kind = A_Procedure_Declaration            or else
         Arg_Kind = A_Function_Declaration             or else
         Arg_Kind = An_Expression_Function_Declaration or else
         Arg_Kind = A_Procedure_Body_Declaration       or else
         Arg_Kind = A_Function_Body_Declaration        or else
         Arg_Kind = A_Null_Procedure_Declaration       or else
         Arg_Kind = A_Procedure_Renaming_Declaration   or else
         Arg_Kind = A_Function_Renaming_Declaration    or else
         Arg_Kind = An_Entry_Declaration               or else
         Arg_Kind = A_Procedure_Body_Stub              or else
         Arg_Kind = A_Function_Body_Stub               or else
         Arg_Kind = A_Procedure_Instantiation          or else
         Arg_Kind = A_Function_Instantiation
      then

         Check_Node := Node (Declaration);

         case Arg_Kind is
            when A_Procedure_Declaration            |
                 A_Function_Declaration             |
                 An_Expression_Function_Declaration |
                 A_Procedure_Body_Declaration       |
                 A_Function_Body_Declaration        |
                 A_Procedure_Body_Stub              |
                 A_Function_Body_Stub               |
                 A_Procedure_Renaming_Declaration   |
                 A_Function_Renaming_Declaration    |
                 A_Null_Procedure_Declaration     =>
               Check_Node := Specification (Check_Node);
            when others =>
               null;
         end case;

         Result := Must_Not_Override (Check_Node);
      end if;

      return Result;
   end Is_Not_Overriding_Declaration;

------------------------------------------------------------------------------

   ------------------------------------
   -- Corresponding_Type_Declaration --
   ------------------------------------

   function Corresponding_Type_Declaration
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Arg_Unit    : Asis.Compilation_Unit;
      Entity_Node : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_Element : Asis.Declaration;
   begin
      Check_Validity
       (Declaration,
        Package_Name & "Corresponding_Type_Declaration");

      if not (Arg_Kind = An_Ordinary_Type_Declaration         or else
              Arg_Kind = A_Task_Type_Declaration              or else
              Arg_Kind = A_Protected_Type_Declaration         or else
              Arg_Kind = An_Incomplete_Type_Declaration       or else
--  |A2005 start
              Arg_Kind = A_Tagged_Incomplete_Type_Declaration or else
--  |A2005 end
              Arg_Kind = A_Private_Type_Declaration           or else
              Arg_Kind = A_Private_Extension_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Type_Declaration",
            Wrong_Kind => Arg_Kind);
      end if;

      if Arg_Kind in A_Private_Type_Declaration ..
                     A_Private_Extension_Declaration
        and then
         not Is_Nil (Corresponding_Type_Completion (Declaration))
        and then
         not Is_Nil (Corresponding_Type_Partial_View (Declaration))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
            "Corresponding_Type_Declaration - obsolescent for Ada 2012",
            Wrong_Kind => Arg_Kind);
      end if;

      --  this query can cross compilation unit boundaries for an
      --  incomplete type defined in the private part of a library
      --  package spec and completed in the package body.

      Arg_Node := R_Node (Declaration);

      if Sloc (Arg_Node) <= Standard_Location then
         --  No private/incomplete declaration in Standard!
         return Nil_Element;
      end if;

      Entity_Node := Defining_Identifier (Arg_Node);
      Arg_Unit := Get_Comp_Unit
        (Encl_Unit_Id (Declaration), Encl_Cont_Id (Declaration));

      if Arg_Kind = An_Incomplete_Type_Declaration       or else
         Arg_Kind = A_Private_Type_Declaration           or else
--  |A2005 start
         Arg_Kind = A_Tagged_Incomplete_Type_Declaration or else
--  |A2005 end
         Arg_Kind = A_Private_Extension_Declaration
      then
         --  finding the full view for incomplete/private type
         Result_Node := Full_View (Entity_Node);

         if No (Result_Node) then
            --  for a legal Ada unit it is possible only for
            --  An_Incomplete_Type_Declaration in the private part of a
            --  package, provided that enclosing unit is a library package or
            --  a library generic package.

            Result_Unit := Corresponding_Body (Arg_Unit);

            if not Exists (Result_Unit) then
               return Nil_Element;
            else
               Arg_Element := Declaration;
               Reset_For_Body (Arg_Element, Result_Unit);
               Arg_Node := R_Node (Arg_Element);
               Entity_Node := Defining_Identifier (Arg_Node);
               Result_Node := Full_View (Entity_Node);
            end if;
         end if;
      else
         --  trying to find the private/incomplete view, if any

         Result_Node := Serach_First_View (Entity_Node);

         if Result_Node = Entity_Node then
            --  no private/incomplete view, therefore:
            return Nil_Element;
         end if;

      end if;

      --  and, finally, one step up from the defining name to the
      --  corresponding type declaration
      Result_Node := Parent (Result_Node);

      --  if we are here, Result_Node is not Empty!
      pragma Assert (Present (Result_Node));

      if not Exists (Result_Unit) then
         Result_Unit :=
            Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);
      end if;

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Type_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Declaration",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Type_Declaration;

--  --|A2010 start

   -----------------------------------
   -- Corresponding_Type_Completion --
   -----------------------------------

   function Corresponding_Type_Completion
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Arg_Unit    : Asis.Compilation_Unit;
      Entity_Node : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_Element : Asis.Declaration;
   begin
      Check_Validity
       (Declaration,
        Package_Name & "Corresponding_Type_Completion");

      if not (Arg_Kind = An_Incomplete_Type_Declaration       or else
              Arg_Kind = A_Tagged_Incomplete_Type_Declaration or else
              Arg_Kind = A_Private_Type_Declaration           or else
              Arg_Kind = A_Private_Extension_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Type_Completion",
            Wrong_Kind => Arg_Kind);
      end if;

      --  this query can cross compilation unit boundaries for an
      --  incomplete type defined in the private part of a library
      --  package spec and completed in the package body.

      Arg_Node := R_Node (Declaration);

      if Sloc (Arg_Node) <= Standard_Location then
         --  No private/incomplete declaration in Standard!
         return Nil_Element;
      end if;

      Entity_Node := Defining_Identifier (Arg_Node);
      Arg_Unit := Get_Comp_Unit
        (Encl_Unit_Id (Declaration), Encl_Cont_Id (Declaration));

      Result_Node := Full_View (Entity_Node);

      if No (Result_Node) then
         --  for a legal Ada unit it is possible only for
         --  An_Incomplete_Type_Declaration in the private part of a
         --  package, provided that enclosing unit is a library package or
         --  a library generic package.

         Result_Unit := Corresponding_Body (Arg_Unit);

         if not Exists (Result_Unit) then
            return Nil_Element;
         else
            Arg_Element := Declaration;
            Reset_For_Body (Arg_Element, Result_Unit);
            Arg_Node := R_Node (Arg_Element);
            Entity_Node := Defining_Identifier (Arg_Node);
            Result_Node := Full_View (Entity_Node);
         end if;
      end if;

      --  one step up from the defining name to the corresponding type
      --  declaration
      Result_Node := Parent (Result_Node);

      --  if we are here, Result_Node is not Empty!
      pragma Assert (Present (Result_Node));

      if not Exists (Result_Unit) then
         Result_Unit :=
            Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);
      end if;

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Type_Completion");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Completion",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Type_Completion;

   -------------------------------------
   -- Corresponding_Type_Partial_View --
   -------------------------------------

   function Corresponding_Type_Partial_View
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Arg_Unit    : Asis.Compilation_Unit;
      pragma Unreferenced (Arg_Unit);
      Entity_Node : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit := Nil_Compilation_Unit;
   begin
      Check_Validity
       (Declaration,
        Package_Name & "Corresponding_Type_Partial_View");

      if not (Arg_Kind = An_Ordinary_Type_Declaration         or else
              Arg_Kind = A_Task_Type_Declaration              or else
              Arg_Kind = A_Protected_Type_Declaration         or else
              Arg_Kind = A_Private_Type_Declaration           or else
              Arg_Kind = A_Private_Extension_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Type_Partial_View",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := R_Node (Declaration);

      if Sloc (Arg_Node) <= Standard_Location then
         --  No private/incomplete declaration in Standard!
         return Nil_Element;
      end if;

      Entity_Node := Defining_Identifier (Arg_Node);
      Arg_Unit := Get_Comp_Unit
        (Encl_Unit_Id (Declaration), Encl_Cont_Id (Declaration));

      Result_Node := Serach_First_View (Entity_Node);

      if Result_Node = Entity_Node then
         --  no private/incomplete view, therefore:
         return Nil_Element;
      end if;

      --  one step up from the defining name to the corresponding type
      --  declaration
      Result_Node := Parent (Result_Node);

      --  if we are here, Result_Node is not Empty!
      pragma Assert (Present (Result_Node));

      if not Exists (Result_Unit) then
         Result_Unit :=
            Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);
      end if;

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Type_Partial_View");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Partial_View",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Type_Partial_View;

--  --|A2010 end

   -----------------------
   -- Discriminant_Part --
   -----------------------

   function Discriminant_Part
     (Declaration : Asis.Declaration)
      return        Asis.Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;

      Result   : Element := Nil_Element;
   begin
      Check_Validity (Declaration, Package_Name & "Discriminant_Part");

      if not (Arg_Kind = An_Ordinary_Type_Declaration          or else
              Arg_Kind = A_Task_Type_Declaration               or else
              Arg_Kind = A_Protected_Type_Declaration          or else
              Arg_Kind = An_Incomplete_Type_Declaration        or else
--  |A2005 start
              Arg_Kind = A_Tagged_Incomplete_Type_Declaration  or else
--  |A2005 end
             Arg_Kind = A_Private_Type_Declaration             or else
             Arg_Kind = A_Private_Extension_Declaration        or else
             Arg_Kind = A_Formal_Type_Declaration              or else
--  |A2012 start
              Arg_Kind = A_Formal_Incomplete_Type_Declaration)
--  |A2012 end
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminant_Part",
            Wrong_Kind => Arg_Kind);
      end if;

      if Special_Case (Declaration) = From_Limited_View then
         --  See Ada2012-B108
         return Nil_Element;
      end if;

      --  There is no appropriate Node in the tree to map the Asis Element
      --  of the An_Unknown_Discriminant_Part or A_Known_Discriminant_Part
      --  Definition_Kinds values. So the (non-nil) result of this function
      --  contains just the same value in the field Node as the argument,
      --  but it differs in the value of the Internal_Kind fields.

      Arg_Node := Node (Declaration);

      if Present (Arg_Node) then

         if Present (Discriminant_Specifications (Arg_Node)) then
            Result := Declaration;
            Set_Int_Kind (Result, A_Known_Discriminant_Part);

         elsif not (Arg_Kind = An_Ordinary_Type_Declaration or else
                    Arg_Kind = A_Task_Type_Declaration      or else
                    Arg_Kind = A_Protected_Type_Declaration)
            and then
               Unknown_Discriminants_Present (Arg_Node)
         then
            Result := Declaration;
            Set_Int_Kind (Result, An_Unknown_Discriminant_Part);
         end if;

      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Discriminant_Part");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminant_Part",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Discriminant_Part;

   -----------
   -- Names --
   -----------

   function Names
     (Declaration : Asis.Declaration)
      return        Asis.Defining_Name_List
   is
      Arg_Node    : Node_Id;
      Decl_Kind   : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;

      Result_Elem : Element;
   begin
      Check_Validity (Declaration, Package_Name & "Names");

      if not (Decl_Kind in Internal_Declaration_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Names",
            Wrong_Kind => Decl_Kind);
      end if;

      Arg_Node := Node (Declaration);

      case Internal_Declaration_Kinds (Decl_Kind) is

         when An_Ordinary_Type_Declaration =>

            Result_Node := R_Node (Declaration);

            if No (Result_Node) then
               --  The declaration of a root or universal numeric type,
               --  the type is anonymous, so the result should be nil
               return Nil_Element_List;
            end if;

            Result_Node := Defining_Identifier (Result_Node);
            Result_Kind := A_Defining_Identifier;

         when A_Variable_Declaration          |
              A_Constant_Declaration          |
              A_Deferred_Constant_Declaration |
              An_Integer_Number_Declaration   |
              A_Real_Number_Declaration       |
              A_Discriminant_Specification    |
              A_Component_Declaration         |
              An_Exception_Declaration        |
              A_Formal_Object_Declaration     =>

            if Is_From_Inherited (Declaration) then
               Result_Elem := Node_To_Element_New
                 (Starting_Element => Declaration,
                  Node             => Node_Field_2 (Declaration));

               return (1 => Result_Elem);
            else
               return Defining_Id_List_From_Normalized
                        (N                => Arg_Node,
                         From_Declaration => Declaration);
            end if;

         when A_Parameter_Specification =>

            return Defining_Id_List_From_Normalized
                     (N                => Arg_Node,
                      From_Declaration => Declaration);

         when A_Task_Type_Declaration                |
              A_Protected_Type_Declaration           |
              An_Incomplete_Type_Declaration         |
--  --|A2005 start
              A_Tagged_Incomplete_Type_Declaration   |
              A_Return_Variable_Specification        |
              A_Return_Constant_Specification        |
--  --|A2005 end
              A_Private_Type_Declaration             |
              A_Private_Extension_Declaration        |
              A_Subtype_Declaration                  |
              A_Single_Task_Declaration              |
              A_Single_Protected_Declaration         |
              A_Loop_Parameter_Specification         |
              A_Generalized_Iterator_Specification   |
              An_Element_Iterator_Specification      |
              An_Object_Renaming_Declaration         |
              A_Task_Body_Declaration                |
              A_Protected_Body_Declaration           |
              An_Entry_Declaration                   |
              An_Entry_Body_Declaration              |
              An_Entry_Index_Specification           |
              A_Package_Body_Stub                    |
              A_Task_Body_Stub                       |
              A_Protected_Body_Stub                  |
              A_Formal_Type_Declaration              |
              A_Formal_Incomplete_Type_Declaration   |
              A_Formal_Package_Declaration           |
              A_Formal_Package_Declaration_With_Box  =>

            Result_Node := Defining_Identifier (Arg_Node);
            Result_Kind := A_Defining_Identifier;
            --  See "Open problems" below

         when An_Exception_Renaming_Declaration =>

            if Special_Case (Declaration) = Numeric_Error_Renaming then
               Result_Elem := Declaration;
               Set_Int_Kind (Result_Elem, A_Defining_Identifier);

               return (1 => Result_Elem);
            else
               Result_Node := Defining_Identifier (Arg_Node);
               Result_Kind := A_Defining_Identifier;
            end if;

         when An_Enumeration_Literal_Specification =>

            Result_Node := Arg_Node;

            if Special_Case (Declaration) = Stand_Char_Literal    or else
               Nkind (Result_Node) = N_Defining_Character_Literal or else
               Character_Code (Declaration) /= 0
            then
               Result_Kind := A_Defining_Character_Literal;
            else
               Result_Kind := A_Defining_Enumeration_Literal;
            end if;

         when   A_Procedure_Body_Declaration
              | A_Function_Body_Declaration
              | A_Package_Declaration
              | A_Procedure_Renaming_Declaration
              | A_Function_Renaming_Declaration
              | A_Procedure_Body_Stub
              | A_Function_Body_Stub
              | A_Generic_Procedure_Declaration
              | A_Generic_Function_Declaration
              | A_Generic_Package_Declaration
              | A_Formal_Procedure_Declaration
              | A_Formal_Function_Declaration  =>

            if Decl_Kind = A_Package_Declaration
              and then
               Nkind (Arg_Node) = N_Formal_Package_Declaration
            then
               --  A special case of expanded  package corresponding
               --  to a formal generic package
               Arg_Node := R_Node (Declaration);
            end if;

            Result_Node := Defining_Unit_Name (Specification (Arg_Node));

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Kind := A_Defining_Expanded_Name;
            else
               Result_Kind := Not_An_Element;
            end if;

         when A_Package_Body_Declaration =>

            Result_Node := Defining_Unit_Name (Arg_Node);

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Kind := A_Defining_Expanded_Name;
            else
               Result_Kind := A_Defining_Identifier;
            end if;

         when   A_Procedure_Declaration
--  --|A2005 start
              | A_Null_Procedure_Declaration
--  --|A2005 end
              | A_Function_Declaration
              | A_Package_Renaming_Declaration
              | A_Generic_Package_Renaming_Declaration
              | A_Generic_Procedure_Renaming_Declaration
              | A_Generic_Function_Renaming_Declaration
              | A_Package_Instantiation
              | A_Procedure_Instantiation
              | A_Function_Instantiation   =>

            if Decl_Kind in A_Procedure_Declaration .. A_Function_Declaration
--  --|A2005 start
              or else
               Decl_Kind = A_Null_Procedure_Declaration
--  --|A2005 end
            then
               Result_Node := Defining_Unit_Name (Specification (Arg_Node));
            else
               Result_Node := Defining_Unit_Name (Arg_Node);
            end if;

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Kind := A_Defining_Expanded_Name;
            else
               Result_Kind := Not_An_Element;
            end if;

--  --|A2012 start
         when An_Expression_Function_Declaration =>
            --  This may be changed again...
            Arg_Node    := R_Node (Declaration);
            Result_Node := Defining_Unit_Name (Specification (Arg_Node));
--  --|A2012 end

         when A_Choice_Parameter_Specification =>
            Result_Node := Arg_Node;
            Result_Kind := A_Defining_Identifier;

      end case;

      return (1 => Node_To_Element_New (Starting_Element => Declaration,
                                        Node             => Result_Node,
                                        Internal_Kind    => Result_Kind));
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Names");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Names",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Names;
   --  OPEN PROBLEMS: 1. May be, we should use R_Node (Declaration) instead
   --  of Node for all possible argument kinds, not only for
   --  An_Ordinary_Type_Declaration to handle properly all the rewritings

   function Defining_Name_Image
     (Defining_Name : Asis.Defining_Name)
      return          Wide_String
   is
      Arg_Node    : Node_Id;
      Def_N_Kind  : Internal_Element_Kinds;
      Ch_Code     : Char_Code;
      Image_Start : Source_Ptr;
      Image_End   : Source_Ptr;

      Name_Prefix : Asis.Expression;
      --  for recursive construction of A_Defining_Expanded_Name image

      function Prefix_Image (Name_Prefix : Asis.Name) return Program_Text;
      --   Returns the string image for the prefix A_Defining_Expanded_Name.

      function Prefix_Image (Name_Prefix : Asis.Name) return Program_Text is
      begin
         if Int_Kind (Name_Prefix) = An_Identifier then
            return Asis.Expressions.Name_Image (Name_Prefix);
         else
            return
              Prefix_Image (Asis.Expressions.Prefix (Name_Prefix))
              & "."
              & Asis.Expressions.Name_Image
                  (Asis.Expressions.Selector (Name_Prefix));
         end if;
      end Prefix_Image;

   begin

      Check_Validity (Defining_Name, Package_Name & "Defining_Name_Image");
      Def_N_Kind := Int_Kind (Defining_Name);

      if Def_N_Kind not in Internal_Defining_Name_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Defining_Name_Image",
            Wrong_Kind => Def_N_Kind);
      end if;

      --  Let's first handle special cases:

      case Special_Case (Defining_Name) is
         when Numeric_Error_Renaming =>
            --  (see B712-0050)
            return "Numeric_Error";
         when Is_From_Imp_Neq_Declaration =>

            if Ekind (Node (Defining_Name)) = E_Function then
               return """/=""";
            end if;

         when others =>
            null;
      end case;

      --  we should make the difference between entities defined in the
      --  package Standard and all the other entities. Entities from the
      --  package Standard are processed on the basis of Name Table, all the
      --  other ones - on the basis of the Source Buffer.

      Arg_Node := Node (Defining_Name);

      if Nkind (Arg_Node) in N_Entity and then
         Ekind (Arg_Node) = E_Enumeration_Literal
      then
         while Present (Alias (Arg_Node)) loop
            --  Traversing the chain of inherited literals to get the
            --  original name (we need the correct casing!)
            Arg_Node := Alias (Arg_Node);
         end loop;
      end if;

      case Def_N_Kind is

         when A_Defining_Character_Literal =>

            if Sloc (Arg_Node) <= Standard_Location then

               if Nkind (Arg_Node) = N_Character_Literal then
                  Ch_Code := UI_To_CC (Char_Literal_Value (Arg_Node));
               else
                  Ch_Code := Character_Code (Defining_Name);
               end if;

               return Stand_Char_Image (Ch_Code);

            elsif Is_From_Inherited (Defining_Name)
              and then
                  Character_Code (Defining_Name) /= 0
            then

               return To_Wide_String (
                      '''
                    & Get_Character (Character_Code (Defining_Name))
                    & ''');

            else
               return ''' & Get_Wide_Ch (Sloc (Arg_Node) + 1) & ''';
            end if;

         when   A_Defining_Identifier |
                A_Defining_Enumeration_Literal =>

            --  the situation is the same for them

            if Sloc (Arg_Node) <= Standard_Location then
               return To_Program_Text (Normalized_Namet_String (Arg_Node));
            else

               Image_Start := Sloc (Arg_Node);

               --  Sloc points to << so we have to go right to the
               --  first letter of the identifier

               if Nkind (Arg_Node) = N_Label then
                  Image_Start := Next_Identifier (Image_Start);
               end if;

               Image_End   := Get_Word_End (P       => Image_Start,
                                            In_Word => In_Identifier'Access);
               return Get_Wide_Word (Image_Start, Image_End);
            end if;
         when Internal_Defining_Operator_Kinds =>

            if Sloc (Arg_Node) <= Standard_Location then
               Not_Implemented_Yet (Package_Name & "Defining_Name_Image: "
                 & "An operator symbol defined in Standard");
            else
               return Wide_String_Image (Arg_Node);
            end if;
         when A_Defining_Expanded_Name =>

            --  there is nothing of this kind in Standard

            Name_Prefix := Defining_Prefix (Defining_Name);
            return  Prefix_Image (Name_Prefix) & "."
              & Defining_Name_Image (Defining_Selector (Defining_Name));
         when others =>

            --  this choice can never been reached, see the condition for
            --  defining the appropriate argument
            raise Internal_Implementation_Error;
      end case;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Defining_Name,
               Outer_Call => Package_Name & "Defining_Name_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Defining_Name_Image",
            Ex          => Ex,
            Arg_Element => Defining_Name);
   end Defining_Name_Image;
   ----------------------------------------------------------------------------
   --  OPEN PROBLEMS:
   --
   --  1. A_Defining_Expanded_Name: is the recursive construction of the result
   --     of the String type a really good thing here? The performance can be
   --     poor but, from the other hand, this can happen not very often.
   --
   --  2. The Asis.Expressions.Name_Image function contains the (almost) exact
   --     copy of the part of the code of this function (except the part for
   --     processing A_Defining_Expanded_Name). May be, it should be better
   --     to separate it on low-level function.
   ----------------------------------------------------------------------------

   function Position_Number_Image
     (Defining_Name : Asis.Defining_Name)
      return          Wide_String
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Defining_Name);
      Arg_Node  : Node_Id;
      Arg_Nkind : Node_Kind;
      Result    : Uint;
   begin
      Check_Validity (Defining_Name,
        Package_Name & "Position_Number_Image");

      if not (Arg_Kind = A_Defining_Character_Literal  or else
              Arg_Kind = A_Defining_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Position_Number_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Defining_Name);
      Arg_Nkind := Nkind (Arg_Node);

      if Arg_Nkind = N_Defining_Identifier or else
         Arg_Nkind = N_Defining_Character_Literal
      then
         Result := Enumeration_Pos (Arg_Node);
         UI_Image (Result, Format => Decimal);

         return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));

      else
         --  Here we have literals of Standard.Character and
         --  Standard.Wide_Cherecter

         return To_Wide_String --  ??? Wide_Character ???
                  (Trim (Character_Code (Defining_Name)'Img, Left));

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Defining_Name,
               Outer_Call => Package_Name & "Position_Number_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Position_Number_Image",
            Ex          => Ex,
            Arg_Element => Defining_Name);
   end Position_Number_Image;
   -------------------------------------------------------------------------

   function Representation_Value_Image
     (Defining_Name : Asis.Defining_Name)
      return          Wide_String
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Defining_Name);
      Arg_Node : Node_Id;
      Arg_Nkind : Node_Kind;
      Result    : Uint;
   begin
      Check_Validity (Defining_Name,
        Package_Name & "Representation_Value_Image");

      if not (Arg_Kind = A_Defining_Character_Literal  or else
              Arg_Kind = A_Defining_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Representation_Value_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Defining_Name);
      Arg_Nkind := Nkind (Arg_Node);

      if Arg_Nkind = N_Defining_Identifier or else
         Arg_Nkind = N_Defining_Character_Literal
      then
         Result := Enumeration_Rep (Arg_Node);
         UI_Image (Result, Format => Decimal);

         return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));

      else
         --  Here we have literals of Standard.Character and
         --  Standard.Wide_Cherecter

         return To_Wide_String  --  ??? Wide_Character ???
                  (Trim (Character_Code (Defining_Name)'Img, Left));

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Defining_Name,
               Outer_Call => Package_Name & "Representation_Value_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Representation_Value_Image",
            Ex          => Ex,
            Arg_Element => Defining_Name);
   end Representation_Value_Image;
   --------------------------------------------------------------------------

   function Defining_Prefix
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Defining_Name);
      Arg_Node : Node_Id;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds;
   begin
      Check_Validity (Defining_Name, Package_Name & "Defining_Prefix");

      if not (Arg_Kind = A_Defining_Expanded_Name) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Defining_Prefix",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Defining_Name);
      Result_Node := Sinfo.Name (Arg_Node);

      if Nkind (Result_Node) = N_Identifier then
         Result_Kind := An_Identifier;
      else
         Result_Kind := A_Selected_Component;
      end if;

      return Node_To_Element_New (
        Node             => Result_Node,
        Starting_Element => Defining_Name,
        Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Defining_Name,
               Outer_Call => Package_Name & "Defining_Prefix");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Defining_Prefix",
            Ex          => Ex,
            Arg_Element => Defining_Name);
   end Defining_Prefix;
   -------------------------------------------------------------------------

   function Defining_Selector
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Defining_Name);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Defining_Name, Package_Name & "Defining_Selector");

      if not (Arg_Kind = A_Defining_Expanded_Name) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Defining_Selector",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Defining_Name);

      return Node_To_Element_New (
        Node          => Defining_Identifier (Arg_Node),
        Starting_Element => Defining_Name,
        Internal_Kind => A_Defining_Identifier);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Defining_Name,
               Outer_Call => Package_Name & "Defining_Selector");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Defining_Selector",
            Ex          => Ex,
            Arg_Element => Defining_Name);
   end Defining_Selector;
   -------------------------------------------------------------------------

   -----------------------------------------------------------------------

   function Type_Declaration_View
     (Declaration : Asis.Declaration)
      return        Asis.Type_Definition
   is
      Arg_Node             : Node_Id;
      Decl_Kind            : Internal_Element_Kinds;
      Result_Node          : Node_Id;
      Result_Internal_Kind : Internal_Element_Kinds;
   begin
      Check_Validity (Declaration, Package_Name & "Type_Declaration_View");
      Decl_Kind := Int_Kind (Declaration);

      if not (Decl_Kind = An_Ordinary_Type_Declaration    or else
        Decl_Kind = A_Task_Type_Declaration         or else
        Decl_Kind = A_Protected_Type_Declaration    or else
        Decl_Kind = A_Private_Type_Declaration      or else
        Decl_Kind = A_Private_Extension_Declaration or else
        Decl_Kind = A_Subtype_Declaration           or else
        Decl_Kind = A_Formal_Type_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Type_Declaration_View",
            Wrong_Kind => Decl_Kind);
      end if;

      --  tree traversing:

      --  first, we have to detect whether we have the declaration of a
      --  root or universal numeric type, and if we have, we should
      --  process it in a special way:

      if Is_Root_Num_Type (Declaration) then
         return Asis.Set_Get.Root_Type_Definition (Declaration);
      end if;

      --  forming the result for incomplete and private types:

      Arg_Node := Node (Declaration);

      case Decl_Kind is
         when A_Private_Type_Declaration |
           A_Private_Extension_Declaration
           =>
            --  There is no appropriate Node in the tree to map the Asis
            --  Element of the
            --
            --    A_Private_Type_Definition,
            --    A_Tagged_Private_Type_Definition,
            --    A_Private_Extension_Definition,
            --
            --  Definition_Kind values. So the result of this function
            --  for the A_Private_Type_Declaration or
            --  A_Private_Extension_Declaration passed as the argument
            --  contains just the same value in the field Node as the , but
            --  argument it differs in the values of the Kind and
            --  Internal_Kind fields.

            if Decl_Kind = A_Private_Extension_Declaration then
               Result_Internal_Kind := A_Private_Extension_Definition;
            elsif Tagged_Present (Arg_Node) then
               Result_Internal_Kind := A_Tagged_Private_Type_Definition;
            else
               Result_Internal_Kind := A_Private_Type_Definition;
            end if;

            return Node_To_Element_New
              (Node          => Arg_Node,
              Starting_Element => Declaration,
              Internal_Kind => Result_Internal_Kind);

         when An_Ordinary_Type_Declaration =>
            Result_Node := Sinfo.Type_Definition (Arg_Node);

         when A_Task_Type_Declaration =>
            Result_Node := Task_Definition (Arg_Node);

            if No (Result_Node) then
               return Nil_Element;
            end if;

         when A_Protected_Type_Declaration =>
            Result_Node := Protected_Definition (Arg_Node);

         when A_Subtype_Declaration =>
            Result_Node := Sinfo.Subtype_Indication (Arg_Node);
            return Node_To_Element_New
              (Node          => Result_Node,
              Starting_Element => Declaration,
              Internal_Kind => A_Subtype_Indication);
            --  Automatic Element Kind determination is also possible
            --  here, but it seems unnecessary

         when A_Formal_Type_Declaration =>
            Result_Node := Sinfo.Formal_Type_Definition (Arg_Node);

         when others =>
            null;
      end case;

      --  forming the result for other kinds of types:

      return Node_To_Element_New
        (Node    => Result_Node,
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Type_Declaration_View");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Type_Declaration_View",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Type_Declaration_View;

   -----------------------------
   -- Object_Declaration_View --
   -----------------------------

   function Object_Declaration_View
     (Declaration : Asis.Declaration)
      return        Asis.Type_Definition
   is
      Arg_Node : Node_Id;
      Decl_Kind            : Internal_Element_Kinds;
      Result_Node          : Node_Id;
      Result_Internal_Kind : Internal_Element_Kinds := Not_An_Element;
      --  We keep using Result_Internal_Kind from old ASIS 95 code, but it is
      --  not used for new Ada 2005 features (access definitions) as well as
      --  for extending the functionality of this query to all forms of
      --  object definitions
   begin
      Check_Validity (Declaration,
        Package_Name & "Object_Declaration_View");
      Decl_Kind := Int_Kind (Declaration);

      if not (Decl_Kind = A_Variable_Declaration          or else
              Decl_Kind = A_Constant_Declaration          or else
              Decl_Kind = A_Deferred_Constant_Declaration or else
              Decl_Kind = A_Single_Protected_Declaration  or else
              Decl_Kind = A_Single_Task_Declaration       or else
              Decl_Kind = A_Component_Declaration         or else
--  |A2005 start
              Decl_Kind = A_Discriminant_Specification    or else
              Decl_Kind = A_Parameter_Specification       or else
              Decl_Kind = A_Return_Variable_Specification or else
              Decl_Kind = A_Return_Constant_Specification or else
              Decl_Kind = A_Formal_Object_Declaration     or else
              Decl_Kind = An_Object_Renaming_Declaration)
--  |A2005 end
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Object_Declaration_View",
            Wrong_Kind => Decl_Kind);
      end if;

      Arg_Node := Node (Declaration);

      case Decl_Kind is

         when A_Variable_Declaration          |
              A_Constant_Declaration          |
              A_Deferred_Constant_Declaration |
              A_Return_Variable_Specification |
              A_Return_Constant_Specification =>

            Result_Node := Object_Definition (Arg_Node);

            case Nkind (Original_Node (Result_Node)) is
               when N_Constrained_Array_Definition =>
                  Result_Internal_Kind := A_Constrained_Array_Definition;

               when N_Unconstrained_Array_Definition =>
                  Result_Internal_Kind := An_Unconstrained_Array_Definition;

--  --|A2005 start
               when N_Access_Definition =>
                  null;
--  --|A2005 end
               when others =>
                  Result_Internal_Kind := A_Subtype_Indication;
            end case;

         when A_Component_Declaration =>
            Result_Node          := Sinfo.Component_Definition (Arg_Node);
            Result_Internal_Kind := A_Component_Definition;

         when A_Single_Protected_Declaration =>
            Result_Node          := Protected_Definition (Arg_Node);
            Result_Internal_Kind := A_Protected_Definition;

         when A_Single_Task_Declaration =>
            Result_Node          := Task_Definition (Arg_Node);
            Result_Internal_Kind := A_Task_Definition;

            if No (Result_Node) then
               return Nil_Element;
            end if;

--  --|A2005 start
         when A_Discriminant_Specification =>
            Result_Node := Discriminant_Type (Arg_Node);
         when A_Parameter_Specification =>
            Result_Node := Parameter_Type (Arg_Node);
         when An_Object_Renaming_Declaration |
              A_Formal_Object_Declaration    =>

            if Present (Access_Definition (Arg_Node)) then
               Result_Node := Access_Definition (Arg_Node);
            else
               Result_Node := Sinfo.Subtype_Mark (Arg_Node);
            end if;

         when others =>
            Not_Implemented_Yet
              (Diagnosis => Package_Name & "Object_Declaration_View");
--  --|A2005 end
      end case;

      return Node_To_Element_New
        (Node             => Result_Node,
         Starting_Element => Declaration,
         Internal_Kind    => Result_Internal_Kind);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Object_Declaration_View");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Object_Declaration_View",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Object_Declaration_View;
   --------------------------------------------------------------------------
   function Initialization_Expression
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity
        (Declaration, Package_Name & "Initialization_Expression");
      if not (Arg_Kind = A_Variable_Declaration          or else
              Arg_Kind = A_Constant_Declaration          or else
              Arg_Kind = An_Integer_Number_Declaration   or else
              Arg_Kind = A_Real_Number_Declaration       or else
              Arg_Kind = A_Discriminant_Specification    or else
              Arg_Kind = A_Component_Declaration         or else
              Arg_Kind = A_Parameter_Specification       or else
--  |A2005 start
              Arg_Kind = A_Return_Variable_Specification or else
              Arg_Kind = A_Return_Constant_Specification or else
--  |A2005 end
              Arg_Kind = A_Formal_Object_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Initialization_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      if Nkind (Arg_Node) = N_Formal_Object_Declaration then
         Result_Node := Sinfo.Default_Expression (Arg_Node);
      else
         Result_Node := Sinfo.Expression (Arg_Node);
      end if;

      if No (Result_Node)
          or else
        (Is_Rewrite_Substitution (Result_Node)
        and then
         not (Comes_From_Source (Original_Node (Result_Node))))
      then
         return Nil_Element;
      else

         return Node_To_Element_New
           (Node             => Result_Node,
           Starting_Element => Declaration);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Initialization_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Initialization_Expression",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Initialization_Expression;
   -----------------------------------------------------------------------
   function Corresponding_Constant_Declaration
     (Name : Asis.Defining_Name)
      return Asis.Declaration
   is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Name);
      Arg_Node      : Node_Id;
      Arg_Decl_Node : Node_Id;
      Result_Node   : Node_Id;
   begin
      Check_Validity (Name,
        Package_Name & "Corresponding_Constant_Declaration");

      Arg_Node := Node (Name);
      Arg_Decl_Node := Parent (Arg_Node);

      if not (Arg_Kind              = A_Defining_Identifier and then
              Nkind (Arg_Decl_Node) = N_Object_Declaration  and then
              Constant_Present (Arg_Decl_Node))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Constant_Declaration",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we should find out, where we are - in a constant
      --  declaration or in a deferred constant declaration:

      if Present (Sinfo.Expression (Parent (Arg_Node))) then
         --  we are in a full constant declaration.
         --  Here we have to traverse the tree to find the corresponding
         --  deferred constant declaration, if any. To optimize the search,
         --  we first check if we are in the private part of a package.
         --
         --  Unfortunately, in 3.10a Is_Private is set on for entities
         --  declared immediately within package bodies, so we have
         --  to do the corresponding check "by hands"::

         if not (Nkind (Parent (Arg_Decl_Node)) =
                 N_Package_Specification
           and then
              List_Containing (Arg_Decl_Node) =
              Private_Declarations (Parent (Arg_Decl_Node)))
         then
            --  it cannot be a completion of a deferred constant,
            --  see RM95 7.4(4)
            return Nil_Element;
         end if;

         --  and here we have to go into the visible part of the same
         --  package and to look for the same defining name in it:

         Result_Node := First_Non_Pragma (Visible_Declarations (Parent (
           Parent (Arg_Node))));

         while Present (Result_Node) loop
            if Nkind (Result_Node) = N_Object_Declaration and then
              Constant_Present (Result_Node)              and then
              No (Sinfo.Expression (Result_Node))         and then
              Full_View (Defining_Identifier (Result_Node)) = Arg_Node
            then
               return Node_To_Element_New
                 (Node             => Result_Node,
                  Internal_Kind    => A_Deferred_Constant_Declaration,
                  Starting_Element => Name);
            end if;

            Result_Node := Next_Non_Pragma (Result_Node);
         end loop;

         --  here we are only if there is no declaration of a deferred
         --  constant corresponding to the given full constant declaration,
         --  so:
         return Nil_Element;
      else
         --  we are in deferred constant declaration

         if Is_Imported (Arg_Node) then
            --  a deferred constant completed by a pragma Import
            return Nil_Element;
         end if;

         Result_Node := Parent (Full_View (Arg_Node));

         return Node_To_Element_New
           (Node             => Result_Node,
           Starting_Element => Name,
           Internal_Kind => A_Constant_Declaration);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Name,
               Outer_Call => Package_Name &
                             "Corresponding_Constant_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Constant_Declaration",
            Ex          => Ex,
            Arg_Element => Name);
   end Corresponding_Constant_Declaration;
   --------------------------------------------------------------------------

   function Declaration_Subtype_Mark
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity (Declaration,
        Package_Name & "Declaration_Subtype_Mark");

      if not (Arg_Kind = A_Discriminant_Specification or else
        Arg_Kind = A_Parameter_Specification    or else
        Arg_Kind = A_Formal_Object_Declaration  or else
        Arg_Kind = An_Object_Renaming_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Declaration_Subtype_Mark",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      if Arg_Kind = A_Formal_Object_Declaration or else
         Arg_Kind = An_Object_Renaming_Declaration
      then
         Result_Node := Sinfo.Subtype_Mark (Arg_Node);

      elsif Arg_Kind = A_Discriminant_Specification then
         Result_Node := Discriminant_Type (Arg_Node);
      else -- Arg_Kind = A_Parameter_Specification
         Result_Node := Parameter_Type (Arg_Node);
      end if;

      if Nkind (Result_Node) = N_Access_Definition then
         Result_Node := Sinfo.Subtype_Mark (Result_Node);
      end if;

      --  if the result node is of N_Attribute_Reference kind, we should
      --  define the kind of this attribute, so general Node_To_Element
      --  function is used, otherwise we set An_Identifier or
      --  A_Selected_Component result kind "by hand"

      --  starting from GNAT 3.09, the compiler rewrite the node corresponding
      --  to the attribute reference in the a context like this:
      --  function "**" (Left: Precision_Float; Right: Integer'Base)
      --                                        ^^^^^^^^^^^^^^^^^^^^

      if Nkind (Result_Node) = N_Attribute_Reference
        or else
         (Is_Rewrite_Substitution (Result_Node)
         and then
          Nkind (Original_Node (Result_Node)) = N_Attribute_Reference)
        or else
         --  See FA13-008.
         (Nkind (Result_Node) = N_Identifier
         and then
          not Comes_From_Source (Result_Node)
         and then
          Ekind (Entity (Result_Node)) = E_Class_Wide_Type)

      then
         null;
         --  not a very elegant solution, but the idea is to keep
         --  Result_Kind set to Not_An_Element and not to disturb the old
         --  workable code
      elsif Nkind (Original_Node (Result_Node)) = N_Identifier then
         Result_Kind := An_Identifier;
      else
         Result_Kind := A_Selected_Component;
      end if;

      return Node_To_Element_New
        (Node            => Result_Node,
        Starting_Element => Declaration,
        Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Declaration_Subtype_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Declaration_Subtype_Mark",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Declaration_Subtype_Mark;
   ----------------------------------------------------------------------

   ------------------------------------------------------------------------
   --  NOT IMPLEMENTED

   function Corresponding_Type_Declaration
     (Declaration : Asis.Declaration;
      The_Context : Asis.Context)
      return        Asis.Declaration
   is
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Type_Declaration");
      Check_Validity (The_Context,
        Package_Name & "Corresponding_Type_Declaration");

      Not_Implemented_Yet (Diagnosis =>
        Package_Name & "Corresponding_Type_Declaration");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Type_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type_Declaration",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Type_Declaration;
   ----------------------------------------------------------------

   function Corresponding_First_Subtype
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;

      Result_Unit : Asis.Compilation_Unit;

   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_First_Subtype");

      if not (Arg_Kind = An_Ordinary_Type_Declaration    or else
              Arg_Kind = A_Task_Type_Declaration         or else
              Arg_Kind = A_Protected_Type_Declaration    or else
              Arg_Kind = A_Private_Type_Declaration      or else
              Arg_Kind = A_Private_Extension_Declaration or else
              Arg_Kind = A_Subtype_Declaration           or else
              Arg_Kind = A_Formal_Type_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_First_Subtype",
            Wrong_Kind => Arg_Kind);
      end if;

      if Arg_Kind = An_Ordinary_Type_Declaration    or else
         Arg_Kind = A_Task_Type_Declaration         or else
         Arg_Kind = A_Protected_Type_Declaration    or else
         Arg_Kind = A_Private_Type_Declaration      or else
         Arg_Kind = A_Private_Extension_Declaration or else
         Arg_Kind = A_Formal_Type_Declaration
      then
         return Declaration;
      end if;

      Arg_Node := Node (Declaration);
      --  now we are processing a subtype declaration, therefore Arg_Node
      --  is of N_Subtype_Declaration kind

      pragma Assert (Nkind (Arg_Node) = N_Subtype_Declaration);
      Result_Node := Sem_Aux.First_Subtype (Defining_Identifier (Arg_Node));

      --  We are in the subtype declaration now, so we can detect this
      --  situation like this:
      if Result_Node = Defining_Identifier (Arg_Node) then
         Result_Node := Sinfo.Subtype_Indication (Arg_Node);

         if Nkind (Result_Node) = N_Subtype_Indication then
            Result_Node := Sinfo.Subtype_Mark (Result_Node);
         end if;

         Result_Node := Entity (Result_Node);

         while Nkind (Parent (Result_Node)) = N_Subtype_Declaration loop
            Result_Node := Sinfo.Subtype_Indication (Parent (Result_Node));

            if Nkind (Result_Node) = N_Subtype_Indication then
               Result_Node := Sinfo.Subtype_Mark (Result_Node);
            end if;

            Result_Node := Entity (Result_Node);
         end loop;
      end if;

      --  and now - from defining name to the corresponding type declaration:
      if No (Parent (Result_Node)) then

         if Is_Itype (Result_Node) then
            --  for now, the only discovered case is the subtyping
            --  from a formal array type.
            Result_Node := Associated_Node_For_Itype (Result_Node);
         else
            --  for now, the only truly discovered case when
            --  Parent (Result_Node) is Empty and Is_Itype is NOT set on
            --  is when Result_Node points
            --  to the implicit entity created by the compiler for
            --  a formal integer type. In this case the following approach
            --  should work:
            Result_Node := Next_Entity (Result_Node);
         end if;
      end if;

      if Nkind (Result_Node) = N_Defining_Identifier then
         --  we should not do this step up when Associated_Node_For_Itype
         --  has been applied!
         Result_Node := Parent (Result_Node);
      end if;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call =>
              Package_Name & "Corresponding_First_Subtype");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_First_Subtype",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_First_Subtype;
   ----------------------------------------------------------------
   function Corresponding_Last_Constraint
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Last_Constraint");

      if not (Arg_Kind = An_Ordinary_Type_Declaration    or else
              Arg_Kind = A_Task_Type_Declaration         or else
              Arg_Kind = A_Protected_Type_Declaration    or else
              Arg_Kind = A_Private_Type_Declaration      or else
              Arg_Kind = A_Private_Extension_Declaration or else
              Arg_Kind = A_Subtype_Declaration           or else
              Arg_Kind = A_Formal_Type_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Last_Constraint",
            Wrong_Kind => Arg_Kind);
      end if;

      if Arg_Kind = An_Ordinary_Type_Declaration    or else
         Arg_Kind = A_Task_Type_Declaration         or else
         Arg_Kind = A_Protected_Type_Declaration    or else
         Arg_Kind = A_Private_Type_Declaration      or else
         Arg_Kind = A_Private_Extension_Declaration or else
         Arg_Kind = A_Formal_Type_Declaration
      then
         return Declaration;
      end if;

      Arg_Node := Node (Declaration);
      --  now we are processing a subtype declaration, therefore Arg_Node
      --  is of N_Subtype_Declaration kind

      Result_Node := Sinfo.Subtype_Indication (Arg_Node);

      if Nkind (Result_Node) = N_Subtype_Indication then
         Result_Node := Sinfo.Subtype_Mark (Result_Node);
      end if;

      Result_Node := Parent (Entity (Result_Node));

      while Nkind (Result_Node) = N_Subtype_Declaration and then
            Nkind (Sinfo.Subtype_Indication (Result_Node)) /=
               N_Subtype_Indication
      loop
         Result_Node := Sinfo.Subtype_Indication (Result_Node);

         if Nkind (Result_Node) = N_Subtype_Indication then
            Result_Node := Sinfo.Subtype_Mark (Result_Node);
         end if;

         Result_Node := Parent (Entity (Result_Node));

      end loop;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call =>
              Package_Name & "Corresponding_Last_Constraint");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Last_Constraint",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Last_Constraint;
   ----------------------------------------------------------------
   function Corresponding_Last_Subtype
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Last_Subtype");

      if not (Arg_Kind = An_Ordinary_Type_Declaration    or else
              Arg_Kind = A_Task_Type_Declaration         or else
              Arg_Kind = A_Protected_Type_Declaration    or else
              Arg_Kind = A_Private_Type_Declaration      or else
              Arg_Kind = A_Private_Extension_Declaration or else
              Arg_Kind = A_Subtype_Declaration           or else
              Arg_Kind = A_Formal_Type_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Last_Subtype",
            Wrong_Kind => Arg_Kind);
      end if;

      if Arg_Kind = An_Ordinary_Type_Declaration    or else
         Arg_Kind = A_Task_Type_Declaration         or else
         Arg_Kind = A_Protected_Type_Declaration    or else
         Arg_Kind = A_Private_Type_Declaration      or else
         Arg_Kind = A_Private_Extension_Declaration or else
         Arg_Kind = A_Formal_Type_Declaration
      then
         return Declaration;
      end if;

      Arg_Node := Node (Declaration);
      --  now we are processing a subtype declaration, therefore Arg_Node
      --  is of N_Subtype_Declaration kind

      Result_Node := Original_Node (Sinfo.Subtype_Indication (Arg_Node));

      if Nkind (Result_Node) = N_Subtype_Indication then
         Result_Node := Original_Node (Sinfo.Subtype_Mark (Result_Node));
      end if;

      while Nkind (Result_Node) = N_Attribute_Reference loop
         --  We need this loop (as well as using Original_Node in case if the
         --  subtype mark is an attribute reference
         Result_Node := Original_Node (Prefix (Result_Node));
      end loop;

      Result_Node := Parent (Entity (Result_Node));

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Last_Subtype");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Last_Subtype",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Last_Subtype;
   ------------------------------------------------------------------
   function Corresponding_Representation_Clauses
     (Declaration : Asis.Declaration)
      return        Asis.Representation_Clause_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : constant Node_Id                := Node (Declaration);

      Arg_Node_Kind       : constant Node_Kind := Nkind (Arg_Node);
      Entity_Node         : Node_Id;
      First_Rep_Item_Node : Node_Id;
      Next_Rep_Item_Node  : Node_Id;
      Rep_Node_Kind       : Node_Kind;
      Result_Unit         : Asis.Compilation_Unit := Nil_Compilation_Unit;

      Actual_Repr_Cl : array (N_Representation_Clause)
        of Boolean := (others => True);
      Actual_Rep_Attr : array (First_Attribute_Name .. Last_Attribute_Name)
        of Boolean := (others => True);
      --  The first flag array Is used to select only first representation
      --  item from the chain, in case if representation clauses are duplicated
      --  in the tree for derived types. For attribute definition clauses we
      --  use second flag array, because we can have more than one attribute
      --  definition clauses, so we have to take the first clause for a given
      --  representation attribute

   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Representation_Clauses");

      if not (Arg_Kind in Internal_Declaration_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name & "Corresponding_Representation_Clauses",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we have to get entity from the declaration:
      if Arg_Node_Kind  = N_Subprogram_Declaration          or else
         Arg_Node_Kind  = N_Abstract_Subprogram_Declaration or else
         Arg_Node_Kind  = N_Subprogram_Body                 or else
         Arg_Node_Kind  = N_Subprogram_Body_Stub            or else
         Arg_Node_Kind  = N_Package_Declaration             or else
         Arg_Node_Kind  = N_Package_Declaration             or else
         Arg_Node_Kind  = N_Subprogram_Renaming_Declaration or else
         Arg_Node_Kind  = N_Generic_Package_Declaration     or else
         Arg_Node_Kind  = N_Generic_Subprogram_Declaration  or else
         Arg_Node_Kind in N_Formal_Subprogram_Declaration
      then
         Entity_Node := Defining_Unit_Name (Specification (Arg_Node));
      elsif
        Arg_Node_Kind = N_Package_Body                           or else
        Arg_Node_Kind = N_Package_Renaming_Declaration           or else
        Arg_Node_Kind = N_Generic_Package_Renaming_Declaration   or else
        Arg_Node_Kind = N_Generic_Procedure_Renaming_Declaration or else
        Arg_Node_Kind = N_Generic_Function_Renaming_Declaration  or else
        Arg_Node_Kind = N_Package_Instantiation                  or else
        Arg_Node_Kind = N_Procedure_Instantiation                or else
        Arg_Node_Kind = N_Function_Instantiation
      then
         Entity_Node := Defining_Unit_Name (Arg_Node);
      elsif
        Arg_Node_Kind = N_Defining_Identifier        or else
        Arg_Node_Kind = N_Defining_Character_Literal
      then
         Entity_Node := Arg_Node;
      else
         Entity_Node := Defining_Identifier (Arg_Node);
      end if;

      if Nkind (Entity_Node) = N_Defining_Program_Unit_Name then
         Entity_Node := Defining_Identifier (Entity_Node);
      end if;

      Asis_Element_Table.Init;

      First_Rep_Item_Node := First_Rep_Item (Entity_Node);
      Next_Rep_Item_Node  := First_Rep_Item_Node;

      while Present (Next_Rep_Item_Node) loop

         Rep_Node_Kind := Nkind (Next_Rep_Item_Node);

         Result_Unit :=
           Enclosing_Unit (Encl_Cont_Id (Declaration), Next_Rep_Item_Node);

         case Rep_Node_Kind is

            when N_Attribute_Definition_Clause =>

               if not From_At_Mod (Next_Rep_Item_Node) and then
                  Actual_Rep_Attr (Chars (Next_Rep_Item_Node))
               then
                  Asis_Element_Table.Append
                    (Node_To_Element_New
                      (In_Unit => Result_Unit,
                       Node    => Next_Rep_Item_Node));

                  Actual_Rep_Attr (Chars (Next_Rep_Item_Node)) := False;
               end if;

            when N_At_Clause .. N_Record_Representation_Clause =>

               if Actual_Repr_Cl (Rep_Node_Kind) then
                  Asis_Element_Table.Append
                    (Node_To_Element_New
                      (In_Unit => Result_Unit,
                       Node    => Next_Rep_Item_Node));

                  Actual_Repr_Cl (Rep_Node_Kind) := False;
               end if;

            when others =>
               --  This query is only about representation clauses, not pragmas
               null;
         end case;

         Next_Rep_Item_Node := Next_Rep_Item (Next_Rep_Item_Node);
      end loop;

      return Asis.Declaration_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call =>
              Package_Name & "Corresponding_Representation_Clauses");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Corresponding_Representation_Clauses",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Representation_Clauses;
   -----------------------------------------------------------------------
   function Specification_Subtype_Definition
     (Specification : Asis.Declaration)
      return          Asis.Discrete_Subtype_Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Specification);
      Arg_Node : Node_Id;

      Result_Node      : Node_Id;
      Result_Kind      : Internal_Element_Kinds;
      Result_Node_Kind : Node_Kind;
   begin
      Check_Validity (Specification,
        Package_Name & "Specification_Subtype_Definition");
      if not (Arg_Kind = A_Loop_Parameter_Specification or else
        Arg_Kind = An_Entry_Index_Specification)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Specification_Subtype_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Specification);
      Result_Node := Sinfo.Discrete_Subtype_Definition (Arg_Node);

      Result_Node_Kind := Nkind (Original_Node (Result_Node));

      case Result_Node_Kind is
         when N_Subtype_Indication |
              N_Identifier         |
              N_Expanded_Name      =>
            Result_Kind := A_Discrete_Subtype_Indication_As_Subtype_Definition;

         when N_Attribute_Reference =>
            Result_Kind :=
              A_Discrete_Range_Attribute_Reference_As_Subtype_Definition;

         when N_Range =>
            Result_Kind :=
              A_Discrete_Simple_Expression_Range_As_Subtype_Definition;
         when others =>
            --  unexpected Node Kind for DISCRETE_SUBTYPE_DEFINITION!!!
            pragma Assert (False);
            null;
      end case;

      return Node_To_Element_New
               (Node          => Result_Node,
                Starting_Element => Specification,
                Internal_Kind => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Specification,
               Outer_Call =>
              Package_Name & "Specification_Subtype_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Specification_Subtype_Definition",
            Ex          => Ex,
            Arg_Element => Specification);
   end Specification_Subtype_Definition;
   -------------------------------------------------------------------

   ---------------------------
   -- Iteration_Scheme_Name --
   ---------------------------

   function Iteration_Scheme_Name
     (Iterator_Specification : Asis.Element)
      return                   Asis.Element
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Iterator_Specification);
      Result_Node : Node_Id;
   begin
      Check_Validity
        (Iterator_Specification, Package_Name & "Iteration_Scheme_Name");

      if Arg_Kind not in
           A_Generalized_Iterator_Specification ..
           An_Element_Iterator_Specification
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Iteration_Scheme_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := Node (Iterator_Specification);
      Result_Node := Sinfo.Name (Result_Node);

      return Node_To_Element_New
               (Node             => Result_Node,
                Starting_Element => Iterator_Specification);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Iterator_Specification,
               Outer_Call =>
              Package_Name & "Iteration_Scheme_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Iteration_Scheme_Name",
            Ex          => Ex,
            Arg_Element => Iterator_Specification);
   end Iteration_Scheme_Name;

   ------------------------
   -- Subtype_Indication --
   ------------------------

   function Subtype_Indication
     (Iterator_Specification : Asis.Element)
      return                   Asis.Element
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Iterator_Specification);
      Result_Node : Node_Id;
   begin
      Check_Validity (Iterator_Specification,
        Package_Name & "Subtype_Indication");

      if Arg_Kind /= An_Element_Iterator_Specification then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Subtype_Indication",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := Node (Iterator_Specification);
      Result_Node := Sinfo.Subtype_Indication (Result_Node);

      if Present (Result_Node) then
         return Node_To_Element_New
                  (Node             => Result_Node,
                   Starting_Element => Iterator_Specification,
                   Internal_Kind    => A_Subtype_Indication);
      else
         return Nil_Element;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Iterator_Specification,
               Outer_Call =>
              Package_Name & "Subtype_Indication");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Subtype_Indication",
            Ex          => Ex,
            Arg_Element => Iterator_Specification);
   end Subtype_Indication;
   -------------------------------------------------------------------
   function Parameter_Profile
     (Declaration : Asis.Declaration)
      return        Asis.Parameter_Specification_List
   is
      Arg_Element      : constant Element := Declaration;
      Arg_Node         : Node_Id;
      Decl_Kind        : Internal_Element_Kinds;
      Result_List      : List_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Parameter_Profile");

      Decl_Kind := Int_Kind (Declaration);

      if not (Decl_Kind = A_Procedure_Declaration            or else
--  |A2005 start
              Decl_Kind = A_Null_Procedure_Declaration       or else
--  |A2005 end
--  |A2012 start
              Decl_Kind = An_Expression_Function_Declaration or else
--  |A2012 end
              Decl_Kind = A_Function_Declaration             or else
              Decl_Kind = A_Procedure_Body_Declaration       or else
              Decl_Kind = A_Function_Body_Declaration        or else
              Decl_Kind = A_Procedure_Renaming_Declaration   or else
              Decl_Kind = A_Function_Renaming_Declaration    or else
              Decl_Kind = An_Entry_Declaration               or else
              Decl_Kind = An_Entry_Body_Declaration          or else
              Decl_Kind = A_Procedure_Body_Stub              or else
              Decl_Kind = A_Function_Body_Stub               or else
              Decl_Kind = A_Generic_Procedure_Declaration    or else
              Decl_Kind = A_Generic_Function_Declaration     or else
              Decl_Kind = A_Formal_Procedure_Declaration     or else
              Decl_Kind = A_Formal_Function_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Parameter_Profile",
            Wrong_Kind => Decl_Kind);
      end if;

--  |A2012 start
      if Decl_Kind = An_Expression_Function_Declaration then
         Arg_Node := R_Node (Arg_Element);
--  |A2012 end
      else
         Arg_Node := Node (Arg_Element);
      end if;

      if Decl_Kind = A_Procedure_Declaration            or else
         Decl_Kind = A_Null_Procedure_Declaration       or else
         Decl_Kind = An_Expression_Function_Declaration or else
         Decl_Kind = A_Function_Declaration             or else
         Decl_Kind = A_Procedure_Body_Declaration       or else
         Decl_Kind = A_Function_Body_Declaration        or else
         Decl_Kind = A_Procedure_Renaming_Declaration   or else
         Decl_Kind = A_Function_Renaming_Declaration    or else
         Decl_Kind = A_Procedure_Body_Stub              or else
         Decl_Kind = A_Function_Body_Stub               or else
         Decl_Kind = A_Generic_Procedure_Declaration    or else
         Decl_Kind = A_Generic_Function_Declaration     or else
         Decl_Kind = A_Formal_Procedure_Declaration     or else
         Decl_Kind = A_Formal_Function_Declaration
      then

         Result_List := Parameter_Specifications (Specification (Arg_Node));

      elsif Decl_Kind = An_Entry_Declaration then
         Result_List := Parameter_Specifications (Arg_Node);

      else
         Result_List :=
            Parameter_Specifications (Entry_Body_Formal_Part (Arg_Node));
      end if;

      if No (Result_List) then
         return Nil_Element_List;

      else

         return N_To_E_List_New
                  (List              => Result_List,
                   Starting_Element  => Declaration,
                   Internal_Kind     => A_Parameter_Specification);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Parameter_Profile");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Parameter_Profile",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Parameter_Profile;
   -----------------------------------------------------------------------

   function Result_Profile
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Node    : Node_Id;
      Decl_Kind   : Internal_Element_Kinds;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin

      Check_Validity (Declaration, Package_Name & "Result_Profile");

      Decl_Kind := Int_Kind (Declaration);

      if not (Decl_Kind = A_Function_Declaration       or else
        Decl_Kind = A_Function_Body_Declaration        or else
        Decl_Kind = An_Expression_Function_Declaration or else
        Decl_Kind = A_Function_Renaming_Declaration    or else
        Decl_Kind = A_Function_Body_Stub               or else
        Decl_Kind = A_Generic_Function_Declaration     or else
        Decl_Kind = A_Formal_Function_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Result_Profile",
            Wrong_Kind => Decl_Kind);
      end if;

      Arg_Node    := Node (Declaration);
      Result_Node := Sinfo.Result_Definition (Specification (Arg_Node));

      if Nkind (Original_Node (Result_Node)) = N_Attribute_Reference
--  --|A2005 start
        or else
         Nkind (Original_Node (Result_Node)) = N_Access_Definition
--  --|A2005 end
      then
         null;
      elsif Nkind (Original_Node (Result_Node)) = N_Identifier then
         Result_Kind := An_Identifier;
      else
         Result_Kind := A_Selected_Component;
      end if;

      return Node_To_Element_New (
        Node             => Result_Node,
        Starting_Element => Declaration,
        Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Result_Profile");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Result_Profile",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Result_Profile;
   ----------------------------------------------------------------------
   function Body_Declarative_Items
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Declarative_Item_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;

   begin
      Check_Validity (Declaration,
        Package_Name & "Body_Declarative_Items");

      if not (Arg_Kind = A_Function_Body_Declaration  or else
        Arg_Kind = A_Procedure_Body_Declaration or else
        Arg_Kind = A_Package_Body_Declaration   or else
        Arg_Kind = A_Task_Body_Declaration      or else
        Arg_Kind = An_Entry_Body_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Body_Declarative_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return N_To_E_List_New
               (List             => Sinfo.Declarations (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Declaration);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Body_Declarative_Items",
            Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Body_Declarative_Items",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Body_Declarative_Items;
   ----------------------------------------------------------------------

   function Body_Statements
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;

      Resilt_List : List_Id := No_List;
   begin
      Check_Validity (Declaration, Package_Name & "Body_Statements");
      if not (Arg_Kind = A_Function_Body_Declaration  or else
              Arg_Kind = A_Procedure_Body_Declaration or else
              Arg_Kind = A_Package_Body_Declaration   or else
              Arg_Kind = A_Task_Body_Declaration      or else
              Arg_Kind = An_Entry_Body_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Body_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      if Present (Handled_Statement_Sequence (Arg_Node)) then
         Resilt_List := Statements (Handled_Statement_Sequence (Arg_Node));
      end if;

      return N_To_E_List_New
               (List             => Resilt_List,
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Declaration);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Body_Statements",
            Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Body_Statements",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Body_Statements;
   ---------------------------------------------------------------------
   function Body_Exception_Handlers
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Exception_Handler_List
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;

      Resilt_List : List_Id := No_List;

   begin
      Check_Validity (Declaration,
        Package_Name & "Body_Exception_Handlers");

      if not (Arg_Kind = A_Function_Body_Declaration  or else
              Arg_Kind = A_Procedure_Body_Declaration or else
              Arg_Kind = A_Package_Body_Declaration   or else
              Arg_Kind = A_Task_Body_Declaration      or else
              Arg_Kind = An_Entry_Body_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Body_Exception_Handlers",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      if Present (Handled_Statement_Sequence (Arg_Node)) then
         Resilt_List :=
            Exception_Handlers (Handled_Statement_Sequence (Arg_Node));
      end if;

      return N_To_E_List_New
               (List             => Resilt_List,
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Declaration);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Body_Exception_Handlers",
            Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Body_Exception_Handlers",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Body_Exception_Handlers;

   ------------------------------------------------------------------------
   --  |GNAT-ASIS: If the body passed as the actual have no declarative items
   --  |GNAT-ASIS  on its own, the result of the function will not test
   --  |GNAT-ASIS  Statements.Is_Declare_Block

   function Body_Block_Statement
     (Declaration : Asis.Declaration)
      return        Asis.Statement
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration,
        Package_Name & "Body_Block_Statement");
      if not (Arg_Kind = A_Function_Body_Declaration  or else
        Arg_Kind = A_Procedure_Body_Declaration or else
        Arg_Kind = A_Package_Body_Declaration   or else
        Arg_Kind = A_Task_Body_Declaration      or else
        Arg_Kind = An_Entry_Body_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Body_Block_Statement",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      --  the dummy block statement is created on the base on the
      --  argument's node; it requires the special processing to be
      --  performed by the block-related functions from Asis_Statements.

      return Node_To_Element_New (
        Node             => Arg_Node,
        Starting_Element => Declaration,
        Internal_Kind    => A_Block_Statement,
        Spec_Case        => A_Dummy_Block_Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Body_Block_Statement");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Body_Block_Statement",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Body_Block_Statement;
------------------------------------------------------------------------------
   function Is_Name_Repeated
     (Declaration : Asis.Declaration)
      return       Boolean
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Last_Comp : Asis.Element;
      S         : Source_Ptr;
      Result    : Boolean;

   begin
      Check_Validity (Declaration, Package_Name & "Is_Name_Repeated");

      if not (Arg_Kind = A_Package_Declaration          or else
              Arg_Kind = A_Package_Body_Declaration     or else
              Arg_Kind = A_Procedure_Body_Declaration   or else
              Arg_Kind = A_Function_Body_Declaration    or else
              Arg_Kind = A_Generic_Package_Declaration  or else
              Arg_Kind = A_Task_Type_Declaration        or else
              Arg_Kind = A_Single_Task_Declaration      or else
              Arg_Kind = A_Task_Body_Declaration        or else
              Arg_Kind = A_Protected_Type_Declaration   or else
              Arg_Kind = A_Single_Protected_Declaration or else
              Arg_Kind = A_Protected_Body_Declaration   or else
              Arg_Kind = An_Entry_Body_Declaration)
      then
         return False;
      end if;

      Reset_Tree_For_Element (Declaration);

      if Arg_Kind = A_Task_Type_Declaration        or else
         Arg_Kind = A_Single_Task_Declaration      or else
         Arg_Kind = A_Protected_Type_Declaration   or else
         Arg_Kind = A_Single_Protected_Declaration
      then
         --  this situation differs from the rest of the appropriate element
         --  kinds, because here we have to analyze type or object declaration
         --  view as the last component, and the potentially repeated name is
         --  a part of this component:
         if Arg_Kind = A_Task_Type_Declaration        or else
            Arg_Kind = A_Protected_Type_Declaration
         then
            Last_Comp := Type_Declaration_View (Declaration);
         else
            Last_Comp := Object_Declaration_View (Declaration);
         end if;

         if Is_Nil (Last_Comp) then
            --  this is the case for a single task or task type declaration
            --  without any entry declaration. such a declaration does not
            --  contain a "is ... end" part in its structure, therefore there
            --  is no "end" for a name to repeat after:
            Result := False;
         else
            --  here we have the end after which the name may be repeated in
            --  the very end of Last_Comp:
            S := Set_Image_End (Last_Comp);

            if not (Get_Character (S) = 'd' or else
                    Get_Character (S) = 'D')
            then
               Result := True;
            elsif not (Get_Character (S - 1) = 'n' or else
                       Get_Character (S - 1) = 'N')
            then
               Result := True;
            elsif not (Get_Character (S - 2) = 'e' or else
                       Get_Character (S - 2) = 'E')
            then
               Result := True;
            elsif not (Get_Character (S - 3) = ' ') then
               Result := True;
            else
               Result := False;
            end if;

         end if;
      else
         --  here we are in the situation when the "end" to repeat the name
         --  after is not a part of any subcomponent of Declaration
         Last_Comp := Get_Last_Component (Declaration);
         S := Set_Image_End (Last_Comp);
         S := S + 1;
         S := Rightmost_Non_Blank (S);

         --  there may be a pathological case of a package or protected spec
         --  with no declaration inside, moreover, in such a case a protected
         --  type declaration may or may not contain a discriminant part

         if Get_Character (S) = 'i' or else Get_Character (S) = 'I' then
            --  the reserved word IS is the only possibility here, therefore:
            S := S + 2;
            S := Rightmost_Non_Blank (S);
         end if;

         --  here we have two possibilities S can put on the beginning of
         --  "end" or "private"

         if Get_Character (S) = 'P' or else Get_Character (S) = 'p' then
            --  skipping "private"
            S := S + 7;
            S := Rightmost_Non_Blank (S);
         end if;

         S := S + 3;
         --  the first character after "end"
         S := Rightmost_Non_Blank (S);
         --  and the final check - what follows the final "end"

         if Get_Character (S) = ';' then
            Result := False;
         else
            Result := True;
         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call =>
              Package_Name & "Is_Name_Repeated");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Name_Repeated",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Is_Name_Repeated;
------------------------------------------------------------------------------
   function Corresponding_Declaration
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Inherited : constant Boolean := Is_From_Inherited (Declaration);

      Arg_Node         : Node_Id;
      Result_Node      : Node_Id;
      Result_Kind      : Internal_Element_Kinds := Not_An_Element;
      Result_Spec_Case : Special_Cases := Not_A_Special_Case;
      Argument_Unit    : Asis.Compilation_Unit;
      Result_Unit      : Asis.Compilation_Unit;

      Result_Element   : Asis.Element := Nil_Element;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Declaration");

      case Arg_Kind is
         --  Appropriate Declaration_Kinds returning the argument Declaration:
         when  A_Generic_Function_Declaration           |
               A_Generic_Package_Declaration            |
               A_Generic_Procedure_Declaration          |
               A_Package_Declaration                    |
               A_Package_Renaming_Declaration           |
               A_Single_Task_Declaration                |
               A_Task_Type_Declaration                  |
               A_Protected_Type_Declaration             |
               A_Single_Protected_Declaration           |
               A_Generic_Package_Renaming_Declaration   |
               A_Generic_Procedure_Renaming_Declaration |
               A_Generic_Function_Renaming_Declaration  =>

            Result_Element := Declaration;

         --  Appropriate Declaration_Kinds returning a specification:
         when  A_Function_Body_Declaration              |
               An_Expression_Function_Declaration       |
               A_Function_Body_Stub                     |
               A_Function_Instantiation                 |
               A_Package_Body_Declaration               |
               A_Package_Body_Stub                      |
               A_Package_Instantiation                  |
               A_Procedure_Body_Declaration             |
               A_Procedure_Body_Stub                    |
               A_Procedure_Instantiation                |
               A_Task_Body_Declaration                  |
               A_Task_Body_Stub                         |
               A_Protected_Body_Declaration             |
               A_Protected_Body_Stub                    |
               A_Formal_Package_Declaration             |
               A_Formal_Package_Declaration_With_Box    |
               An_Entry_Body_Declaration                =>

            --  Some specific processing is needed, so we do not change
            --  Result_Element from its null value
            null;

         when  A_Function_Renaming_Declaration  |
               A_Procedure_Renaming_Declaration =>

            --  Here we have to make the difference between renaming-as-body
            --  and renaming-as-declaration

            if not Is_Renaming_As_Body (Declaration) then
               --  The argument declaration should be returned for
               --   renaming-as-declaration
               Result_Element := Declaration;
            end if;

         when A_Function_Declaration        |
               A_Procedure_Declaration      |
               A_Null_Procedure_Declaration =>
            --  Here we have to make the difference between inherited and
            --  non-inherited subprograms

            if not Inherited then
               Result_Element := Declaration;
            end if;

         when others =>
            --  Definitely non-appropriate argument
            Raise_ASIS_Inappropriate_Element
              (Package_Name & "Corresponding_Declaration",
               Wrong_Kind => Arg_Kind);
      end case;

      --  and here we have to (try to) return the corresponding spec:
      --
      --  There are the following cases:
      --
      --  1. A body which is the library_item of some compilation
      --     unit;
      --  2. Bodies declared immediately within a library package/library
      --     generic package, but it does not make any problem for this
      --     query, because the tree containing a package body also
      --     contains the package spec;
      --  3. Generic instantiations;
      --  3.1 Formal package declarations (this is ASIS-for-GNAT-specific and
      --      we believe, that in fact this is a fix of the hole in the
      --      ASIS definition
      --  4. making the difference between renaming-as-declaration and
      --     renaming-as-body (the problem does not exist together with
      --     the case No.1);
      --  5. body stubs
      --  7. declarations of implicit inherited subprograms
      --  6. The "common" case;
      --  8. ???

      if Is_Nil (Result_Element) and then
         not Acts_As_Spec (Declaration)
      then

         if Arg_Kind = A_Function_Instantiation  then
            Result_Kind      := A_Function_Declaration;
            Result_Spec_Case := Expanded_Subprogram_Instantiation;
         elsif Arg_Kind = A_Procedure_Instantiation then
            Result_Kind      := A_Procedure_Declaration;
            Result_Spec_Case := Expanded_Subprogram_Instantiation;
         elsif Arg_Kind = A_Package_Instantiation      or else
               Arg_Kind = A_Formal_Package_Declaration or else
               Arg_Kind = A_Formal_Package_Declaration_With_Box
         then
            Result_Kind      := A_Package_Declaration;
            Result_Spec_Case := Expanded_Package_Instantiation;
         end if;

         --  first, we compute the result enclosing unit, and we return
         --  Nil_Element if it is Nil_Compilation_Unit or if it is
         --  of Nonexistent kind:

         Argument_Unit := Get_Comp_Unit
           (Encl_Unit_Id (Declaration), Encl_Cont_Id (Declaration));

         if Is_Identical (Declaration, Unit_Declaration (Argument_Unit)) then

            --  A body which is the library_item of some compilation unit
            if Class (Argument_Unit) = A_Public_Declaration_And_Body or else
              Kind (Argument_Unit) in A_Subunit
            then
               Result_Element := Nil_Element;
               --  This assignment does not change anything, we use it just to
               --  simplify the revising of the old code of the function
            elsif not (Arg_Kind = A_Function_Instantiation      or else
                       Arg_Kind = A_Package_Instantiation       or else
                       Arg_Kind = A_Procedure_Instantiation     or else
                       Arg_Kind = A_Formal_Package_Declaration  or else
                       Arg_Kind = A_Formal_Package_Declaration_With_Box)
            then
               Result_Unit := Corresponding_Declaration (Argument_Unit);

               if Exists (Result_Unit) then
                  Result_Element := Unit_Declaration (Result_Unit);
               end if;

            else
               --  here we have to return the expanded generic specification:
               Result_Node := Unit (Top (Argument_Unit));
               --  note, that in case of a library-level generic instantiation,
               --  this is a rewritten node!

               if Arg_Kind = A_Function_Instantiation or else
                  Arg_Kind = A_Procedure_Instantiation
               then
                  --  the difference between the library-level package
                  --  instantiation and the library-level function or procedure
                  --  instantiation is that in the latter case the compiler
                  --  creates the artificial enclosing package:
                  --  Note, that this path may not work correctly because of
                  --  the bug in the tree structure for library-level
                  --  subprogram  instantiations

                  if Nkind (Result_Node) = N_Package_Body then
                     --  the tree created for a compilation unit which is a
                     --  library-level subprogram instantiation
                     Result_Node := Last (Sinfo.Declarations (Result_Node));

                     --  This gave us the expanded body, so
                     Result_Node :=
                        Parent (Parent (Corresponding_Spec (Result_Node)));

                  else
                     Result_Node := Last
                        (Visible_Declarations (Specification (Result_Node)));
                  end if;

               else
                  Reset_Instance_Tree (Argument_Unit, Result_Node);

                  --  In case of a package instantiations, the node is
                  --  rewritten in package body node in case if the generic
                  --  unit has a body
                  if Nkind (Result_Node) = N_Package_Body then
                     Result_Node := Corresponding_Spec (Result_Node);
                     Result_Node := Parent (Result_Node);

                     if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
                        Result_Node := Parent (Result_Node);
                     end if;

                     Result_Node := Parent (Result_Node);
                  end if;

               end if;

               Result_Element :=
                 Node_To_Element_New
                   (Node               => Result_Node,
                   Internal_Kind       => Result_Kind,
                   Spec_Case           => Result_Spec_Case,
                   Using_Original_Node => False,
                   Starting_Element    => Declaration);
            end if;

         else

            --  and now - processing the general case, in which we separate the
            --  following situations:
            --  - generic instantiations - the expanded template should be
            --    returned
            --  - a declaration of an implicit inherited subprogram - the
            --    corresponding explicit user-defined subprogram should be
            --    returned;
            --  - all the rest

            Arg_Node := R_Node (Declaration);
            --  this resets the tree if needed!

            if Arg_Kind = A_Function_Instantiation     or else
               Arg_Kind = A_Procedure_Instantiation    or else
               Arg_Kind = A_Package_Instantiation      or else
               Arg_Kind = A_Formal_Package_Declaration or else
               Arg_Kind = A_Formal_Package_Declaration_With_Box
            then
               Result_Node := Get_Expanded_Spec (Arg_Node);
               Result_Unit := Argument_Unit;
            elsif Inherited then
               --  coming from the declaration of implicit inherited subprogram
               --  to the corresponding explicit declaration of user-defined
               --  subprogram, from which it was ultimately inherited
               Result_Node := Arg_Node;

               Result_Unit :=
                  Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);
            else
               --  and, finally, the general case
               Result_Node := Corresponding_Decl_Node (Arg_Node);
               --  Note, that when called here, Corresponding_Decl_Node should
               --  return some non-Empty node.
               --  and here we should check the situation when the
               --  corresponding declaration is located in another
               --  Compilation Unit.

               if Is_From_Instance (Declaration) then
                  Result_Unit := Argument_Unit;
               else
                  Result_Unit :=
                     Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);
               end if;

            end if;

            if Special_Case (Declaration) in Expanded_Spec then
               Result_Spec_Case := Special_Case (Declaration);
            end if;

            Result_Element :=
              Node_To_Element_New
                (Node          => Result_Node,
                 Internal_Kind => Result_Kind,
                 Spec_Case     => Result_Spec_Case,
                 In_Unit       => Result_Unit);

            if Is_From_Implicit (Result_Element) then
               --  See FA01-004. This patch is for the situation when a parent
               --  subprogram is defined by a subprogram instantiation. This is
               --  the only case when for Result_Node we have Comes_From_Source
               --  set to False. So we just fix Result_Element:

               Set_From_Implicit (Result_Element, False);
               Set_Special_Case
                 (Result_Element, Expanded_Subprogram_Instantiation);
            end if;

         end if;

      end if;

      return Result_Element;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Declaration",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Declaration;
-----------------------------------------------------------------------------
   --  NOT IMPLEMENTED

   function Corresponding_Declaration
     (Declaration : Asis.Declaration;
      The_Context : Asis.Context)
      return        Asis.Declaration
   is
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Declaration");
      Check_Validity (The_Context,
        Package_Name & "Corresponding_Declaration");

      Not_Implemented_Yet (Diagnosis =>
        Package_Name & "Corresponding_Declaration");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Declaration",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Declaration;
------------------------------------------------------------------------------
   function Corresponding_Body
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind          : Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node          : Node_Id;
      Arg_Element       : Asis.Declaration := Declaration;
      Result_Node       : Node_Id := Empty;
      Result_Kind       : Internal_Element_Kinds := Not_An_Element;
      Result_Spec_Case  : Special_Cases := Not_A_Special_Case;
      Argument_Unit     : Asis.Compilation_Unit;
      Result_Unit       : Asis.Compilation_Unit;

      Use_Original_Node : Boolean := True;
   begin

      Check_Validity (Declaration, Package_Name & "Corresponding_Body");

      case Arg_Kind is
         --  Appropriate Declaration_Kinds returning the argument Declaration:
         when A_Function_Body_Declaration              |
              A_Function_Body_Stub                     |
              An_Expression_Function_Declaration       |
              A_Function_Renaming_Declaration          |
              A_Package_Body_Declaration               |
              A_Package_Body_Stub                      |
              A_Package_Renaming_Declaration           |
              A_Procedure_Body_Declaration             |
              A_Procedure_Renaming_Declaration         |
              A_Procedure_Body_Stub                    |
              A_Task_Body_Declaration                  |
              A_Task_Body_Stub                         |
              A_Protected_Body_Declaration             |
              A_Protected_Body_Stub                    |
              A_Generic_Package_Renaming_Declaration   |
              A_Generic_Procedure_Renaming_Declaration |
              A_Generic_Function_Renaming_Declaration  |
              An_Entry_Body_Declaration                =>

            return Declaration;

         --  Appropriate Declaration_Kinds returning a body:
         when A_Function_Declaration                |
              A_Function_Instantiation              |
              A_Generic_Package_Declaration         |
              A_Generic_Procedure_Declaration       |
              A_Generic_Function_Declaration        |
              A_Package_Declaration                 |
              A_Package_Instantiation               |
              A_Procedure_Declaration               |
              A_Procedure_Instantiation             |
              A_Single_Task_Declaration             |
              A_Task_Type_Declaration               |
              A_Protected_Type_Declaration          |
              A_Single_Protected_Declaration        |
              A_Formal_Package_Declaration          |
              A_Formal_Package_Declaration_With_Box =>

            null;

         when An_Entry_Declaration =>
            --  This query can work on protected entries only

            if Int_Kind (Enclosing_Element (Declaration)) /=
               A_Protected_Definition
            then
               Raise_ASIS_Inappropriate_Element
                 (Package_Name & "Corresponding_Body",
                  Wrong_Kind => Arg_Kind);
            end if;

         when others =>
            Raise_ASIS_Inappropriate_Element
              (Package_Name & "Corresponding_Body",
               Wrong_Kind => Arg_Kind);
      end case;

      if Trait_Kind (Declaration) = An_Abstract_Trait
        or else
         Is_Implicit_Neq_Declaration (Declaration)
      then
         return Nil_Element;
      end if;

      if (Arg_Kind = A_Function_Declaration or else
          Arg_Kind = A_Procedure_Declaration)
         and then
          Is_From_Inherited (Declaration)
      then
         --  for an implicit inherited subprogram ASIS Corresponding_Body is
         --  a body of an explicit subprogram from which it is inherited
         --  (possibly indirectly)
         Arg_Element := Corresponding_Declaration (Declaration);

         case Declaration_Kind (Arg_Element) is
            when A_Procedure_Body_Declaration     |
                 A_Function_Body_Declaration      |
                 A_Procedure_Renaming_Declaration |
                 A_Function_Renaming_Declaration  =>
               return Arg_Element;
            when A_Procedure_Declaration |
                 A_Function_Declaration  =>
               null;
            when others =>
               return Nil_Element;
         end case;

      elsif Arg_Kind in An_Internal_Generic_Instantiation
         or else
            Arg_Kind in A_Formal_Package_Declaration ..
            A_Formal_Package_Declaration_With_Box
      then
         --  for an implicit inherited subprogram ASIS Corresponding_Body is
         --  a body of an explicit subprogram from which it is inherited
         --  (possibly indirectly)
         Arg_Element := Corresponding_Declaration (Declaration);

         if Is_Nil (Arg_Element) or else
            Int_Kind (Arg_Element) in A_Procedure_Renaming_Declaration ..
                                      A_Function_Renaming_Declaration
         then
            --  This stands here because implicit constructs are supported
            --  only partially by ASIS (???)
            --  The second condition is needed for subprograms inherited
            --  from explicit renamings, see 8728-A23
            return Nil_Element;
         end if;

      else
         Arg_Element := Declaration;
         --  in case if Declaration is in a library package spec and the
         --  package body is not in the enclosing element's tree, Arg_Element
         --  should be reset to Is_Equial Element obtained from the tree
         --  for the package body, but we are trying to do this expensive
         --  operation only when it is really necessary.
      end if;

      Arg_Kind := Int_Kind (Arg_Element);

      --  and for these cases we return the corresponding body, if any:
      --
      --    A_Function_Declaration
      --    A_Generic_Package_Declaration
      --    A_Generic_Procedure_Declaration
      --    A_Generic_Function_Declaration
      --    A_Package_Declaration
      --    A_Procedure_Declaration
      --    A_Single_Task_Declaration
      --    A_Task_Type_Declaration
      --    A_Protected_Type_Declaration
      --    A_Single_Protected_Declaration
      --    A_Function_Instantiation
      --    A_Package_Instantiation
      --    A_Procedure_Instantiation
      --
      --  There are following cases which require special processing:
      --  1. Program units declared within a library package or a library
      --     generic package (directly or within a nested package)- we have
      --     to use the tree containing the corresponding library unit body,
      --     if any, otherwise Corresponding_Body will be Nil_Element;
      --  2. renaming-as-body: in this case in the tree the Corresponding_Body
      --     field set for nodes representing declarations which can (must)
      --     have bodies as completions points not to the corresponding
      --     renaming declaration, but to the body implicitly created by the
      --     compiler;
      --  3. Library-level program units
      --  4. Expanded generic bodies
      --  5. Pragma Import as a subprogram completion

      --  First, deal with the case of pragma Import:

      if (Int_Kind (Declaration) in
           A_Procedure_Declaration .. A_Function_Declaration
         or else
          Int_Kind (Declaration) in
           A_Generic_Procedure_Declaration .. A_Generic_Function_Declaration)
        and then
          not Is_Part_Of_Inherited (Declaration)
      then
         Arg_Node := R_Node (Declaration);
         Arg_Node := Defining_Unit_Name (Specification (Arg_Node));

         if Nkind (Arg_Node) = N_Defining_Program_Unit_Name then
            Arg_Node := Defining_Identifier (Arg_Node);
         end if;

         if Is_Imported (Arg_Node) then

            Result_Node := Get_Importing_Pragma (Arg_Node);

            pragma Assert (Present (Result_Node));

            return Node_To_Element_New
              (Node             => Result_Node,
               Starting_Element => Arg_Element);

         end if;

      end if;

      Argument_Unit := Get_Comp_Unit
        (Encl_Unit_Id (Arg_Element), Encl_Cont_Id (Arg_Element));

      if Is_From_Instance (Arg_Element) then
         Result_Unit := Argument_Unit;
      else
         Result_Unit := Corresponding_Body (Argument_Unit);

         --  Note, that this is not a correct enclosing unit for a result
         --  in case for a subprogram declared in a local package for which
         --  the package body is presented by the stub. So we'll have to
         --  recompute Result_Unit before forming a result.

         if Is_Identical (Arg_Element, Unit_Declaration (Argument_Unit)) then

            if not Exists (Result_Unit) then
               return Nil_Element;
               --  what else could we do?
            else
               return Unit_Declaration (Result_Unit);
            end if;

         end if;

      end if;

      Arg_Node    := R_Node (Arg_Element);
      --  this resets the tree!
      --  we use R_Node, because a single task declaration is rewritten
      --  in a task type declaration, and the Corresponding_Body field
      --  is not set for the original node

      --  if we are here, we have Arg_Element as some declaration inside the
      --  spec of a library package or library generic package (may be, nested
      --  in a local subpackage). There are two possibilities for a
      --  corresponding body (which is to be a completion of Arg_Element):
      --  renaming as a body in the library (generic) package spec or
      --  some construct in the corresponding library package body.

      --  For the second possibility, we have to be in the tree created for
      --  a package body (if any) to be able to compute the corresponding
      --  body for Arg_Element. But if Arg_Element is obtained from the tree
      --  containing only spec for this package, we have to recompute
      --  Arg_Element from the tree for the package body. It is expansive,
      --  and we would like to avoid this if possible.

      if Arg_Kind = A_Function_Declaration or else
         Arg_Kind = A_Procedure_Declaration
      then
         Result_Node := Get_Renaming_As_Body (Node      => Arg_Node,
                                              Spec_Only => True);
         if Present (Result_Node) then
            --  this means, that the subprogram declaration is completed
            --  by renaming-as-body already in the package spec, and we can
            --  return the non-nil result without dealing with the body
            --  of this package
            if Arg_Kind = A_Function_Declaration then
               Result_Kind := A_Function_Renaming_Declaration;
            else
               Result_Kind := A_Procedure_Renaming_Declaration;
            end if;

            return Node_To_Element_New
              (Node             => Result_Node,
               Internal_Kind    => Result_Kind,
               Starting_Element => Arg_Element);

         end if;
      end if;

      --  if we are here, Arg_Element is not completed in the spec of the
      --  library (generic) package.

      if not Exists (Result_Unit) then
         return Nil_Element;
      end if;

      Result_Node := Corresponding_Body_Node (Arg_Node);

      if No (Result_Node) then
         --  There are two possibilities when the corresponding body may
         --  exist:
         --
         --  1. A subprogram completed by pragma Import
         --
         --  2. Arg_Element represent a declaration which is located
         --     in a library (generic) package spec (and Arg_Element
         --     is not from expanded generic!). The corresponding body
         --     may be located in a library package body, which may not
         --     be presented in the tree which is currently accessed
         --  3. Arg_Element represent a declaration which is from expanded
         --     generic spec corresponding to a library-level instantiation.
         --     in this case, if the construct is completed in an expanded
         --     body, we need the tree for expanded body, and this tree
         --     is presented only in the tree created for such a library-level
         --     instantiation.

         if Arg_Kind in A_Procedure_Declaration .. A_Function_Declaration
           and then
            Special_Case (Arg_Element) not in Expanded_Spec
         then
            Result_Node := Defining_Unit_Name (Specification (Arg_Node));

            Result_Node := First_Rep_Item (Result_Node);

            while Present (Result_Node) loop
               if Nkind (Result_Node) = N_Pragma
                and then
                  Chars (Pragma_Identifier (Result_Node)) = Name_Import
               then
                  exit;
               end if;

               Result_Node := Next_Rep_Item (Result_Node);
            end loop;

            if Nkind (Result_Node) = N_Pragma
             and then
               Chars (Pragma_Identifier (Result_Node)) = Name_Import
            then
               Result_Kind := An_Import_Pragma;
            else
               Result_Node := Empty;
            end if;

         end if;

         if No (Result_Node) then

            case Kind (Argument_Unit) is

               when A_Package |
                    A_Generic_Package =>

                  --  ??? This case statement should definitely be simplified
                  --  ??? by unifying the actions needed to reset the tree in a
                  --  ??? way to get the tree containing the body in question

                  if not Is_From_Instance (Arg_Element) then
                     Reset_For_Body (Arg_Element, Result_Unit);
                     --  ??? Should be replaced by less expensive
                     --  GNAT tree traversing
                     Arg_Node := R_Node (Arg_Element);
                  else
                     Reset_Instance_Tree (Argument_Unit, Arg_Node);
                     --  Should we check here if Arg_Node is already in some
                     --  package body? There is a check in Reset_Instance_Tree,
                     --  but is it enough?
                  end if;

               when A_Generic_Unit_Instance |
                    A_Library_Unit_Body =>
                  --  Here we are sure, that we are working not with the
                  --  instantiation itself, but with some spec from expanded
                  --  generic. This expanded generic spec may be in some body
                  --  unit, and if this body unit is not compiled on its own,
                  --  the needed spec -> body link is missed for the expanded
                  --  generics, so we have to go to the main tree for the body
                  --   (see 9327-007)

                  if Is_From_Instance (Arg_Node)
                    or else
                     Arg_Kind in An_Internal_Generic_Instantiation
                    or else
                     Special_Case (Arg_Element) in Expanded_Spec
                  then
                     Reset_Instance_Tree (Argument_Unit, Arg_Node);
                  end if;

               when others =>
                  null;
            end case;

            Result_Node := Corresponding_Body_Node (Arg_Node);
         end if;
      end if;

      if Special_Case (Arg_Element) in Expanded_Spec then
         Result_Spec_Case := Special_Case (Arg_Element);
         Use_Original_Node := False;
      end if;

      if Present (Result_Node) then
         Result_Unit :=
           Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

         return Node_To_Element_New
           (Node                => Result_Node,
            Internal_Kind       => Result_Kind,
            Spec_Case           => Result_Spec_Case,
            Using_Original_Node => Use_Original_Node,
            In_Unit             => Result_Unit);

      else
         return Nil_Element;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Body");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Body",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Body;
------------------------------------------------------------------------------
   --  NOT IMPLEMENTED

   function Corresponding_Body
     (Declaration : Asis.Declaration;
      The_Context : Asis.Context)
      return        Asis.Declaration
   is
   begin
      Check_Validity (Declaration, Package_Name & "Corresponding_Body");
      Check_Validity (The_Context,     Package_Name & "Corresponding_Body");

      Not_Implemented_Yet (Diagnosis =>
        Package_Name & "Corresponding_Body");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Body");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Body",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Body;
------------------------------------------------------------------------------
   function Corresponding_Subprogram_Derivation
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Result_Unit : Asis.Compilation_Unit;
      Result_Node : Node_Id;
      Subpr_Node  : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
      Inherited   : Boolean := False;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Subprogram_Derivation");

      if not ((Arg_Kind = A_Function_Declaration or else
               Arg_Kind = A_Procedure_Declaration)
        and then
               Is_From_Inherited (Declaration))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Subprogram_Derivation",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := Alias (Node_Field_1 (Declaration));

      --  and now - up the tree to the declaration node if needed
      if No (Alias (Result_Node)) then
         Result_Node := Parent (Result_Node);

         if Nkind (Result_Node) = N_Procedure_Specification or else
           Nkind (Result_Node) = N_Function_Specification
         then
            Result_Node := Parent (Result_Node);
         end if;

      else
         Inherited := True;

         Subpr_Node  := Result_Node;
         Result_Node := Node (Declaration);

         if Arg_Kind = A_Function_Declaration then
            Result_Kind := A_Function_Declaration;
         else
            Result_Kind := A_Procedure_Declaration;
         end if;

      end if;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New
        (Node          => Result_Node,
         Node_Field_1  => Subpr_Node,
         Internal_Kind => Result_Kind,
         Inherited     => Inherited,
         In_Unit       => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call =>
              Package_Name & "Corresponding_Subprogram_Derivation");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Corresponding_Subprogram_Derivation",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Subprogram_Derivation;
------------------------------------------------------------------------------
   function Corresponding_Type
     (Declaration : Asis.Declaration)
      return        Asis.Type_Definition
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity (Declaration, Package_Name & "Corresponding_Type");

      if not ((Arg_Kind = A_Function_Declaration  or else
               Arg_Kind = A_Procedure_Declaration)
            and then
               Is_From_Implicit (Declaration))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Type",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := Parent (Node_Field_1 (Declaration));

      if Nkind (Result_Node) = N_Private_Extension_Declaration then
         Result_Kind := A_Private_Extension_Definition;
      else
         Result_Node := Sinfo.Type_Definition (Result_Node);
      end if;

      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New (Node          => Result_Node,
                                  Internal_Kind => Result_Kind,
                                  In_Unit       => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Type");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Type",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Type;
------------------------------------------------------------------------------

   function Corresponding_Equality_Operator
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind     : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Result       :          Asis.Declaration       := Nil_Element;
      Res_Node     : Node_Id;
      Res_NF_1     : Node_Id := Empty;
      Is_Exp_Spb   : Boolean := False;
      Is_Inherited : Boolean := False;
      Is_From_Inst : Boolean := False;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Equality_Operator");

      if not (Arg_Kind = A_Function_Declaration      or else
              Arg_Kind = A_Function_Body_Declaration or else
              Arg_Kind = A_Function_Renaming_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Equality_Operator",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_Bool_Eq_Declaration (Declaration) then

         if Is_Part_Of_Inherited (Declaration) then
            Res_Node := Node_Field_1 (Declaration);
         else
            Res_Node :=
              Defining_Unit_Name (Specification (Node (Declaration)));
         end if;

         if Is_Generic_Instance (Res_Node) then
            --  If "=" is defined by a generic instantiation, we have to go
            --  to the enclosing scope - where the instantiation itself takes
            --  place
            Res_Node := Defining_Unit_Name (Parent (Node (Declaration)));
         end if;

         while not (Nkind (Res_Node) = N_Defining_Operator_Symbol
                 and then
                    Chars (Res_Node) = Name_Op_Ne)
         loop
            Res_Node := Next_Entity (Res_Node);
         end loop;

         Res_Node := Parent (Parent (Res_Node));

         Result := Node_To_Element_New
                        (Node             => Res_Node,
                         Spec_Case        => Is_From_Imp_Neq_Declaration,
                         In_Unit          => Encl_Unit (Declaration));

         Set_From_Implicit (Result, True);

         --  In no case the result can be inherited:
         Set_From_Inherited (Result, False);

      elsif Is_Implicit_Neq_Declaration (Declaration) then
         Res_Node := Defining_Unit_Name (Specification (Node (Declaration)));
         Res_Node := Corresponding_Equality (Res_Node);

         if Nkind (Original_Node (Parent (Res_Node))) in
              N_Formal_Type_Declaration .. N_Private_Type_Declaration
         then
            Res_NF_1 := Res_Node;

            Res_Node := Alias (Res_Node);

            while Present (Alias (Res_Node))
                and then
                  not Comes_From_Source (Res_Node)
            loop
               Res_Node := Alias (Res_Node);
            end loop;

            Is_Inherited := True;
            Is_From_Inst := Is_From_Instance (Res_NF_1);
         end if;

         if Is_Generic_Instance (Res_Node) then
            Is_Exp_Spb := True;
         end if;

         Res_Node := Parent (Parent (Res_Node));

         Result := Node_To_Element_New
                        (Node             => Res_Node,
                         Node_Field_1     => Res_NF_1,
                         In_Unit          => Encl_Unit (Declaration));

         if not (Is_From_Inherited (Declaration)) then
            Set_From_Implicit (Result, False);
         end if;

         if Is_Exp_Spb then
            Set_From_Instance (Result, True);
            Set_Special_Case   (Result, Expanded_Subprogram_Instantiation);
         else
            Set_Special_Case (Result, Not_A_Special_Case);
         end if;

         if Is_Inherited then
            Set_From_Implicit  (Result, True);
            Set_From_Inherited (Result, True);
            Set_From_Instance  (Result, Is_From_Inst);
         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Equality_Operator");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Equality_Operator",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Equality_Operator;
-----------------------------------------------------------------------------

   function Visible_Part_Declarative_Items
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Declarative_Item_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration,
        Package_Name & "Visible_Part_Declarative_Items");
      if not (Arg_Kind = A_Package_Declaration or else
        Arg_Kind = A_Generic_Package_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Visible_Part_Declarative_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := R_Node (Declaration);

      if Arg_Node = Standard_Package_Node then
         return
           N_To_E_List_New
              (List             =>
                 Visible_Declarations (Specification (Arg_Node)),
               Include_Pragmas  => Include_Pragmas,
               Starting_Element => Declaration)
            &
               Get_Numeric_Error_Renaming;

      else

         declare
            Result : constant Element_List :=
              N_To_E_List_New
                (List             =>
                   Visible_Declarations (Specification (Arg_Node)),
                 Include_Pragmas  => Include_Pragmas,
                 Starting_Element => Declaration);
            First_Idx         : constant List_Index   := Result'First;
            Last_Idx          : constant ASIS_Integer := Result'Last;
            Last_Limited_View :          ASIS_Integer := First_Idx - 1;

            Limited_Views : Element_List (First_Idx .. Last_Idx);
         begin
            if Is_From_Limited_View (Declaration) then
               for J in Result'Range loop
                  if Belongs_To_Limited_View (Result (J)) then
                     Last_Limited_View := Last_Limited_View + 1;
                     Limited_Views (Last_Limited_View) := Result (J);
                     Convert_To_Limited_View
                       (Limited_Views (Last_Limited_View));
                  end if;
               end loop;

               return Limited_Views (First_Idx .. Last_Limited_View);
            else
               return Result;
            end if;
         end;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Visible_Part_Declarative_Items",
               Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Visible_Part_Declarative_Items",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Visible_Part_Declarative_Items;
------------------------------------------------------------------------------

   function Is_Private_Present
     (Declaration : Asis.Declaration)
      return        Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Private_Present");

      if not (Arg_Kind = A_Package_Declaration or else
              Arg_Kind = A_Generic_Package_Declaration)
        or else
         Is_From_Limited_View (Declaration)
      then
         return False;
      end if;

      Arg_Node := Node (Declaration);

      return Present (Private_Declarations (Specification (Arg_Node)));
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Is_Private_Present");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Private_Present",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Is_Private_Present;
------------------------------------------------------------------------------

   function Private_Part_Declarative_Items
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Declarative_Item_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration,
        Package_Name & "Private_Part_Declarative_Items");

      if not (Arg_Kind = A_Package_Declaration or else
        Arg_Kind = A_Generic_Package_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           ("Private_Part_Declarative_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_From_Limited_View (Declaration) then
         return Nil_Element_List;
      end if;

      Arg_Node := R_Node (Declaration);

      return N_To_E_List_New
        (List             => Private_Declarations (Specification (Arg_Node)),
        Include_Pragmas  => Include_Pragmas,
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Private_Part_Declarative_Items",
               Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Private_Part_Declarative_Items",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Private_Part_Declarative_Items;
------------------------------------------------------------------------------
   function Renamed_Entity
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;

      Result : Asis.Expression;
   begin
      Check_Validity (Declaration, Package_Name & "Renamed_Entity");
      if not (Arg_Kind = An_Object_Renaming_Declaration           or else
        Arg_Kind = An_Exception_Renaming_Declaration        or else
        Arg_Kind = A_Package_Renaming_Declaration           or else
        Arg_Kind = A_Procedure_Renaming_Declaration         or else
        Arg_Kind = A_Function_Renaming_Declaration          or else
        Arg_Kind = A_Generic_Package_Renaming_Declaration   or else
        Arg_Kind = A_Generic_Procedure_Renaming_Declaration or else
        Arg_Kind = A_Generic_Function_Renaming_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Renamed_Entity",
            Wrong_Kind => Arg_Kind);
      end if;

      if Special_Case (Declaration) = Numeric_Error_Renaming then
         Result := Declaration;
         Set_Int_Kind (Result, An_Identifier);
      else
         Arg_Node := Node (Declaration);
         Result_Node := Sinfo.Name (Arg_Node);

         Result := Node_To_Element_New
                     (Node             => Result_Node,
                      Starting_Element => Declaration);

      end if;

      --  In case if we have a subprogram renaming, and the renamed entity is
      --  an attribute reference, we have to correct the result kind, see
      --  FA26-004

      if Arg_Kind in
           A_Procedure_Renaming_Declaration .. A_Function_Renaming_Declaration
       and then
         (Int_Kind (Result) = A_Function_Call
         or else
           Int_Kind (Result) = A_Procedure_Call_Statement)
      then
         Set_Int_Kind (Result, Subprogram_Attribute_Kind (Result_Node));
      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Renamed_Entity");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Renamed_Entity",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Renamed_Entity;
------------------------------------------------------------------------------
   function Corresponding_Base_Entity
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);

      Arg_Node      : Node_Id;
      Result_Node   : Node_Id                := Empty;
      Res_Kind      : Node_Kind;
      Res_Elem_Kind : Internal_Element_Kinds := Not_An_Element;
      Result_Unit   : Asis.Compilation_Unit;
   begin
      Check_Validity (Declaration,
        Package_Name & "Corresponding_Base_Entity");

      if not (Arg_Kind = An_Object_Renaming_Declaration           or else
              Arg_Kind = An_Exception_Renaming_Declaration        or else
              Arg_Kind = A_Procedure_Renaming_Declaration         or else
              Arg_Kind = A_Function_Renaming_Declaration          or else
              Arg_Kind = A_Package_Renaming_Declaration           or else
              Arg_Kind = A_Generic_Package_Renaming_Declaration   or else
              Arg_Kind = A_Generic_Procedure_Renaming_Declaration or else
              Arg_Kind = A_Generic_Function_Renaming_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Base_Entity",
            Wrong_Kind => Arg_Kind);
      end if;

      --  Let's first handle a special case of fake Numeric_Error renaming
      --  (see B712-0050

      if Special_Case (Declaration) = Numeric_Error_Renaming then
         --  Nothing to unwind:
         return Renamed_Entity (Declaration);
      end if;

      Arg_Node := Node (Declaration);
      --  the first renaming declaration in the renaming chain

      Result_Node := Sinfo.Name (Arg_Node);
      Arg_Node := R_Node (Declaration);
      --  to be able to deal with renamings of task entries in a uniform way

      while Present (Arg_Node) loop

         if Is_Rewrite_Substitution (Arg_Node) and then
            Nkind (Arg_Node) = N_Subprogram_Declaration
         then
            exit;
            --  renaming a task entry as a subprogram, no renaming chain
            --  any more
         end if;

         Res_Kind := Nkind (Result_Node);

         if Res_Kind = N_Selected_Component   or else
            Res_Kind = N_Indexed_Component    or else
            Res_Kind = N_Explicit_Dereference or else
            Res_Kind = N_Slice                or else
            Res_Kind = N_Function_Call        or else
            Res_Kind = N_Type_Conversion
         then

            --  The name after 'renames' has no chance to be defined by a
            --  renamed declaration
            exit;

         elsif Entity_Present (Result_Node) then
            Arg_Node := Entity (Result_Node);

         elsif Res_Kind = N_Attribute_Reference then

            if Arg_Kind in A_Procedure_Renaming_Declaration ..
                           A_Function_Renaming_Declaration
            then
               --  Renaming of an attribute-subprogram as a "normal" subprogram
               Res_Elem_Kind := Subprogram_Attribute_Kind  (Result_Node);
               exit;
            end if;

         else
            null;
            pragma Assert (False);
         end if;

         Arg_Node := Parent (Arg_Node);

         if Nkind (Arg_Node) in N_Subprogram_Specification then
            Arg_Node := Parent (Arg_Node);
         end if;

         if Nkind (Arg_Node) not in N_Renaming_Declaration then
            --  we have achieved the end of the renaming chain
            exit;
         else
            --  we are still in the chain
            Result_Node := Sinfo.Name (Arg_Node);
         end if;

      end loop;

      --  and now we have to compute the Result_Unit and to make the
      --  consistency check:

      Result_Unit := Enclosing_Unit
        (Encl_Cont_Id (Declaration), Result_Node);

      return Node_To_Element_New (Node          => Result_Node,
                                  Internal_Kind => Res_Elem_Kind,
                                  In_Unit       => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Corresponding_Base_Entity");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Base_Entity",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Corresponding_Base_Entity;
------------------------------------------------------------------------------

   function Protected_Operation_Items
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Declarative_Item_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Declaration,
        Package_Name & "Protected_Operation_Items");

      if not (Arg_Kind = A_Protected_Body_Declaration) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Protected_Operation_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return N_To_E_List_New
               (List             => Sinfo.Declarations (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Declaration);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Protected_Operation_Items",
               Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Protected_Operation_Items",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Protected_Operation_Items;
------------------------------------------------------------------------------

   function Entry_Family_Definition
     (Declaration : Asis.Declaration)
      return        Asis.Discrete_Subtype_Definition
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity (Declaration,
        Package_Name & "Entry_Family_Definition");
      if not (Arg_Kind = An_Entry_Declaration) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Entry_Family_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);
      Result_Node := Sinfo.Discrete_Subtype_Definition (Arg_Node);

      if Present (Result_Node) then
         return Node_To_Element_New
           (Node             => Result_Node,
           Starting_Element => Declaration);
      else
         return Nil_Element;
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Entry_Family_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Entry_Family_Definition",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Entry_Family_Definition;
------------------------------------------------------------------------------
   function Entry_Index_Specification
     (Declaration : Asis.Declaration)
      return        Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
      Result_Node : Node_Id;
   begin
      Check_Validity (Declaration,
        Package_Name & "Entry_Index_Specification");
      if not (Arg_Kind = An_Entry_Body_Declaration) then
         Raise_ASIS_Inappropriate_Element (Diagnosis =>
           Package_Name & "Entry_Index_Specification",
           Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      Result_Node := Entry_Index_Specification
        (Entry_Body_Formal_Part (Arg_Node));

      if Present (Result_Node) then
         return Node_To_Element_New
           (Node             => Result_Node,
           Starting_Element => Declaration,
           Internal_Kind => An_Entry_Index_Specification);
      else
         return Nil_Element;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Entry_Index_Specification");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Entry_Index_Specification",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Entry_Index_Specification;
------------------------------------------------------------------------------
   function Entry_Barrier
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Entry_Barrier");
      if not (Arg_Kind = An_Entry_Body_Declaration) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Entry_Barrier",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return Node_To_Element_New
        (Node             => Condition (Entry_Body_Formal_Part (Arg_Node)),
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Entry_Barrier");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Entry_Barrier",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Entry_Barrier;
------------------------------------------------------------------------------
   function Corresponding_Subunit
     (Body_Stub : Asis.Declaration)
      return      Asis.Declaration
   is
      Arg_Node       : Node_Id;
      Arg_Kind       : constant Internal_Element_Kinds := Int_Kind (Body_Stub);
      Arg_Unit       : Asis.Compilation_Unit;
      Result_Subunit : Asis.Compilation_Unit;
   begin
      Check_Validity (Body_Stub, Package_Name & "Corresponding_Subunit");

      if not (Arg_Kind = A_Function_Body_Stub  or else
              Arg_Kind = A_Package_Body_Stub   or else
              Arg_Kind = A_Procedure_Body_Stub or else
              Arg_Kind = A_Task_Body_Stub      or else
              Arg_Kind = A_Protected_Body_Stub)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Subunit",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Body_Stub);

      Arg_Unit := Encl_Unit (Body_Stub);

      Result_Subunit := Get_Subunit (Parent_Body => Arg_Unit,
                                     Stub_Node   => Arg_Node);

      if Exists (Result_Subunit) then
         return Unit_Declaration (Result_Subunit);
      else
         return Nil_Element;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Body_Stub,
               Outer_Call => Package_Name & "Corresponding_Subunit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Subunit",
            Ex          => Ex,
            Arg_Element => Body_Stub);
   end Corresponding_Subunit;
------------------------------------------------------------------------------
   --  NOT IMPLEMENTED

   function Corresponding_Subunit
     (Body_Stub   : Asis.Declaration;
      The_Context : Asis.Context)
      return        Asis.Declaration
   is
   begin
      Check_Validity (Body_Stub, Package_Name & "Corresponding_Subunit");
      Check_Validity (The_Context, Package_Name & "Corresponding_Subunit");

      Not_Implemented_Yet (Diagnosis =>
        Package_Name & "Corresponding_Subunit");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Body_Stub,
               Outer_Call => Package_Name & "Corresponding_Subunit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Subunit",
            Ex          => Ex,
            Arg_Element => Body_Stub);
   end Corresponding_Subunit;
------------------------------------------------------------------------------

   function Is_Subunit (Declaration : Asis.Declaration) return Boolean is
      Arg_Kind   : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_R_Node : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Is_Subunit");

      if not (Arg_Kind in Internal_Declaration_Kinds) then
         return False;
      else
         Arg_R_Node := R_Node (Declaration);
         return Nkind (Parent (Arg_R_Node)) = N_Subunit;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Is_Subunit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Subunit",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Is_Subunit;
------------------------------------------------------------------------------
   function Corresponding_Body_Stub
     (Subunit : Asis.Declaration)
      return    Asis.Declaration
   is
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Asis.Compilation_Unit;
   begin
      Check_Validity (Subunit, Package_Name & "Corresponding_Body_Stub");

      if not Is_Subunit (Subunit) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Body_Stub",
            Wrong_Kind => Int_Kind (Subunit));
      end if;

      Arg_Node := Node (Subunit);
      Result_Node := Corresponding_Stub (Parent (Arg_Node));
      Result_Unit := Enclosing_Unit
        (Encl_Cont_Id (Subunit), Result_Node);

      return Node_To_Element_New (Node    => Result_Node,
                                  In_Unit => Result_Unit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Subunit,
               Outer_Call => Package_Name & "Corresponding_Body_Stub");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Body_Stub",
            Ex          => Ex,
            Arg_Element => Subunit);
   end Corresponding_Body_Stub;
-----------------------------------------------------------------------------
   --  NOT IMPLEMENTED

   function Corresponding_Body_Stub
     (Subunit     : Asis.Declaration;
      The_Context : Asis.Context)
      return        Asis.Declaration
   is
   begin
      Check_Validity (Subunit, Package_Name & "Corresponding_Body_Stub");
      Check_Validity (
        The_Context, Package_Name & "Corresponding_Body_Stub");

      Not_Implemented_Yet (Diagnosis =>
        Package_Name & "Corresponding_Body_Stub");
      --  ASIS_Failed is raised, Not_Implemented_Error status is set

      return Nil_Element; -- to make the code syntactically correct

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Subunit,
               Outer_Call => Package_Name & "Corresponding_Body_Stub");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Body_Stub",
            Ex          => Ex,
            Arg_Element => Subunit);
   end Corresponding_Body_Stub;
------------------------------------------------------------------------------
   function Generic_Formal_Part
     (Declaration     : Asis.Declaration;
      Include_Pragmas : Boolean := False)
      return            Asis.Generic_Formal_Parameter_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
      Result_Node_List : List_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Generic_Formal_Part");

      if not (Arg_Kind = A_Generic_Package_Declaration   or else
        Arg_Kind = A_Generic_Procedure_Declaration or else
        Arg_Kind = A_Generic_Function_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Generic_Formal_Part",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node         := Node (Declaration);
      Result_Node_List := Generic_Formal_Declarations (Arg_Node);

      return N_To_E_List_New
        (List             => Result_Node_List,
        Include_Pragmas  => Include_Pragmas,
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Generic_Formal_Part",
               Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Generic_Formal_Part",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Include_Pragmas);
   end Generic_Formal_Part;
-----------------------------------------------------------------------------

   function Generic_Unit_Name
     (Declaration : Asis.Declaration)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Declaration, Package_Name & "Generic_Unit_Name");
      if not (Arg_Kind = A_Function_Instantiation    or else
        Arg_Kind = A_Package_Instantiation      or else
        Arg_Kind = A_Procedure_Instantiation    or else
        Arg_Kind = A_Formal_Package_Declaration or else
        Arg_Kind = A_Formal_Package_Declaration_With_Box)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Generic_Unit_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      return Node_To_Element_New
        (Node             => Sinfo.Name (Arg_Node),
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Generic_Unit_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Generic_Unit_Name",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Generic_Unit_Name;
------------------------------------------------------------------------------
   function Generic_Actual_Part
     (Declaration : Asis.Declaration;
      Normalized  : Boolean := False)
      return        Asis.Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);

      Arg_Node           : Node_Id;
      Template_Node      : Node_Id;
   begin

      Check_Validity (Declaration, Package_Name & "Generic_Actual_Part");

      if not (Arg_Kind = A_Function_Instantiation  or else
        Arg_Kind = A_Package_Instantiation   or else
        Arg_Kind = A_Procedure_Instantiation or else
        Arg_Kind = A_Formal_Package_Declaration)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Generic_Actual_Part",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Declaration);

      if Normalized then
         --  the only things we do here are calculating the node of the
         --  template and the number of the normalized associations in the
         --  result list. Then we call Normalized_Generic_Associations,
         --  which does the actual job.
         Template_Node := Entity (Sinfo.Name (Arg_Node));

         if Nkind (Parent (Template_Node)) = N_Defining_Program_Unit_Name then
            Template_Node := Parent (Template_Node);
         end if;

         Template_Node := Parent (Template_Node);

         while Nkind (Template_Node) in N_Generic_Renaming_Declaration loop
            Template_Node := Entity (Sinfo.Name (Template_Node));

            if Nkind (Parent (Template_Node)) =
               N_Defining_Program_Unit_Name
            then
               Template_Node := Parent (Template_Node);
            end if;

            Template_Node := Parent (Template_Node);
         end loop;

         Template_Node := Parent (Template_Node);

         if Is_Non_Empty_List (
              Generic_Formal_Declarations (Template_Node))
         then

            return Normalized_Generic_Associations
              (Inst_Elem          => Declaration,
               Templ_Node         => Template_Node);
         else
            return Nil_Element_List;
         end if;

      else

         declare
            Result : Asis.Association_List :=
               N_To_E_List_New
                  (List             => Generic_Associations (Arg_Node),
                   Internal_Kind    => A_Generic_Association,
                   Starting_Element => Declaration);

            Tmp_El         : Asis.Element;
            More_Inversion : Boolean := True;
         begin

            --  We have to reorder Result to follow the situation in, because
            while More_Inversion loop

               More_Inversion := False;

               for J in 1 .. Result'Last - 1 loop

                  if Sloc (Node (Result (J))) >
                     Sloc (Node (Result (J + 1)))
                  then
                     Tmp_El         := Result (J + 1);
                     Result (J + 1) := Result (J);
                     Result (J)     := Tmp_El;
                     More_Inversion := True;
                  end if;

               end loop;

            end loop;

            return Result;
         end;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Generic_Actual_Part",
               Bool_Par   => Normalized);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Generic_Actual_Part",
            Ex          => Ex,
            Arg_Element => Declaration,
            Bool_Par_ON => Normalized);
   end Generic_Actual_Part;
------------------------------------------------------------------------------

   function Formal_Subprogram_Default
     (Declaration : Asis.Generic_Formal_Parameter)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin
      Arg_Node := Node (Declaration);
      Check_Validity (Declaration,
        Package_Name & "Formal_Subprogram_Default");
      if not ((Arg_Kind = A_Formal_Procedure_Declaration
               or else
                 Arg_Kind = A_Formal_Function_Declaration)
              and then
                Present (Default_Name (Arg_Node)))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Formal_Subprogram_Default",
            Wrong_Kind => Arg_Kind);
      end if;

      return Node_To_Element_New
        (Node             => Default_Name (Arg_Node),
        Starting_Element => Declaration);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument   => Declaration,
               Outer_Call => Package_Name & "Formal_Subprogram_Default");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Formal_Subprogram_Default",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Formal_Subprogram_Default;
------------------------------------------------------------------------------
   function Corresponding_Generic_Element
     (Reference : Asis.Element)
      return      Asis.Defining_Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Reference);

      Arg_Def_Name     : Asis.Element;
      Gen_Unit_Elem    : Asis.Element;
      Decl_In_Instanse : Asis.Element;
      Is_Instance_Name : Boolean := False;
      Is_From_Body     : Boolean := False;
      Tmp_Elem         : Element;

      Result_Element   : Asis.Element;
   begin
      Check_Validity (Reference,
        Package_Name & "Corresponding_Generic_Element");

      if not (Arg_Kind in Internal_Defining_Name_Kinds   or else
              Arg_Kind = An_Identifier                   or else
              Arg_Kind in Internal_Operator_Symbol_Kinds or else
              Arg_Kind = A_Character_Literal             or else
              Arg_Kind = An_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Generic_Element",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we heed the defining name in case if an argument is of
      --  An_Expression kind

      if Arg_Kind in Internal_Defining_Name_Kinds then
         Arg_Def_Name := Reference;
      else
         Arg_Def_Name :=
           Asis.Expressions.Corresponding_Name_Definition (Reference);
      end if;

      --  then, checking if there is a corresponding generic element
      if Is_Nil (Arg_Def_Name) or else
        not (Is_From_Instance (Arg_Def_Name))
      then
         return Nil_Element;
      end if;

      --  !!! NOTE: references to formal parameters cannot be processed
      --            for now ??? But why?

      --  Now - computing Gen_Unit_Elem as the declaration of generic
      --  unit in which the corresponding generic element in question is
      --  supposed to be
      Decl_In_Instanse := Enclosing_Element (Arg_Def_Name);
      Gen_Unit_Elem    := Decl_In_Instanse;

      --  A defining name of (nested) instance is a special case -
      --  we have to skip such an instance

      if Int_Kind (Gen_Unit_Elem) in An_Internal_Generic_Instantiation
       and then
         Is_Equal (Arg_Def_Name, Names (Gen_Unit_Elem) (1))
      then
         Gen_Unit_Elem := Enclosing_Element (Gen_Unit_Elem);
      end if;

      while Int_Kind (Gen_Unit_Elem) not in
            An_Internal_Generic_Instantiation
         and then
            (Int_Kind (Gen_Unit_Elem) not in
             A_Formal_Package_Declaration ..
             A_Formal_Package_Declaration_With_Box
            or else
             Is_Equal (Reference, Names (Gen_Unit_Elem) (1)))
      loop
         Tmp_Elem      := Gen_Unit_Elem;
         Gen_Unit_Elem := Enclosing_Element (Gen_Unit_Elem);
      end loop;

      if (Int_Kind (Tmp_Elem) = A_Procedure_Body_Declaration or else
          Int_Kind (Tmp_Elem) = A_Function_Body_Declaration  or else
          Int_Kind (Tmp_Elem) = A_Package_Body_Declaration)
         and then
         --  We should not treat artificial declarations created to pass
         --  actuals as being from body!
          not (Pass_Generic_Actual (Node (Decl_In_Instanse)))
      then
         Is_From_Body := True;
      end if;

      Gen_Unit_Elem := Generic_Unit_Name (Gen_Unit_Elem);

      if Int_Kind (Gen_Unit_Elem) = A_Selected_Component then
         Gen_Unit_Elem := Asis.Expressions.Selector (Gen_Unit_Elem);
      end if;

      Gen_Unit_Elem :=
         Asis.Expressions.Corresponding_Name_Declaration (Gen_Unit_Elem);

      if Int_Kind (Gen_Unit_Elem) in An_Internal_Renaming_Declaration then
         Gen_Unit_Elem := Corresponding_Base_Entity (Gen_Unit_Elem);

         if Int_Kind (Gen_Unit_Elem) = A_Selected_Component then
            Gen_Unit_Elem := Asis.Expressions.Selector (Gen_Unit_Elem);
         end if;

         Gen_Unit_Elem :=
            Asis.Expressions.Corresponding_Name_Declaration (Gen_Unit_Elem);

      end if;

      Tmp_Elem := Enclosing_Element (Arg_Def_Name);

      if Int_Kind (Tmp_Elem) = A_Defining_Expanded_Name then
         Tmp_Elem := Enclosing_Element (Tmp_Elem);
      end if;

      Tmp_Elem := Enclosing_Element (Tmp_Elem);

      if Int_Kind (Tmp_Elem) in An_Internal_Generic_Instantiation
       or else
         Int_Kind (Tmp_Elem) in A_Formal_Package_Declaration ..
         A_Formal_Package_Declaration_With_Box
      then
         Is_Instance_Name := True;
      end if;

      if Is_Instance_Name then
         Result_Element := Names (Gen_Unit_Elem) (1);
      else

         if Is_From_Body then
            Gen_Unit_Elem := Corresponding_Body (Gen_Unit_Elem);
         end if;

         Result_Element := Get_Corresponding_Generic_Element (
                              Gen_Unit => Gen_Unit_Elem,
                              Def_Name => Arg_Def_Name);
      end if;

      return Result_Element;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Outer_Call => Package_Name & "Corresponding_Generic_Element",
               Argument   => Reference);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Generic_Element",
            Ex          => Ex,
            Arg_Element => Reference);
   end Corresponding_Generic_Element;

   ------------------------------
   -- Is_Dispatching_Operation --
   ------------------------------

   function Is_Dispatching_Operation
     (Declaration : Asis.Element)
      return        Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Declaration, Package_Name & "Is_Dispatching_Operation");

      if not (Arg_Kind = A_Procedure_Declaration          or else
              Arg_Kind = A_Null_Procedure_Declaration     or else
              Arg_Kind = A_Function_Declaration           or else
              Arg_Kind = A_Procedure_Body_Declaration     or else
              Arg_Kind = A_Function_Body_Declaration      or else
              Arg_Kind = A_Procedure_Renaming_Declaration or else
              Arg_Kind = A_Function_Renaming_Declaration  or else
              Arg_Kind = A_Procedure_Body_Stub            or else
              Arg_Kind = A_Function_Body_Stub)
      then
         return False;
      end if;

      Arg_Node := Defining_Unit_Name (Specification (Node (Declaration)));

      if Nkind (Arg_Node) = N_Defining_Program_Unit_Name then
         return False;
      else
         return Is_Dispatching_Operation (Arg_Node);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Dispatching_Operation",
               Argument   => Declaration);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Is_Dispatching_Operation;

end Asis.Declarations;
