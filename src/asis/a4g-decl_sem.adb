------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . D E C L _ S E M                          --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines needed for semantic queries from
--  the Asis.Declarations package

with Asis.Declarations; use Asis.Declarations;
with Asis.Definitions;  use Asis.Definitions;
with Asis.Iterator;     use Asis.Iterator;
with Asis.Elements;     use Asis.Elements;
with Asis.Errors;       use Asis.Errors;
with Asis.Exceptions;   use Asis.Exceptions;
with Asis.Extensions;   use Asis.Extensions;

with Asis.Set_Get;      use Asis.Set_Get;

with A4G.A_Sem;         use A4G.A_Sem;
with A4G.Int_Knds;      use A4G.Int_Knds;
with A4G.Vcheck;        use A4G.Vcheck;
with A4G.Mapping;       use A4G.Mapping;

with Atree;             use Atree;
with Einfo;             use Einfo;
with Namet;             use Namet;
with Nlists;            use Nlists;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;

package body A4G.Decl_Sem is

   -----------------------------
   -- Corresponding_Body_Node --
   -----------------------------

   function Corresponding_Body_Node (Decl_Node : Node_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := Corresponding_Body (Decl_Node);

      if No (Result_Node) then
         --  package without a body
         return Result_Node;
      end if;

      Result_Node := Parent (Result_Node);

      if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
         Result_Node := Parent (Result_Node);
      end if;

      if Nkind (Result_Node) = N_Function_Specification  or else
         Nkind (Result_Node) = N_Procedure_Specification
      then
         Result_Node := Parent (Result_Node);
      end if;

      if Nkind (Parent (Result_Node)) = N_Subunit then
         --  we come back to the stub!
         Result_Node := Corresponding_Stub (Parent (Result_Node));
      end if;

      if not Comes_From_Source (Result_Node)
        and then
          not (Is_Rewrite_Substitution (Result_Node)
                 --  SCz
--              and then
              --                Nkind (Original_Node (Result_Node)) = N_Expression_Function)
              )
      then
         --  implicit body created by the compiler for renaming-as-body.
         --  the renaming itself is the previous list member, so
         Result_Node := Get_Renaming_As_Body (Decl_Node);
      end if;

      return Result_Node;

   end Corresponding_Body_Node;

   -----------------------------
   -- Corresponding_Decl_Node --
   -----------------------------

   function Corresponding_Decl_Node (Body_Node : Node_Id) return Node_Id is
      Result_Node        : Node_Id := Empty;
      Protected_Def_Node : Node_Id;
      Tmp_Node           : Node_Id := Empty;
   begin

      case Nkind (Body_Node) is
         when N_Body_Stub =>
            Result_Node := Corr_Decl_For_Stub (Body_Node);

         when N_Entry_Body =>

            Protected_Def_Node := Corresponding_Spec (Parent (Body_Node));

            if Ekind (Protected_Def_Node) = E_Limited_Private_Type then
               Protected_Def_Node := Full_View (Protected_Def_Node);
            end if;

            Protected_Def_Node := Parent (Protected_Def_Node);
            Protected_Def_Node := Protected_Definition (Protected_Def_Node);

            Tmp_Node :=
              First_Non_Pragma (Visible_Declarations (Protected_Def_Node));

            while Present (Tmp_Node) loop

               if Nkind (Tmp_Node) = N_Entry_Declaration and then
                  Parent (Corresponding_Body (Tmp_Node)) = Body_Node
               then
                  Result_Node := Tmp_Node;
                  exit;
               end if;

               Tmp_Node := Next_Non_Pragma (Tmp_Node);

            end loop;

            if No (Result_Node) and then
               Present (Private_Declarations (Protected_Def_Node))
            then
               Tmp_Node :=
                 First_Non_Pragma (Private_Declarations (Protected_Def_Node));

               while Present (Tmp_Node) loop

                  if Nkind (Tmp_Node) = N_Entry_Declaration and then
                     Parent (Corresponding_Body (Tmp_Node)) = Body_Node
                  then
                     Result_Node := Tmp_Node;
                     exit;
                  end if;

                  Tmp_Node := Next_Non_Pragma (Tmp_Node);

               end loop;

            end if;

         when others =>
            Result_Node := Corresponding_Spec (Body_Node);
            Result_Node := Parent (Result_Node);

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Node := Parent (Result_Node);
            end if;

      end case;

      pragma Assert (Present (Result_Node));
      --  now - from a defining entity to the declaration itself; note,
      --  that here we cannot get a defining expanded name, because the
      --  corresponding declaration for library units are obtained in
      --  another control flow
      case Nkind (Result_Node) is
         when N_Function_Specification  |
              N_Procedure_Specification |
              N_Package_Specification =>
            Result_Node := Parent (Result_Node);
         when N_Private_Type_Declaration =>
            --  this is the case when a task type is the completion
            --  of a private type
            Result_Node := Full_View (Defining_Identifier (Result_Node));
            Result_Node := Parent (Result_Node);
         when others =>
            null;
      end case;

      return Result_Node;
   end Corresponding_Decl_Node;

   ---------------------------------------
   -- Get_Corresponding_Generic_Element --
   ---------------------------------------

   function Get_Corresponding_Generic_Element
     (Gen_Unit : Asis.Declaration;
      Def_Name : Asis.Element)
      return     Asis.Element
   is
      Kind_To_Check   : constant Internal_Element_Kinds := Int_Kind (Def_Name);
      Sloc_To_Check   : constant Source_Ptr := Sloc (Node (Def_Name));
      Line_To_Check   : constant Physical_Line_Number :=
         Get_Physical_Line_Number (Sloc_To_Check);
      Column_To_Check : constant Column_Number :=
        Get_Column_Number (Sloc_To_Check);

      Result_Element  : Asis.Element := Nil_Element;

      Tmp_El          : Asis.Element;

      Check_Inherited_Element : constant Boolean :=
         Is_Part_Of_Inherited (Def_Name);

      Sloc_To_Check_1 : constant Source_Ptr := Sloc (Node_Field_1 (Def_Name));
      Line_To_Check_1 : constant Physical_Line_Number :=
         Get_Physical_Line_Number (Sloc_To_Check_1);
      Column_To_Check_1 : constant Column_Number :=
        Get_Column_Number (Sloc_To_Check_1);
      --  Used in case if we are looking for an implicit Element

      function Is_Found (E : Asis.Element) return Boolean;
      --  Checks if the Element being traversed is a corresponding generic
      --  element for Def_Name

      function Is_Found (E : Asis.Element) return Boolean is
         Elem_Sloc   : constant Source_Ptr := Sloc (Node (E));
         Elem_Sloc_1 :          Source_Ptr;
         Result      : Boolean             := False;
      begin

         if not (Check_Inherited_Element xor Is_Part_Of_Inherited (E)) then

            Result :=
               Line_To_Check = Get_Physical_Line_Number (Elem_Sloc)
            and then
               Column_To_Check = Get_Column_Number (Elem_Sloc);

            if Result
             and then
               Check_Inherited_Element
            then
               Elem_Sloc_1 := Sloc (Node_Field_1 (E));

               Result :=
                  Line_To_Check_1 = Get_Physical_Line_Number (Elem_Sloc_1)
                 and then
                  Column_To_Check_1 = Get_Column_Number (Elem_Sloc_1);
            end if;

         end if;

         return Result;
      end Is_Found;

      --  and now, variables and actuals for Traverse_Element
      My_Control : Traverse_Control := Continue;
      My_State   : No_State         := Not_Used;

      procedure Pre_Op
        (Element    :        Asis.Element;
         Control    : in out Traverse_Control;
         State      : in out No_State);

      procedure Look_For_Corr_Gen_El is new Traverse_Element
        (State_Information => No_State,
         Pre_Operation     => Pre_Op,
         Post_Operation    => No_Op);

      procedure Pre_Op
        (Element    :        Asis.Element;
         Control    : in out Traverse_Control;
         State      : in out No_State)
      is
         pragma Unreferenced (State);

         Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      begin

         case Arg_Kind is

            when An_Internal_Body_Stub =>

               if Kind_To_Check = A_Defining_Identifier or else
                  Kind_To_Check in Internal_Defining_Operator_Kinds
               then
                  --  We have to traverse the code of the subunit -
                  --  see 9217-015. But before doing this, let's check the
                  --  name of the subunit:

                  Tmp_El := Asis.Declarations.Names (Element) (1);

                  if Int_Kind (Tmp_El) = Kind_To_Check and then
                     Is_Found (Tmp_El)
                  then
                     Result_Element := Tmp_El;
                     Control        := Terminate_Immediately;

                     return;
                  end if;

               end if;

               --  If we are here, we have to traverse the proper body:

               Tmp_El := Corresponding_Subunit (Element);

               if not Is_Nil (Tmp_El) then
                  Look_For_Corr_Gen_El (Element => Tmp_El,
                                        Control => My_Control,
                                        State   => My_State);
               end if;

            when Internal_Defining_Name_Kinds =>

               if Int_Kind (Element) = Kind_To_Check and then
                  Is_Found (Element)
               then
                  Result_Element := Element;
                  Control        := Terminate_Immediately;
               end if;

            when A_Derived_Type_Definition             |
                 A_Derived_Record_Extension_Definition |
                 A_Formal_Derived_Type_Definition      =>

                  if Check_Inherited_Element then

                     declare
                        Inherited_Decls : constant Asis.Element_List :=
                         Implicit_Inherited_Declarations (Element);

                        Inherited_Subprgs : constant Asis.Element_List :=
                         Implicit_Inherited_Subprograms (Element);
                     begin

                        for J in Inherited_Decls'Range loop
                           exit when My_Control = Terminate_Immediately;

                           Look_For_Corr_Gen_El
                             (Element => Inherited_Decls (J),
                              Control => My_Control,
                              State   => My_State);
                        end loop;

                        for J in Inherited_Subprgs'Range loop
                           exit when My_Control = Terminate_Immediately;

                           Look_For_Corr_Gen_El
                            (Element => Inherited_Subprgs (J),
                             Control => My_Control,
                             State   => My_State);
                        end loop;

                     end;

                  end if;

            when others =>
               null;
         end case;

      end Pre_Op;

   begin  -- Get_Corresponding_Generic_Element
      Look_For_Corr_Gen_El (Element => Gen_Unit,
                            Control => My_Control,
                            State   => My_State);
      return Result_Element;

   exception
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Nil_Element,
               Outer_Call => "A4G.Decl_Sem.Get_Corresponding_Generic_Element");
         end if;

         raise;
   end Get_Corresponding_Generic_Element;

   -----------------------
   -- Get_Expanded_Spec --
   -----------------------

   function Get_Expanded_Spec (Instance_Node : Node_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      --  GNAT constructs the structure corresponding to an expanded generic
      --  specification just before the instantiation itself, except the case
      --  of the formal package with box:

      if Nkind (Instance_Node) = N_Package_Declaration and then
         Nkind (Original_Node (Instance_Node)) = N_Formal_Package_Declaration
      then
         Result_Node := Instance_Node;
      else
         Result_Node := Prev_Non_Pragma (Instance_Node);
      end if;

      if Nkind (Result_Node) = N_Package_Body then
         --  Here we have the expanded generic body, therefore - one
         --  more step up the list
         Result_Node := Prev_Non_Pragma (Result_Node);
      end if;

      --  in case of a package instantiation, we have to take the whole
      --  expanded package, but in case of a subprogram instantiation we
      --  need only the subprogram declaration, which is the last element
      --  of the visible declarations list of the "artificial" package
      --  spec created by the compiler
      if not (Nkind (Instance_Node) = N_Package_Instantiation or else
              Nkind (Original_Node (Instance_Node)) =
              N_Formal_Package_Declaration)
      then
         Result_Node  := Last_Non_Pragma (Visible_Declarations
                            (Specification (Result_Node)));

         if Nkind (Result_Node) = N_Subprogram_Body then
            Result_Node := Parent (Parent (Corresponding_Spec (Result_Node)));
         end if;

         pragma Assert (Nkind (Result_Node) = N_Subprogram_Declaration);
      end if;

      return Result_Node;
   end Get_Expanded_Spec;

   --------------------------
   -- Get_Renaming_As_Body --
   --------------------------

   function Get_Renaming_As_Body
     (Node      : Node_Id;
      Spec_Only : Boolean := False)
      return      Node_Id
   is
      Entity_Node      : Node_Id;
      Scope_Node       : Node_Id;
      Result_Node      : Node_Id := Empty;
      List_To_Search   : List_Id;
      Search_Node      : Node_Id := Node;
      --  in the first List_To_Search we start not from the very beginning;
      --  but from the node representing the argument subprogram declaration
      Completion_Found : Boolean := False;

      procedure Search_In_List;
      --  looks for a possible renaming-as-bode node being a completion for
      --  Node, using global settings for List_To_Search and Search_Node
      procedure Search_In_List is
      begin

         while Present (Search_Node) loop

            if Nkind (Search_Node) = N_Subprogram_Renaming_Declaration and then
               Corresponding_Spec (Search_Node) = Entity_Node
            then
               Result_Node := Search_Node;
               Completion_Found := True;
               return;
            end if;

            Search_Node := Next_Non_Pragma (Search_Node);
         end loop;

      end Search_In_List;
   begin  --  Get_Renaming_As_Body
      Entity_Node    := Defining_Unit_Name (Specification (Node));
      List_To_Search := List_Containing (Node);
      Search_In_List;

      if Completion_Found then
         goto end_of_search;
      end if;

      --  here we have to see, where we are. If we are not in a package,
      --  we have nothing to do, but if we are in the package, we may
      --  have to search again in another lists (the private part and
      --  the body)

      Scope_Node := Scope (Entity_Node);
      --  Node here can be of N_Subprogram_Declaration only!
      if Nkind (Parent (Scope_Node)) = N_Implicit_Label_Declaration then
         --  this is the implicit name created for a block statement,
         --  so we do not have any other list to search in
         goto end_of_search;
      else
         Scope_Node := Parent (Scope_Node);
      end if;

      if Nkind (Scope_Node) = N_Defining_Program_Unit_Name then
         Scope_Node := Parent (Scope_Node);
      end if;
      --  now if we are not in  N_Package_Specification, we have no
      --  other list to search in

      if  Nkind (Scope_Node) /= N_Package_Specification then
         goto end_of_search;
      end if;

      --  and here we are in N_Package_Specification

      if List_To_Search = Visible_Declarations (Scope_Node) then
         --  continuing in the private part:
         List_To_Search := Private_Declarations (Scope_Node);
         if not (No (List_To_Search)
            or else Is_Empty_List (List_To_Search))
         then
            Search_Node    := First_Non_Pragma (List_To_Search);
            Search_In_List;
         end if;
         if Completion_Found or else Spec_Only then
            goto end_of_search;
         end if;
      end if;

      --  and here we have to go into the package body, if any:
      Scope_Node := Corresponding_Body (Parent (Scope_Node));
      if Present (Scope_Node) then

         while Nkind (Scope_Node) /= N_Package_Body loop
            Scope_Node := Parent (Scope_Node);
         end loop;

         --  and to continue to search in the package body:
         List_To_Search := Sinfo.Declarations (Scope_Node);

         if not (No (List_To_Search)
            or else Is_Empty_List (List_To_Search))
         then
            Search_Node    := First_Non_Pragma (List_To_Search);
            Search_In_List;
         end if;

      end if;

      << end_of_search >>

      return Result_Node;
   end Get_Renaming_As_Body;

   -----------------------
   -- Serach_First_View --
   -----------------------

   function Serach_First_View (Type_Entity : Entity_Id) return Entity_Id is
      Type_Chars  : constant Name_Id := Chars (Type_Entity);
      Type_Decl   : constant Node_Id := Parent (Type_Entity);
      Result_Node : Node_Id          := Empty;
      Scope_Node  : Node_Id;
      Scope_Kind  : Node_Kind;
      Search_List : List_Id;

      Private_Decls_Passed : Boolean := False;

      procedure Sesrch_In_List (L : List_Id);
      --  we have a separate procedure for searching in a list of
      --  declarations, because we have to do this search from one to
      --  three times in case of a package. This procedure uses Type_Chars,
      --  Type_Decl and Result_Node as global values, and it sets
      --  Result_Node equal to the node defining the type with the same name
      --  as the name of the type represented by Type_Entity, if the
      --  search is successful, otherwise it remains is equal to Empty.
      --  this procedure supposes, that L is not No_List

      procedure Sesrch_In_List (L : List_Id) is
         Next_Decl          : Node_Id;
         Next_Decl_Original : Node_Id;
         Next_Kind          : Node_Kind;
      begin
         Next_Decl          := First_Non_Pragma (L);
         Next_Decl_Original := Original_Node (Next_Decl);
         Next_Kind          := Nkind (Next_Decl_Original);

         while Present (Next_Decl) loop

            if (Comes_From_Source (Next_Decl_Original)
                 and then
                   (Next_Kind = N_Full_Type_Declaration or else
                    Next_Kind = N_Task_Type_Declaration or else
                    Next_Kind = N_Protected_Type_Declaration or else
                    Next_Kind = N_Private_Type_Declaration or else
                    Next_Kind = N_Private_Extension_Declaration or else
                    Next_Kind = N_Formal_Type_Declaration or else
--  impossible in ASIS, but possible in the tree
--  because of the tree rewritings
                    Next_Kind = N_Incomplete_Type_Declaration))
--  these cases correspond to non-rewritten type
--  declarations
                or else
                  (not (Comes_From_Source (Next_Decl_Original))
                 and then
                    Next_Kind = N_Subtype_Declaration)
--  the declaration of a derived type rewritten into a
--  subtype declaration
            then

               if Is_Not_Duplicated_Decl (Next_Decl) then
--  ??? <tree problem 2>  - we need this "if" only because of this problem
                  if Next_Decl_Original = Type_Decl then
                     --  no private or incomplete view
                     Result_Node := Type_Entity;
                     return;
                  end if;

                  if Type_Chars = Chars (Defining_Identifier (Next_Decl)) then
                     --  we've found something...
                     Result_Node := Defining_Identifier (Next_Decl);
                     return;
                  end if;

               end if;
            end if;

            Next_Decl := Next_Non_Pragma (Next_Decl);
            Next_Decl_Original := Original_Node (Next_Decl);
            Next_Kind          := Nkind (Next_Decl_Original);

         end loop;
      end Sesrch_In_List;

   begin  --  Serach_First_View
      --  first, defining the scope of the Type_Entity. In case of a package
      --  body it will be a package spec anyway.
      Scope_Node := Scope (Type_Entity);

      if Nkind (Parent (Scope_Node)) = N_Implicit_Label_Declaration then
         --  this is the implicit name created for a block statement
         Scope_Node := Parent (Block_Node (Scope_Node));
      else
         Scope_Node := Parent (Scope_Node);
      end if;

      if Nkind (Scope_Node) = N_Defining_Program_Unit_Name then
         Scope_Node := Parent (Scope_Node);
      end if;
      --  now we are in N_Function_Specification, N_Procedure_Specification
      --  or in N_Package_Specification
      Scope_Kind := Nkind (Scope_Node);

      if Scope_Kind = N_Function_Specification  or else
         Scope_Kind = N_Procedure_Specification
      then
         --  we do not do this additional step for packages, because
         --  N_Package_Specification_Node already contains references to
         --  declaration lists, and for a package we gave to start from the
         --  declarations in the package spec, but for a subprogram
         --  we have to go to a subprogram body, because nothing interesting
         --  for this function can be declared in a separate subprogram
         --  specification (if any) or in a generic formal part (if any)
         Scope_Node := Parent (Scope_Node);
         Scope_Kind := Nkind (Scope_Node);
      end if;

      if Scope_Kind = N_Subprogram_Declaration
        or else
         Scope_Kind = N_Generic_Subprogram_Declaration
        or else
         Scope_Kind = N_Task_Type_Declaration
        or else
         Scope_Kind = N_Entry_Declaration
        or else
         Scope_Kind = N_Subprogram_Body_Stub
      then
         Scope_Node := Corresponding_Body (Scope_Node);
         Scope_Node := Parent (Scope_Node);

         if Nkind (Scope_Node) = N_Defining_Program_Unit_Name then
            Scope_Node := Parent (Scope_Node);
         end if;

         if Nkind (Scope_Node) = N_Function_Specification  or else
            Nkind (Scope_Node) = N_Procedure_Specification
         then
            Scope_Node := Parent (Scope_Node);
         end if;

         Scope_Kind := Nkind (Scope_Node);
      end if;

      --  now, defining the list to search. In case of generics, we do not
      --  have to start from parsing the list of generic parameters, because
      --  a generic formal type cannot have a completion as its full view,
      --  and it cannot be a completion of some other type.

      if Scope_Kind = N_Subprogram_Body or else
         Scope_Kind = N_Task_Body       or else
         Scope_Kind = N_Block_Statement or else
         Scope_Kind = N_Entry_Body
      then
         Search_List := Sinfo.Declarations (Scope_Node);
      elsif Scope_Kind = N_Package_Specification then
         Search_List := Visible_Declarations (Scope_Node);

         if Is_Empty_List (Search_List) then
            --  note, that Visible_Declarations cannot be No_List
            Private_Decls_Passed := True;
            Search_List := Private_Declarations (Scope_Node);

            if No (Search_List) or else Is_Empty_List (Search_List) then
               --  here we should go to the declarative part of the package
               --  body. Note, that if we are in a legal ada program, and if
               --  we start from a type declaration, Search_List cannot
               --  be No_List or an empty list
               Scope_Node := Parent (Corresponding_Body (Parent (Scope_Node)));
               --  note, that Search_Kind is unchanged here
               Search_List := Sinfo.Declarations (Scope_Node);
            end if;

         end if;

      end if;

      Sesrch_In_List (Search_List);

      if Result_Node /= Empty then
         if Result_Node /= Type_Entity and then
            Full_View (Result_Node) /= Type_Entity
         then
            --  The case when Type_Entity is a full type declaration that
            --  completes a private type/extension declaration that in turn
            --  completes an incomplete type.
            Result_Node := Full_View (Result_Node);
         end if;

         return Result_Node;
      end if;

      --  it is possible only for a package - we have to continue in the
      --  private part or/and in the body

      pragma Assert (Scope_Kind = N_Package_Specification);

      --  first, try a private part, if needed and if any

      if not Private_Decls_Passed then
         --  Scope_Node is still of N_Package_Specification kind here!
         Private_Decls_Passed := True;
         Search_List := Private_Declarations (Scope_Node);

         if Present (Search_List) and then Is_Non_Empty_List (Search_List) then
            Sesrch_In_List (Search_List);

            if Result_Node /= Empty then
               return Result_Node;
            end if;

         end if;

      end if;

      --  if we are here, Scope_Node is still of N_Package_Specification,
      --  and the only thing we have to do now is to check the package
      --  body
      --  There is some redundancy in the code - in fact, we need only
      --  one boolean flag (Private_Decls_Passed) to control the search in
      --  case of a package
      Scope_Node := Parent (Corresponding_Body (Parent (Scope_Node)));

      if Nkind (Scope_Node) = N_Defining_Program_Unit_Name then
         Scope_Node := Parent (Scope_Node);
      end if;

      Search_List := Sinfo.Declarations (Scope_Node);
      Sesrch_In_List (Search_List);

      if Result_Node /= Empty then
         return Result_Node;
      else
         pragma Assert (False);
         return Empty;
      end if;

   end Serach_First_View;

end A4G.Decl_Sem;
