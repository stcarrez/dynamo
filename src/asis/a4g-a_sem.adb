------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . A _ S E M                             --
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
-- CHANTABILITY or  FITNESS  FOR A PARTICULAR PURPOSE.  See the GNU General --
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
-- Sciences.  ASIS-for-GNAT  is  now  maintained  by  AdaCore               --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Declarations; use Asis.Declarations;
with Asis.Elements;     use Asis.Elements;
with Asis.Expressions;  use Asis.Expressions;
with Asis.Extensions;   use Asis.Extensions;
with Asis.Iterator;     use Asis.Iterator;

with Asis.Set_Get;      use Asis.Set_Get;

with A4G.A_Types;       use A4G.A_Types;
with A4G.Contt.TT;      use A4G.Contt.TT; use A4G.Contt;
with A4G.Contt.UT;      use A4G.Contt.UT;
with A4G.Mapping;       use A4G.Mapping;

with Atree;             use Atree;
with Namet;             use Namet;
with Nlists;            use Nlists;
with Sem_Aux;           use Sem_Aux;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;
with Snames;            use Snames;

package body A4G.A_Sem is

   ----------------------
   -- Local subprogram --
   ----------------------

   function Is_Importing_Pragma
     (N        : Node_Id;
      For_Name : Name_Id)
      return     Boolean;
   --  Checks if N is a node representing Import or Interface pragma that
   --  is applied to the name For_Name

   -----------------------------
   -- Belongs_To_Limited_View --
   -----------------------------

   function Belongs_To_Limited_View (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Declaration_Kind (Decl) is
         when An_Ordinary_Type_Declaration         |
              A_Task_Type_Declaration              |
              A_Protected_Type_Declaration         |
              An_Incomplete_Type_Declaration       |
              A_Tagged_Incomplete_Type_Declaration |
              A_Private_Type_Declaration           |
              A_Private_Extension_Declaration      |
              A_Package_Declaration                =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Belongs_To_Limited_View;

   ------------------------------
   -- Char_Defined_In_Standard --
   ------------------------------

   function Char_Defined_In_Standard (N : Node_Id) return Boolean is
      N_Etype  : Node_Id;
   begin
      N_Etype := Etype  (N);

      if No (N_Etype) then
         --  It may happen for array literal rewritten into a string literal,
         --  so some additional digging is needed
         N_Etype := Parent (N);

         if Nkind (N_Etype) = N_String_Literal then
            N_Etype := Etype (N_Etype);

            if Ekind (N_Etype) = E_String_Literal_Subtype then
               N_Etype := Component_Type (N_Etype);
            end if;

         else
            N_Etype := Empty;
         end if;

      end if;

      return Present (N_Etype) and then
             Sloc    (N_Etype) <= Standard_Location;
   end Char_Defined_In_Standard;

   ------------------------
   -- Corr_Decl_For_Stub --
   ------------------------

   function Corr_Decl_For_Stub (Stub_Node : Node_Id) return Node_Id is
      Result_Node       : Node_Id := Empty;
      Stub_Entity_Node  : Node_Id;
      Scope_Node        : Node_Id;
      Search_Node       : Node_Id;
      Search_Node_Kind  : Node_Kind;
      List_To_Search    : List_Id;
      Search_In_Package : Boolean;
      Decl_Found        : Boolean := False;
      Priv_Decl_Passed  : Boolean := False;
      Body_Passed       : Boolean := False;

      procedure Search_In_List;
      --  looks for a possible subprogram declaration node for which
      --  the given stub is a completion, using global settings for
      --  List_To_Search and Search_Node

      function Is_Spec_For_Stub
        (Search_Node      : Node_Id;
         Stub_Node        : Node_Id;
         Stub_Entity_Node : Node_Id)
         return Boolean;
      --  check if the current Search_Node is a corresponding definition
      --  for a given stub. We cannot directly use the Corresponding_Body
      --  field here, because in case when subunits are around, this field
      --  will point to a proper body of a subunit, but not to a stub
      --  This function is called only for those nodes for which
      --  Corresponding_Body field makes sense

      function Is_Spec_For_Stub
        (Search_Node      : Node_Id;
         Stub_Node        : Node_Id;
         Stub_Entity_Node : Node_Id)
         return Boolean
      is
         Corr_Body_Node : constant Node_Id := Corresponding_Body (Search_Node);
         N              : Node_Id;
      begin

         if Corr_Body_Node = Stub_Entity_Node then
            return True;
         else
            --  we have to check if we are in the proper body of a subunit
            N := Parent (Corr_Body_Node);

            if Nkind (N) = N_Procedure_Specification or else
               Nkind (N) = N_Function_Specification
            then
               N := Parent (N);
            end if;

            N := Parent (N);
            --  now, in case of subunit's parent body, we should be in
            --  N_Subunit node

            if Nkind (N) = N_Subunit then
               return Corresponding_Stub (N) = Stub_Node;
            else
               return False;
            end if;

         end if;

      end Is_Spec_For_Stub;

      procedure Search_In_List is
      begin

         while Present (Search_Node) loop
            Search_Node_Kind := Nkind (Search_Node);

            if   (Search_Node_Kind = N_Subprogram_Declaration         or else
                  Search_Node_Kind = N_Generic_Subprogram_Declaration or else
                  Search_Node_Kind = N_Package_Declaration            or else
                  Search_Node_Kind = N_Generic_Package_Declaration    or else
                  Search_Node_Kind = N_Single_Task_Declaration        or else
                  Search_Node_Kind = N_Task_Type_Declaration          or else
                  Search_Node_Kind = N_Single_Protected_Declaration   or else
                  Search_Node_Kind = N_Protected_Type_Declaration)
               and then
                  Is_Spec_For_Stub (Search_Node, Stub_Node, Stub_Entity_Node)
                  --  ???Corresponding_Body (Search_Node) = Stub_Entity_Node
            then
               --  the corresponding declaration for the stub is found
               Result_Node := Search_Node;
               Decl_Found := True;

               return;

            elsif Search_Node = Stub_Node then
               --  no need to search any mode, no declaration exists,
               --  the stub itself works as a declaration
               Decl_Found := True;

               return;

            end if;

            Search_Node := Next_Non_Pragma (Search_Node);
         end loop;

      end Search_In_List;

   begin  --  Corr_Decl_For_Stub

      --  first, setting Stub_Entity_Node:
      if Nkind (Stub_Node) = N_Subprogram_Body_Stub then
         Stub_Entity_Node := Defining_Unit_Name (Specification (Stub_Node));
      else
         Stub_Entity_Node := Defining_Identifier (Stub_Node);
      end if;

      --  then, defining the scope node and list to search in:
      Scope_Node := Scope (Stub_Entity_Node);

      if No (Scope_Node) then
         --  Unfortunately, this is the case for stubs of generic units
         --  with no (non-generic) parameters
         Scope_Node := Stub_Entity_Node;

         while not (Nkind (Scope_Node) = N_Package_Body or else
                    Nkind (Scope_Node) = N_Subprogram_Body)
         loop
            Scope_Node := Parent (Scope_Node);
         end loop;

         if Nkind (Scope_Node) = N_Package_Body then
            Scope_Node := Corresponding_Spec (Scope_Node);
         else
            Scope_Node := Defining_Unit_Name (Specification (Scope_Node));
         end if;

      end if;

      if Ekind (Scope_Node) = E_Generic_Package or else
         Ekind (Scope_Node) = E_Package
      then
         Search_In_Package := True;
         Scope_Node := Parent (Scope_Node);

         if Nkind (Scope_Node) = N_Defining_Program_Unit_Name then
            --  we are in a child library package
            Scope_Node := Parent (Scope_Node);
         end if;

         --  now we are in the package spec
         List_To_Search := Visible_Declarations (Scope_Node);

         if No (List_To_Search) then
            List_To_Search := Private_Declarations (Scope_Node);
            Priv_Decl_Passed := True;

            if No (List_To_Search) then
               List_To_Search := List_Containing (Stub_Node);
               --  what else could it be?
               Body_Passed := True;
            end if;

         end if;

      else

         Search_In_Package := False;
         List_To_Search    := List_Containing (Stub_Node);

         --  The following code was here for many years, but it seems that the
         --  only effect of this conditional processing is failures in case
         --  if we have a stub following the corresponding declaration in the
         --  body of library generic subprogram. We keep it commented out just
         --  in case.

--         --  The situation of the stub for generic subprogram having
--         --  (non-generic) parameters makes a special case:
--         if Ekind (Scope_Node) in Generic_Unit_Kind
--           and then
--            Corresponding_Stub (Parent (Parent (Parent (Corresponding_Body
--              (Parent (Parent (Scope_Node))))))) =
--            Stub_Node
--         then
--            return Parent (Parent (Scope_Node));
--         else
--            Search_In_Package := False;
--            List_To_Search    := List_Containing (Stub_Node);
--         end if;

      end if;

      Search_Node := First_Non_Pragma (List_To_Search);
      Search_In_List;

      --  now, if we are in a package, and if we have not found the result
      --  (or passed the stub node), we have to continue:

      if Search_In_Package and then not Decl_Found then
         --  where should we continue the search?

         if not Priv_Decl_Passed then
            List_To_Search := Private_Declarations (Scope_Node);
            Priv_Decl_Passed := True;

            if No (List_To_Search) then
               List_To_Search := List_Containing (Stub_Node);
               Body_Passed := True;
            end if;

         elsif not Body_Passed then
            List_To_Search := List_Containing (Stub_Node);
            Body_Passed := True;
         end if;

         Search_Node := First_Non_Pragma (List_To_Search);
         Search_In_List;

         if not Decl_Found then
            --  if we are here, we have to search the package body,
            --  where the stub itself is
            List_To_Search := List_Containing (Stub_Node);
            Search_Node := First_Non_Pragma (List_To_Search);
            Search_In_List;
         end if;

      end if;

      return Result_Node;

   end Corr_Decl_For_Stub;

   -------------------------
   -- Defined_In_Standard --
   -------------------------

   function Defined_In_Standard (N : Node_Id) return Boolean is
      N_Entity : Node_Id := Empty;
      N_Etype  : Node_Id := Empty;
      Result   : Boolean := False;
   begin

      if Nkind (N) in N_Has_Entity then
         N_Entity := Entity (N);
      elsif Nkind (N) in Sinfo.N_Entity then
         N_Entity := N;
      end if;

      if Present (N_Entity) then
         N_Etype  := Etype  (N_Entity);
      end if;

      Result :=
        Present (N_Entity)                      and then
        Present (N_Etype)                       and then
        Sloc    (N_Entity) <= Standard_Location and then
        Sloc    (N_Etype)  <= Standard_Location;

      return Result;
   end Defined_In_Standard;

   --------------------
   -- Entity_Present --
   --------------------

   function Entity_Present (N : Node_Id) return Boolean is
      Result : Boolean := Present (Entity (N));
   begin
      if Result then
         Result := Nkind (Entity (N)) in N_Entity;
      end if;

      return Result;
   end Entity_Present;

   --------------------------------
   -- Explicit_Parent_Subprogram --
   --------------------------------

   function Explicit_Parent_Subprogram (E : Entity_Id) return Entity_Id is
      Result      : Entity_Id             := Empty;
      E_Ekind     : constant Entity_Kind := Ekind (E);
      Parent_Type : Entity_Id;
      Tmp_Res     : Entity_Id;
   begin

      --  The problem here is that we can not just traverse the Alias chain,
      --  because in case if the parent subprogram is declared by the
      --  subprogram renaming and the renamed entity is an intrinsic
      --  subprogram, the Alias field of the derived subprogram will
      --  point not to the parent renaming declaration, but to this
      --  intrinsic subprogram (see F407-016).

      if Is_Intrinsic_Subprogram (E)
        and then
         Present (Alias (E))
        and then
         Defined_In_Standard (Alias (E))
      then
         --  Here we may have a renaming declaration, and the renamed entity
         --  is a predefined operation. So we have to traverse the derivation
         --  chain and to try to locate the explicit renaming that is the cause
         --  of the existing of this derived subprogram.

         Parent_Type := Etype (E);
         Parent_Type := Etype (Parent_Type);
         Parent_Type := Parent (Parent_Type);
         Parent_Type := Defining_Identifier (Parent_Type);

         --  Here we should have Parent_Type pointing to the entity of the
         --  parent type

         Tmp_Res := Next_Entity (Parent_Type);

         while Present (Tmp_Res) loop

            if Ekind (Tmp_Res) = E_Ekind
              and then
               Is_Intrinsic_Subprogram (Tmp_Res)
              and then
                 Chars (Tmp_Res) = Chars (E)
              and then
               Alias (Tmp_Res) = Alias (E)
            then
               Result := Tmp_Res;
               exit;
            end if;

            Tmp_Res := Next_Entity (Tmp_Res);
         end loop;

         if Present (Result)
           and then
            not Comes_From_Source (Result)
         then
            Result := Explicit_Parent_Subprogram (Result);
         end if;

      else
         Result := Alias (E);

         while Present (Alias (Result))
             and then
               not Comes_From_Source (Result)
         loop
            Result := Alias (Result);
         end loop;
      end if;

      return Result;
   end Explicit_Parent_Subprogram;

   --------------------------
   -- Get_Actual_Type_Name --
   --------------------------

   function Get_Actual_Type_Name (Type_Mark_Node : Node_Id) return Node_Id is
      Result   : Node_Id := Type_Mark_Node;
      Tmp_Node : Node_Id;
   begin

      if Is_From_Instance (Type_Mark_Node) then
         Tmp_Node := Entity (Type_Mark_Node);

         if Present (Tmp_Node)
           and then
            Ekind (Tmp_Node) in Einfo.Type_Kind
         then
            Tmp_Node := Parent (Tmp_Node);
         end if;

         if Nkind (Tmp_Node) = N_Subtype_Declaration
           and then
            not Is_Rewrite_Substitution (Tmp_Node)
           and then
            not Comes_From_Source (Tmp_Node)
         then
            Result := Sinfo.Subtype_Indication (Tmp_Node);
            --  In case of nested instantiations, we have to traverse
            --  the chain of subtype declarations created by the compiler
            --  for actual types
            while Is_From_Instance (Result)
               and then
                  Nkind (Parent (Entity (Result))) = N_Subtype_Declaration
               and then
                  not Comes_From_Source (Parent (Entity (Result)))
            loop
               Result := Parent (Entity (Result));

               if Is_Rewrite_Substitution (Result) then
                  --  The case when the actual type is a derived type. Here
                  --  the chain of subtypes leads to the artificial internal
                  --  type created by the compiler, but not to the actual type
                  --  (8924-006)
                  Result := Sinfo.Defining_Identifier (Result);

                  while Present (Homonym (Result)) loop
                     Result := Homonym (Result);
                  end loop;

                  exit;

               end if;

               Result := Sinfo.Subtype_Indication (Result);
            end loop;

         end if;

      end if;

      return Result;

   end Get_Actual_Type_Name;

   ----------------------------
   -- Get_Corr_Called_Entity --
   ----------------------------

   function Get_Corr_Called_Entity
     (Call : Asis.Element)
      return Asis.Declaration
   is
      Arg_Node          : Node_Id;
      Arg_Node_Kind     : Node_Kind;
      Result_Node       : Node_Id;
      Result_Unit       : Compilation_Unit;
      Special_Case      : Special_Cases := Not_A_Special_Case;
      Result_Kind       : Internal_Element_Kinds := Not_An_Element;
      Inherited         : Boolean := False;
      Res_Node_Field_1  : Node_Id := Empty;
      Tmp_Node          : Node_Id;

      Result_El         : Asis.Element;
   begin

      --  The general implementation approach is:
      --
      --  1. First, we try to define Result_Node as pointing to the tree
      --     node on which the resulting ASIS Element should be based.
      --     During this step Arg_Node is also set (and probably adjusted)
      --
      --  2. If the result looks like representing an Ada implicit construct
      --     (for now the main and the only check is
      --     Comes_From_Source (Result_Node)), at the second step we
      --     form the representation of the implicit inherited user-defined
      --     subprogram by setting Result_Node pointing to the explicit
      --     declaration of the subprogram being inherited, and
      --     Res_Node_Field_1 pointing to the defining identifier node
      --     corresponding to the given implicit subprogram. Note, that
      --     at the moment implicit predefined operations are not
      --     implemented.
      --
      --  3. On the last step we compute additional attributes of the
      --     resulting Element.

      ------------------------------------------------------------------
      --  1. Defining Result_Node (and adjusting Arg_Node, if needed) --
      ------------------------------------------------------------------

      Arg_Node      := R_Node (Call);
      Arg_Node_Kind := Nkind (Arg_Node);
      Tmp_Node      := Node (Call);
      --  Rewritten node should know everything. But if in case of a function
      --  call this node is the result of compile-time optimization,
      --  we have to work with original node only:

      if Arg_Node_Kind = N_String_Literal            or else
         Arg_Node_Kind = N_Integer_Literal           or else
         Arg_Node_Kind = N_Real_Literal              or else
         Arg_Node_Kind = N_Character_Literal         or else
         Arg_Node_Kind = N_Raise_Constraint_Error    or else
         Arg_Node_Kind = N_Raise_Program_Error       or else
         Arg_Node_Kind = N_Conditional_Expression    or else
         Arg_Node_Kind = N_Explicit_Dereference      or else
         Arg_Node_Kind = N_Type_Conversion           or else
         Arg_Node_Kind = N_Unchecked_Type_Conversion or else
         Arg_Node_Kind = N_Identifier                or else
        (Arg_Node_Kind in N_Op
         and then
         (Nkind (Tmp_Node) = N_Function_Call
          or else
          (Nkind (Tmp_Node) in N_Op
           and then
          Entity_Present (Tmp_Node)
           and then
          (Pass_Generic_Actual (Parent (Parent ((Entity (Tmp_Node)))))))))
      then
         Arg_Node      := Node (Call);
         Arg_Node_Kind := Nkind (Arg_Node);
      end if;

      case Arg_Node_Kind is

         when  N_Attribute_Reference =>

            return Nil_Element;

            --  call to a procedure-attribute or to a function-attribute
            --  but in case when a representation clause was applied
            --  to define stream IOU attributes, we can return something
            --  more interesting, then Nil_Element, see the corresponding
            --  Aladdin's message

         when  N_Entry_Call_Statement     |
               N_Procedure_Call_Statement |
               N_Function_Call =>
            --  here we have to filter out the case when Nil_Element
            --  should be returned for a call through access-to-function:

            if Nkind (Sinfo.Name (Arg_Node)) = N_Explicit_Dereference then

               return Nil_Element;
            end if;

            if Arg_Node_Kind = N_Entry_Call_Statement then
               Arg_Node := Sinfo.Name (Arg_Node);
               --  Arg_Node points to the name of the called entry

               if Nkind (Arg_Node) = N_Indexed_Component then
                  --  this is the case for a call to an entry from an
                  --  entry family
                  Arg_Node := Prefix (Arg_Node);
               end if;

               Result_Node := Entity (Selector_Name (Arg_Node));

            else
               --  here we have Arg_Node_Kind equal to
               --  N_Procedure_Call_Statement or to N_Function_Call, and this
               --  is the right place to check if this is a dispatching call.
               --  We do not want to use Asis.Extensions.Is_Dispatching_Call
               --  query here to avoid introducing dependency on
               --  Asis.Extensions

               if Present (Controlling_Argument (Arg_Node)) then
                  return Nil_Element;
               end if;

               Arg_Node := Sinfo.Name (Arg_Node);

               if Nkind (Arg_Node) = N_Selected_Component then
                  --  this is the case for calls to protected subprograms
                  Result_Node := Entity (Selector_Name (Arg_Node));
               else
                  Result_Node := Entity (Arg_Node);
               end if;

            end if;

            if No (Result_Node)
              and then
               Arg_Node_Kind = N_Function_Call
              and then
               Is_From_Unknown_Pragma (R_Node (Call))
            then
               return Nil_Element;
            end if;

         when N_Op =>
            --  all the predefined operations (??)
            Result_Node := Entity (Arg_Node);

         when others =>
            pragma Assert (False);
            null;
      end case;

      if Present (Result_Node)
        and then
         not Comes_From_Source (Result_Node)
        and then
         Nkind (Parent (Result_Node)) = N_Defining_Program_Unit_Name
      then
         --  Case of a child subprogram for that an explicit separate spec is
         --  not given. Result_Node points to the defining identifier from
         --  the subprogram spec artificially created by the compiler. We
         --  reset it to point to the proper defining identifier from the
         --  explicitly given body
         Result_Node := Parent (Parent (Parent (Result_Node)));
         pragma Assert (Nkind (Result_Node) = N_Subprogram_Declaration);
         Result_Node := Corresponding_Body (Result_Node);
      end if;

      pragma Assert (Present (Result_Node));

      --  it is possible, that for a subprogram defined by a stub, the
      --  subprogram body declaration from the corresponding subunit is
      --  returned. In this case we have to go to the corresponding
      --  stub (the subprogram body which is the proper body from a
      --  subunit can never be returned as a corresponding called entity)

      Set_Stub_For_Subunit_If_Any (Result_Node);

      if Is_Generic_Instance (Result_Node) then
         Result_Node := Get_Instance_Name (Result_Node);
      end if;

      Tmp_Node := Original_Node (Parent (Parent (Result_Node)));

      while Nkind (Tmp_Node) = N_Subprogram_Renaming_Declaration
          and then
            not (Comes_From_Source (Tmp_Node))
          and then
            not Pass_Generic_Actual (Tmp_Node)
      loop
         --  Result_Node is a defining name from the artificial renaming
         --  declarations created by the compiler in the for wrapper
         --  package for expanded subprogram instantiation. We
         --  have to go to expanded subprogram spec which is renamed.
         --
         --  We have to do this in a loop in case of nested instantiations

         Result_Node := Sinfo.Name   (Tmp_Node);

         if Nkind (Result_Node) = N_Selected_Component then
            Result_Node := Selector_Name (Result_Node);
         end if;

         Result_Node := Entity (Result_Node);

         Tmp_Node := Parent (Parent (Result_Node));
      end loop;

      --  F703-020: operations of an actual type provided for the formal
      --  derived type (we are in the expanded generic)

      if not Comes_From_Source (Result_Node)
        and then
          Present (Alias (Result_Node))
           and then
            not (Is_Intrinsic_Subprogram (Result_Node))
           and then
            Pass_Generic_Actual (Parent (Result_Node))
      then
         --  This means that we have an operation of an actual that corresponds
         --  to the generic formal derived type. In the tree, these operations
         --  are "(re)defined" for the artificial subtype declaration used to
         --  pass the actual type into expanded template. We go one step up
         --  the aliases chain to get to the proper declaration of the type
         --  operation

         Result_Node := Alias (Result_Node);
      end if;

      --  the code below is very similar to what we have in
      --  A4G.Expr_Sem.Identifier_Name_Definition (this name may be changed)!
      --  In future we'll probably have to re-study this again (???)

      --  first, defining the Enclosing Unit and doing the consistency check

      -----------------------------------------------------------
      -- 2. Defining Association_Etype as the type "producing" --
      --    a given implicit construct (if needed)             --
      -----------------------------------------------------------

      --  We have to turn off for a while the full processing of the
      --  implicit elements (Hope to fix this soon).

      if (not Comes_From_Source (Result_Node)
        or else
          Is_Artificial_Protected_Op_Item_Spec (Result_Node))
        and then
           not Pass_Generic_Actual (Parent (Parent (Result_Node)))
      then

         if Present (Alias (Result_Node))
           and then
            Nkind (Original_Node (Parent (Result_Node))) in
              N_Formal_Type_Declaration .. N_Private_Extension_Declaration
         then
            --  ???Is this the right test for implicit inherited user-defined
            --  subprogram???
            Inherited         := True;
            Res_Node_Field_1  := Result_Node;

            while Present (Alias (Result_Node))
                and then
                  not Comes_From_Source (Result_Node)
            loop
               Result_Node := Alias (Result_Node);
            end loop;

         elsif Is_Generic_Instance (Result_Node) then

            Special_Case := Expanded_Subprogram_Instantiation;

         elsif Is_Artificial_Protected_Op_Item_Spec (Result_Node) then
            Result_Node := Corresponding_Body (Parent (Parent (Result_Node)));

         elsif Ekind (Result_Node) = E_Function
               and then
                not Comes_From_Source (Result_Node)
               and then
                Chars (Result_Node) = Name_Op_Ne
               and then
                Present (Corresponding_Equality (Result_Node))
         then
            Special_Case := Is_From_Imp_Neq_Declaration;
--  |A2012 start  SCz
--           elsif Nkind (Original_Node ((Parent (Parent (Result_Node))))) =
--                 N_Expression_Function
--           then
--              null;
--  |A2012 end
         else

            return Nil_Element;
            --  ???!!! this turns off all the predefined operations!!!

         end if;

      end if;

      --  Now, checking if we have a call to an entry/procedure/function of
      --  derived task/protected type
      Tmp_Node := Arg_Node;

      if Nkind (Tmp_Node) = N_Selected_Component then
         Tmp_Node := Prefix (Tmp_Node);
         Tmp_Node := Etype (Tmp_Node);

         if Ekind (Tmp_Node) in Concurrent_Kind then

            while not Comes_From_Source (Original_Node (Parent (Tmp_Node)))
            loop
               Tmp_Node := Etype (Tmp_Node);
            end loop;

            Tmp_Node := Parent (Tmp_Node);

            if Nkind (Tmp_Node) = N_Full_Type_Declaration
              and then
               Nkind (Sinfo.Type_Definition (Tmp_Node)) =
               N_Derived_Type_Definition
            then
               Inherited         := True;
               Res_Node_Field_1  := Tmp_Node;
            end if;

         end if;

      end if;

      if Present (Res_Node_Field_1) then
         Result_Unit :=
            Enclosing_Unit (Encl_Cont_Id (Call), Res_Node_Field_1);
      else
         Result_Unit :=
            Enclosing_Unit (Encl_Cont_Id (Call), Result_Node);
      end if;
      --  ???  should be changed when full processing of implicit elements
      --  will be ready

      --  And now - from a defining name to a declaration itself
      --  (this also may need adjustment for the full implementation
      --  of the implicit stuff)

      if Inherited then

         --  For inherited subprograms we have to set the result kind manually
         --  to get subprogram declarations in case of inheriting from
         --  subprogram ransoming (8728-023)

         if Ekind (Result_Node) = E_Function or else
            Ekind (Result_Node) = E_Operator
         then
            Result_Kind := A_Function_Declaration;
         elsif Ekind (Result_Node) = E_Procedure then
            if Null_Present (Parent (Result_Node)) then
               Result_Kind := A_Null_Procedure_Declaration;
            else
               Result_Kind := A_Procedure_Declaration;
            end if;
         end if;

      end if;

      if Special_Case not in Predefined then

         if Nkind (Result_Node) in N_Entity
          and then
            Ekind (Result_Node) = E_Enumeration_Literal
         then
            --  This happens if an enumeration literal is used as an actual for
            --  a formal function, and if we process the corresponding function
            --  call in the instantiation. See EBB11-004

            Result_Kind := An_Enumeration_Literal_Specification;
         else
            Result_Node := Parent (Result_Node);

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Node := Parent (Result_Node);
            end if;

            if Nkind (Result_Node) = N_Procedure_Specification or else
               Nkind (Result_Node) = N_Function_Specification
            then
               Result_Node := Parent (Result_Node);
            end if;

         end if;

      elsif Special_Case in Predefined then
         Result_Kind := A_Function_Declaration;

      end if;

      Result_El :=
        Node_To_Element_New
          (Node          => Result_Node,
           Node_Field_1  => Res_Node_Field_1,
           Internal_Kind => Result_Kind,
           Spec_Case     => Special_Case,
           Inherited     => Inherited,
           In_Unit       => Result_Unit);

      --  Fix for C125-002: Is_Part_Of_Instance of the result is defined on
      --  the base of Result_Node which points to the explicit subprogram.
      --  That is, if we define the type derived from some other type declared
      --  inside the instance, we will get all its inherited subprograms
      --  being Is_Part_Of_Instance even if the derived type is not declared
      --  inside any instance. And the other way around.

      if Present (Res_Node_Field_1) then

         if Is_From_Instance (Res_Node_Field_1) then
            Set_From_Instance (Result_El, True);
         else
            Set_From_Instance (Result_El, False);
         end if;

      end if;

      return Result_El;
   end Get_Corr_Called_Entity;

   ----------------------
   -- Get_Derived_Type --
   ----------------------

   function Get_Derived_Type
     (Type_Entity     : Entity_Id;
      Inherited_Subpr : Entity_Id)
      return            Entity_Id
   is
      Result            : Entity_Id := Type_Entity;
      Derived_Type      : Entity_Id;
      Next_Derived_Type : Entity_Id;
   begin
      Derived_Type      := Original_Node (Parent (Inherited_Subpr));

      Next_Derived_Type := Derived_Type;

      if Nkind (Next_Derived_Type) = N_Full_Type_Declaration then
         Next_Derived_Type := Sinfo.Type_Definition (Next_Derived_Type);
      elsif Nkind (Next_Derived_Type) = N_Formal_Type_Declaration then
         Next_Derived_Type := Sinfo.Formal_Type_Definition (Next_Derived_Type);
      end if;

      if Nkind (Next_Derived_Type) = N_Formal_Derived_Type_Definition then
         Next_Derived_Type := Sinfo.Subtype_Mark (Next_Derived_Type);
      else
         Next_Derived_Type := Sinfo.Subtype_Indication (Next_Derived_Type);
      end if;

      Derived_Type := Defining_Identifier (Derived_Type);

      if Nkind (Next_Derived_Type) = N_Subtype_Indication then
         Next_Derived_Type := Sinfo.Subtype_Mark (Next_Derived_Type);
      end if;

      Next_Derived_Type := Entity (Next_Derived_Type);

      loop

         if Next_Derived_Type = Type_Entity then
            Result := Derived_Type;
            exit;

         elsif Is_Derived_Type (Next_Derived_Type) then

            Next_Derived_Type := Original_Node (Parent (Next_Derived_Type));

            if Nkind (Next_Derived_Type) = N_Full_Type_Declaration then
               Next_Derived_Type := Sinfo.Type_Definition (Next_Derived_Type);
            end if;

            Next_Derived_Type := Sinfo.Subtype_Indication (Next_Derived_Type);

            if Nkind (Next_Derived_Type) = N_Subtype_Indication then
               Next_Derived_Type := Sinfo.Subtype_Mark (Next_Derived_Type);
            end if;

            Next_Derived_Type := Entity (Next_Derived_Type);

         else
            exit;
         end if;

      end loop;

      return Result;

   end Get_Derived_Type;

   --------------------------
   -- Get_Importing_Pragma --
   --------------------------

   function Get_Importing_Pragma (E : Entity_Id) return Node_Id is
      Result      :          Node_Id := Empty;
      Tmp_Node    :          Node_Id;
      Pragma_Node :          Node_Id;
      Arg_Chars   : constant Name_Id := Chars (E);

   begin
      --  First, check if we have the corresponding pragma in the list of
      --  representation items applied to the argument node:

      Pragma_Node := First_Rep_Item (E);

      while Present (Pragma_Node) loop

         if Is_Importing_Pragma (Pragma_Node, Arg_Chars) then
            Result := Pragma_Node;
            exit;
         else
            Pragma_Node := Next_Rep_Item (Pragma_Node);
         end if;

      end loop;

      if No (Result) then
         --  That means that Import or Interface pragma is applied to an
         --  overloaded entities
         Pragma_Node := Next (Parent (Parent (E)));

         while Present (Pragma_Node) loop

            if Is_Importing_Pragma (Pragma_Node, Arg_Chars) then
               Result := Pragma_Node;
               exit;
            else
               Next (Pragma_Node);
            end if;

         end loop;

      end if;

      if No (Result) then
         Tmp_Node := Parent (Parent (Parent (E)));

         if Nkind (Tmp_Node) = N_Package_Specification
           and then
            List_Containing (Parent (Parent (E))) =
              Visible_Declarations (Tmp_Node)
         then
            --  this is a somewhat exotic case - a subprogram declaration in
            --  the visible part of a package spec, and the corresponding
            --  pragma is in the corresponding private part.
            Pragma_Node := First (Private_Declarations (Tmp_Node));

            while Present (Pragma_Node) loop

               if Is_Importing_Pragma (Pragma_Node, Arg_Chars) then
                  Result := Pragma_Node;
                  exit;
               else
                  Next (Pragma_Node);
               end if;

            end loop;

         end if;

      end if;

      pragma Assert (Present (Result));
      return Result;
   end Get_Importing_Pragma;

   -----------------------
   -- Get_Instance_Name --
   -----------------------

   function Get_Instance_Name (Int_Name : Node_Id) return Node_Id is
      Result_Node : Node_Id := Empty;
      Decl_Node   : Node_Id;
   begin

      Decl_Node := Parent (Int_Name);

      if Nkind (Decl_Node) = N_Defining_Program_Unit_Name then
         Decl_Node := Parent (Decl_Node);
      end if;

      Decl_Node := Parent (Decl_Node);

      if Nkind (Decl_Node) = N_Subprogram_Declaration then
         Decl_Node := Parent (Parent (Decl_Node));
      end if;

      if (not Is_List_Member (Decl_Node)
        and then
          not Is_Rewrite_Substitution (Decl_Node))
         or else
          (Is_List_Member (Decl_Node)
          and then
           Nkind (Original_Node (Decl_Node)) = N_Formal_Package_Declaration)
      then
         --  The first condition corresponds to the case when a library
         --  package is instantiated at library level - no artificial package
         --  is created in this case.
         --  The second condition corresponds to the defining name from
         --  a formal package declaration (it is also classified as
         --  Is_Generic_Instance)

         return Int_Name;

      end if;
      --  now Decl_Node points to the declaration of an artificial package
      --  created by the compiler for the instantiation

      if Is_Rewrite_Substitution (Decl_Node) then
         Decl_Node := Original_Node (Decl_Node);

         if Is_Rewrite_Substitution (Decl_Node) then
            --  The node can be rewritten twice in case when a library-level
            --  instantiation is a supporter of a main unit, and the expanded
            --  body of this instantiation is required according to Lib (h),
            --  see 9418-015, 9416-A01 and 9426-A13
            Decl_Node := Original_Node (Decl_Node);
         end if;

         if Nkind (Original_Node (Decl_Node)) =
               N_Formal_Package_Declaration
         then
            Result_Node := Defining_Identifier (Original_Node (Decl_Node));
         else
            Result_Node := Defining_Unit_Name (Original_Node (Decl_Node));
         end if;

      else

         Decl_Node := Next_Non_Pragma (Decl_Node);

         while Present (Decl_Node) loop
            if Nkind (Decl_Node) in N_Generic_Instantiation then
               Result_Node := Defining_Unit_Name (Decl_Node);
               exit;

            else
               Decl_Node := Next_Non_Pragma (Decl_Node);
            end if;

         end loop;

      end if;

      pragma Assert (Present (Result_Node));

      return Result_Node;

   end Get_Instance_Name;

   ------------------
   -- Is_Anonymous --
   ------------------

   function Is_Anonymous (E : Entity_Kind) return Boolean is
      Result : Boolean := False;
   begin
      case E is
         when E_Anonymous_Access_Subprogram_Type           |
              E_Anonymous_Access_Protected_Subprogram_Type |
              E_Anonymous_Access_Type                      =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Anonymous;

   -------------------
   -- Is_Applied_To --
   -------------------

   function Is_Applied_To
     (Pragma_Node : Node_Id;
      Entity_Node : Entity_Id)
      return        Boolean
   is
      Result      : Boolean := False;
      Pragma_Arg  : Node_Id := Empty;
      Entity_Decl : Node_Id;
   begin

      case Pragma_Name (Pragma_Node) is

         --  Cases when the second pragma argument indicates the entity
         --  the pragma is applied to:
         when Name_Component_Alignment |
              Name_Convention          |
              Name_Export              |
              Name_External            |
              Name_Import              |
              Name_Interface           =>

            Pragma_Arg := First (Pragma_Argument_Associations (Pragma_Node));
            Pragma_Arg := Sinfo.Expression (Next (Pragma_Arg));

            if Entity (Pragma_Arg) = Entity_Node
             or else
               Chars (Pragma_Arg) = Chars (Entity_Node)
            then
               Result := True;
            end if;

         --  Cases when a pragma may have several arguments, and any of then
         --  may indicate the entity the pragma is applied to
         when Name_Inline               |
              Name_Inline_Always        |
              Name_No_Return            |
              Name_Unmodified           |
              Name_Unreferenced         |
              Name_Unreferenced_Objects =>
            Pragma_Arg := First (Pragma_Argument_Associations (Pragma_Node));

            while Present (Pragma_Arg) loop
               Pragma_Arg := Sinfo.Expression (Pragma_Arg);

               if Entity (Pragma_Arg) = Entity_Node
                or else
                  Chars (Pragma_Arg) = Chars (Entity_Node)
               then
                  Result := True;
                  exit;
               end if;

               Pragma_Arg := Next (Parent (Pragma_Arg));
            end loop;

         --  Cases when only the first argument of a pragma may indicate the
         --  entity the pragma is applied to
         when --  GNAT-specific pragmas first
              Name_Common_Object                |
              Name_Complex_Representation       |
              Name_CPP_Class                    |
              Name_CPP_Constructor              |
              Name_Export_Exception             |
              Name_Export_Function              |
              Name_Export_Object                |
              Name_Export_Procedure             |
              Name_Export_Valued_Procedure      |
              Name_Favor_Top_Level              |
              Name_Finalize_Storage_Only        |
              Name_Import_Exception             |
              Name_Import_Function              |
              Name_Import_Object                |
              Name_Import_Procedure             |
              Name_Import_Valued_Procedure      |
              Name_Inline_Generic               |
              Name_Interface_Name               |
              Name_Keep_Names                   |
              Name_Linker_Alias                 |
              Name_Linker_Constructor           |
              Name_Linker_Destructor            |
              Name_Linker_Section               |
              Name_Machine_Attribute            |
              Name_No_Strict_Aliasing           |
              Name_Persistent_BSS               |
              Name_Psect_Object                 |
              Name_Pure_Function                |
              Name_Shared                       |
              Name_Stream_Convert               |
              Name_Suppress_Initialization      |
              Name_Task_Storage                 |
              Name_Universal_Aliasing           |
              Name_Weak_External                |
              --  Standard Ada 2005 pragmas
              Name_Asynchronous                 |
              Name_Atomic                       |
              Name_Atomic_Components            |
              Name_Attach_Handler               |
              Name_Controlled                   |
              Name_Discard_Names                |
              Name_Interrupt_Handler            |
              Name_Pack                         |
              Name_Preelaborable_Initialization |
              Name_Unchecked_Union              |
              Name_Volatile                     |
              Name_Volatile_Components          =>
            Pragma_Arg := First (Pragma_Argument_Associations (Pragma_Node));
            Pragma_Arg := Sinfo.Expression (Pragma_Arg);

            if Entity (Pragma_Arg) = Entity_Node
             or else
               Chars (Pragma_Arg) = Chars (Entity_Node)
            then
               Result := True;
            end if;

         --  Cases when a specific processing is needed
         when Name_Float_Representation =>
            Pragma_Arg := First (Pragma_Argument_Associations (Pragma_Node));

            if Present (Next (Pragma_Arg)) then
               Pragma_Arg := Next (Pragma_Arg);
            end if;

            Pragma_Arg := Sinfo.Expression (Pragma_Arg);

            if Entity (Pragma_Arg) = Entity_Node
             or else
               Chars (Pragma_Arg) = Chars (Entity_Node)
            then
               Result := True;
            end if;

         when Name_Obsolescent =>

            if Is_Obsolescent (Entity_Node) then
               --  This pragma may or may not contain the reference to the
               --  entity it is applied to. The pragma may or may not contain
               --  arguments
               if Present (Pragma_Argument_Associations (Pragma_Node))
                 and then
                  List_Length (Pragma_Argument_Associations (Pragma_Node)) >= 2
               then
                  Pragma_Arg :=
                    First (Pragma_Argument_Associations (Pragma_Node));
                  Pragma_Arg := Sinfo.Expression (Pragma_Arg);
               end if;

               if No (Pragma_Arg)
                 or else
                  Chars (Pragma_Arg) = Chars (Entity_Node)
               then
                  --  here we have to check if the pragma immediately follows
                  --  the declaration that defines Entity_Node, or the pragma
                  --  is the first declarative element in the package spec and
                  --  Entity_Node defines this package. Pragma_Arg is used as
                  --  temporary node below
                  Pragma_Arg := Prev (Pragma_Node);

                  if Present (Pragma_Arg) then
                     --  Go to the declaration that declares Entity_Node
                     Entity_Decl := Parent (Entity_Node);

                     while Present (Entity_Decl)
                        and then
                           not Is_List_Member (Entity_Decl)
                     loop
                        Entity_Decl := Parent (Entity_Decl);
                     end loop;

                     Result := Entity_Decl = Pragma_Arg;
                  else
                     --  With the current implementation of the ASIS
                     --  Corresponding_Pragmas query this code never works!

                     --  Check if the pragma Obsolescent is the program unit
                     --  pragma:
                     Pragma_Arg := Parent (Pragma_Node);

                     if Nkind (Pragma_Arg) = N_Package_Specification then

                        if Nkind (Parent (Pragma_Arg)) =
                           N_Package_Declaration
                        then
                           --  To filter out the case of generic packages
                           Pragma_Arg := Defining_Unit_Name (Pragma_Arg);

                           if Nkind (Pragma_Arg) =
                              N_Defining_Program_Unit_Name
                           then
                              Pragma_Arg := Defining_Identifier (Pragma_Arg);
                           end if;

                           Result := Pragma_Arg = Entity_Node;
                        end if;

                     end if;

                  end if;

               else
                  --  With the current implementation of the ASIS
                  --  Corresponding_Pragmas query this code never works!

                  --  Case when a pragma may be applied to an enumeration
                  --  literal.

                  if Ekind (Entity_Node) = E_Enumeration_Literal then
                     Entity_Decl := Parent (Parent (Entity_Node));

                     Result := Next (Entity_Decl) = Pragma_Node;
                  end if;
               end if;

            end if;

         --  All the other pragmas cannot be a part of the result
         when others =>
            null;
      end case;

      return Result;
   end Is_Applied_To;

   ------------------------------------------
   -- Is_Artificial_Protected_Op_Item_Spec --
   ------------------------------------------

   function Is_Artificial_Protected_Op_Item_Spec
     (E :    Entity_Id)
      return Boolean
   is
      Arg    : Entity_Id := E;
      Result : Boolean   := False;
   begin
      if Nkind (Arg) = N_Defining_Identifier then
         --  No need to consider defining expanded names

         if Ekind (Arg) in Formal_Kind then
            Arg := Parent (Parent (Arg));

            if Nkind (Arg) in N_Subprogram_Specification then
               Arg := Defining_Unit_Name (Arg);
            end if;

         end if;

         if Nkind (Arg) in N_Entity
           and then
            (Ekind (Arg) in Formal_Kind or else Ekind (Arg) in Subprogram_Kind)
           and then
            not Comes_From_Source (Parent (Arg))
           and then
            Nkind (Parent (Parent (Parent (Arg)))) = N_Protected_Body
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Artificial_Protected_Op_Item_Spec;

   -------------------------
   -- Is_Derived_Rep_Item --
   -------------------------

   function Is_Derived_Rep_Item
     (Type_Entity : Entity_Id;
      Rep_Item :    Node_Id)
      return        Boolean
   is
      Result   : Boolean := True;
      Type_Ard : Node_Id := Empty;
   begin

      case Nkind (Rep_Item) is

         when N_Attribute_Definition_Clause =>

            if Entity (Sinfo.Name (Rep_Item)) = Type_Entity then
               Result := False;
            end if;

         when N_Pragma =>

            Type_Ard := Sinfo.Expression
                          (First (Pragma_Argument_Associations (Rep_Item)));

            if Entity (Type_Ard) = Type_Entity then
               Result := False;
            end if;

         when N_Enumeration_Representation_Clause |
              N_Record_Representation_Clause =>

            if Entity (Sinfo.Identifier (Rep_Item)) = Type_Entity then
               Result := False;
            end if;

         when  others =>
            null;
            pragma Assert (False);
      end case;

      return Result;
   end Is_Derived_Rep_Item;

   ----------------------
   -- Is_From_Instance --
   ----------------------

   function Is_From_Instance (Node : Node_Id) return Boolean is
   begin

      return
        (Sloc (Node) > Standard_Location
        and then
         Instantiation (Get_Source_File_Index (Sloc (Node))) /= No_Location)
      or else
        (Present (Parent (Node))
        and then
         Nkind (Parent (Node)) = N_Package_Specification
        and then
         Is_From_Instance ((Parent (Node))));

   end Is_From_Instance;

   ---------------------------------
   -- Is_From_Rewritten_Aggregate --
   ---------------------------------

   function Is_From_Rewritten_Aggregate (Node : Node_Id) return Boolean is
      Result    : Boolean := False;
      Next_Aggr : Node_Id;
   begin
      if Nkind (Node) = N_Component_Association then
         Next_Aggr := Parent (Node);

         while Nkind (Next_Aggr) = N_Aggregate
           or else
               Nkind (Next_Aggr) = N_Extension_Aggregate
         loop
            if Is_Rewrite_Substitution (Next_Aggr) then
               Result := True;
               exit;
            end if;

            Next_Aggr := Parent (Next_Aggr);
         end loop;
      end if;

      return Result;
   end Is_From_Rewritten_Aggregate;

   ----------------------------
   -- Is_From_Unknown_Pragma --
   ----------------------------

   function Is_From_Unknown_Pragma (Node : Node_Id) return Boolean is
      Result : Boolean := False;
      Tmp    : Node_Id := Parent (Node);
      N      : Name_Id;
   begin
      while Nkind (Tmp) /= N_Compilation_Unit loop

         case Nkind (Tmp) is

            when N_Pragma =>

               N := Pragma_Name (Tmp);

               --  See Snames.Get_Pragma_Id
               if not (
                     N in First_Pragma_Name .. Last_Pragma_Name
                    or else
                     N = Name_AST_Entry
                    or else
                     N = Name_Interface
                    or else
                     N = Name_Priority
                    or else
                     N = Name_Storage_Size
                    or else
                     N = Name_Storage_Unit)
               then
                  Result := True;
               end if;

               exit;

            when N_Statement_Other_Than_Procedure_Call |
                 N_Procedure_Call_Statement            |
                 N_Representation_Clause               |
                 N_Component_Declaration ..
                 N_Generic_Procedure_Renaming_Declaration =>

               exit;

            when others =>
               Tmp := Parent (Tmp);
         end case;

      end loop;

      return Result;
   end Is_From_Unknown_Pragma;

   -----------------
   -- Is_Impl_Neq --
   -----------------

   function Is_Impl_Neq (Def_Op : Entity_Id) return Boolean is
      Result : Boolean := False;
   begin

      if Nkind (Def_Op) in N_Entity
          and then Ekind (Def_Op) = E_Function
          and then not Comes_From_Source (Def_Op)
          and then Chars (Def_Op) = Name_Op_Ne
        and then Present (Corresponding_Equality (Def_Op))
      then
         Result := True;
      end if;

      return Result;
   end Is_Impl_Neq;

   -------------------------
   -- Is_Importing_Pragma --
   -------------------------

   function Is_Importing_Pragma
     (N        : Node_Id;
      For_Name : Name_Id)
      return     Boolean
   is
      Result : Boolean := False;
      Tmp    : Node_Id;
   begin

      if Nkind (N) = N_Pragma
       and then
        (Pragma_Name (N) = Name_Import
        or else
         Pragma_Name (N) = Name_Interface)
      then
         Tmp := First (Pragma_Argument_Associations (N));
         Tmp := Sinfo.Expression (Next (Tmp));

         Result := Chars (Tmp) = For_Name;
      end if;

      return Result;
   end Is_Importing_Pragma;

   ------------------------------------
   -- Is_Name_Of_Expanded_Subprogram --
   -------------------------------------

   function Is_Name_Of_Expanded_Subprogram (Node : Node_Id) return Boolean is
      Result : Boolean := False;
   begin
      if Nkind (Node) = N_Defining_Identifier
        and then
         Is_Generic_Instance (Node)
        and then
         Ekind (Node) in E_Function .. E_Procedure
      then
         Result := True;
      end if;

      return Result;
   end Is_Name_Of_Expanded_Subprogram;

   -------------------
   -- Is_Predefined --
   -------------------

   function Is_Predefined (Def_Op : Node_Id) return Boolean is
      Result : Boolean := False;
      Tmp    : Entity_Id;
   begin

      if Ekind (Def_Op) in E_Function .. E_Operator
        and then
         not Comes_From_Source (Def_Op)
        and then
         not Is_Impl_Neq (Def_Op)
      then

         if Sloc (Def_Op) <= Standard_Location
           or else
            No (Alias (Def_Op))
           or else
            No (Parent (Def_Op))
         then
            Result := True;

         elsif Present (Alias (Def_Op)) then
            Tmp := Alias (Def_Op);

            while Present (Alias (Tmp)) loop
               Tmp := Alias (Tmp);
            end loop;

            if not Comes_From_Source (Tmp)
              and then
               No (Parent (Tmp))
            then
               Result := True;
            end if;

         end if;

      end if;

      return Result;
   end Is_Predefined;

   ------------------------------
   -- Is_Range_Memberchip_Test --
   ------------------------------

   function Is_Range_Memberchip_Test (E : Asis.Element) return Boolean is
      Tmp    : Asis.Element;
      Result : Boolean := False;
   begin
      if No (Alternatives (Node (E))) then
         Tmp    := Membership_Test_Choices (E) (1);
         Result := Constraint_Kind (Tmp) in
           A_Range_Attribute_Reference .. A_Simple_Expression_Range;
      end if;

      return Result;
   end Is_Range_Memberchip_Test;

   -----------------------------
   -- Is_Type_Memberchip_Test --
   -----------------------------

   function Is_Type_Memberchip_Test (E : Asis.Element) return Boolean is
      Tmp_El : Asis.Element;
      Result : Boolean := False;
   begin
      if No (Alternatives (Node (E))) then
         Tmp_El := Membership_Test_Choices (E) (1);

         case Expression_Kind (Tmp_El) is
            when An_Identifier          |
                 A_Selected_Component   |
                 An_Attribute_Reference =>
               Tmp_El := Normalize_Reference (Tmp_El);
               Result := Is_Type (Entity (R_Node (Tmp_El)));
            when others => null;
         end case;

      end if;

      return Result;
   end Is_Type_Memberchip_Test;

   -----------------------
   -- Limited_View_Kind --
   -----------------------

   function Limited_View_Kind
     (Decl : Asis.Element)
      return Internal_Element_Kinds
   is
      Result   : Internal_Element_Kinds := Int_Kind (Decl);
      Type_Def : Asis.Element;
   begin
      case Result is
         when A_Private_Extension_Declaration =>
            Result := A_Tagged_Incomplete_Type_Declaration;

         when A_Task_Type_Declaration       |
              A_Protected_Type_Declaration  =>
            Result := An_Incomplete_Type_Declaration;

         when An_Ordinary_Type_Declaration |
                 A_Private_Type_Declaration  =>
               Type_Def := Type_Declaration_View (Decl);

               case Int_Kind (Type_Def) is
                  when A_Derived_Record_Extension_Definition |
                       A_Tagged_Record_Type_Definition       |
                       Internal_Interface_Kinds              |
                       A_Tagged_Private_Type_Definition      =>
                     Result := A_Tagged_Incomplete_Type_Declaration;
                  when others =>
                     Result := An_Incomplete_Type_Declaration;
               end case;

         when others =>
            null;
      end case;

      return Result;
   end Limited_View_Kind;

   -------------------------
   -- Pass_Generic_Actual --
   -------------------------

   function Pass_Generic_Actual (N : Node_Id) return Boolean is
      Arg_Node : constant Node_Id := Original_Node (N);
      Result   : Boolean           := False;
   begin
      --  See the discussion in F424-031 and F427-008
      case Nkind (Arg_Node) is
         when N_Subtype_Declaration =>
            Result :=
               not Comes_From_Source (Arg_Node)
             and then
               not Is_Internal_Name (Chars (Defining_Identifier (Arg_Node)))
             and then
               Is_From_Instance (Defining_Identifier (Arg_Node));

         when N_Subprogram_Renaming_Declaration =>
            Result := Present (Corresponding_Formal_Spec (Arg_Node));
         when N_Object_Renaming_Declaration |
              N_Object_Declaration          =>
            Result :=
                Present (Corresponding_Generic_Association (Arg_Node))
              or else
                (not Comes_From_Source (Arg_Node)
                and then
                 Is_From_Instance (Defining_Identifier (Arg_Node)));
         when N_Formal_Object_Declaration =>
            --  Here we should correctly process the situation in the expanded
            --  spec that corresponds to a formal package. In case if the
            --  given generic formal parameter of the formal package is not
            --  specified in the formal package declaration, the corresponding
            --  parameter is presented in the expanded spec as a formal
            --  parameter, but not as a renaming
            Result :=
                Is_From_Instance (Arg_Node)
              and then
                Comes_From_Source (Arg_Node)
              and then
                not Comes_From_Source (Defining_Identifier (Arg_Node));

         when others =>
            null;
      end case;

      return Result;
   end Pass_Generic_Actual;

   ---------------------------------
   -- Part_Of_Pass_Generic_Actual --
   ---------------------------------

   function Part_Of_Pass_Generic_Actual (N : Node_Id) return Boolean is
      Result : Boolean := Pass_Generic_Actual (N);
      Tmp_N  : Node_Id := Parent (N);
   begin

      if not Result then

         while Present (Tmp_N) loop

            if Pass_Generic_Actual (Tmp_N) then
               Result := True;
               exit;
            else

               case Nkind (Tmp_N) is
                  --  The idea is to stop tree traversing as soon as possible
                  when N_Statement_Other_Than_Procedure_Call |
                      N_Renaming_Declaration                 |
                      N_Later_Decl_Item                      |
                      N_Component_Declaration ..
                        N_Private_Type_Declaration           |
                      N_Formal_Subprogram_Declaration        =>
                     exit;
                  when others =>
                     null;
               end case;

            end if;

            Tmp_N := Parent (Tmp_N);
         end loop;

      end if;

      return Result;
   end Part_Of_Pass_Generic_Actual;

   --------------------------------------------
   -- Represents_Class_Wide_Type_In_Instance --
   --------------------------------------------

   function Represents_Class_Wide_Type_In_Instance
     (N    : Node_Id)
      return Boolean
   is
      Result : Boolean := False;
      A_Node : Node_Id;
   begin
      if Nkind (N) = N_Identifier then
         A_Node := Associated_Node (N);

         if Present (A_Node)
           and then
            Nkind (A_Node) in N_Entity
           and then
            Ekind (A_Node) in E_Class_Wide_Type .. E_Class_Wide_Subtype
         then
            Result := True;
         end if;
      end if;

      return Result;
   end Represents_Class_Wide_Type_In_Instance;

   --------------------------------------
   -- Represents_Base_Type_In_Instance --
   --------------------------------------

   function Represents_Base_Type_In_Instance (N : Node_Id) return Boolean is
      Result : Boolean := False;
   begin
      if Nkind (N) = N_Identifier
        and then
         not Comes_From_Source (N)
        and then
          Is_Internal_Name (Chars (N))
        and then
         Present (Associated_Node (N))
        and then
         Ekind (Associated_Node (N)) in
           E_Enumeration_Type .. E_Floating_Point_Subtype
      then
         Result := True;
      end if;

      return Result;
   end Represents_Base_Type_In_Instance;

   --------------------
   -- Reset_For_Body --
   --------------------

   procedure Reset_For_Body
     (El        : in out Asis.Element;
      Body_Unit : Asis.Compilation_Unit)
   is
      Spec_CU   : constant Unit_Id    := Encl_Unit_Id (El);
      Arg_Tree  : constant Tree_Id    := Encl_Tree (El);
      Body_Tree : Tree_Id;
      Result_El : Asis.Element := Nil_Element;

      --  and the rest of the local declarations is needed for traversal
      Spec_El  : Asis.Element;

      My_State : No_State              := Not_Used;
      Control  : Asis.Traverse_Control := Continue;

      procedure Pre_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);

      procedure Pre_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
         pragma Unreferenced (State);

         El_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      begin

         case El_Kind is
            when A_Task_Type_Declaration         |
                 A_Single_Task_Declaration       |
                 An_Incomplete_Type_Declaration  |
                 A_Procedure_Declaration         |
                 A_Function_Declaration          |
                 An_Entry_Declaration            |
                 A_Generic_Procedure_Declaration |
                 A_Generic_Function_Declaration
               =>
               --  here we have declarations which may have completion in the
               --  package body, but their subcomponents cannot have a
               --  completion

               if Is_Equal (Element, El) then
                  Result_El := Element;
                  Control := Terminate_Immediately;
               else
                  Control := Abandon_Children;
               end if;

            when A_Protected_Type_Declaration    |
                 A_Single_Protected_Declaration  |
                 A_Package_Declaration           |
                 A_Generic_Package_Declaration
               =>
               --  here we have declarations which may have completion in the
               --  package body, their subcomponents also can have a completion

               if Is_Equal (Element, El) then
                  Result_El := Element;
                  Control := Terminate_Immediately;
               end if;

            when A_Protected_Definition =>
               Control := Continue;
               --  To look for protected entries and subprograms

            when others =>
               Control := Abandon_Children;
         end case;

      end Pre_Op;

      procedure Find_For_Reset is new Traverse_Element
        (State_Information => No_State,
         Pre_Operation     => Pre_Op,
         Post_Operation    => No_Op);

   begin
      Reset_Tree_For_Unit (Body_Unit);
      Body_Tree := Get_Current_Tree;

      if Arg_Tree = Body_Tree then
         return;
      end if;

      Spec_El := Node_To_Element_New
                   (Node             => Unit (Top (Spec_CU)),
                    Starting_Element => El);

      Find_For_Reset (Spec_El, Control, My_State);

      pragma Assert (not Is_Nil (Result_El));

      El := Result_El;

   end Reset_For_Body;

   ---------------------------------
   -- Set_Stub_For_Subunit_If_Any --
   ---------------------------------

   procedure Set_Stub_For_Subunit_If_Any (Def_Name : in out Node_Id)
   is
      Stub_Node    : Node_Id;
      Decl_Node    : Node_Id;
      Node_Context : constant Node_Id := Parent (Parent (Parent (Def_Name)));
   begin

      if not (Nkind (Def_Name) = N_Defining_Identifier               and then
              Nkind (Node_Context) = N_Subunit                       and then
              Nkind (Proper_Body (Node_Context)) = N_Subprogram_Body and then
              Def_Name =  Defining_Unit_Name (Specification
                (Proper_Body (Node_Context))))
      then
         --  nothing to change
         return;

      else
         Def_Name := Defining_Unit_Name
                       (Specification (Corresponding_Stub (Node_Context)));
         Stub_Node := Parent (Parent (Def_Name));
         Decl_Node := Corr_Decl_For_Stub (Stub_Node);

         if Present (Decl_Node) then
            Def_Name := Defining_Unit_Name (Specification (Decl_Node));
         end if;

      end if;

   end Set_Stub_For_Subunit_If_Any;

   ---------------------
   -- Unwind_Renaming --
   ---------------------

   function Unwind_Renaming (Def_Name : Node_Id) return Node_Id is
      Parent_Decl : Node_Id;
      Result_Node : Node_Id;
   begin
      --  a recursive algorithm is probably not the most effective,
      --  but it is easy-to-maintain. Moreover, we do not really
      --  expect long renaming chains in not-crazy programs
      --  When the implementation of this function is stable, we probably
      --  should replace the recursive code by the iteration-based code

      Result_Node := Def_Name;
      Parent_Decl := Parent (Result_Node);

      case Nkind (Parent_Decl) is

         when N_Renaming_Declaration =>
            --  unwinding once again
            Result_Node := Sinfo.Name (Entity (Parent_Decl));

            return Unwind_Renaming (Result_Node);

         when N_Function_Specification | N_Procedure_Specification =>
            --  two cases are possible: if this subprogram specification
            --  is the component of another (subprogram) renaming
            --  declaration, we should unwind again,
            --  otherwise we have got the result:

            if Nkind (Parent (Parent_Decl)) =
               N_Subprogram_Renaming_Declaration
            then
               --  unwinding once again
               --  Result_Node := Sinfo.Name (Entity (Parent (Parent_Decl)));
               Result_Node := Entity (Sinfo.Name (Parent (Parent_Decl)));

               return Unwind_Renaming (Result_Node);

            else

               if Is_Rewrite_Substitution (Parent (Parent_Decl)) and then
                  Nkind (Original_Node (Parent (Parent_Decl))) =
                                        N_Subprogram_Renaming_Declaration
               then
                  --  this means, that we have met the renaming of a
                  --  subprogram-attribute, so
                  return Empty;

               else
                  --  all the ransoming (if any) have already been unwounded
                  return Result_Node;

               end if;

            end if;

         when others =>

            return Result_Node;

      end case;

   end Unwind_Renaming;

end A4G.A_Sem;
