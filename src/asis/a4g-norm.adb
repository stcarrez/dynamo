------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                             A 4 G . N O R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
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

with Asis.Declarations; use  Asis.Declarations;
with Asis.Elements;     use  Asis.Elements;
with Asis.Expressions;  use  Asis.Expressions;
with Asis.Statements;   use  Asis.Statements;

with Asis.Set_Get;      use  Asis.Set_Get;

with A4G.Asis_Tables;   use A4G.Asis_Tables;
with A4G.A_Debug;       use A4G.A_Debug;
with A4G.A_Output;      use A4G.A_Output;
with A4G.A_Types;       use A4G.A_Types;
with A4G.Contt.UT;      use A4G.Contt.UT;
with A4G.Int_Knds;      use A4G.Int_Knds;
with A4G.Itests;        use A4G.Itests;
with A4G.Mapping;       use A4G.Mapping;

with Atree;             use Atree;
with Einfo;             use Einfo;
with Namet;             use Namet;
with Nlists;            use Nlists;
with Output;            use Output;
with Sinfo;             use Sinfo;

package body A4G.Norm is

   use Asis;

   ----------------------
   --  Local functions --
   ----------------------

   function Get_Type_With_Discr (Constr_Node : Node_Id) return Node_Id;
   --  for a node of N_Index_Or_Discriminant_Constraint type, which
   --  represents a discriminant constraint, this function yields the
   --  node representing the type declaration with a known discriminant
   --  part, where discriminants being constrained are defined. The
   --  intent is to get the right discriminant declarations (as subcomponents
   --  of the results), so these discriminant declarations may be implicit,
   --  as well as the type represented by the result node.
   --
   --  NOTE: this function looks like a particular case of some more
   --        general function useful for finding out the implicit
   --        components of a derived type

   function Unwind_Till_Discriminants (Type_Decl : Node_Id) return Node_Id;
   --  Starting from the node representing the declaration of access or
   --  derived type (ether formal or "normal"), it gives the type
   --  declaration node representing the designated or ancestor type
   --  (unwinding all the intermediate access or derived types) which
   --  has discriminant declarations. This function is supposed to be called
   --  in the situation, when it is known for sure, that the desired
   --  type indeed has discriminants
   --
   --  NOTE: this function looks like a particular case of some more
   --        general function useful for finding out the first non-access
   --        designated/non-derived ancestor type for a given access/derived
   --        type

   ----------------------------
   -- Defining_Gen_Parameter --
   ----------------------------

   function Defining_Gen_Parameter (Gen_Form_Par : Node_Id) return Node_Id is
      Result_Node   : Node_Id;
      Gen_Form_Pars : List_Id;
      Next_Par      : Node_Id;
   begin
      --  ONLY PARTIAL IMPLEMENTATION FOR OPERATOR_SYMBOL!!!!

      --  traversing the tree till the declaration of the corresponding
      --  generic unit:

      Result_Node := Parent (Gen_Form_Par); --  generic association
      Result_Node := Parent (Result_Node);  --  generic instantiation
      Result_Node := Sinfo.Name (Original_Node (Result_Node));

      --  the name of the generic  unit being instantiated
      Result_Node := Entity (Result_Node);

      --  the defining name of the generic  unit being instantiated
      while Nkind (Original_Node (Result_Node)) not in
            N_Generic_Declaration
      loop
         Result_Node := Parent (Result_Node);
      end loop;

      Result_Node := Original_Node (Result_Node); --  ???

      --  the node representing the declaration of the corresponding generic
      --  unit, but this is a rewritten node in the current GNAT model for
      --  generics, therefore:

      --  looking for the defining occurrence of the corresponding
      --  parameter:

      Gen_Form_Pars := Generic_Formal_Declarations (Result_Node);
      Next_Par      := First_Non_Pragma (Gen_Form_Pars);

      while Present (Next_Par) loop
         --  !!! only partial implementation for generic parameters which
         --  !!! are generic functions-operators
         if Nkind (Next_Par) in N_Formal_Subprogram_Declaration and then
            Nkind (Specification (Next_Par)) = N_Function_Specification
         then
            Result_Node := Defining_Unit_Name (Specification (Next_Par));
            --  cannot have a prefix!!!
            if Chars (Result_Node) = Chars (Gen_Form_Par) then
               --  note, that this is only a "very partial" solution
               --  for generic "+" and "-" functions!!!
               return Result_Node;
            end if;
         end if;

         Next_Par := Next_Non_Pragma (Next_Par);

      end loop;

      return Empty;
      --  to raise an exception in the calling context if we are wrong
   end Defining_Gen_Parameter;

   --------------------
   -- Discr_Def_Name --
   --------------------

   function Discr_Def_Name
     (Association : Asis.Discriminant_Association)
      return        Asis.Defining_Name
   is
      Result_Node     : Node_Id;
      Result_Unit     : Asis.Compilation_Unit;
      Result_Element  : Asis.Defining_Name;
      Inherited_Discr : Boolean;
   begin
      --  a normalized association contains the node on which the result
      --  should be built as the Node field of Association, see the
      --  documentation in the body of Normalized_Discriminant_Associations.
      --  So we may start for creating the "rough" version of Result_Element:

      Result_Node := Node (Association);
      Result_Unit := Enclosing_Unit (Encl_Cont_Id (Association), Result_Node);

      Result_Element := Node_To_Element_New
                          (Node          => Result_Node,
                           Internal_Kind => A_Defining_Identifier,
                           In_Unit       => Result_Unit);

      --  and now we have to correct, if needed, the fields
      --  Is_Part_Of_Implicit, Is_Part_Of_Inherited and Is_Part_Of_Instance
      --  of Result_Element, which are set False by default

      Inherited_Discr := Is_Inherited_Discriminant (Result_Node);

      Set_From_Implicit  (Result_Element, Inherited_Discr);
      Set_From_Inherited (Result_Element, Inherited_Discr);
      --  for discriminants, to be implicit and to be inherited mean the
      --  same
      Set_From_Instance  (Result_Element, Is_From_Instance (Result_Node));

      return Result_Element;
   end Discr_Def_Name;

   -------------------------
   -- Get_Type_With_Discr --
   -------------------------

   function Get_Type_With_Discr (Constr_Node : Node_Id) return Node_Id is
      Result_Node    : Node_Id;
      Curr_Node      : Node_Id;
      Curr_Node_Kind : Node_Kind;
   begin

      -------------------------
      -- Implementation Note --
      -------------------------
      --  The current implementation definitely contains errors and it
      --  is far from being perfect.
      --
      --  For now, at least the problem with the formal types is known
      --  as a real source for ASIS crashes. The problem is that GNAT
      --  rewrites completely all the tree structures for generic
      --  declarations, and the original structures ARE NOT completely
      --  decorated. And (at least, now) ASIS works with original
      --  tree structures only. So some semantic node fields may be
      --  Empty in the original tree structures corresponding to
      --  generics. (ASIS will crash, when a discriminant constraint
      --  being analyzed is in a generic declaration)

      --  first, we have to go from the constraint node to the subtype
      --  indication to which this constraint is applied
      Curr_Node := Sinfo.Subtype_Mark (Parent (Constr_Node));

      --  second, we have to go from this subtype indication to the
      --  corresponding base type
      Curr_Node := Base_Type (Entity (Curr_Node));
      --  this is a defining identifier,
      Curr_Node := Parent (Curr_Node);
      --  and this should be the node representing a type declaration,
      --  the only possible Node Kinds for its *original* node are:
      --
      --     N_Formal_Type_Declaration,
      --     N_Full_Type_Declaration,
      --     N_Incomplete_Type_Declaration,
      --     N_Private_Extension_Declaration,
      --     N_Private_Type_Declaration,
      --     N_Protected_Type_Declaration,
      --
      --  But, for derived and formal derived types, GNAT rewrites the
      --  corresponding tree structures: it replaces the original node
      --  of N_Formal_Type_Declaration or N_Full_Type_Declaration kind
      --  with N_Subtype_Decalration node, but the immediately preceding
      --  node is the (inserted) node of N_Full_Type_Declaration kind,
      --  this type implicitly defines all the discriminants and
      --  components which are inherited by the derived type.

      if Debug_Flag_A then
         Write_Eol;
         Write_Str ("Norm.Get_Type_With_Discr: type on which discriminant ");
         Write_Str ("constraint is imposed");
         Write_Eol;
         Write_Node (Curr_Node);
         Write_Eol;
      end if;

      Curr_Node_Kind := Nkind (Curr_Node);

      --  and now we have to analyze the type declaration (definition?)
      --  and to choose the further traversing depending on this analysis.

      case Curr_Node_Kind is
         when N_Incomplete_Type_Declaration   |
              N_Private_Extension_Declaration |
              N_Private_Type_Declaration      |
              N_Protected_Type_Declaration    =>
            --  these cases are easy: we already have needed discriminants
            --  in the type declaration:
            Result_Node := Curr_Node;

         when N_Formal_Type_Declaration =>
            case Nkind (Sinfo.Formal_Type_Definition (Curr_Node)) is
               when N_Formal_Private_Type_Definition =>
                  --  the situation is the same as for the previous
                  --  alternative of the external case - the discriminant
                  --  declarations are already here
                  Result_Node := Curr_Node;

               --  when N_Formal_Derived_Type_Definition =>
                  --  RM95 12.5.1(11): "The declaration of a formal derived
                  --  type shall not have a known_discriminant_part"
                  --  therefore we have to find implicit discriminant
                  --  declarations

                  --  in fact, this case can never happen - GNAT inserts
                  --  the declaration of (implicit) type having the
                  --  corresponding discriminant declarations just before
                  --  the derived type declaration, and all the semantic
                  --  links in the tree are set to this implicit type

               when N_Access_To_Object_Definition =>
                  --  we have to go to the designated type (unwinding other
                  --  access types, if any) which has these discriminants
                  Result_Node := Unwind_Till_Discriminants (Result_Node);

               when others =>
                  --  We should never be here
                  raise Internal_Implementation_Error;
            end case;

         when N_Full_Type_Declaration =>
            case Nkind (Sinfo.Type_Definition (Curr_Node)) is
            --   when N_Derived_Type_Definition =>
            --      ?????? (it may have its "own" discriminants, and it may
            --      inherit discriminants from an ancestor
            --
            --  in fact, this case can never happen - GNAT inserts
            --  the declaration of (implicit) type having the
            --  corresponding discriminant declarations just before
            --  the derived type declaration, and all the semantic
            --  links in the tree are set to this implicit type

               when N_Record_Definition =>
                  --  the situation is the same as for the previous
                  --  alternative of the external case - the discriminant
                  --  declarations are already here
                  Result_Node := Curr_Node;

               when N_Access_To_Object_Definition =>
                  --  we have to go to the designated type (unwinding other
                  --  access types, if any) which has these discriminants
                  Result_Node := Unwind_Till_Discriminants (Result_Node);

               when others =>
                  --  We should never be here
                  raise Internal_Implementation_Error;
            end case;
         when others =>
            --  We should never be here
            raise Internal_Implementation_Error;
      end case;

      return Result_Node;
   end Get_Type_With_Discr;

   ------------------------------------------
   -- Normalized_Discriminant_Associations --
   ------------------------------------------

   function Normalized_Discriminant_Associations
     (Constr_Elem : Asis.Element;
      Constr_Node : Node_Id)
      return Asis.Association_List
   is
      Association_Node_List : constant List_Id := Constraints (Constr_Node);
      --  Association_Node_List cannot be No_List or Empty_List

      Result_Length         : Natural := 0;
      Curr_Association_Node : Node_Id;
   begin
      -------------------------
      -- Implementation Note --
      -------------------------
      --  This code may be incomplete or it may contain errors originated
      --  from incomplete analyziz of the situation. But we need some code
      --  to start from. So this is the first step of the step-by-step
      --  development of this function

      --  First, we have to compute the length of the result. We cannot use
      --  List_Length (Association_Node_List) for it because of the constraints
      --  like this
      --
      --     T (1, 2, D3|D4 => 3)

      Curr_Association_Node := First_Non_Pragma (Association_Node_List);

      --  First, counting positional associations which go first (if any)
      while Present (Curr_Association_Node) and then
            Nkind (Curr_Association_Node) /= N_Discriminant_Association
      loop
         Result_Length         := Result_Length + 1;
         Curr_Association_Node := Next_Non_Pragma (Curr_Association_Node);
      end loop;

      --  Now, we have to count named associations (if any), taking into
      --  account bad style like D3|D4 => 3 in discriminant constraints :[

      while Present (Curr_Association_Node) loop
         --  and if we are here, Curr_Association_Node may be of
         --  N_Discriminant_Association kind only
         Result_Length := Result_Length +
                          Natural (List_Length
                            (Selector_Names (Curr_Association_Node)));
         Curr_Association_Node := Next_Non_Pragma (Curr_Association_Node);
      end loop;

      declare
         Result_List : Asis.Association_List (1 .. Result_Length);

         --  and now we have to build Result_List in a Element-by-Element
         --  manner
         --
         --  The current implementation approach is:
         --
         --  - GNAT (3.05) does not rewrite the tree structures for
         --    discriminant associations, so we can use the R_Node fields
         --    of the result Elements to point to the corresponding
         --    expression (and, therefore, we can use the general
         --    R_Node-based approach for Enclosing_Element), and we can
         --    use the Node field to point to the defining occurrence
         --    of the corresponding discriminant
         --
         --  - We have to find and to go through the definitions of the
         --    corresponding discriminants anyway - to order the discriminant
         --    associations in the result, as it is required by the
         --    Normalized parameter set True. So it would not make sense to
         --    postpone finding of the discriminant defining name till
         --    it will be required by the
         --    Asis.Expressions.Discriminant_Selector_Names query

         Discr_Spec_Nodes : List_Id;
         --  here we should have the discriminant specifications from the
         --  corresponding discriminant part
         Type_Decl_Node   : Node_Id;
         --  the type declaration containing these discriminant
         --  specifications
         Discr_Spec_Node       : Node_Id;
         Discr_Def_Id_N        : Node_Id;
         Discr_Def_Name        : Name_Id;

         Association_Trav_Node : Node_Id;
         Discr_Sel_N_Node      : Node_Id;

         Is_Positional_Association  : Boolean := True;
         --  flag indicating if the discriminant association to process is
         --  a positional association
      begin
         Type_Decl_Node   := Get_Type_With_Discr (Constr_Node);
         Discr_Spec_Nodes := Discriminant_Specifications (Type_Decl_Node);
         --  we prefer to have this extra step with Type_Decl_Node to
         --  make the maintenance of finding the discriminant specifications
         --  easier

         Discr_Spec_Node       := First_Non_Pragma (Discr_Spec_Nodes);
         Curr_Association_Node := First_Non_Pragma (Association_Node_List);

         for I in 1 .. Result_Length loop
            --  Result_Length is the same as List_Lenght (Discr_Spec_Nodes),
            --  otherwise it is an error!

            --  We have to process positional and named associations
            --  separately

            --  Positional associations go first, and they do not have
            --  a discriminant names in the tree structures for the constraint
            --  being processed, so we have to pick up elements one-by-one
            --  from two lists - Discr_Spec_Nodes and Association_Node_List -
            --  and to form one component of Result_List from each pair

            --  Named associations go last, one named association may
            --  be transformed into several normalized ASIS discriminant
            --  associations

            Discr_Def_Id_N := Defining_Identifier (Discr_Spec_Node);

            if Nkind (Curr_Association_Node) = N_Discriminant_Association then
               Is_Positional_Association := False;
            end if;

            if Is_Positional_Association then
               --  First, we create A_Discriminant_Association Element
               --  from Curr_Association_Node, as we do it for non-normalized
               --  associations
               Result_List (I) :=  Node_To_Element_New
                              (Starting_Element => Constr_Elem,
                               Node             => Curr_Association_Node,
                               Internal_Kind    => A_Discriminant_Association,
                               Norm_Case        => Is_Normalized);

               --  And now we have to correct the Node field of this Element:
               Set_Node (Result_List (I), Discr_Def_Id_N);

               --  And now we have to prepare the next iteration of the loop:
               Curr_Association_Node :=
                 Next_Non_Pragma (Curr_Association_Node);
            else
               Discr_Def_Name := Chars (Discr_Def_Id_N);
               Association_Trav_Node  := Curr_Association_Node;
               --  as soon as all the positional associations have been
               --  processed, Curr_Association_Node is frozen as pointed to the
               --  first named association (if any)

               Scan_Named_Associations : while Present (Association_Trav_Node)
               loop
                  --  this loop scans all the named associations
                  Discr_Sel_N_Node :=
                     First_Non_Pragma (Selector_Names (Association_Trav_Node));

                  while Present (Discr_Sel_N_Node) loop
                  --  This loop cans all the discriminant selector names
                  --  in a given named discriminant association

                     exit Scan_Named_Associations
                        when Chars (Discr_Sel_N_Node) = Discr_Def_Name;

                     Discr_Sel_N_Node := Next_Non_Pragma (Discr_Sel_N_Node);
                  end loop;

                  Association_Trav_Node :=
                    Next_Non_Pragma (Association_Trav_Node);
               end loop Scan_Named_Associations;

               --  and when we are here, Association_Trav_Node points to the
               --  (named) discriminant association containing the expression
               --  for Discr_Spec_Node and Discr_Def_Id_N being processed. So:

               Result_List (I) := Node_To_Element_New
                 (Starting_Element => Constr_Elem,
                  Node             => Sinfo.Expression (Association_Trav_Node),
                  Internal_Kind    => A_Discriminant_Association,
                  Norm_Case        => Is_Normalized);

               --  And now we have to correct the Node field of this Element,
               --  just as for positional associations:

               Set_Node (Result_List (I), Discr_Def_Id_N);
            end if;
            Discr_Spec_Node := Next_Non_Pragma (Discr_Spec_Node);
            --  this should not be under "if Is_Positional_Association",
            --  because we have to go one step ahead in Discr_Spec_Nodes
            --  for every iteration of this loop
         end loop;

         return Result_List;

      end;
   end Normalized_Discriminant_Associations;

   -------------------------------------
   -- Normalized_Generic_Associations --
   -------------------------------------

   function Normalized_Generic_Associations
     (Inst_Elem          : Asis.Element;
      Templ_Node         : Node_Id)
      return               Asis.Association_List
   is

      Inst_Node           : Node_Id := R_Node (Inst_Elem);
      --  Represents the instantiation

      Exp_Inst_Node : Node_Id := R_Node (Inst_Elem);
      --  Should be send to point to the expanded package created to the
      --  instantiation

      Current_Formal      : Node_Id;
      Curr_Form_Original  : Node_Id;
      --  the generic formal parameter declaration node currently
      --  processed, corresponds to Current_Associationin the Result

      Current_Formal_Def  : Node_Id;
      --  the node representing the defining identifier or the defining
      --  program unit name from Current_Formal

      Current_Actual      : Node_Id;
      --  the node representing the actual parameter being a part of
      --  normalized generic association to be constructed

      Tmp : Node_Id;

      Current_Norm_Case   : Normalization_Cases;

      Next_Renaming_In_Instance : Node_Id;
      --  Next artificial declaration created in the instance to pass
      --  actual parameters

      Still_Positional    : Boolean := True;
      --  during constructing the result we traverse both the formal part
      --  and instantiation. This flag shows if we are in the positional
      --  (if any) or in the named (if any) part of the generic
      --  actual part.
      Next_Positional_Actual : Node_Id := Empty;
      --  This is the next generic association (if any) being traversed in
      --  the positional part of the generic actual part. When all
      --  the positional part is traversed, but there are some named
      --  associations after them it is set to the first named
      --  association. Remains equal to Empty, if there is
      --  no generic actual part.
      Next_Actual : Node_Id;
      --  just needed to traverse the generic actual part, when looking
      --  among the named associations for the explicitly provided actual
      Actual_Found : Boolean;

      No_More_Actual : Boolean := False;
      --  Is set to True, when there is no more actual to investigate
      --  in the generic actual part

      procedure Find_Default;
      --  This procedure works on all the objects declared above in
      --  Normalized_Generic_Associations as on global variables.
      --  It should be called if a generic actual part does not contain
      --  any actual for a given pair of Current_Formal-Current_Association,
      --  and it defines the default to be used in the normalized generic
      --  association, that is, it defines Current_Actual and
      --  Current_Spec_Case.

      procedure Find_Default is
      begin

         --  if we do not have a box in the declaration of the
         --  corresponding actual parameter, then everything is simple:
         if Nkind (Current_Formal) = N_Formal_Object_Declaration then
            Tmp := Current_Formal;

            while Prev_Ids (Tmp) loop
               Tmp := Prev_Non_Pragma (Tmp);
            end loop;

            Current_Actual    := Sinfo.Default_Expression (Tmp);
            Current_Norm_Case := Is_Normalized_Defaulted;
         elsif Nkind (Current_Formal) in
                  N_Formal_Subprogram_Declaration and then
               Present (Default_Name (Current_Formal))
         then
            Current_Actual    := Default_Name (Current_Formal);
            Current_Norm_Case := Is_Normalized_Defaulted;

         elsif Nkind (Current_Formal) in
                  N_Formal_Subprogram_Declaration and then
               Box_Present (Current_Formal)
         then
            --  and this is the most interesting case...
            --  ASIS requires in this case to return some "implicit naming
            --  expression" being a reference to the actual subprogram. The
            --  tree already contains such an expression.

            while not (Nkind (Next_Renaming_In_Instance) =
                         N_Subprogram_Renaming_Declaration
                   and then
                       Corresponding_Formal_Spec (Next_Renaming_In_Instance) =
                         Current_Formal_Def)
            loop
               Next_Renaming_In_Instance := Next (Next_Renaming_In_Instance);
            end loop;

            Current_Actual := Sinfo.Name (Next_Renaming_In_Instance);

            Current_Norm_Case := Is_Normalized_Defaulted_For_Box;
         end if;

      end Find_Default;

   begin --  Normalized_Generic_Associations

      Asis_Element_Table.Init;

      ----------------------------------------------------------
      --  The implementation approach and the representation  --
      --  of the normalized generic associations              --
      ----------------------------------------------------------

      --  Each normalized generic association in ASIS is represented by
      --  three nodes:
      --
      --  R_Node -        represents the corresponding instantiation,
      --                  is used for Enclosing_Element purposes only;
      --  Node   -        represents the defining node of the corresponding
      --                  generic formal parameter;
      --  Node_Field_2 -  represents the actual parameter
      --
      --  See also the documentation of Asis.Expressions.Actual_Parameter
      --  for the details of obtaining the actual parameter from
      --  a normalized generic association

      if Nkind (Inst_Node) not in
         N_Function_Instantiation .. N_Procedure_Instantiation
      then
         Inst_Node := Node (Inst_Elem);
      end if;

      if Is_Rewrite_Substitution (Exp_Inst_Node)
        and then
         Nkind (Original_Node (Exp_Inst_Node)) in N_Generic_Instantiation
      then
         --  This is the case for library-level instantiations
         if Nkind (Exp_Inst_Node) = N_Package_Body then
            Exp_Inst_Node := Corresponding_Spec (Exp_Inst_Node);

            while Nkind (Exp_Inst_Node) /= N_Package_Declaration loop
               Exp_Inst_Node := Parent (Exp_Inst_Node);
            end loop;
         end if;

      elsif Nkind (Exp_Inst_Node) = N_Package_Declaration
          and then
            Nkind (Original_Node (Exp_Inst_Node)) =
              N_Formal_Package_Declaration
      then
         null;

      else
         Exp_Inst_Node := Prev (Exp_Inst_Node);
      end if;

      if Nkind (Exp_Inst_Node) = N_Package_Body then
         --  Skipping expanded body
         Exp_Inst_Node := Prev (Exp_Inst_Node);
      end if;

      pragma Assert (Nkind (Exp_Inst_Node) = N_Package_Declaration);

      Next_Renaming_In_Instance := Specification (Exp_Inst_Node);
      Next_Renaming_In_Instance :=
        First_Non_Pragma (Visible_Declarations (Next_Renaming_In_Instance));

      if No (Generic_Associations (Inst_Node)) then
         Still_Positional := False;
         No_More_Actual   := True;
      else
         Next_Positional_Actual :=
           First_Non_Pragma (Generic_Associations (Inst_Node));

         if Present (Selector_Name (Next_Positional_Actual)) then
            Still_Positional := False;
         end if;

      end if;

      Current_Formal :=
        First_Non_Pragma (Generic_Formal_Declarations (Templ_Node));

      while Present (Current_Formal)
           and then
            (not Comes_From_Source (Original_Node (Current_Formal))
            or else
             Nkind (Current_Formal) = N_Use_Package_Clause
            or else
             Nkind (Current_Formal) = N_Use_Type_Clause)
      loop
         --  This loop is needed because of formal packages - the front-end
         --  creates the artificial package declarations nodes for them, so
         --  we have to skip these nodes
         Current_Formal := Next_Non_Pragma (Current_Formal);
      end loop;

      Curr_Form_Original := Original_Node (Current_Formal);
      --  it cannot be any pragma inside a formal part, so we do not
      --  need First/Next_Non_Pragma here

      --  This is the main loop, where the next normalized generic
      --  association is created

      while Present (Current_Formal) loop
         --  The only thing we really have to compute is the actual
         --  parameter for a given association. Three cases can be possible:
         --  1. an actual is passed in the generic association
         --  2. the default defined in the specification of the corresponding
         --     formal parameter is used
         --  3. the corresponding formal procedure has A_Box_Default, and
         --     the actual is defined at the place of the instantiation

         if Nkind (Curr_Form_Original) in N_Formal_Subprogram_Declaration
          or else
            Nkind (Curr_Form_Original) = N_Package_Declaration
         then
            Current_Formal_Def :=
               Defining_Unit_Name (Specification (Curr_Form_Original));
         else
            Current_Formal_Def := Defining_Identifier (Curr_Form_Original);
         end if;

         --  first, trying to define, if there is an actual in the generic
         --  actual part
         if Still_Positional then
            --  well-defined case - we have an actual in the generic
            --  actual part. So, let's just pick it up:
            Current_Actual :=
               Explicit_Generic_Actual_Parameter (Next_Positional_Actual);
            Current_Norm_Case := Is_Normalized;

            --  And now we have to try to move Next_Positional_Actual in the
            --  generic actual part.
            if Present (Next_Non_Pragma (Next_Positional_Actual)) then
               Next_Positional_Actual :=
                 Next_Non_Pragma (Next_Positional_Actual);

               if Present (Selector_Name (Next_Positional_Actual)) then
                  Still_Positional := False;
               end if;
            else
               Still_Positional := False;
               No_More_Actual   := True;
            end if;

         elsif not No_More_Actual then

            --  Here we have to investigate the named generic associations
            --  to see if there is an actual for the currently created
            --  normalized ASIS association. We just go through all the
            --  named associations and compare Current_Formal_Def with
            --  the name of the formal from an association
            Next_Actual := Next_Positional_Actual;
            Actual_Found := False;

            while Present (Next_Actual) loop

               --  comparing Current_Formal_Def with the name of the
               --  formal parameter in a named association all we have
               --  to compare are Chars field
               if Comes_From_Source (Next_Actual) and then
                  (Chars (Current_Formal_Def) =
                   Chars (Selector_Name (Next_Actual)))
               then
                  Current_Actual :=
                     Explicit_Generic_Actual_Parameter (Next_Actual);
                  Current_Norm_Case := Is_Normalized;
                  Actual_Found := True;
                  exit;
               end if;

               Next_Actual := Next_Non_Pragma (Next_Actual);
            end loop;

            if not Actual_Found then
               Find_Default;
            end if;

         else
            --  for sure, no actual is provided in the generic actual
            --  part for a given association. We have to pick up the
            --  corresponding default parameter
            Find_Default;
         end if;

         Asis_Element_Table.Append (
            Node_To_Element_New (Node             => R_Node (Inst_Elem),
                                 Node_Field_2     => Current_Actual,
                                 Starting_Element => Inst_Elem,
                                 Internal_Kind    => A_Generic_Association,
                                 Norm_Case        => Current_Norm_Case));

         --  and here we have to correct the following fields in this
         --  component of the result:

         --  first, Node_To_Element_New set the Node field as Original_Node
         --  of Inst_Node, but we need the defining occurrence of the formal
         --  parameter here, therefore:
         Set_Node
           (Asis_Element_Table.Table (Asis_Element_Table.Last),
            Current_Formal_Def);

         --  then, we have to set On the Is_Part_Of_Implicit flag
         Set_From_Implicit
           (Asis_Element_Table.Table (Asis_Element_Table.Last));

         Current_Formal := Next_Non_Pragma (Current_Formal);

         while Present (Current_Formal)
              and then
               (not Comes_From_Source (Original_Node (Current_Formal))
               or else
                Nkind (Current_Formal) = N_Use_Package_Clause
               or else
                Nkind (Current_Formal) = N_Use_Type_Clause)
         loop
            Current_Formal := Next_Non_Pragma (Current_Formal);
         end loop;

         Curr_Form_Original   := Original_Node (Current_Formal);
      end loop;

      return Asis.Association_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));
   end Normalized_Generic_Associations;

   -----------------------------------
   -- Normalized_Param_Associations --
   -----------------------------------

   function Normalized_Param_Associations
     (Call_Elem : Asis.Element)
      return      Asis.Association_List
   is
      Call_Node : Node_Id;
      --  This node represents a call construct. It may be either
      --  A_Fuction_Call expression element or a call statement

      Subprogram_Entity : Entity_Id := Empty;
      --  In case if Call_Elem represents the call to inherited subprogram,
      --  should be sent to subprogram entity, otherwise is Empty

      Next_Formal : Node_Id;
      --  The specification of the formal parameters of the called entity. We
      --  iterate through these specifications to construct the list of
      --  normalized associations

      Next_Call_Association : Node_Id := Empty;
      --  The next parameter association in the call construct. We iterate
      --  through these associations one by one while we are in positional
      --  associations (if any). Next_Call_Association is equal to Empty only
      --  before creating the first association (that is, before processing
      --  the first association of the call construct, if any)

      Actual_Node : Node_Id := Empty;
      --  The node representing the actual parameter. Depending on whether this
      --  parameter is submitted explicitly or not, is taken either from
      --  Next_Association node or from Next_Formal node.

      Still_Positional : Boolean := True;
      --  Flag indicating if we are still processing position associations. Is
      --  set of by Get_Actual is it detects that we already are in named
      --  associations or if there are no associations (any more) in the call
      --  context

      Norm_Case : Normalization_Cases;

      function Get_Call_Node (E : Asis.Element) return Node_Id;
      --  Returns the node representing the call.

      function Get_First_Formal (E : Asis.Element) return Entity_Id;
      --  Returns the declaration node of the first formal parameter of the
      --  called entity

      function Get_Actual return Node_Id;
      --  This function tries to get the next actual parameter from the call,
      --  using Next_Call_Association, Still_Positional and Next_Formal as
      --  global variables. In case if the next actual parameter is still from
      --  positional associations from the call, it moves Next_Call_Association
      --  to point to this positional association, otherwise
      --  the value of Next_Call_Association is undefined (but tot Empty!)

      function Get_Default (N : Node_Id) return Node_Id;
      --  Returns the default initialization expression from the declaration
      --  of a formal parameter

      function Get_Subprogram_Entity (N : Node_Id) return Entity_Id;
      --  Provided that N represents a call, checks if this is a call to an
      --  inherited subprogram and if it is, returns the entity node of this
      --  subprogram. Otherwise returns Empty.

      ----------------
      -- Get_Actual --
      ----------------

      function Get_Actual return Node_Id is
         Result           : Node_Id := Empty;
         Next_Association : Node_Id;
      begin

         if No (Next_Call_Association) then
            --  This is the first call to this function. we have to check if
            --  there are positional associations:

            if Nkind (Call_Node) in N_Binary_Op then
               Next_Call_Association := Left_Opnd (Call_Node);
            elsif Nkind (Call_Node) in N_Op then
               Next_Call_Association := Right_Opnd (Call_Node);
            else
               Next_Call_Association :=
                 First_Non_Pragma (Parameter_Associations (Call_Node));
            end if;

            if Nkind (Next_Call_Association) /= N_Parameter_Association then
               Result := Next_Call_Association;
            else
               Still_Positional := False;
            end if;

         end if;

         --  Try to get the next appropriate positional association
         if No (Result)
           and then
            Still_Positional
         then
            if Nkind (Call_Node) in N_Binary_Op then
               Next_Call_Association := Right_Opnd (Call_Node);
            elsif Nkind (Call_Node) in N_Op then
               Next_Call_Association := Empty;
            else
               Next_Call_Association :=
                 Next_Non_Pragma (Next_Call_Association);
            end if;

            if Nkind (Next_Call_Association) /=
               N_Parameter_Association
            then
               Result := Next_Call_Association;
            else
               Still_Positional := False;
            end if;

         end if;

         --  Try to get the actual from the appropriate named association
         if No (Result) then

            pragma Assert (not Still_Positional);
            Next_Association := Next_Call_Association;

            while Present (Next_Association) loop

               pragma Assert
                 (Nkind (Next_Association) = N_Parameter_Association);

               if Comes_From_Source (Next_Association)
                and then
                  Chars (Selector_Name (Next_Association)) =
                  Chars (Defining_Identifier (Next_Formal))
               then
                  Result := Explicit_Actual_Parameter (Next_Association);
                  exit;
               end if;

               Next_Association := Next_Non_Pragma (Next_Association);
            end loop;

         end if;

         return Result;

      end Get_Actual;

      -------------------
      -- Get_Call_Node --
      -------------------

      function Get_Call_Node (E : Asis.Element) return Node_Id is
         Result : Node_Id := Node (E);
      begin

         if Is_Prefix_Notation (E) then
            Result := R_Node (E);

            if Is_Rewrite_Substitution (Result)         and then
               Nkind (Result) = N_Explicit_Dereference  and then
               Nkind (Prefix (Result)) = N_Function_Call
            then
               Result := Prefix (Result);
            end if;
         end if;

         if Nkind (R_Node (E)) = N_Function_Call
           and then
            (Nkind (Result) in  N_Op_Add .. N_Op_Xor
            or else
             Nkind (Result) in N_Op_Abs .. N_Op_Plus
            or else
             Nkind (Result) /= N_Function_Call)
         then
            --  Call to a user-defined operator function
            Result := R_Node (E);
         end if;

         return Result;
      end Get_Call_Node;

      -----------------
      -- Get_Default --
      -----------------

      function Get_Default (N : Node_Id) return Node_Id is
         Result : Node_Id := N;
      begin

         while Prev_Ids (Result) loop
            Result := Prev_Non_Pragma (Result);
         end loop;

         Result := Sinfo.Expression (Result);
         pragma Assert (Present (Result));

         return Result;
      end Get_Default;

      ----------------------
      -- Get_First_Formal --
      ----------------------

      function Get_First_Formal (E : Asis.Element) return Entity_Id is
         Called_Entity_Element : Asis.Element;
         Result                : Entity_Id;
      begin
         --  We use the ASIS semantic queries here to minimize the
         --  maintenance efforts.

         if Expression_Kind (Call_Elem) = A_Function_Call then
            Called_Entity_Element := Corresponding_Called_Function (E);
         else
            Called_Entity_Element := Corresponding_Called_Entity (E);
         end if;

         if Declaration_Kind (Called_Entity_Element) in
            A_Procedure_Instantiation .. A_Function_Instantiation
         then
            Called_Entity_Element :=
              Corresponding_Declaration (Called_Entity_Element);
         end if;

         pragma Assert (not Is_Nil (Called_Entity_Element));

         Result := Node (Called_Entity_Element);

         if Nkind (Result) /= N_Entry_Declaration then
            Result := Specification (Result);
         end if;

         Result := First_Non_Pragma (Parameter_Specifications (Result));

         return Result;

      end Get_First_Formal;

      ---------------------------
      -- Get_Subprogram_Entity --
      ---------------------------

      function Get_Subprogram_Entity (N : Node_Id) return Entity_Id is
         Tmp    : Node_Id   := N;
         Result : Entity_Id := Empty;
      begin

         if Nkind (N) not in N_Op then
            Tmp := Sinfo.Name (N);
         end if;

         if Nkind (Tmp) in N_Has_Entity then
            Tmp := Entity (Tmp);

            if Nkind (Parent (Tmp)) = N_Subtype_Declaration then
               Result := Tmp;
            end if;

         end if;

         return Result;

      end Get_Subprogram_Entity;

   begin  --  Normalized_Param_Associations

      --  ??? Should we have the common code for this function and for
      --  Normalized_Generic_Associations???

      ----------------------------------------------------------
      --  The implementation approach and the representation  --
      --  of the normalized parameter associations            --
      ----------------------------------------------------------

      --  Each normalized generic association in ASIS is represented by
      --  three nodes:
      --
      --  R_Node -        represents the corresponding call,
      --                  is used for Enclosing_Element purposes only;
      --  Node   -        represents the defining node of the corresponding
      --                  formal parameter;
      --  Node_Field_1 -  in case of a call to implicit inherited subprogram
      --                  points to subprogram defining node, otherwise Empty
      --  Node_Field_2 -  represents the actual parameter

      Call_Node         := Get_Call_Node    (Call_Elem);
      Subprogram_Entity := Get_Subprogram_Entity (Call_Node);
      Next_Formal       := Get_First_Formal (Call_Elem);

      Asis_Element_Table.Init;

      while Present (Next_Formal) loop

         Actual_Node := Get_Actual;
         Norm_Case   := Is_Normalized;

         if No (Actual_Node) then
            Actual_Node := Get_Default (Next_Formal);
            Norm_Case   := Is_Normalized_Defaulted;
         end if;

         Asis_Element_Table.Append (
            Node_To_Element_New
              (Node             => Call_Node,
               Node_Field_1     => Subprogram_Entity,
               Node_Field_2     => Actual_Node,
               Starting_Element => Call_Elem,
               Internal_Kind    => A_Parameter_Association,
               Norm_Case        => Norm_Case));

            --  first, Node_To_Element_New set the Node field as Original_Node
            --  of Call_Node, but we need the defining occurrence of the formal
            --  parameter here, therefore:
            Set_Node
              (Asis_Element_Table.Table (Asis_Element_Table.Last),
               Defining_Identifier (Next_Formal));

            --  then, we have to set On the Is_Part_Of_Implicit flag
            Set_From_Implicit
              (Asis_Element_Table.Table (Asis_Element_Table.Last));

            Next_Formal := Next_Non_Pragma (Next_Formal);
      end loop;

      return Asis.Association_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));
   end Normalized_Param_Associations;

   ----------------------------------------------
   -- Normalized_Record_Component_Associations --
   ----------------------------------------------

   --  PARTIALLY IMPLEMENTED, CAN NOT PROCESS VARIANT PARTS!

   function Normalized_Record_Component_Associations
     (Aggregate : Asis.Element)
      return      Asis.Element_List
   is
      Next_Association : Node_Id;
      pragma Unreferenced (Next_Association);
      --  This node represents the next association in (rewritten) aggregate

      Next_Component : Entity_Id;
      pragma Unreferenced (Next_Component);
      --  Next component entity

      In_Discriminants : constant Boolean := False;
      pragma Unreferenced (In_Discriminants);
      --  Flag that indicates if we are iterating through discriminants

      Arg_Node : Node_Id;
      --  For Ada95-oriented temporary placeholder implementation!

   begin
      --  This routine is supposed to generate the list of normalized record
      --  component associations for Ada 2005 record aggregates. At the
      --  moment it just repeats a simple-minded approach used for Ada 95 code.
      --  So it is no more that a placeholder that works on Ada 95 code and
      --  on Ada 2005 aggregates that do not contain boxes. The commented
      --  code is a part of the attempts to provide a proper Ada 2005
      --  implementation

      Arg_Node := R_Node (Aggregate);

      return N_To_E_List_New
               (List             => Component_Associations (Arg_Node),
                Node_Knd         => N_Component_Association,
                Internal_Kind    => A_Record_Component_Association,
                Norm_Case        => Is_Normalized,
                Starting_Element => Aggregate);

--      Asis_Element_Table.Init;

--     Next_Association := First (Component_Associations (R_Node (Aggregate)));

--      Next_Component := Etype (R_Node (Aggregate));

--      while Ekind (Next_Component) = E_Record_Subtype loop
--         Next_Component := Etype (Next_Component);
--      end loop;

--      Next_Component := Parent (Next_Component);

--      pragma Assert (Nkind (Next_Component) = N_Full_Type_Declaration);
--      --  ??? This may be wrong - what about private or derived types?

--      if Present (Discriminant_Specifications (Next_Component)) then
--         Next_Component :=
--           First (Discriminant_Specifications (Next_Component));
--         In_Discriminants := True;
--      else
--         --  We know for sure that the record type has components!
--         Next_Component :=
--           Component_List (Sinfo.Type_Definition (Next_Component));
--      end if;

--      Not_Implemented_Yet
--        (Diagnosis => "Normalized_Record_Component_Associations");

--      return Asis.Association_List
--               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));
   end Normalized_Record_Component_Associations;

   -------------------------------
   -- Unwind_Till_Discriminants --
   -------------------------------

   function Unwind_Till_Discriminants (Type_Decl : Node_Id) return Node_Id is
      Result_Node : Node_Id;
      Curr_Node   : Node_Id;
   begin
      Result_Node := Type_Decl;

      while not Has_Discriminants (Defining_Identifier (Result_Node)) loop
         --  each iteration of the loop unwinds one step of "deriving"
         --  or "accessing"
         --
         --  !!!???!!!??? What really happens with private types in the tree
         if Nkind (Result_Node) = N_Full_Type_Declaration then
            Curr_Node := Sinfo.Type_Definition (Result_Node);

            if Nkind (Curr_Node) = N_Derived_Type_Definition or else
               --  ??can it really happen???
               Nkind (Curr_Node) = N_Access_To_Object_Definition
            then
               Curr_Node := Sinfo.Subtype_Mark (Curr_Node);
            else
               goto Error;
            end if;

            if Nkind (Curr_Node) = N_Subtype_Indication then
               --  this is an error situation - we cannot get any type
               --  with discriminants
               goto Error;
            end if;

         elsif Nkind (Result_Node) = N_Formal_Type_Declaration then
            Curr_Node := Sinfo.Formal_Type_Definition (Result_Node);

            if Nkind (Curr_Node) = N_Formal_Derived_Type_Definition then
               --  ??can it really happen???
               Curr_Node := Sinfo.Subtype_Mark (Curr_Node);
            elsif Nkind (Curr_Node) = N_Access_To_Object_Definition then
               Curr_Node := Sinfo.Subtype_Indication (Curr_Node);
               if Nkind (Curr_Node) = N_Subtype_Indication then
                  --  this is an error situation - we cannot get any type
                  --  with discriminants
                  goto Error;
               end if;
               Curr_Node := Base_Type (Curr_Node);
            else
               goto Error;
            end if;
         else
            goto Error;
         end if;

         --  as the result of this if statement, we have the subtype mark
         --  of the designated/parent subtype

         Curr_Node   := Base_Type (Entity (Curr_Node));
         Result_Node := Parent (Curr_Node);
      end loop;

      return Result_Node;

      <<Error>> raise Internal_Implementation_Error;
   end Unwind_Till_Discriminants;

end A4G.Norm;
