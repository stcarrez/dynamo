------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                   A 4 G . S P A N _ B E G I N N I N G                    --
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

with Asis;               use Asis;
with Asis.Clauses;       use  Asis.Clauses;
with Asis.Declarations;  use  Asis.Declarations;
with Asis.Definitions;   use  Asis.Definitions;
with Asis.Elements;      use  Asis.Elements;
with Asis.Expressions;   use  Asis.Expressions;
with Asis.Extensions;    use  Asis.Extensions;
with Asis.Statements;    use  Asis.Statements;

with Asis.Set_Get;       use  Asis.Set_Get;

with A4G.A_Sinput;       use A4G.A_Sinput;
with A4G.A_Types;        use A4G.A_Types;
with A4G.Span_End;

with Atree;              use Atree;
with Nlists;             use Nlists;
with Sinfo;              use Sinfo;

package body A4G.Span_Beginning is

   -----------------------
   -- Local subprograms --
   -----------------------

   function First_Label_Beginning (E : Asis.Element) return Source_Ptr;
   --  If E is a statement element and if this statement has label(s), returns
   --  the location of the first '<' of the first label. Returns No_Location
   --  otherwise

   function Prev_Word_Beginning (E : Asis.Element) return Source_Ptr;
   --  Encapsulates the common case when Sloc (Node (E)) points to the second
   --  lexem in the element image

   ------------------------------------
   -- Assignment_Statement_Beginning --
   ------------------------------------

   function Assignment_Statement_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      El : constant Asis.Element := Assignment_Variable_Name (E);
   begin

      return Set_Image_Beginning (El);

   end Assignment_Statement_Beginning;

   ---------------------------
   -- Association_Beginning --
   ---------------------------

   function Association_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (E);
      Name     : Asis.Element                    := Nil_Element;
      Expr     : Asis.Element;
   begin

      case Arg_Kind is

         when A_Parameter_Association       |
              A_Generic_Association         |
              A_Pragma_Argument_Association =>

            Name := Formal_Parameter (E);
            Expr := Actual_Parameter (E);

         when A_Discriminant_Association =>

            if not Is_Nil (Discriminant_Selector_Names (E)) then
               Name := Discriminant_Selector_Names (E) (1);
            end if;

            Expr := Discriminant_Expression (E);

         when others =>
            null;
      end case;

      if Is_Nil (Name) then

         return Set_Image_Beginning (Expr);
      else
         --  we have nothing to do: Sloc points to the first token of the
         --  formal parameter name in the given Named association

         return No_Search (E);
      end if;

   end Association_Beginning;

   ---------------------------------
   -- A_Then_Abort_Path_Beginning --
   ---------------------------------

   function A_Then_Abort_Path_Beginning (E : Asis.Element) return Source_Ptr
     renames Prev_Word_Beginning;

--  --|A2012 start

   ---------------------------------------
   -- An_Else_Expression_Path_Beginning --
   ---------------------------------------

   function An_Else_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Set_Image_Beginning (Dependent_Expression (E));
   begin
      S := Search_Prev_Word (S);
      S := Search_Beginning_Of_Word (S);

      return S;
   end An_Else_Expression_Path_Beginning;

--  --|A2012 end

   ------------------------------
   -- Call_Statement_Beginning --
   ------------------------------

   function Call_Statement_Beginning (E : Asis.Element) return Source_Ptr is
      First_Comp : Asis.Element;
   begin

      if Is_Prefix_Notation (E) then
         First_Comp := Asis.Statements.Call_Statement_Parameters (E) (1);
         First_Comp := Asis.Expressions.Actual_Parameter (First_Comp);
      else
         First_Comp := Asis.Statements.Called_Name (E);
      end if;

      return Set_Image_Beginning (First_Comp);
   end Call_Statement_Beginning;

--  --|A2012 start

   ------------------------------------
   -- Case_Expression_Path_Beginning --
   ------------------------------------

   function Case_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr renames Prev_Word_Beginning;
--  --|A2012 end

   ---------------------------------------------------
   -- Component_And_Parameter_Declaration_Beginning --
   ---------------------------------------------------

   function Component_And_Parameter_Declaration_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      Ls : constant Asis.Element_List := Names (E);
   begin

      return Get_Location (Ls (Ls'First));

   end Component_And_Parameter_Declaration_Beginning;

   -------------------------------------
   -- Component_Association_Beginning --
   -------------------------------------

   function Component_Association_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      First_Choice : Asis.Element := Asis.Nil_Element;
   begin

      if Int_Kind (E) = An_Array_Component_Association then

         declare
            Choices : constant Asis.Element_List :=
              Array_Component_Choices (E);
         begin

            if not Is_Nil (Choices) then
               First_Choice := Choices (Choices'First);
            end if;

         end;

      else  --  Int_Kind (E) = A_Record_Component_Association

         declare
            Choices : constant Asis.Element_List :=
              Record_Component_Choices (E);
         begin

            if not Is_Nil (Choices) then
               First_Choice := Choices (Choices'First);
            end if;

         end;
      end if;

      if not Is_Nil (First_Choice) then

         return Set_Image_Beginning (First_Choice);
      else

         return Set_Image_Beginning (Component_Expression (E));
      end if;

   end Component_Association_Beginning;

   --------------------------------
   -- Component_Clause_Beginning --
   --------------------------------

   function Component_Clause_Beginning (E : Asis.Element) return Source_Ptr is

      El : constant Asis.Element := Representation_Clause_Name (E);

   begin

      return Set_Image_Beginning (El);

   end Component_Clause_Beginning;

   ------------------------------------
   -- Component_Definition_Beginning --
   ------------------------------------

   function Component_Definition_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      Arg_Node   : constant Node_Id    := Node (E);
      Arg_N_Kind : constant Node_Kind  := Nkind (Arg_Node);
      S          : Source_Ptr          := Sloc (Arg_Node);
      --  S may be corrected
      Dummy      : Asis.Element := E;
      --  I am not sure that using this Dummy element makes a good style...
      --  ???
   begin
      --  first, we should set S pointing to the beginning of the subtype
      --  indication:

      case Arg_N_Kind is
         when N_Component_Definition =>
            null;
            --  no need to adjust S
         when N_Expanded_Name | N_Attribute_Reference =>
            --  S points to period and it should be processed as
            --  A_Selected_Component element here:
            Set_Int_Kind (Dummy, A_Selected_Component); --  ???
            S := Search_Prefix_Beginning (Dummy);

         when others =>
            --  We should never be here
            pragma Assert (False);
            null;
      end case;

      return S;
   end Component_Definition_Beginning;

--  --|A2012 start

   -------------------------------------------
   -- Conditional_Expression_Path_Beginning --
   -------------------------------------------

   function Conditional_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Set_Image_Beginning (Condition_Expression (E)); --  ???
   begin
      S := Search_Prev_Word (S);
      S := Search_Beginning_Of_Word (S);

      return S;
   end Conditional_Expression_Path_Beginning;

--  --|A2012 end

   -----------------------------------
   -- Defining_Identifier_Beginning --
   -----------------------------------

   function Defining_Identifier_Beginning
     (E :    Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Get_Location (E);
   begin
      if Nkind (Node (E)) = N_Label then
         S := Search_Beginning_Of_Word (S + 2);
      end if;

      return S;
   end Defining_Identifier_Beginning;

   ----------------------------------
   -- Derived_Definition_Beginning --
   ----------------------------------

   function Derived_Definition_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Get_Location (E);
   begin
      if Trait_Kind (E) = An_Abstract_Trait and then
         Int_Kind (E) /=  A_Formal_Derived_Type_Definition
      then
         --  the second condition is just a patch, needed because of the
         --  tree rewriting...
         S := Search_Prev_Word (S);
         S := Search_Beginning_Of_Word (S);
      end if;

      return S;

   end Derived_Definition_Beginning;

   -------------------------
   -- Else_Path_Beginning --
   -------------------------

   function Else_Path_Beginning (E : Asis.Element) return Source_Ptr is
      Ls : constant Asis.Element_List :=
        Sequence_Of_Statements (Path            => E,
                                Include_Pragmas => True);
      S : Source_Ptr := Set_Image_Beginning (Ls (Ls'First));
   begin
      S := Search_Prev_Word (S);
      S := Search_Beginning_Of_Word (S);

      return S;

   end Else_Path_Beginning;

   -------------------------------------
   -- Exception_Declaration_Beginning --
   -------------------------------------

   function Exception_Declaration_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      Ls : constant Asis.Element_List := Names (E);
   begin
      return Get_Location (Ls (Ls'First));
   end Exception_Declaration_Beginning;

   ------------------------------------
   -- Explicit_Dereference_Beginning --
   ------------------------------------

   function Explicit_Dereference_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
   begin
      return Set_Image_Beginning (Asis.Expressions.Prefix (E));
   end Explicit_Dereference_Beginning;

   ---------------------------
   -- First_Label_Beginning --
   ---------------------------

   function First_Label_Beginning (E : Asis.Element) return Source_Ptr is
      N, N1  : Node_Id;
      Result : Source_Ptr := No_Location;
   begin

      --  We use the direct tree traversing because of the performance
      --  reasons. The implementation is in fact a simplified version of the
      --  code of Asis.Statements.Label_Names

      if Int_Kind (E) in Internal_Statement_Kinds then
         N  := Node (E);
         N1 := Parent (R_Node (E));

         if Nkind (N1) = N_Loop_Statement
          and then
            Is_Rewrite_Substitution (N1)
          and then
            Nkind (Original_Node (N1)) = N_Goto_Statement
          and then
            N = First (Sinfo.Statements (N1))
         then
            --  First, process a special case when an infinite loop is
            --  programmed as
            --
            --  <<Target>> Stmt;
            --  ...
            --  goto Target;
            --
            --  If Stmt has exactly one label attached to it, the front-end
            --  rewrites this construct as a subtree headed by N_Loop_Statement
            --  node
            N1     := Sinfo.Identifier (N1);
            Result := Sloc (N1);

            --  N1 is not N_Label node, so we have to get to the first '<' in
            --  '<<"
            Result := Search_Prev_Word (Result) - 1;

         elsif Nkind (N) = N_Goto_Statement
           and then
               Nkind (R_Node (E)) = N_Loop_Statement
           and then
               Is_Empty_List (Sinfo.Statements (R_Node (E)))
         then
            --  This is a pathological case of
            --
            --   <<Target>> goto Target;
            N1     := Sinfo.Identifier (R_Node (E));
            Result := Sloc (N1);

            --  Here N1 also is not N_Label node, so we have to get to the
            --  first '<' in '<<"
            Result := Search_Prev_Word (Result) - 1;
         elsif Is_List_Member (N) then
            N1 := Prev (N);

            while Nkind (N1) = N_Label loop
               N := N1;
               N1 := Prev (N1);
            end loop;

            if Nkind (N) = N_Label then
               Result := Sloc (N);
            end if;

         end if;

      end if;

      return Result;

   end First_Label_Beginning;

   -----------------------------------------
   -- Formal_Object_Declaration_Beginning --
   -----------------------------------------

   function Formal_Object_Declaration_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      N : constant Node_Id := Node (E);
   begin

      return Sloc (Sinfo.Defining_Identifier (N));

   end Formal_Object_Declaration_Beginning;

   ----------------------------------
   -- For_Loop_Statement_Beginning --
   ----------------------------------

   function For_Loop_Statement_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      El : Asis.Element := Statement_Identifier (E);
      S  : Source_Ptr;

   begin

      if not Is_Nil (El) then
         return Set_Image_Beginning (El);
      end if;

      El := For_Loop_Parameter_Specification (E);

      if not Is_Nil (El) then
         S := Set_Image_Beginning (El);
         S := Search_Prev_Word (S);
         S := Search_Beginning_Of_Word (S);

         return S;

      else

         return Get_Location (E);
      end if;

   end For_Loop_Statement_Beginning;

   -----------------------------
   -- Function_Call_Beginning --
   -----------------------------

   function Function_Call_Beginning (E : Asis.Element) return Source_Ptr is
      Tmp     : Asis.Element;
      Arg     : Asis.Element := E;
      New_Arg : Asis.Element;
   begin

      --  The implementation uses iteration instead of recursive processing of
      --  the function call parameters to avoid getting into very deep
      --  recursive chains for the calls like:
      --
      --  "aaaaaaaaaaaa" &
      --  "bbbbbbbbbbbb" &
      --  ..........
      --  "yyyyyyyyyyyy" &
      --  "zzzzzzzzzzzz"

      --  For the cases like this we parse arguments of the (nested!) calls
      --  to "&" in a loop, using the Arg variable to point to the leftmost
      --  parameter of the currently processed (nested) call.
      --
      --  The recursion is used only for the cases that on practice can
      --  never result in very long recursive call chains.

      <<Next_Iteration>>
      if Is_Prefix_Call (Arg) then

         if Is_Prefix_Notation (Arg) then
            Tmp := Function_Call_Parameters (Arg) (1);
            Tmp := Actual_Parameter (Tmp);

            return Set_Image_Beginning (Tmp);
         else
            return Set_Image_Beginning (Asis.Expressions.Prefix (Arg));
         end if;

      else

         declare
            Param : constant Asis.Element_List :=
              Function_Call_Parameters (Arg);
         begin

            if Param'Length > 1 then
               New_Arg := Param (Param'First);

               --  We have an infix call, so New_Arg cannot be a named
               --  association!

               New_Arg := Actual_Parameter (New_Arg);

               if Expression_Kind (New_Arg) = A_Function_Call then
                  Arg := New_Arg;
                  goto Next_Iteration;
               end if;

               return Set_Image_Beginning (New_Arg);
            else
               return Set_Image_Beginning (Asis.Expressions.Prefix (Arg));
            end if;
         end;

      end if;
   end Function_Call_Beginning;

--  --|A2012 start
   -----------------------------
   -- If_Expression_Beginning --
   -----------------------------

   function If_Expression_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : constant Source_Ptr :=
        Set_Image_Beginning (Expression_Paths (E) (1));
   begin
      return S;
   end If_Expression_Beginning;
--  --|A2012 end

   ---------------------------------
   -- Indexed_Component_Beginning --
   ---------------------------------

   function Indexed_Component_Beginning (E : Asis.Element) return Source_Ptr
   is
   begin
      return Set_Image_Beginning (Asis.Expressions.Prefix (E));
   end Indexed_Component_Beginning;

--  --|A2005 start
   ------------------------------------
   -- Interface_Definition_Beginning --
   ------------------------------------

   function Interface_Definition_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      N : Node_Id := Node (E);
      S : Source_Ptr;
   begin
      --  We start from the defining name of the corresponding type
      N := Defining_Identifier (Parent (N));
      S := Sloc (N);

      --  and skip this name and 'IS' keyword
      S := Search_End_Of_Word (S);
      S := Search_Next_Word (S);

      S := Search_End_Of_Word (S);
      S := Search_Next_Word (S);

      return S;
   end Interface_Definition_Beginning;
--  --|A2005 end

   -------------------------------
   -- Membership_Test_Beginning --
   -------------------------------

   function Membership_Test_Beginning (E : Asis.Element) return Source_Ptr is
   begin
      return Set_Image_Beginning (Membership_Test_Expression (E));
   end Membership_Test_Beginning;

   -------------------------------
   -- Named_Statement_Beginning --
   -------------------------------

   function Named_Statement_Beginning (E : Asis.Element) return Source_Ptr is
      El : constant Asis.Element := Statement_Identifier (E);
   begin

      if not Is_Nil (El) then
         return Set_Image_Beginning (El);
      else
         return Get_Location (E);
      end if;

   end Named_Statement_Beginning;

   ---------------
   -- No_Search --
   ---------------

   function No_Search (E : Asis.Element) return Source_Ptr is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return S;
   end No_Search;

   ------------------------------
   -- Null_Component_Beginning --
   ------------------------------

   function Null_Component_Beginning (E : Asis.Element) return Source_Ptr is
      S : Source_Ptr := Get_Location (E);
   begin
      --  first, looking for ';' after "null"
      S := Search_Rightmost_Symbol (S, ';');
      --  then, going to the left to this "null" itself:
      S := Search_Prev_Word (S);
      --  it can ne nothing except comments between this ';' and "null", so:
      S := S - 3;

      return S;

   end Null_Component_Beginning;

   ----------------------------------------
   -- Parenthesized_Expression_Beginning --
   ----------------------------------------

   function Parenthesized_Expression_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      El : constant Asis.Element := Expression_Parenthesized (E);
      S  : constant Source_Ptr   := Set_Image_Beginning (El);
   begin
      return  Search_Left_Parenthesis (S);
   end Parenthesized_Expression_Beginning;

--  --|A2005 start

   ---------------------------------------
   -- Possible_Null_Exclusion_Beginning --
   ---------------------------------------

   function Possible_Null_Exclusion_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Get_Location (E);
   begin

      if Trait_Kind (E) = A_Null_Exclusion_Trait then
         S := Search_Prev_Word_Start (S);
         S := Search_Prev_Word_Start (S);
      end if;

      return S;
   end Possible_Null_Exclusion_Beginning;

   ---------------------------------------------
   -- Possible_Overriding_Indicator_Beginning --
   ---------------------------------------------

   function Possible_Overriding_Indicator_Beginning
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Get_Location (E);
   begin
      if Is_Overriding_Declaration (E) then
         S := Search_Prev_Word_Start (S);
      elsif Is_Not_Overriding_Declaration (E) then
         S := Search_Prev_Word_Start (S);
         S := Search_Prev_Word_Start (S);
      end if;

      return S;
   end Possible_Overriding_Indicator_Beginning;
--  --|A2005 end

   -------------------------
   -- Prev_Word_Beginning --
   -------------------------

   function Prev_Word_Beginning (E : Asis.Element) return Source_Ptr is
      Result : Source_Ptr := Sloc (Node (E));
   begin
      Result := Search_Prev_Word_Start (Result);
      return Result;
   end Prev_Word_Beginning;

   --------------------------------------------
   -- Private_Extension_Definition_Beginning --
   --------------------------------------------

   function Private_Extension_Definition_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      D : constant Node_Id := Node (E);
      N :          Node_Id := D;
      S :          Source_Ptr;
   begin
      --  PRIVATE_EXTENSION_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] is
      --      [abstract] new ancestor_SUBTYPE_INDICATION with private;

      --  Note: private extension declarations are not allowed in Ada 83 mode

      --  N_Private_Extension_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant
      --  Abstract_Present (Flag4)
      --  Subtype_Indication (Node5)

      N := Sinfo.Subtype_Indication (N);

      while Nkind (N) = N_Expanded_Name loop
         N := Prefix (N);
      end loop;

      S := Sloc (N);
      --  S points to the first character of ancestor_SUBTYPE_INDICATION

      S := Search_Prev_Word_Start (S);
      --  "new" was skipped now
      if Abstract_Present (D) then
         --  skipping "abstract":
         S := Search_Prev_Word_Start (S);
      end if;

      return S;
   end Private_Extension_Definition_Beginning;

   ---------------------------------------
   -- Private_Type_Definition_Beginning --
   ---------------------------------------

   function Private_Type_Definition_Beginning
     (E :    Asis.Element)
      return Source_Ptr
   is
      N : constant Node_Id := Node (E);
      S : Source_Ptr       := Sloc (N);
   begin
         --  PRIVATE_TYPE_DECLARATION ::=
         --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
         --      is [[abstract] tagged] [limited] private;

         --  Note: TAGGED is not permitted in Ada 83 mode

         --  N_Private_Type_Declaration
         --  Sloc points to TYPE
         --  Defining_Identifier (Node1)
         --  Discriminant_Specifications (List4) (set to No_List if no
         --   discriminant part)
         --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant
         --  Abstract_Present (Flag4)
         --  Tagged_Present (Flag15)
         --  Limited_Present (Flag17)

      --  if the enclosing type declaration contains a discriminant part, we
      --  should skip it first

      if Nkind (N) = N_Private_Type_Declaration and then
         Present (Discriminant_Specifications (N))
      then
         declare
            Discr_Part : constant Asis.Element_List :=
              Discriminants (Discriminant_Part (Enclosing_Element (E)));
            Tmp : Asis.Element := Discr_Part (Discr_Part'Last);
         begin
            Tmp := Get_Last_Component (Tmp);
            S   := A4G.Span_End.Set_Image_End (Tmp);
         end;

         S := Search_Rightmost_Symbol (S, ')');
      end if;

      S := Search_Rightmost_Symbol (S, ';');

      --  skipping "private"
      S := Search_Prev_Word_Start (S);
      if Tagged_Present (N) then
         --  skipping "tagged"
         S := Search_Prev_Word_Start (S);
      end if;

      --  skipping "abstract"
      if Abstract_Present (N) then
         S := Search_Prev_Word_Start (S);
      end if;

      --  skipping "limited"
      if Limited_Present (N) then
         S := Search_Prev_Word_Start (S);
      end if;

      return S;
   end Private_Type_Definition_Beginning;

   ----------------------------
   -- Private_Unit_Beginning --
   ----------------------------

   function Private_Unit_Beginning (E : Asis.Element) return Source_Ptr is
      Result : Source_Ptr := Set_Image_Beginning (E);
   begin
      Result := Search_Prev_Word_Start (Result);
      return Result;
   end Private_Unit_Beginning;

   ---------------------------------
   -- Search_Identifier_Beginning --
   ---------------------------------

   function Search_Identifier_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      S   : Source_Ptr := Get_Location (E);
      Tmp : Asis.Element;
   begin

      if Nkind (Node (E)) = N_Attribute_Definition_Clause then
         --  this is a case of the attribute designator in a
         --  pseudo-attribute-reference from an attribute definition clause.
         --  Note, that we can have more than one attribute here (see FA30-016)
         --  e.g.
         --        for Root'Class'Output use Class_Output;

         S := Sloc (Sinfo.Name (Node (E)));

         Tmp := Prefix (Enclosing_Element (E));
         S   := Search_Rightmost_Symbol (S, ''');
         S   := Next_Identifier (S);

         if Expression_Kind (Tmp) = An_Attribute_Reference then
            S := Search_Rightmost_Symbol (S, ''');
            S := Next_Identifier (S);
         end if;

      elsif Special_Case (E) = Dummy_Class_Attribute_Designator then
         S := Search_Rightmost_Symbol (S, ''');
         S := Next_Identifier (S);
      elsif Nkind (Node (E)) = N_Attribute_Reference then
         S := Next_Identifier (S);
      end if;

      return S;
   end Search_Identifier_Beginning;

   -------------------------
   -- Set_Image_Beginning --
   -------------------------

   function Set_Image_Beginning (E : Asis.Element) return Source_Ptr is
      Result : Source_Ptr := First_Label_Beginning (E);
   begin

      if Result = No_Location then
         Result := Switch (Int_Kind (E)) (E);
      end if;

      return Result;

   end Set_Image_Beginning;

   -----------------------------------
   -- Search_Left_Parenthesis_After --
   -----------------------------------

   function Search_Left_Parenthesis_After
      (E : Asis.Element) return Source_Ptr
   is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return Search_Rightmost_Symbol (S, '(');
   end Search_Left_Parenthesis_After;

   -----------------------------
   -- Search_Prefix_Beginning --
   -----------------------------

   function Search_Prefix_Beginning (E : Asis.Element) return Source_Ptr is
      Prefix_To_Search : Asis.Element := E;
      E_Kind : constant Internal_Element_Kinds := Int_Kind (Prefix_To_Search);
   begin
      if E_Kind = A_Range_Attribute_Reference          or else
         E_Kind = A_Discrete_Range_Attribute_Reference or else
         E_Kind = A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
      then
         Prefix_To_Search :=
            Asis.Definitions.Range_Attribute (Prefix_To_Search);

      elsif E_Kind = A_Qualified_Expression or else
            E_Kind = A_Type_Conversion
      then

         Prefix_To_Search :=
            Asis.Expressions.Converted_Or_Qualified_Subtype_Mark
              (Prefix_To_Search);
      end if;

      if Int_Kind (Prefix_To_Search) = A_Defining_Expanded_Name then
         Prefix_To_Search :=
            Asis.Declarations.Defining_Prefix (Prefix_To_Search);

      elsif Int_Kind (Prefix_To_Search) /= An_Identifier then
         Prefix_To_Search := Asis.Expressions.Prefix (Prefix_To_Search);
      end if;

      return Set_Image_Beginning (Prefix_To_Search);

   end Search_Prefix_Beginning;

   -----------------------------------------
   -- Search_Subtype_Indication_Beginning --
   -----------------------------------------

   function Search_Subtype_Indication_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
   begin
      return Set_Image_Beginning (Asis.Definitions.Subtype_Mark (E));
   end Search_Subtype_Indication_Beginning;

   ----------------------------------
   -- Select_Alternative_Beginning --
   ----------------------------------

   function Select_Alternative_Beginning (E : Asis.Element) return Source_Ptr
   is
      Result : Source_Ptr;
      Tmp    : constant Asis.Element := Guard (E);
   begin

      if Is_Nil (Tmp) then

         declare
            Stmts : constant Asis.Element_List :=
              Sequence_Of_Statements (E, True);
         begin
            Result := Set_Image_Beginning (Stmts (Stmts'First));
         end;

      else
         Result := Set_Image_Beginning (Tmp);
         Result := Search_Prev_Word (Result);
         Result := Search_Beginning_Of_Word (Result);
      end if;

      return Result;
   end Select_Alternative_Beginning;

   -----------------------------
   -- Short_Circuit_Beginning --
   -----------------------------

   function Short_Circuit_Beginning (E : Asis.Element) return Source_Ptr is
   begin
      return Set_Image_Beginning (Short_Circuit_Operation_Left_Expression (E));
   end Short_Circuit_Beginning;

   ---------------------------------------
   -- Simple_Expression_Range_Beginning --
   ---------------------------------------

   function Simple_Expression_Range_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      El : constant Asis.Element := Lower_Bound (E);
   begin

      return Set_Image_Beginning (El);

   end Simple_Expression_Range_Beginning;

   -------------------------------
   -- Subprogram_Spec_Beginning --
   -------------------------------

   function Subprogram_Spec_Beginning (E : Asis.Element) return Source_Ptr is
      N      : Node_Id := Node (E);
      Result : Source_Ptr;
   begin
      N      := Specification (N);
      Result := Sloc (N);

--  --|A2005 start
      if Is_Overriding_Declaration (E) then
         Result := Search_Prev_Word_Start (Result);
      elsif Is_Not_Overriding_Declaration (E) then
         Result := Search_Prev_Word_Start (Result);
         Result := Search_Prev_Word_Start (Result);
      end if;
--  --|A2005 end

      return Result;
   end Subprogram_Spec_Beginning;

   -----------------------
   -- Subunit_Beginning --
   -----------------------

   function Subunit_Beginning (E : Asis.Element) return Source_Ptr is
      Result : Source_Ptr := Set_Image_Beginning (E);
   begin
      Result := Search_Left_Parenthesis (Result);
      Result := Search_Prev_Word_Start (Result);
      return Result;
   end Subunit_Beginning;

   --------------------------------------
   -- Tagged_Type_Definition_Beginning --
   --------------------------------------

   function Tagged_Type_Definition_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      S  : Source_Ptr                := Get_Location (E);
      Tr : constant Asis.Trait_Kinds := Trait_Kind (E);
   begin

      --  S points either to RECORD or to NULL. Therefore, we have
      --  to go at least one word left to take into account the
      --  TAGGED keyword. Depending on the trait, we may have one
      --  ot two more words left

      S := Search_Prev_Word (S);
      S := Search_Beginning_Of_Word (S);

      if Tr = An_Abstract_Limited_Trait         or else
         Tr = A_Limited_Trait                   or else
         Tr = An_Abstract_Limited_Private_Trait or else
         Tr = A_Limited_Private_Trait
      then
         S := Search_Prev_Word (S);
         S := Search_Beginning_Of_Word (S);
      end if;

      if Tr = An_Abstract_Limited_Trait         or else
         Tr = An_Abstract_Trait                 or else
         Tr = An_Abstract_Limited_Private_Trait or else
         Tr = An_Abstract_Private_Trait
      then
         S := Search_Prev_Word (S);
         S := Search_Beginning_Of_Word (S);
      end if;

      return S;

   end Tagged_Type_Definition_Beginning;

   -------------------------------
   -- Type_Definition_Beginning --
   -------------------------------

   function Type_Definition_Beginning (E : Asis.Element) return Source_Ptr is
      S  : Source_Ptr                := Get_Location (E);
      Tr : constant Asis.Trait_Kinds := Trait_Kind (E);
   begin
      if Tr = A_Limited_Trait or else Tr = A_Limited_Private_Trait then
         S := Search_Prev_Word (S);
         S := Search_Beginning_Of_Word (S);
      end if;

      return S;

   end Type_Definition_Beginning;

   ------------------------------------
   -- While_Loop_Statement_Beginning --
   ------------------------------------

   function While_Loop_Statement_Beginning
     (E : Asis.Element)
      return Source_Ptr
   is
      El : Asis.Element := Statement_Identifier (E);
      S  : Source_Ptr;
   begin

      if not Is_Nil (El) then
         return Set_Image_Beginning (El);
      end if;

      El := While_Condition (E);

      S := Set_Image_Beginning (El);
      S := Search_Prev_Word (S);
      S := Search_Beginning_Of_Word (S);

      return S;

   end While_Loop_Statement_Beginning;

   ---------------------------
   -- With_Clause_Beginning --
   ---------------------------

   function With_Clause_Beginning (E : Asis.Element) return Source_Ptr is

      S : Source_Ptr := Get_Location (E);

   begin

      S := Search_Prev_Word (S);

      S := Search_Beginning_Of_Word (S);

--  --|A2005 start
      case Trait_Kind (E) is
         when A_Limited_Trait |
              A_Private_Trait =>
            S := Search_Prev_Word (S);
            S := Search_Beginning_Of_Word (S);
         when A_Limited_Private_Trait =>
            S := Search_Prev_Word (S);
            S := Search_Beginning_Of_Word (S);
            S := Search_Prev_Word (S);
            S := Search_Beginning_Of_Word (S);
         when others =>
            null;
      end case;
--  --|A2005 end

      return S;

   end With_Clause_Beginning;

end A4G.Span_Beginning;
