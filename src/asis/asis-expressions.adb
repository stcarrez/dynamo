------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                      A S I S . E X P R E S S I O N S                     --
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

with Asis.Declarations;       use Asis.Declarations;
with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Extensions;         use Asis.Extensions;
with Asis.Elements;           use Asis.Elements;
with Asis.Iterator;           use Asis.Iterator;
with Asis.Limited_Views;      use Asis.Limited_Views;
with Asis.Statements;         use Asis.Statements;

with Asis.Set_Get;            use  Asis.Set_Get;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Output;            use A4G.A_Output;
with A4G.A_Sem;               use A4G.A_Sem;
with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.Asis_Tables;         use A4G.Asis_Tables;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Expr_Sem;            use A4G.Expr_Sem;
with A4G.Mapping;             use A4G.Mapping;
with A4G.Norm;                use A4G.Norm;
with A4G.Vcheck;              use A4G.Vcheck;

with Atree;                   use Atree;
with Einfo;                   use Einfo;
with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Output;                  use Output;
with Sinfo;                   use Sinfo;
with Snames;                  use Snames;
with Stand;                   use Stand;
with Uintp;                   use Uintp;

package body Asis.Expressions is

   Package_Name : constant String := "Asis.Expressions.";

------------------------------------------------------------------------------

   ---------------------------
   -- ASIS 2005 Draft stuff --
   ---------------------------

   ----------------------------------------------
   -- Corresponding_Expression_Type_Definition --
   ----------------------------------------------

   function Corresponding_Expression_Type_Definition
     (Expression : Asis.Expression)
      return       Asis.Element
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Result   : constant Asis.Expression   := Asis.Nil_Element;
   begin
      Check_Validity
        (Expression, Package_Name &
         "Corresponding_Expression_Type_Definition");

      if Arg_Kind not in Internal_Expression_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Corresponding_Expression_Type_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we filter out cases for which Nil_Element should be
      --  returned (may be, the check below is too trivial???)

      if not Is_True_Expression (Expression) then
         return Nil_Element;
      end if;

      Not_Implemented_Yet
        (Diagnosis => Package_Name &
                      "Corresponding_Expression_Type_Definition");

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Corresponding_Expression_Type_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Corresponding_Expression_Type_Definition",
            Ex          => Ex,
            Arg_Element => Expression);
   end Corresponding_Expression_Type_Definition;

------------------------------------------------------------------------------
--                       GENERAL DOCUMENTATION ITEMS                        --
------------------------------------------------------------------------------

   -----------------------------
   -- A_Function_Call Problem --
   -----------------------------

--  As for now, the following consideration can be applied to explicit
--  elements only!!!

--  The following situations should be distinguished:
--
--  - the called function is a predefined operation (it can also be its
--    renaming);
--  - the called function is a user-defined function, but it has an operation
--    symbol as its designator;
--  - the called function is a "usual" user-defined function;
--  - the called function is an attribute;
--
--  - the call is prefix;
--  - the call is infix;
--
--  As a result, in an AST we can have the following situations;
--
--  - prefix call to a predefined   operator:
--
--      Node is rewritten, the Original Node is of N_Function_Call kind
--      and its Name field points to the node of N_Operator_Symbol kind,
--      or of N_Expanded_Name kind, but anyway
--      the rewritten node is of N_Op_Xxxx kind and Original Node
--      contains the corresponding substructures only for naming
--      associations (if any). The rewritten node contains both parameters,
--      but in the form of positional association.
--
--      Compiler-time optimization for static expressions: the rewritten
--      node may be of N_Identifier kind (when optimizing calls to
--      boolean functions), N_Integer/Real_Literal king, when optimizing
--      function returning numeric results,
--
--  - infix  call to a predefined   operator:
--
--      Usually the node is unchanged, it is of N_Op_Xxx kind.
--      But the node may be rewritten into N_Integer_Literal or
--      N_Real_Literl, if the compiler optimizes an expression like
--      1 + 2! It also may be rewritten into N_Identifier node,
--      if the compiler optimizes an expression like "not True"
--      or "False and True". Finally, it may be rewritten into
--      N_String_Literal, if the compiler optimizes an expression like
--      "The " & "Beatles"
--
--      The  situation when a predefined operator is (re)defined by a renaming
--      declaration should be considered as a special case. The call is
--      rewritten into the node of the same N_Op_Xxx kind.
--
--  - prefix call to a user-defined operator:
--
--      node is unchanged, it is of N_Function_Call kind, but its Name
--      field points to the node of N_Operator_Symbol kind, if the prefix
--      consists on the operator symbol only, or to the node of N_Expanded_Name
--      kind.
--
--  - infix  call to a user-defined operator:
--
--      node is unchanged, it is of N_Function_Call kind, but its Name
--      field points to the node of N_Identifier kind.
--
--      !! THIS MEANS THAT AN ELEMENT OF An_Operator_Symbol KIND MAY BE
--         BASED ON THE NODE OF N_Identifier kind!!!
--
--  - prefix call to a "usual" user-defined function:
--
--      node is unchanged, it is of N_Function_Call kind, its Name
--      field points to the node of N_Identifier or N_Expanded_Name kind
--      (as it should).
--
--  - call to an attribute-function (only the prefix form is possible)
--
--       node may or may nor be rewritten, but the Node field of the
--       corresponding element definitely is of N_Attribute_Reference
--       kind, and its Expressions field should be used for obtaining the
--       list of the function call parameters.
--
--  This is important for the functions:
--
--       Prefix
--       Is_Prefix_Call
--       Corresponding_Called_Function (?)
--       Function_Call_Parameters
--
--  having A_Function_Call as their appropriate kind, and also for
--  Name_Image (in the case of getting the image of an operator symbol)
--
--  THE MAPPING.2 DOCUMENT ALSO REQUIRES CORRECTIONS!!!
------------------------------------------------------------------------------

   function Corresponding_Expression_Type
     (Expression : Asis.Expression)
      return       Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
   begin
      --  this is the result of the first attempt to implement this function
      --  the code is rather dirty, non-effective and not well-organized

      Check_Validity
        (Expression, Package_Name & "Corresponding_Expression_Type");

      if Arg_Kind not in Internal_Expression_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Expression_Type",
            Wrong_Kind => Arg_Kind);
      end if;

      --  first, we filter out cases for which Nil_Element should be
      --  returned (may be, the check below is too trivial???)

      if not Is_True_Expression (Expression) then
         return Nil_Element;
      end if;

      --  we incapsulate the real processing in the function
      --  A4G.Expr_Sem.Expr_Type:

      return Expr_Type (Expression);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Corresponding_Expression_Type");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Expression_Type",
            Ex          => Ex,
            Arg_Element => Expression);
   end Corresponding_Expression_Type;

   ----------------
   -- Name_Image --
   ----------------

   function Name_Image (Expression : Asis.Expression) return Wide_String is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node    : Node_Id;
      Arg_R_Node  : Node_Id;

      Def_Name    : Asis.Element;

      Tmp_Node    : Node_Id;

      Image_Start : Source_Ptr;
      Image_End   : Source_Ptr;
   begin
      Check_Validity (Expression, "Asis_Declarations.Name_Image");

      if not (Arg_Kind =  An_Identifier                  or else
              Arg_Kind in Internal_Operator_Symbol_Kinds or else
              Arg_Kind =  A_Character_Literal            or else
              Arg_Kind =  An_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Name_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      --  Let's first handle special cases:
      --
      --  = of fake Numeric_Error renaming (see B712-0050)
      --  = of a named number rewritten as a literal node (see BB10-002)

      case Special_Case (Expression) is
         when Numeric_Error_Renaming =>
            return "Constraint_Error";
         when Rewritten_Named_Number =>
            --  The problem here is that the references to the named numbers
            --  are rewritten as numeric literals, and in expanded generics the
            --  original tree structure for such references is lost. So in an
            --  expended generic we have to get the image from the defining
            --  occurrence of the name
            if Is_Part_Of_Instance (Expression) then
               return Defining_Name_Image
                        (Corresponding_Name_Definition (Expression));
            end if;
         when Dummy_Class_Attribute_Designator =>
            return "Class";
         when Dummy_Base_Attribute_Designator =>
            return "Base";
         when others =>
            null;
      end case;

      Arg_Node   := Node (Expression);
      Arg_R_Node := R_Node (Expression);

      if not Comes_From_Source (Arg_Node)
        and then
         Arg_Kind not in Internal_Operator_Symbol_Kinds
        and then
         Arg_Kind /= An_Enumeration_Literal
        and then
         Special_Case (Expression) not in
           Dummy_Base_Attribute_Prefix .. Dummy_Class_Attribute_Prefix
        and then
         Nkind (Arg_Node) /= N_Attribute_Reference
        and then
         Part_Of_Pass_Generic_Actual (Arg_Node)
      then
         Arg_Node   := Entity (Arg_Node);
         Arg_R_Node := Arg_Node;
      end if;

      case Arg_Kind is
         when A_Character_Literal =>

            return ''' &
                   Wide_Character'Val
                     (Integer (UI_To_CC
                       (Char_Literal_Value (Arg_Node)))) &
                    ''';

         when Internal_Operator_Symbol_Kinds =>
         --  three alternatives can be possible:
         --
         --  the Element is based on N_Op_Xxx node
         --               -> an infix call of a predefined operator
         --
         --  the Element is based on N_Operator_Symbol node
         --               -> definitely a prefix call
         --
         --  the Element is based on N_Identifier node
         --               -> an infix call of a user-defined operator
         --
         --  But in any case the result should be enclosed in quoters
            if Nkind (Arg_Node) = N_Operator_Symbol then
               return Wide_String_Image (Arg_Node);
            else
               --  N_Identifier and N_Op_Xxx nodes are processed
               --  in the same way
               return To_Wide_String (Operator_Image (Arg_Node));
            end if;
         when others =>
            --  really only   An_Identifier | An_Enumeration_Literal
            --  are possible,
            --  see the condition for defining the appropriate
            --  argument

            --  The special case exists for names which are type marks
            --  in the parameter and result profiles in the implicit inherited
            --  subprograms

            Tmp_Node := Node_Field_1 (Expression);

            if Nkind (Arg_Node) /= N_Attribute_Reference
              and then
               Nkind (Arg_Node) in N_Has_Entity
              and then
               Entity_Present (Arg_Node)
              and then
               Ekind (Entity (Arg_Node)) in Einfo.Type_Kind
              and then
               Is_From_Inherited (Expression)
              and then
               (Ekind (Tmp_Node) = E_Procedure or else
                Ekind (Tmp_Node) = E_Function)
            then
               Arg_Node := Get_Derived_Type
                 (Type_Entity     => Entity (Arg_Node),
                  Inherited_Subpr => Tmp_Node);
            end if;

            if Sloc (Arg_Node) <= Standard_Location
              or else
               (Special_Case (Expression) = Is_From_Imp_Neq_Declaration
               and then
                Nkind (Parent (Arg_Node)) = N_Function_Specification
               and then
                Arg_Node = Result_Definition (Parent (Arg_Node)))
               --  This condition describes the return type of implicit
               --  "/=". It is always Boolean
              or else
                Normalization_Case (Expression) =
                Is_Normalized_Defaulted_For_Box
               --  This condition defines the "implicit naming expression"
               --  that should be returned as a part of normalized generic
               --  association if the formal contains the box default, and
               --  the actual subprogram is defined at place of instantiation.
               --  We do not really care about the name, but the problem
               --  is that the Sloc field for the corresponding node is not
               --  set properly whereas Chars is.
            then
               return To_Wide_String (Normalized_Namet_String (Arg_Node));

            elsif Is_Rewrite_Substitution (Arg_R_Node)
              and then
                  Nkind (Arg_R_Node) = N_Identifier
              and then
                  Nkind (Arg_Node) = N_Identifier
              and then
                  Is_From_Instance (Arg_R_Node)
              and then
                  Ekind (Entity (Arg_R_Node)) = E_Package
              and then
                  Renamed_Entity (Entity (Arg_Node)) =
                     Entity (Arg_R_Node)
            then
               --  This condition corresponds to the case when we have a name
               --  that is a reference to the generic unit name inside expanded
               --  generic (see F627-001)

               Def_Name := Corresponding_Name_Definition (Expression);

               if Defining_Name_Kind (Def_Name) = A_Defining_Expanded_Name then
                  Def_Name := Defining_Selector (Def_Name);
               end if;

               return Defining_Name_Image (Def_Name);
            else

               if Special_Case (Expression) = Dummy_Class_Attribute_Prefix then
                  Arg_Node := Etype (Entity (Arg_Node));

               elsif Special_Case (Expression) =
                       Dummy_Base_Attribute_Prefix
               then
                  Arg_Node := Associated_Node (Arg_Node);

                  while Is_Itype (Arg_Node) loop
                     Arg_Node := Associated_Node_For_Itype (Arg_Node);
                     Arg_Node := Defining_Identifier (Arg_Node);
                  end loop;

               end if;

               Image_Start := Sloc (Arg_Node);

               --  special processing is needed for some cases:
               if Get_Character (Image_Start) = ''' then
                  --  special processing for an "ordinary" attribute
                  --  designator
                  Image_Start := Next_Identifier (Image_Start);
               elsif Get_Character (Image_Start) = '.' then
                  --  This is the case for a subtype mark in expanded
                  --  subprogram spec in case if in the template we have a
                  --  formal type, and the actual type is represented by
                  --  an expanded name
                  Image_Start := Image_Start + 1;
               elsif Nkind (Arg_Node) = N_Attribute_Definition_Clause then
                  --  special processing for an attribute designator being
                  --  a child element of a pseudo attribute reference from
                  --  an attribute definition clause
                  Image_Start := Search_Rightmost_Symbol (Image_Start, ''');
                  Image_Start := Next_Identifier (Image_Start);

                  if Nkind (Sinfo.Name (Arg_Node)) = N_Attribute_Reference then
                     --  See FA30-016
                     Image_Start := Search_Rightmost_Symbol (Image_Start, ''');
                     Image_Start := Next_Identifier (Image_Start);
                  end if;

               end if;

               Image_End   := Get_Word_End (P       => Image_Start,
                                            In_Word => In_Identifier'Access);

               return Get_Wide_Word (Image_Start, Image_End);

            end if;

      end case;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Name_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Name_Image",
            Ex          => Ex,
            Arg_Element => Expression);
   end Name_Image;
-----------------------------------------------------------------------------

   function Value_Image (Expression : Asis.Expression) return Wide_String
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;

      Image_Start : Source_Ptr;
      Image_End   : Source_Ptr;
   begin
      Check_Validity (Expression, Package_Name & "Value_Image");

      if not (Arg_Kind = An_Integer_Literal or else
              Arg_Kind = A_Real_Literal     or else
              Arg_Kind = A_String_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Value_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      if Sloc (Arg_Node) <= Standard_Location then

         case Arg_Kind is
            when An_Integer_Literal =>
               UI_Image (Intval (Arg_Node));
               return To_Wide_String (UI_Image_Buffer (1 .. UI_Image_Length));
            when A_Real_Literal =>
               return To_Wide_String (Ureal_Image (Arg_Node));
            when others =>
               raise Internal_Implementation_Error;
         end case;

      else
         case Arg_Kind is
            when   An_Integer_Literal
                 | A_Real_Literal      =>

               Image_Start := Sloc (Arg_Node);
               Image_End   := Get_Num_Literal_End (P => Image_Start);
               return Get_Wide_Word (Image_Start, Image_End);

            when A_String_Literal =>
               return Wide_String_Image (Arg_Node);
            when others =>
               raise Internal_Implementation_Error;
               --  this choice can never been reached,
               --  see the condition for defining the appropriate argument
         end case;
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Value_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Value_Image",
            Ex          => Ex,
            Arg_Element => Expression);
   end Value_Image;
-----------------------------------------------------------------------------

------------------------------------------------------------------------------
--  OPEN PROBLEMS:
--
--  1. A_Defining_Expanded_Name: is the recursive construction of the result
--     of the String type a really good thing here? The performance can be poor
--     but, from the other hand, this can happen not very often.
--
--  2. The Asis_Declarations.Defining_Name_Image function contains the
--     (almost) exact
--     copy of the part of the code of this function (except the part for
--     processing A_Defining_Expanded_Name). May be, it should be better
--     to separate it on low-level function.
-------------------------------------------------------------------------------
--  PARTIALLY IMPLEMENTED, case Implicitly => True is not implemented

   function References
     (Name            : Asis.Element;
      Within_Element  : Asis.Element;
      Implicitly      : Boolean := False)
      return            Asis.Name_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Name);
      Arg_Name : Asis.Element;

      Search_Reference_Control : Traverse_Control := Continue;
      Search_Reference_State   : No_State         := Not_Used;

      procedure Check_Reference
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);
      --  Actual for Pre_Operation: checks if Element is a reference to
      --  Name, and if it is, puts it into Element table

      procedure Collect_References is new Traverse_Element
        (State_Information => No_State,
         Pre_Operation     => Check_Reference,
         Post_Operation    => No_Op);

      procedure Check_Reference
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
      begin

         pragma Unreferenced (Control);
         pragma Unreferenced (State);

         if Is_Reference (Arg_Name, Element) then
            Asis_Element_Table.Append (Element);
         end if;

      end Check_Reference;

   begin
      Check_Validity (Name, Package_Name & "References");

      if not (Arg_Kind in Internal_Defining_Name_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "References",
            Wrong_Kind => Arg_Kind);
      elsif Implicitly then
         Not_Implemented_Yet
           (Diagnosis => Package_Name & "References (Implicitly => True)");
      end if;

      Arg_Name := Enclosing_Element (Name);

      if Int_Kind (Arg_Name) /= A_Defining_Expanded_Name then
         Arg_Name := Name;
      end if;

      Asis_Element_Table.Init;

      Collect_References
        (Element => Within_Element,
         Control => Search_Reference_Control,
         State   => Search_Reference_State);

      return Asis.Name_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Name,
               Bool_Par   => Implicitly,
               Outer_Call => Package_Name & "References");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "References",
            Ex            => Ex,
            Arg_Element   => Name,
            Arg_Element_2 => Within_Element,
            Bool_Par_ON   => Implicitly);
   end References;
------------------------------------------------------------------------------
--  PARTIALLY IMPLEMENTED, case Implicitly => True is not implemented

   function Is_Referenced
     (Name            : Asis.Element;
      Within_Element  : Asis.Element;
      Implicitly      : Boolean := False)
      return            Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Name);
      Arg_Name : Asis.Element;

      Result   : Boolean := False;

      Search_Reference_Control : Traverse_Control := Continue;
      Search_Reference_State   : No_State         := Not_Used;

      procedure Search_Reference
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);
      --  Actual for Pre_Operation: looks for (the first) reference to Name.
      --  Sets Result to True if such a reference is found

      procedure Check_Reference is new Traverse_Element
        (State_Information => No_State,
         Pre_Operation     => Search_Reference,
         Post_Operation    => No_Op);

      procedure Search_Reference
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
      begin

         pragma Unreferenced (State);

         if Is_Reference (Arg_Name, Element) then
            Result  := True;
            Control := Terminate_Immediately;
         end if;

      end Search_Reference;

   begin --  Is_Referenced
      Check_Validity (Name, Package_Name & "Is_Referenced");

      if not (Arg_Kind in Internal_Defining_Name_Kinds) then
         return False;

      elsif Implicitly then
         Not_Implemented_Yet
           (Diagnosis => Package_Name & "Is_Referenced (Implicitly => True)");
      end if;

      Arg_Name := Enclosing_Element (Name);

      if Int_Kind (Arg_Name) /= A_Defining_Expanded_Name then
         Arg_Name := Name;
      end if;

      Check_Reference
        (Element => Within_Element,
         Control => Search_Reference_Control,
         State   => Search_Reference_State);

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Name,
               Bool_Par   => Implicitly,
               Outer_Call => Package_Name & "Is_Referenced");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Referenced",
            Ex            => Ex,
            Arg_Element   => Name,
            Arg_Element_2 => Within_Element,
            Bool_Par_ON   => Implicitly);
   end Is_Referenced;

   -----------------------------------
   -- Corresponding_Name_Definition --
   -----------------------------------

   function Corresponding_Name_Definition
     (Reference : Asis.Expression)
      return      Asis.Defining_Name
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Reference);
      Result      : Element;
      Parent_Node : Node_Id;
   begin
      Check_Validity
        (Reference, Package_Name & "Corresponding_Name_Definition");

      if not (Arg_Kind =  An_Identifier                  or else
              Arg_Kind in Internal_Operator_Symbol_Kinds or else
              Arg_Kind =  A_Character_Literal            or else
              Arg_Kind =  An_Enumeration_Literal)        or else
              Is_Aspect_Mark (Reference)                 or else
              Is_Aspect_Specific_Identifier (Reference)  or else
              (not Is_Uniquely_Defined (Reference))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Name_Definition",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_From_Limited_View (Reference) then
         return Nil_Element;
      end if;

      if Arg_Kind = An_Identifier or else
         Arg_Kind = An_Enumeration_Literal or else
         Arg_Kind in Internal_Operator_Symbol_Kinds
      then

         --  A special case of fake Numeric_Error renaming is handled
         --  separately (see B712-005)

         if Special_Case (Reference) = Numeric_Error_Renaming then

            Result :=
              Node_To_Element_New
                (Node             => Standard_Constraint_Error,
                 Starting_Element => Reference,
                 Internal_Kind    => A_Defining_Identifier,
                 Spec_Case        => Explicit_From_Standard);

         else

            if Is_Predefined_Operator (Reference)
              or else
               Is_Default_For_Null_Procedure (Reference)
            then
               Result := Nil_Element;
            else
               --  First, process a special case when  Reference is a label
               --  from goto statement that is rewritten into an infinite loop
               --  in the tree. The node of such Reference does not have the
               --  Entity field set

               Parent_Node := Parent (R_Node (Reference));

               if Nkind (Parent_Node) = N_Loop_Statement
                 and then
                  Is_Rewrite_Substitution (Parent_Node)
                 and then
                  Nkind (Original_Node (Parent_Node)) = N_Goto_Statement
               then
                  Result := Enclosing_Element (Reference);
                  Result := Corresponding_Destination_Statement (Result);
                  Result := Label_Names (Result) (1);
               else
                  Result := Identifier_Name_Definition (Reference);
               end if;
            end if;

            if Is_Nil (Result) then
               null;
            elsif Special_Case (Reference) = Rewritten_Named_Number then
               Correct_Result (Result, Reference);
            elsif Defining_Name_Kind (Result) /= A_Defining_Expanded_Name
               and then
                  Is_Implicit_Formal_Par (Result)
            then
               Correct_Impl_Form_Par (Result, Reference);
            end if;

         end if;

      else

         Result := Character_Literal_Name_Definition (Reference);

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Reference,
               Outer_Call => Package_Name & "Corresponding_Name_Definition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Name_Definition",
            Ex          => Ex,
            Arg_Element => Reference);
   end Corresponding_Name_Definition;

   ----------------------------------------
   -- Corresponding_Name_Definition_List --
   ----------------------------------------

   function Corresponding_Name_Definition_List
     (Reference : Asis.Element)
      return      Asis.Defining_Name_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Reference);
   begin
      Check_Validity (Reference, Package_Name & "Name_Definition_List");

      if not (Arg_Kind =  An_Identifier                  or else
              Arg_Kind in Internal_Operator_Symbol_Kinds or else
              Arg_Kind =  A_Character_Literal            or else
              Arg_Kind =  An_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Name_Definition_List",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_From_Limited_View (Reference) then
         return Nil_Element_List;
      end if;

      Asis_Element_Table.Init;

      if Needs_List (Reference) then
         Collect_Overloaded_Entities (Reference);
      else
         Asis_Element_Table.Append (Corresponding_Name_Definition (Reference));
      end if;

      return Asis.Declaration_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (
               Argument  => Reference,
               Outer_Call => Package_Name & "Name_Definition_List");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Name_Definition_List",
            Ex          => Ex,
            Arg_Element => Reference);
   end Corresponding_Name_Definition_List;

------------------------------------------------------------------------------
   function Corresponding_Name_Declaration
     (Reference : Asis.Expression)
      return      Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Reference);
      Result   : Asis.Element;
   begin
      Check_Validity
        (Reference, Package_Name & "Corresponding_Name_Declaration");

      if not (Arg_Kind =  An_Identifier                  or else
              Arg_Kind in Internal_Operator_Symbol_Kinds or else
              Arg_Kind =  A_Character_Literal            or else
              Arg_Kind =  An_Enumeration_Literal)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Name_Declaration",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_From_Limited_View (Reference) then
         return Nil_Element;
      end if;

      Result := Corresponding_Name_Definition (Reference);

      if not Is_Nil (Result) then
         Result := Enclosing_Element (Result);
      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Reference,
               Outer_Call => Package_Name & "Corresponding_Name_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Name_Declaration",
            Ex          => Ex,
            Arg_Element => Reference);
   end Corresponding_Name_Declaration;
-----------------------------------------------------------------------------

   function Prefix (Expression : Asis.Expression) return Asis.Expression is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node      : Node_Id;
      Returned_Kind : Internal_Expression_Kinds;
      Res_Spec_Case : Special_Cases := Not_A_Special_Case;
      Result_Node   : Node_Id;
   begin

      --  ???
      --  the code is really awful!!! too many return statements!!!
      Check_Validity (Expression, Package_Name & "Prefix");

      if not (Arg_Kind =  An_Explicit_Dereference            or else
               Arg_Kind in Internal_Attribute_Reference_Kinds or else
               Arg_Kind =  A_Function_Call                    or else
               Arg_Kind =  An_Indexed_Component               or else
               Arg_Kind =  A_Selected_Component               or else
               Arg_Kind =  A_Slice)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Prefix",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      if (Nkind (Arg_Node) = N_Identifier or else
          Nkind (Arg_Node) = N_Expanded_Name)
        and then
         --  Special case: F.A, where either F or A is a function call
         Nkind (R_Node (Expression)) = N_Function_Call
      then
         Arg_Node := R_Node (Expression);
      end if;

      --  We might implement this as shown below (the name following
      --  the arrow (->) is the name of the Sinfo-defined tree access
      --  function which should be used to obtain the result node,
      --  the infix form of A_Function_Call should be treated as a special
      --  case:

--    case Arg_Kind is
--
--    when An_Explicit_Dereference =>
--       -- N_Explicit_Dereference -> Prefix
--
--    when Internal_Attribute_Reference_Kinds =>
--       -- N_Attribute_Reference -> Prefix
--
--    when A_Function_Call         =>
--       -- N_Function_Call -- prefix call -> Name
--       -- N_Op_*          -- infix call  -> the result should be based
--       --                                    on the same node
--       -- N_Attribute_Reference          -> the result should be based
--                                            on the same node
--
--    when An_Indexed_Component    =>
--       -- N_Indexed_Component -> Prefix
--
--    when A_Selected_Component    =>
--       -- N_Selected_Component -> Prefix
--       -- N_Expanded_Name      -> Prefix
--
--    when A_Slice                 =>
--       -- N_Slice ->  Prefix
--
--    when others =>
--
--       return Nil_Element; -- to make the code formally correct,
--                           -- see the condition for determining the
--    end case;              -- appropriate element

      --  but it is more convenient to use the Node_Kind-driven
      --  case statement for implementing just the same processing:

      Result_Node := Arg_Node;
      --  for N_Op_* cases only; it may seem as being a bit tricky, but another
      --  variants are too long ;-)

      if Debug_Flag_1 then
         Write_Node (Arg_Node, "Prefix: Arg_Node -> ");
         Write_Eol;
      end if;

      case Nkind (Arg_Node) is

         when   N_Explicit_Dereference   -- Prefix node access function
              | N_Slice                  -- should be used for tree traversing
              | N_Indexed_Component      -- traversing
              | N_Selected_Component
              | N_Expanded_Name       =>

            --  !!The Node of N_Identifier kind cannot be processed by the
            --  !!general Node_To_Element function (involving the auto
            --  !!determination of the Element kind), because the subcomponents
            --  !!of the prefix of a defining_unit_name do not have the Entity
            --  !!attribute set.

            Result_Node := Prefix (Arg_Node);

            if Nkind (Result_Node) = N_Identifier and then
               not Is_Rewrite_Substitution (Result_Node)
            then

               return Node_To_Element_New (
                        Node                     => Result_Node,
                        Starting_Element         => Expression,
                        Internal_Kind            => An_Identifier,
                        Considering_Parent_Count => False);

            else

               return Node_To_Element_New (
                        Node                     => Result_Node,
                        Starting_Element         => Expression,
                        Considering_Parent_Count => False);
            end if;

         when N_Attribute_Definition_Clause =>
            --  special processing for pseudo-attribute being the child
            --  element of An_Attribute_Definition_Clause Element
            Result_Node := Sinfo.Name (Arg_Node);

            return Node_To_Element_New
                     (Starting_Element => Expression,
                      Node             => Result_Node);

         when N_Attribute_Reference =>
         --  two cases should be distinguished: when the function argument is
         --  of A_Function_Call kind and when it is in
         --  Internal_Attribute_Reference_Kinds

            if Debug_Flag_1 then
               Write_Str
                  (Package_Name & "Prefix: "
                 & "processing N_Attribute_Reference Node...");
               Write_Eol;
               Write_Node (Arg_Node, "Arg Node -> ");
               Write_Eol;
            end if;

            if Arg_Kind =  A_Function_Call then
               --  the result should be based on the same node, so the
               --  Result_Node setting should not be changed, but the kind
               --  of the result should be determined by hand

               Returned_Kind := Subprogram_Attribute_Kind (Result_Node);

               return Node_To_Element_New (
                        Node                     => R_Node (Expression),
                        Starting_Element         => Expression,
                        Internal_Kind            => Returned_Kind,
                        Considering_Parent_Count => False);

            else
               --  just the same processing as for the previous
               --  case alternative:
               Result_Node := Prefix (Arg_Node);

               if Debug_Flag_1 then
                  Write_Str (Package_Name & "Prefix: "
                     & "processing N_Attribute_Reference Prefix Node...");
                  Write_Eol;
                  Write_Node (Result_Node, "Result Node -> ");
                  Write_Eol;
               end if;

               if Nkind (Result_Node) = N_Identifier and then
                  not Is_Rewrite_Substitution (Result_Node)
               then

                  return Node_To_Element_New (
                           Node                     => Result_Node,
                           Starting_Element         => Expression,
                           Internal_Kind            => An_Identifier,
                           Considering_Parent_Count => False);
               else

                  return Node_To_Element_New (
                           Node                     => Result_Node,
                           Starting_Element         => Expression,
                           Considering_Parent_Count => False);
               end if;

            end if;

      when   N_Function_Call |
             N_Procedure_Call_Statement =>

         --  N_Procedure_Call_Statement corresponds to the argument of pragma
         --  Debug which is treated as A_Function_Call
         --  Name node access function should be used for tree traversing

         if Debug_Flag_1 then
            Write_Str ("Prefix: processing N_Function_Call Node:");
            Write_Eol;
            Write_Str ("Node is rewritten more than once - ");
            Write_Eol;
            Write_Str (Boolean'Image (Is_Rewrite_Substitution (Arg_Node)));
            Write_Eol;
         end if;

         --  See comments under "A_Function_Call Problem" headline in the
         --  beginning of the package body - we shall distinguish the case
         --  of the infix call of the user - defined operator. The Name
         --  function gives the result of N_Identifier kind, but really it
         --  corresponds to the An_Operator_Symbol Element!

         Result_Node := Sinfo.Name (Arg_Node);

         if Is_Rewrite_Substitution (Result_Node)
           and then
            Nkind (Result_Node) = N_Explicit_Dereference
           and then
            Nkind (Prefix (Result_Node)) = N_Function_Call
         then
            --  Needed to process cases like F (1), where F - parameterless
            --  function that returns access-to-subprogram result.
            Result_Node := Prefix (Result_Node);
         end if;

         if Debug_Flag_1 then
            Write_Node (Result_Node, "Prefix: Result_Node -> ");
            Write_Eol;
         end if;

         if Nkind (Result_Node) = N_Identifier and then
            Chars (Result_Node) in Any_Operator_Name
         then
            --  really we have a infix call of a user-defined operator!
            Returned_Kind :=
               A4G.Mapping.N_Operator_Symbol_Mapping (Result_Node);
         else
            return Node_To_Element_New (
                     Node                     => Result_Node,
                     Starting_Element         => Expression,
                     Considering_Parent_Count => False);
         end if;

--    when N_Op_* => --  N_Op_* cases corresponding to the infix function
                     --  call, the result should be based on the same node;
                     --  only the internal kind of the returned element is
                     --  determined in the case statement; the return
                     --  statement for N_Op_* alternatives is located
                     --  outside the case statement
      when N_Op_And       =>  -- "and"
         Returned_Kind := An_And_Operator;
      when N_Op_Or        =>  -- "or"
         Returned_Kind := An_Or_Operator;
      when N_Op_Xor       =>  -- "xor"
         Returned_Kind := An_Xor_Operator;
      when N_Op_Eq        =>  -- "="
         Returned_Kind := An_Equal_Operator;
      when N_Op_Ne        =>  -- "/="
         Returned_Kind := A_Not_Equal_Operator;
      when N_Op_Lt        =>  -- "<"
         Returned_Kind := A_Less_Than_Operator;
      when N_Op_Le        =>  -- "<="
         Returned_Kind := A_Less_Than_Or_Equal_Operator;
      when N_Op_Gt        =>  -- ">"
         Returned_Kind := A_Greater_Than_Operator;
      when N_Op_Ge        =>  -- ">="
         Returned_Kind := A_Greater_Than_Or_Equal_Operator;
      when N_Op_Add       =>  -- "+"  (binary)
         Returned_Kind := A_Plus_Operator;
      when N_Op_Subtract  =>  -- "-"  (binary)
         Returned_Kind := A_Minus_Operator;
      when N_Op_Concat    =>  -- "&"
         Returned_Kind := A_Concatenate_Operator;
      when N_Op_Plus      =>  -- "+"  (unary)
         Returned_Kind := A_Unary_Plus_Operator;
      when N_Op_Minus     =>  -- "-"  (unary)
         Returned_Kind := A_Unary_Minus_Operator;
      when N_Op_Multiply  =>  -- "*"
         Returned_Kind := A_Multiply_Operator;
      when N_Op_Divide    =>  -- "/"
         Returned_Kind := A_Divide_Operator;
      when N_Op_Mod       =>  -- "mod"
         Returned_Kind := A_Mod_Operator;
      when N_Op_Rem       =>  -- "rem"
         Returned_Kind := A_Rem_Operator;
      when N_Op_Expon     =>  -- "**"
         Returned_Kind := An_Exponentiate_Operator;
      when N_Op_Abs       =>  -- "abs"
         Returned_Kind := An_Abs_Operator;
      when N_Op_Not       =>  -- "not"
         Returned_Kind := A_Not_Operator;

      when N_Identifier =>
         --  See F410-011. The argument is the artificial 'Class attribute
         --  reference in the instance that corresponds to the actual
         --  class-wide type.

         if Is_Part_Of_Instance (Expression)
           and then
            (Represents_Class_Wide_Type_In_Instance (Arg_Node)
            or else
             Represents_Base_Type_In_Instance (Arg_Node))
         then
            Result_Node   := Arg_Node;
            Returned_Kind := An_Identifier;

            if Represents_Class_Wide_Type_In_Instance (Arg_Node) then
               Res_Spec_Case := Dummy_Class_Attribute_Prefix;
            else
               Res_Spec_Case := Dummy_Base_Attribute_Prefix;
            end if;

         elsif (Nkind (Arg_Node) = N_Identifier
               and then
                not Comes_From_Source (Arg_Node)
               and then
                Ekind (Entity (Arg_Node)) = E_Class_Wide_Type) --  See FA13-008
             or else
               Is_Aspect_Mark (Expression)
         then
            Result_Node   := Arg_Node;
            Returned_Kind := An_Identifier;
         else
            pragma Assert (False);
            null;
         end if;

      when others =>
         --  to make the code formally correct, nothing else could be possible
         pragma Assert (False);
         return Nil_Element;

      end case;

      --  forming the result for N_Op_* cases and for the infix call of a
      --  user-defined operator:

      --  ??? !!! This is the ad hoc patch for Enclosing_Element needs:
      --  we should keep rewritten node for function calls rewritten as
      --  results of compiler-time optimisations

      if Returned_Kind in Internal_Operator_Symbol_Kinds
       and then
         Nkind (Node (Expression)) /= N_Function_Call
       and then
         Is_Rewrite_Substitution (R_Node (Expression))
      then
         Result_Node := R_Node (Expression);
      end if;

      return Node_To_Element_New (
               Node                     => Result_Node,
               Starting_Element         => Expression,
               Internal_Kind            => Returned_Kind,
               Spec_Case                => Res_Spec_Case,
               Considering_Parent_Count => False);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Prefix");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Prefix",
            Ex          => Ex,
            Arg_Element => Expression);
   end Prefix;
------------------------------------------------------------------------------

   function Index_Expressions
     (Expression : Asis.Expression)
      return       Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Expression, Package_Name & "Index_Expressions");

      if not (Arg_Kind =  An_Indexed_Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Index_Expressions",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return N_To_E_List_New
               (List             => Sinfo.Expressions (Arg_Node),
                Starting_Element => Expression);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Index_Expressions");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Index_Expressions",
            Ex          => Ex,
            Arg_Element => Expression);
   end Index_Expressions;
-----------------------------------------------------------------------------

   function Slice_Range
     (Expression : Asis.Expression)
      return       Asis.Discrete_Range
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Expression, Package_Name & "Slice_Range");

      if not (Arg_Kind = A_Slice) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Slice_Range",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);
      return Node_To_Element_New
               (Node             => Sinfo.Discrete_Range (Arg_Node),
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Slice_Range");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Slice_Range",
            Ex          => Ex,
            Arg_Element => Expression);
   end Slice_Range;
-----------------------------------------------------------------------------

   -----------------------------------------------
   -- local functions for the Selector function --
   -----------------------------------------------

   function Last_Selector (Node : Node_Id) return Boolean;
   --  Node should be of N_Identifier kind, obtained as a Selector_Name
   --  form N_Selected_Component or N_Expanded name node;
   --  a caller is responsible for this. This function checks if
   --  Node is the last selector in the corresponding selected component
   --  or expanded name

   function Is_Enumeration_Literal (Node : Node_Id) return Boolean;
   --  Node should be of N_Identifier kind, obtained as a Selector_Name
   --  form N_Selected_Component or N_Expanded name node;
   --  moreover, this is the last selector in the enclosing construct
   --  a caller is responsible for this. This function checks if
   --  its argument should be classified as An_Enumeration_Literal
   --  by checking the Entity fields of the outermost "enclosing"
   --  node of N_Expanded_Name kind

   function Last_Selector (Node : Node_Id) return Boolean is
   begin
      return not ((Nkind (Parent (Node)) = N_Expanded_Name or else
                    Nkind (Parent (Node)) = N_Selected_Component)
                 and then
                   (Nkind (Parent (Parent (Node))) = N_Expanded_Name or else
                    Nkind (Parent (Parent (Node))) = N_Selected_Component));
   end Last_Selector;

   function Is_Enumeration_Literal (Node : Node_Id) return Boolean is
      Entity_Node : Node_Id := Empty;
   begin
      Entity_Node := Entity (Node);

      if No (Entity_Node) then

         Entity_Node := Original_Node (Parent (Node));

         if Nkind (Entity_Node) = N_Function_Call then
            --  this may be the case for an expanded name which is a reference
            --  to an overloaded enumeration literal
            Entity_Node := Sinfo.Name (Entity_Node);
         end if;

         if Nkind (Entity_Node) in N_Has_Entity then
            Entity_Node := Entity (Entity_Node);
         end if;

      end if;

      if Nkind (Entity_Node) in N_Entity and then
         Ekind (Entity_Node) = E_Enumeration_Literal
      then
         return True;
      else
         return False;
      end if;

   end Is_Enumeration_Literal;

   function Selector
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds;
   begin

      Check_Validity (Expression, Package_Name & "Selector");

      if not (Arg_Kind = A_Selected_Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Selector",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      Result_Node := Selector_Name (Arg_Node);

      if not (Nkind (Result_Node) = N_Identifier) then

         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Expression);
      end if;

      if Last_Selector (Result_Node) and then
         Is_Enumeration_Literal (Result_Node)
      then
         Result_Kind := An_Enumeration_Literal;
      else
         Result_Kind := An_Identifier;
      end if;

      return Node_To_Element_New (Node             => Result_Node,
                                  Internal_Kind    => Result_Kind,
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Selector");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Selector",
            Ex          => Ex,
            Arg_Element => Expression);
   end Selector;
-----------------------------------------------------------------------------

   function Attribute_Designator_Identifier
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Result   : Asis.Element                    := Nil_Element;
   begin
      Check_Validity
        (Expression, Package_Name & "Attribute_Designator_Identifier");

      if not (Arg_Kind in Internal_Attribute_Reference_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Attribute_Designator_Identifier",
            Wrong_Kind => Arg_Kind);
      end if;

      --  Attribute designator is based on the same node as the argument of
      --  the function, this is a special case of identifier handling!!!

      Result := Node_To_Element_New
        (Starting_Element         => Expression,
         Node                     => R_Node (Expression),
         Internal_Kind            => An_Identifier);

      if Represents_Class_Wide_Type_In_Instance (Node (Expression))
         --  See F410-011
        or else
         Nkind (Parent (R_Node (Expression))) = N_Aspect_Specification
      then

         Set_Special_Case (Result, Dummy_Class_Attribute_Designator);
      elsif Represents_Base_Type_In_Instance ((Node (Expression))) then
         Set_Special_Case (Result, Dummy_Base_Attribute_Designator);
      end if;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Attribute_Designator_Identifier");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Attribute_Designator_Identifier",
            Ex          => Ex,
            Arg_Element => Expression);
   end Attribute_Designator_Identifier;
-----------------------------------------------------------------------------

   function Attribute_Designator_Expressions
     (Expression : Asis.Expression)
      return       Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Attribute_Designator_Expressions");

      if not (Arg_Kind = A_First_Attribute                   or else
              Arg_Kind = A_Last_Attribute                    or else
              Arg_Kind = A_Length_Attribute                  or else
              Arg_Kind = A_Range_Attribute                   or else
              Arg_Kind = An_Implementation_Defined_Attribute or else
              Arg_Kind = An_Unknown_Attribute)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Attribute_Designator_Expressions",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      if Nkind (Arg_Node) = N_Attribute_Definition_Clause or else
         Is_GNAT_Attribute_Routine (Arg_Node)
      then
         return Nil_Element_List;
         --  just in case - for an implementation-defined attribute in an
         --  attribute definition clause
      end if;

      if Debug_Flag_1 then
         Write_Str ("Attribute_Designator_Expressions: Arg_Node:");
         Write_Eol;
         Write_Node (Arg_Node, "->");
         Write_Eol;
      end if;

      return N_To_E_List_New
               (List             => Sinfo.Expressions (Arg_Node),
                Starting_Element => Expression);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Attribute_Designator_Expressions");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Attribute_Designator_Expressions",
            Ex          => Ex,
            Arg_Element => Expression);
   end Attribute_Designator_Expressions;
------------------------------------------------------------------------------
   function Record_Component_Associations
     (Expression : Asis.Expression;
      Normalized : Boolean := False)
      return       Asis.Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;

      Returned_List_Length   : ASIS_Integer;
      Positional_List_Length : Int;
      Named_List_Length      : Int;
   begin

      Check_Validity
        (Expression, Package_Name & "Record_Component_Associations");

      if not (Arg_Kind =  A_Record_Aggregate or else
              Arg_Kind =  An_Extension_Aggregate)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Record_Component_Associations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      if Null_Record_Present (Arg_Node) then
         return Nil_Element_List;
      end if;

      if Normalized then
         return Normalized_Record_Component_Associations (Expression);
      end if;

      --  computing the returned list length:

      if Present (Sinfo.Expressions (Arg_Node)) then
         Positional_List_Length := List_Length (Sinfo.Expressions (Arg_Node));
      else
         Positional_List_Length := 0;
      end if;

      if Present (Component_Associations (Arg_Node)) then
         Named_List_Length := List_Length (Component_Associations (Arg_Node));
      else
         Named_List_Length := 0;
      end if;

      Returned_List_Length :=
         ASIS_Integer (Positional_List_Length + Named_List_Length);
      --  Returned_List_Length cannot be equal to 0 here!

      declare -- for proper exception handling
         Returned_List : Asis.Association_List (1 .. Returned_List_Length);
      begin
         --  obtaining the association list:

         if Debug_Flag_1 then
            Write_Str ("obtaining the association list");
            Write_Eol;
            Write_Str ("List length is ");
            Write_Int (Int (Returned_List_Length));
            Write_Eol;
         end if;

         Returned_List :=
            N_To_E_List_New
              (List             => Sinfo.Expressions (Arg_Node),
               Internal_Kind    => A_Record_Component_Association,
               Starting_Element => Expression)
          &
            N_To_E_List_New
              (List             => Component_Associations (Arg_Node),
               Internal_Kind    => A_Record_Component_Association,
               Starting_Element => Expression);

         return Returned_List;

      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Bool_Par   => Normalized,
               Outer_Call => Package_Name & "Record_Component_Associations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Record_Component_Associations",
            Ex          => Ex,
            Arg_Element => Expression,
            Bool_Par_ON => Normalized);
   end Record_Component_Associations;
-----------------------------------------------------------------------------

   function Extension_Aggregate_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Extension_Aggregate_Expression");

      if not (Arg_Kind =  An_Extension_Aggregate) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Extension_Aggregate_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);
      return Node_To_Element_New
               (Node             => Ancestor_Part (Arg_Node),
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Extension_Aggregate_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Extension_Aggregate_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Extension_Aggregate_Expression;
-----------------------------------------------------------------------------

   function Array_Component_Associations
     (Expression : Asis.Expression)
      return       Asis.Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);

      Arg_Node           : Node_Id;
      Named_Associations : List_Id;
      Pos_Associations   : List_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Array_Component_Associations");

      if not (Arg_Kind = A_Positional_Array_Aggregate or else
              Arg_Kind = A_Named_Array_Aggregate)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Array_Component_Associations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node           := Node (Expression);
      Named_Associations := Component_Associations (Arg_Node);
      Pos_Associations   := Sinfo.Expressions (Arg_Node);

      if Arg_Kind = A_Named_Array_Aggregate then

         return N_To_E_List_New
                  (List             => Named_Associations,
                   Internal_Kind    => An_Array_Component_Association,
                   Starting_Element => Expression);

      elsif No (Named_Associations) then
         --  that is, no "others" choice in a positional array aggregate

         return N_To_E_List_New
                  (List             => Pos_Associations,
                   Internal_Kind    => An_Array_Component_Association,
                   Starting_Element => Expression);

      else
         --  a positional array aggregate with "others"

         return (N_To_E_List_New
                   (List          => Pos_Associations,
                    Internal_Kind => An_Array_Component_Association,
                    Starting_Element => Expression)
                &
                 Node_To_Element_New
                   (Node             => First (Named_Associations),
                    Internal_Kind    => An_Array_Component_Association,
                    Starting_Element => Expression));

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Array_Component_Associations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Component_Associations",
            Ex          => Ex,
            Arg_Element => Expression);
   end Array_Component_Associations;
-----------------------------------------------------------------------------

   function Array_Component_Choices
     (Association : Asis.Association)
      return        Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Association, Package_Name & "Array_Component_Choices");

      if not (Arg_Kind = An_Array_Component_Association) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Array_Component_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);

      if Nkind (Arg_Node) = N_Component_Association  then
         --  named association
         return Discrete_Choice_Node_To_Element_List
                 (Choice_List      => Choices (Arg_Node),
                  Starting_Element => Association);

      else
         --  positional association
         return Nil_Element_List;
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Array_Component_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Component_Choices",
            Ex          => Ex,
            Arg_Element => Association);
   end Array_Component_Choices;
-----------------------------------------------------------------------------
   function Record_Component_Choices
     (Association : Asis.Association)
      return        Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node : Node_Id;

      Result_Node    : Node_Id;
      Temp_Node      : Node_Id;
      Result_Unit    : Asis.Compilation_Unit;
      Result_Element : Asis.Element;
      --  for handling the normalized A_Record_Component_Association only
   begin

      Check_Validity (Association, Package_Name & "Record_Component_Choices");

      if not (Arg_Kind = A_Record_Component_Association) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Record_Component_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);

      if Normalization_Case (Association) = Is_Normalized  then
         --  it is definitely A_Record_Component_Association
         --  based on the N_Component_Association Node
         --  and the returned list should definitely contain exactly one
         --  component of A_Defining_Name kind, which should not
         --  test as Is_Normalized
         --
         --  Note, that if the argument Is_Normalized, its Node and R_Node
         --  fields are the same

         Result_Node :=
            Original_Record_Component (Entity (First (Choices (Arg_Node))));

         Result_Unit :=
           Enclosing_Unit (Encl_Cont_Id (Association), Result_Node);

         Result_Element := Node_To_Element_New
                             (Starting_Element => Association,
                              Node             => Result_Node,
                              Internal_Kind    => A_Defining_Identifier,
                              In_Unit          => Result_Unit);

         --  And now we have to correct some fields in Result_Element.
         --  First, Association Is_Normalized, but its components are
         --  not Is_Normalized. Therefore

         Set_Special_Case (Result_Element, Not_A_Special_Case);

         --  Then, we should check whether or not Result_Element represents
         --  the implicit inherited component of some derived type
         --  The idea (based on the tree structure of 3.05) is to go from
         --  Result_Node up to the corresponding full type declaration,
         --  then one step down to the type defining identifier and then
         --  to check if it Is_Internal

         Temp_Node := Parent (Parent (Result_Node));
         --  this Parent (Parent) gives us either N_Component_List node
         --  (if  Result_Node corresponds to a record component) or
         --  N_Full_Type_Declaration node (if  Result_Node corresponds to a
         --  discriminant). In the former case we have to apply Parent
         --  twice more to go to a N_Full_Type_Declaration node

         if Nkind (Temp_Node) = N_Component_List then
            Temp_Node := Parent (Parent (Temp_Node));
         end if;

         --  and now - the test and the related corrections if the test
         --  is successful:
         if Nkind (Temp_Node) = N_Full_Type_Declaration then
            --  if Result_Node corresponds to a record component from a record
            --  extension part, we should be in N_Derived_Type_Definition
            --  node here, and we have nothing to correct in Result_Element
            --  in that case

            Temp_Node := Defining_Identifier (Temp_Node);
            if Is_Internal (Temp_Node) then
               Set_From_Implicit  (Result_Element);
               Set_From_Inherited (Result_Element);
            end if;
         end if;

         return (1 => Result_Element);

      end if;

      --  processing a non-normalized association:

      if Nkind (Arg_Node) = N_Component_Association then

         return N_To_E_List_New
                  (List             => Choices (Arg_Node),
                   Starting_Element => Association);
      else
         return Nil_Element_List;
         --  what else can we get from a positional association?
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Record_Component_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Record_Component_Choices",
            Ex          => Ex,
            Arg_Element => Association);
   end Record_Component_Choices;
-----------------------------------------------------------------------------
   function Component_Expression
     (Association : Asis.Association)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node : Node_Id;

      Returned_Element : Element;
      Result_Node      : Node_Id;
      Result_Kind      : Internal_Element_Kinds := Not_An_Element;
      Temp_Node        : Node_Id;
      Norm_Expr_Sloc   : Source_Ptr;
   begin

      Check_Validity (Association, Package_Name & "Component_Expression");

      if not (Arg_Kind = A_Record_Component_Association or else
              Arg_Kind = An_Array_Component_Association)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Component_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);
      --  ??? May be, we have to use R_Node here???

      if Normalization_Case (Association) = Is_Normalized then
         --  the idea of the implementation is: to go up to the
         --  (rewritten!) N_Aggregate node, then to go to the corresponding
         --  expression in the corresponding non-normalized association
         --  through the original aggregate node. The corresponding
         --  expression is the expression having the same Sloc value
         --  (we are traversing the same tree all the time, so we do not
         --  need relative Slocs.
         --
         --  It may look a bit crazy - why do not we use the expression
         --  subtree from the rewritten aggregate. At least one reason is
         --  that some details of the original expression structure are
         --  lost in the rewritten aggregate as a result of compile-time
         --  optimizations of static expressions

         Result_Node := Parent (Arg_Node);
         --  here we are in the rewritten aggregate node. Now coming to its
         --  original node:
         Result_Node := Original_Node (Result_Node);

         --  and now - trying to find the corresponding expression:

         Norm_Expr_Sloc := Sloc (Sinfo.Expression (Arg_Node));

         if Present (Sinfo.Expressions (Result_Node)) then
            --  starting from positional associations, if any:
            Temp_Node := First (Sinfo.Expressions (Result_Node));

            while Present (Temp_Node) loop

               if Sloc (Temp_Node) = Norm_Expr_Sloc then
                  Result_Node := Temp_Node;
                  goto Find;
               end if;

               Temp_Node := Next (Temp_Node);
            end loop;

         end if;

         if Present (Component_Associations (Result_Node)) then

            Temp_Node := First (Component_Associations (Result_Node));

            while Present (Temp_Node) loop

               if Sloc (Sinfo.Expression (Temp_Node)) = Norm_Expr_Sloc then
                  Result_Node := (Sinfo.Expression (Temp_Node));
                  goto Find;
               end if;

               Temp_Node := Next (Temp_Node);
            end loop;

         end if;

         <<Find>>
         if Nkind (Result_Node) = N_Aggregate then
            --  This means, that there is some error in the implementation,
            --  or the tree structure has been changed, and it does not
            --  correspond to this implementation approach any more
            raise Internal_Implementation_Error;
         end if;

         Returned_Element := Node_To_Element_New
                               (Starting_Element => Association,
                                Node             => Result_Node);

         --  And now we have to correct the Result_Element before returning
         --  it. Association Is_Normalized, but its components are
         --  not Is_Normalized. Therefore

         Set_Special_Case (Returned_Element, Not_A_Special_Case);

         return Returned_Element;

      else
         --  processing non-normalized A_Record_Component_Association or
         --  An_Array_Component_Association

         if Nkind (Arg_Node) = N_Component_Association then
            Result_Node := Sinfo.Expression (Arg_Node);
         else
            Result_Node := R_Node (Association);
         end if;

         if Special_Case (Association) = Rewritten_Named_Number then
            Result_Kind := An_Identifier;
         end if;

         return Node_To_Element_New (Node             => Result_Node,
                                     Internal_Kind    => Result_Kind,
                                     Starting_Element => Association);

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Component_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Expression",
            Ex          => Ex,
            Arg_Element => Association);
   end Component_Expression;
------------------------------------------------------------------------------
--  PARTIALLY IMPLEMENTED, CANNOT HANDLE THE NORMALIZED ARGUMENT
--  ??? NEEDS REVISING BADLY!!!

   function Formal_Parameter
     (Association : Asis.Association)
      return        Asis.Element
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Unit : Compilation_Unit;
      Result_Kind : Internal_Element_Kinds := An_Identifier;

      Is_Iherited       : Boolean := False;
      Subprogram_Entity : Entity_Id := Empty;

      Nil_To_Be_Returned  : Boolean := False;

   begin
      Check_Validity (Association, Package_Name & "Formal_Parameter");

      if not (Arg_Kind = A_Parameter_Association or else
              Arg_Kind = A_Generic_Association   or else
              Arg_Kind = A_Pragma_Argument_Association)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Formal_Parameter",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);

      if Normalization_Case (Association) in Normalized_Association then

         if Arg_Kind = A_Generic_Association
          or else
            Arg_Kind = A_Parameter_Association
         then
            --  see the documentation for the body of
            --  Norm.Normalized_Generic_Associations:
            Result_Node := Arg_Node;

            if Nkind (Result_Node) = N_Defining_Identifier then
               Result_Kind := A_Defining_Identifier;
            else
               Result_Kind := Not_An_Element;
               --  In this case Result_Kind should be detected
               --  automatically
            end if;

            Subprogram_Entity := Node_Field_1 (Association);
            Is_Iherited       := Present (Subprogram_Entity);

            Result_Unit :=
               Enclosing_Unit (Encl_Cont_Id (Association), Result_Node);

            return Node_To_Element_New (Node          => Result_Node,
                                        Internal_Kind => Result_Kind,
                                        Node_Field_1  => Subprogram_Entity,
                                        Inherited     => Is_Iherited,
                                        In_Unit       => Result_Unit);
         else
            Not_Implemented_Yet (Diagnosis =>
                Package_Name & "Formal_Parameter: "
              &  ASIS_Line_Terminator
              & "     Cannot handle the NORMALIZED parameter association");
         end if;

      else
         if Arg_Kind = A_Parameter_Association then

            if not (Nkind (Arg_Node) = N_Parameter_Association) then
               --  positional (non-normalized) association
               Nil_To_Be_Returned := True;
            else
               Result_Node := Selector_Name (Arg_Node);
            end if;

         elsif Arg_Kind = A_Generic_Association then

            if Nkind (Arg_Node) = N_Others_Choice then
               Result_Kind := An_Others_Choice;
               Result_Node := Arg_Node;

               --  For the rest of IF paths
               --  Arg_Node_Kind = N_Generic_Association
               --  is always True
            elsif No (Selector_Name (Arg_Node)) then
               --  positional (non-normalized) association
               Nil_To_Be_Returned := True;
            else
               Result_Node := Selector_Name (Arg_Node);

               if Nkind (Result_Node) = N_Operator_Symbol then
                  Result_Kind := Not_An_Element;
                  --  In this case Result_Kind should be detected
                  --  automatically
               end if;

            end if;

         else -- Arg_Kind = A_Pragma_Argument_Association

            --  special treatment by an identifier in the tree

            if Chars (Arg_Node) = No_Name then
               --  no pragma argument identifier
               Nil_To_Be_Returned := True;
            else
               Result_Node := Arg_Node;
            end if;

         end if;

         if Nil_To_Be_Returned then
            return Nil_Element;

         else
            return Node_To_Element_New (
                       Node             => Result_Node,
                       Internal_Kind    => Result_Kind,
                       Starting_Element => Association);
         end if;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Formal_Parameter");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Formal_Parameter",
            Ex          => Ex,
            Arg_Element => Association);
   end Formal_Parameter;
------------------------------------------------------------------------------

   function Actual_Parameter
     (Association : Asis.Association)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);

      Arg_Node         : Node_Id;
      Result_Node      : Node_Id;
      Result_Kind      : Internal_Element_Kinds := Not_An_Element;
      Result_Norm_Case : Normalization_Cases    := Is_Not_Normalized;
      Result_Spec_Case : Special_Cases          := Not_A_Special_Case;
      Result_Unit      : Compilation_Unit;
      Pragma_Chars     : Name_Id;

      Is_Inherited       : Boolean := False;
      Subprogram_Entity  : Entity_Id := Empty;

      Result : Asis.Element;

   begin
      Check_Validity (Association, Package_Name & "Actual_Parameter");

      if not (Arg_Kind = A_Parameter_Association or else
              Arg_Kind = A_Generic_Association   or else
              Arg_Kind = A_Pragma_Argument_Association)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Actual_Parameter",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);

      if Normalization_Case (Association) in Normalized_Association then

         if Arg_Kind = A_Generic_Association
           or else
            Arg_Kind = A_Parameter_Association
         then
            --  see the documentation for the body of
            --  Norm.Normalized_Generic_Associations:
            Result_Node := Node_Field_2 (Association);

            if Normalization_Case (Association) = Is_Normalized_Defaulted then
               --  the actual parameter is taken by default from the
               --  declaration of the corresponding formal, it may be
               --  in another Compilation Unit
               Result_Unit :=
                  Enclosing_Unit (Encl_Cont_Id (Association), Result_Node);
            else
               Result_Unit := Encl_Unit (Association);
            end if;

            --  in case of the default defined at the place of
            --  an instantiation for A_Box_Default; we will keep
            --  Special_Case equal to Is_Normalized_Defaulted_For_Box
            --  (just in case).
            if Normalization_Case (Association) =
                  Is_Normalized_Defaulted_For_Box
            then
               Result_Norm_Case := Is_Normalized_Defaulted_For_Box;
            end if;

            if Normalization_Case (Association) = Is_Normalized
              and then
               Arg_Kind = A_Generic_Association
            then
               Result_Spec_Case := Is_From_Gen_Association;
            end if;

            if Is_Defaulted_Association (Association) then
               Subprogram_Entity := Node_Field_1 (Association);
               Is_Inherited      := Present (Subprogram_Entity);
            end if;

            --  See FB27-003 - we need a special processing for the
            --  case when we have an attribute passed as an actual for a
            --  formal subprogram

            if Arg_Kind = A_Generic_Association
              and then
               Nkind (Result_Node) = N_Attribute_Reference
              and then
               Nkind (Original_Node (Parent (Result_Node))) =
                 N_Subprogram_Renaming_Declaration
            then
               Result_Kind := Subprogram_Attribute_Kind (Result_Node);
            end if;

            Result := Node_To_Element_New (Node          => Result_Node,
                                           Norm_Case     => Result_Norm_Case,
                                           Spec_Case     => Result_Spec_Case,
                                           Node_Field_1  => Subprogram_Entity,
                                           Inherited     => Is_Inherited,
                                           Internal_Kind => Result_Kind,
                                           In_Unit       => Result_Unit);

            --  If we have an actual corresponding to
            --  Is_Normalized_Defaulted_For_Box, we may have to correct
            --  Is_Part_Of_Instance of the result. The corresponding naming
            --  expression is taken from the artificial renaming, so
            --  it in any case is classified as being from instance. But
            --  for this particular case of getting actual from normalized
            --  association we should check that not the result node, but the
            --  corresponding instantiation is from instance, that is, that the
            --  instantiation chain has at least three elements. Another
            --  possibility to check this is to check if the corresponding
            --  instantiation is from instance

            if (Result_Norm_Case = Is_Normalized_Defaulted_For_Box
              or else
                Result_Spec_Case = Is_From_Gen_Association)
              and then
               not Is_From_Instance (Enclosing_Element (Association))
            then
               Set_From_Instance (Result, False);
            end if;

         else
            Not_Implemented_Yet (Diagnosis =>
                Package_Name & "Actual_Parameter: "
              &  ASIS_Line_Terminator
              & "     Cannot handle the NORMALIZED parameter association");
         end if;
      else

         if Arg_Kind = A_Parameter_Association then

            if not (Nkind (Arg_Node) = N_Parameter_Association) then
               --  positional (non-normalized) association
               Result_Node := R_Node (Association);

               if Nkind (Result_Node) = N_Unchecked_Type_Conversion then

                  --  Sometimes the front-end creates a "wrapper"
                  --  N_Unchecked_Type_Conversion structures for calls from
                  --  RTL, see E622-015 and the corresponding note in
                  --  Function_Call_Parameters
                  Result_Node := Sinfo.Expression (Result_Node);
               end if;

            else
               Result_Node := Explicit_Actual_Parameter (Arg_Node);
            end if;

         elsif Arg_Kind = A_Generic_Association then

            if Nkind (Arg_Node) = N_Others_Choice then
               return Nil_Element;
            else
               --  NKind (Arg_Node)  = N_Generic_Association is always True
               Result_Spec_Case := Is_From_Gen_Association;
               Result_Node := Explicit_Generic_Actual_Parameter (Arg_Node);

               --  See FB27-003 - we need a special processing for the
               --  case when we have an attribute passed as an actual for a
               --  formal subprogram

               if Nkind (Result_Node) = N_Attribute_Reference
                 and then
                  Nkind (Original_Node (Parent (Result_Node))) =
                    N_Subprogram_Renaming_Declaration
               then
                  Result_Kind := Subprogram_Attribute_Kind (Result_Node);
               end if;

            end if;

         else
            --  Arg_Kind = A_Pragma_Argument_Association
            --  Special processing is needed for a Debug pragma:
            Pragma_Chars := Pragma_Name (Original_Node (Parent (Arg_Node)));

            if Pragma_Chars = Name_Debug then

               --  We have to use the rewritten tree structures here, because
               --  in the original structures the procedure call in pragma
               --  Debug is not decorated. The problem with the rewritten
               --  structure is that it is an IF statement with block statement
               --  with procedure call from the Debug pragma as its statement
               --  sequence.

               Result_Node := Parent (Arg_Node);
               Result_Node := First (Then_Statements (Result_Node));
               Result_Node :=
                 First (Sinfo.Statements
                        (Handled_Statement_Sequence (Result_Node)));

               while Nkind (Result_Node) /= N_Procedure_Call_Statement loop
                  Result_Node := Next (Result_Node);
               end loop;

               Result_Kind := A_Function_Call;
            else
               Result_Node := Sinfo.Expression (Arg_Node);
            end if;

         end if;

         Result := Node_To_Element_New (Node             => Result_Node,
                                        Starting_Element => Association,
                                        Spec_Case        => Result_Spec_Case,
                                        Internal_Kind    => Result_Kind);
      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Actual_Parameter");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Actual_Parameter",
            Ex          => Ex,
            Arg_Element => Association);
   end Actual_Parameter;
------------------------------------------------------------------------------
--  PARTIALLY IMPLEMENTED, CANNOT HANDLE THE NORMALIZED ARGUMENT

   function Discriminant_Selector_Names
     (Association : Asis.Discriminant_Association)
      return        Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Association, Package_Name & "Discriminant_Selector_Names");

      if not (Arg_Kind = A_Discriminant_Association) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminant_Selector_Names",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Association);

      if Normalization_Case (Association) = Is_Normalized then

         return (1 => Discr_Def_Name (Association));
      else

         if not (Nkind (Arg_Node) = N_Discriminant_Association) then
            --  positional association
            return Nil_Element_List;
         else
            return N_To_E_List_New
                     (List             => Selector_Names (Arg_Node),
                      Internal_Kind    => An_Identifier,
                      Starting_Element => Association);
         end if;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Discriminant_Selector_Names");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminant_Selector_Names",
            Ex          => Ex,
            Arg_Element => Association);
   end Discriminant_Selector_Names;

   -----------------------------
   -- Discriminant_Expression --
   -----------------------------

   function Discriminant_Expression
     (Association : Asis.Discriminant_Association)
      return        Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Association);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Association, Package_Name & "Discriminant_Expression");

      if not (Arg_Kind = A_Discriminant_Association) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Discriminant_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := R_Node (Association);

      if Nkind (Arg_Node) = N_Discriminant_Association then
         --  named association
         return Node_To_Element_New (
                      Node             => Sinfo.Expression (Arg_Node),
                      Starting_Element => Association);
      else
         --  positional or normalized association
         return Node_To_Element_New (
                      Node             => Arg_Node,
                      Starting_Element => Association);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Association,
               Outer_Call => Package_Name & "Discriminant_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Discriminant_Expression",
            Ex          => Ex,
            Arg_Element => Association);
   end Discriminant_Expression;
-----------------------------------------------------------------------------

   function Is_Normalized (Association : Asis.Association) return Boolean is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Association);
      Norm_Case : Normalization_Cases;
   begin
      Check_Validity (Association, Package_Name & "Is_Normalized");

      if not (Arg_Kind = A_Discriminant_Association     or else
              Arg_Kind = A_Record_Component_Association or else
              Arg_Kind = A_Parameter_Association        or else
              Arg_Kind = A_Generic_Association)
      then
         return False;
      else
         Norm_Case := Normalization_Case (Association);

         return Norm_Case in Normalized_Association;
      end if;
   end Is_Normalized;
-----------------------------------------------------------------------------
   function Is_Defaulted_Association
     (Association : Asis.Element)
      return        Boolean
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Association);
      Norm_Case : Normalization_Cases;
   begin
      Check_Validity (Association, Package_Name & "Is_Defaulted_Association");

      if not (Arg_Kind = A_Parameter_Association or else
              Arg_Kind = A_Generic_Association)
      then
         return False;
      else
         Norm_Case := Normalization_Case (Association);

         return Norm_Case in Defaulted_Association;
      end if;
   end Is_Defaulted_Association;
------------------------------------------------------------------------------
   function Expression_Parenthesized
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);

      Res_Parenth_Count : Nat;
      Result            : Asis.Expression;

   begin

      Check_Validity (Expression, Package_Name & "Expression_Parenthesized");

      if not (Arg_Kind = A_Parenthesized_Expression) then
         Raise_ASIS_Inappropriate_Element
            (Diagnosis  => Package_Name & "Expression_Parenthesized",
             Wrong_Kind => Arg_Kind);
      end if;

      Res_Parenth_Count := Parenth_Count (Expression) - 1;

      if Res_Parenth_Count = 0 then
         --  Returning unparenthesized expression
         Result := Node_To_Element_New
                     (Node                     => R_Node (Expression),
                      Considering_Parent_Count => False,
                      Starting_Element         => Expression);
      else
         --  Cut away one level of parentheses
         Result := Expression;
         Set_Parenth_Count (Result, Res_Parenth_Count);
      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Expression_Parenthesized");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Expression_Parenthesized",
            Ex          => Ex,
            Arg_Element => Expression);
   end Expression_Parenthesized;
-----------------------------------------------------------------------------

   function Is_Prefix_Call (Expression : Asis.Expression) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Expression, Package_Name & "Is_Prefix_Call");

      if Arg_Kind /= A_Function_Call then
         return False;
      end if;

      Arg_Node := Node (Expression);

      if (Nkind (Arg_Node) = N_Identifier or else
          Nkind (Arg_Node) = N_Expanded_Name)
        and then
         --  Special case: F.A, where either F or A is a function call
         Nkind (R_Node (Expression)) = N_Function_Call
      then
         Arg_Node := R_Node (Expression);
      end if;

      if Nkind (Arg_Node) = N_Attribute_Reference or else
         Nkind (Arg_Node) = N_Procedure_Call_Statement
      then
         --  N_Procedure_Call_Statement corresponds to the argument of
         --  Debug pragma
         return True;
      elsif Nkind (Arg_Node) = N_Identifier then
         --  Special case: F.A , where F - parameterless function returning
         --  a record type
         return True;
      elsif not (Nkind (Arg_Node) = N_Function_Call) then
         return False;
      else
         return Nkind (Sinfo.Name (Arg_Node)) /= N_Identifier
                or else
                Chars (Sinfo.Name (Arg_Node)) not in Any_Operator_Name;
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Is_Prefix_Call");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Prefix_Call",
            Ex          => Ex,
            Arg_Element => Expression);
   end Is_Prefix_Call;
-----------------------------------------------------------------------------

   function Corresponding_Called_Function
     (Expression : Asis.Expression)
      return       Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
   begin
      Check_Validity
        (Expression, Package_Name & "Corresponding_Called_Function");

      if not (Arg_Kind = A_Function_Call) then
         Raise_ASIS_Inappropriate_Element
            (Diagnosis  => Package_Name & "Corresponding_Called_Function",
             Wrong_Kind => Arg_Kind);
      end if;

      return Get_Corr_Called_Entity (Expression);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Corresponding_Called_Function");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Called_Function",
            Ex          => Ex,
            Arg_Element => Expression);
   end Corresponding_Called_Function;
------------------------------------------------------------------------------
   function Function_Call_Parameters
     (Expression : Asis.Expression;
      Normalized : Boolean := False)
      return       Asis.Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
      Tmp_El   : Asis.Element;

      Function_Call_Node      : Node_Id;
      Func_Call_Or_Node       : Node_Id;
      Function_Call_Node_Kind : Node_Kind;

      Association_Node_List   : List_Id;

      Infix_Operands          : Asis.Association_List (1 .. 2);
   begin
      --  !!! In case if Normalized is set ON, we return non-nil result only if
      --  Corresponding_Called_Function (Expression) is not nil!

      Check_Validity (Expression, Package_Name & "Function_Call_Parameters");

      Arg_Node := Node (Expression);

      if not (Arg_Kind = A_Function_Call) or else
         (Normalized and then Nkind (Arg_Node) = N_Attribute_Reference)
      then
         Raise_ASIS_Inappropriate_Element
            (Diagnosis  => Package_Name & "Function_Call_Parameters",
             Wrong_Kind => Arg_Kind);
      end if;

      --  There is a number of different situations when the frontend
      --  rewrites or changes in some other way the node heading
      --  the subtree representing a function call. So we have to start
      --  from computing the node to represent a function call.

      if Nkind (R_Node (Expression)) = N_Raise_Constraint_Error or else
         Nkind (R_Node (Expression)) = N_Raise_Program_Error    or else
         Nkind (R_Node (Expression)) = N_Conditional_Expression
      then
         Function_Call_Node := Node (Expression);
      elsif Nkind (Arg_Node) = N_Attribute_Reference then
         Function_Call_Node := Arg_Node;
      elsif Nkind (R_Node (Expression)) = N_Unchecked_Type_Conversion then
         --  Sometimes the front-end creates a "wrapper"
         --  N_Unchecked_Type_Conversion structures for calls from RTL, see
         --  E622-015 and the corresponding note in Actual_Parameter
         Function_Call_Node := Sinfo.Expression (R_Node (Expression));
      else
         Function_Call_Node := R_Node (Expression);

         --  Special cases when we need not the rewritten, but the original
         --  node (or something else.

         if Nkind (Function_Call_Node) = N_Integer_Literal or else
            Nkind (Function_Call_Node) = N_Real_Literal    or else
            Nkind (Function_Call_Node) = N_Identifier      or else
            Nkind (Function_Call_Node) = N_String_Literal  or else
            --  this means, that the compiler has optimized a call like
            --  1 + 2, and we have to go back to the original node!

            Nkind (Function_Call_Node) = N_Explicit_Dereference
            --  this happens, but I do not know why...
         then
            Function_Call_Node := Arg_Node;
         end if;

         if Nkind (Function_Call_Node) = N_Op_Not        and then
            Is_Rewrite_Substitution (Function_Call_Node) and then
            Nkind (Original_Node (Function_Call_Node)) = N_Op_Ne
         then
            --  '/=' is rewritten as 'not (  =  )'
            Function_Call_Node := Arg_Node;
         end if;

         if Nkind (Function_Call_Node) = N_Type_Conversion and then
            Is_Rewrite_Substitution (Function_Call_Node)   and then
            Nkind (Original_Node (Function_Call_Node)) =
            Nkind (Original_Node (Sinfo.Expression (Function_Call_Node)))
         then
            Function_Call_Node := Sinfo.Expression (Function_Call_Node);
         end if;

      end if;

      --  See the general comment under the "A_Function_Call Problem"
      --  headline in the beginning of the package body!! The prefix call
      --  of the predefined operations should be processed on the base of
      --  the rewritten node, in all the other cases, except
      --  N_Attribute_Reference (which corresponds to the call to an
      --  attribute-function and is handled separately: the node for such
      --  a call may or may not be rewritten, but the processing is based on
      --  the original node) the node is not rewritten.

      --  --  temporary fix for "+"(1, 2) problem ??? 3.11w
      --
      --  if Nkind (Node (Expression)) = N_Function_Call and then
      --     (Nkind (R_Node (Expression)) in N_Op_Add .. N_Op_Xor or else
      --      Nkind (R_Node (Expression)) in N_Op_Abs .. N_Op_Plus)
      --  then
      --      Function_Call_Node := R_Node (Expression);
      --  end if;
      --
      --  to activate this fix, one should decomment this if statement
      --  and to comment out the fragment between
      --   --  temporary fix for "+"(1, 2) problem - start and
      --   --  temporary fix for "+"(1, 2) problem - end sentinels

      if Normalized then

         Tmp_El := Corresponding_Called_Function (Expression);

         if Declaration_Kind (Tmp_El) in
           A_Procedure_Instantiation .. A_Function_Instantiation
         then
            Tmp_El := Corresponding_Declaration (Tmp_El);
         end if;

         if Is_Nil (Tmp_El)
           or else
            Is_Nil (Parameter_Profile (Tmp_El))
         then
            return Nil_Element_List;
         else
            return Normalized_Param_Associations (Call_Elem => Expression);
         end if;

      else

         Function_Call_Node_Kind := Nkind (Function_Call_Node);

         if Function_Call_Node_Kind = N_Attribute_Reference then
            --  the (prefix) call of the attribute function

            return N_To_E_List_New
              (List             => Sinfo.Expressions (Function_Call_Node),
               Starting_Element => Expression,
               Internal_Kind    => A_Parameter_Association);

         elsif Function_Call_Node_Kind = N_Function_Call or else
               Function_Call_Node_Kind = N_Procedure_Call_Statement
         then

            --  N_Procedure_Call_Statement corresponds to the argument of Debug
            --  pragma

            if No (Parameter_Associations (Function_Call_Node)) then
               return Nil_Element_List;
            else
               return N_To_E_List_New
                 (List             => Parameter_Associations
                    (Function_Call_Node),
                  Starting_Element => Expression,
                  Internal_Kind    => A_Parameter_Association);
            end if;

         elsif Function_Call_Node_Kind in N_Op_Add .. N_Op_Xor then

            --  here we have infix or prefix call of a binary predefined
            --  operation

            --  first, we construct the non-normalized association list

            Infix_Operands (1) :=
              Node_To_Element_New
                (Node             => Left_Opnd (Function_Call_Node),
                 Internal_Kind    => A_Parameter_Association,
                 Starting_Element => Expression);

            Infix_Operands (2) :=
              Node_To_Element_New
                (Node             => Right_Opnd (Function_Call_Node),
                 Internal_Kind    => A_Parameter_Association,
                 Starting_Element => Expression);

   --  temporary fix for "+"(1, 2) problem - start

            if Is_Prefix_Call (Expression) then
               --  It is a real pity, but we have to worry about the crazy
               --  situation like "+" (Right => X, Left => Y). For a prefix
               --  call to a predefined operation an argument node is
               --  rewritten to N_Op_Xxx node, and the original node of
               --  N_Function_Call kind contains references to named
               --  parameter associations, if any

               --  So, we have to check if this situation takes place

               --  If not Is_Prefix_Call (Expression), we have nothing to do!
               Func_Call_Or_Node := Node (Expression);

               if Func_Call_Or_Node /= Function_Call_Node and then
                  --  Func_Call_Or_Node can be of N_Function_Call kind only!
                  --  and we have the prefix call here!
                  --
                  --  Present (Parameter_Associations (Func_Call_Or_Node))
                  --
                  --  cannot be used to complete the check, because we have
                  --  empty list, but not No_List if there is positional
                  --  associations. Therefore -
                  List_Length (Parameter_Associations (Func_Call_Or_Node)) > 0
               then
                  --  we have named associations, and we have to correct the
                  --  result
                  Association_Node_List :=
                     Parameter_Associations (Func_Call_Or_Node);

                  if List_Length (Association_Node_List) = 2 then
                     --  we have two named associations, so we cannot return
                     --  Infix_Operands. We will not correct it, we will
                     --  recreate the returned list:

                     return N_To_E_List_New
                       (List             => Association_Node_List,
                        Include_Pragmas  => False,  --  ???
                        Starting_Element => Expression,
                        Internal_Kind    => A_Parameter_Association);

                  else
                     --  if we are here, the only possibility is that
                     --  List_Length (Association_Node_List) = 1 and we are
                     --  processing the call like "+"(13, Right => Y).
                     --  So the first component of Infix_Operands is OK,
                     --  but the second should be re-created from the
                     --  positional association pointed by the original node:

                     Infix_Operands (2) := Node_To_Element_New (
                            Node             => First (Association_Node_List),
                            Internal_Kind    => A_Parameter_Association,
                            Starting_Element => Expression);
                  end if;
               end if;
            end if;

   --  temporary fix for "+"(1, 2) problem - end

            return Infix_Operands;

         elsif Function_Call_Node_Kind in N_Op_Abs .. N_Op_Plus then
            --  unary operation, Sinfo.ads rev. 1.251
            --  infix_call, here we have infix or prefix call of an unary
            --  predefined  operation

            --  the situation is more simple, then for binary predefined
            --  operation - we have only one component in the returned list

            --  we start from checking if we have the crazy case with
            --  named association (something like "+"(Right => X)

            Func_Call_Or_Node := Node (Expression);

            if Func_Call_Or_Node /= Function_Call_Node     and then
               Nkind (Func_Call_Or_Node) = N_Function_Call and then
               List_Length (Parameter_Associations (Func_Call_Or_Node)) > 0
            then
               --  we have named association
               Association_Node_List :=
                  Parameter_Associations (Func_Call_Or_Node);

               Infix_Operands (1) :=
                  Node_To_Element_New
                    (Node             => First (Association_Node_List),
                     Internal_Kind    => A_Parameter_Association,
                     Starting_Element => Expression);
            else
               Infix_Operands (1) :=
                  Node_To_Element_New
                    (Node             => Right_Opnd (Function_Call_Node),
                     Internal_Kind    => A_Parameter_Association,
                     Starting_Element => Expression);
            end if;

            return Infix_Operands (1 .. 1);

         else
            --  really nothing else could be possible, this alternative
            --  could be chosen only as the result of some bug in the
            --  implementation
            raise Internal_Implementation_Error;
         end if;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Bool_Par   => Normalized,
               Outer_Call => Package_Name & "Function_Call_Parameters");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Function_Call_Parameters",
            Ex          => Ex,
            Arg_Element => Expression,
            Bool_Par_ON => Normalized);
   end Function_Call_Parameters;

-----------------------------------------------------------------------------

   function Short_Circuit_Operation_Left_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Short_Circuit_Operation_Left_Expression");

      if not (Arg_Kind = An_And_Then_Short_Circuit or else
              Arg_Kind = An_Or_Else_Short_Circuit)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name & "Short_Circuit_Operation_Left_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New (Node             => Left_Opnd (Arg_Node),
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Short_Circuit_Operation_Left_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Short_Circuit_Operation_Left_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Short_Circuit_Operation_Left_Expression;
-----------------------------------------------------------------------------

   function Short_Circuit_Operation_Right_Expression
      (Expression : Asis.Expression)
       return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression,
         Package_Name & "Short_Circuit_Operation_Right_Expression");

      if not (Arg_Kind = An_And_Then_Short_Circuit or else
              Arg_Kind = An_Or_Else_Short_Circuit)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name & "Short_Circuit_Operation_Right_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New (Node             => Right_Opnd (Arg_Node),
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Short_Circuit_Operation_Right_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Short_Circuit_Operation_Right_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Short_Circuit_Operation_Right_Expression;
-----------------------------------------------------------------------------

   function Membership_Test_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Expression, Package_Name & "Membership_Test_Expression");

      if Arg_Kind not in
         An_In_Membership_Test .. A_Not_In_Membership_Test
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Membership_Test_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New (Node             => Left_Opnd (Arg_Node),
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Membership_Test_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Membership_Test_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Membership_Test_Expression;
-----------------------------------------------------------------------------

   function Membership_Test_Range
     (Expression : Asis.Expression)
      return       Asis.Range_Constraint
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;

      Result_Node    : Node_Id;
      Result_Or_Node : Node_Id;
      Result_Kind    : Internal_Element_Kinds;
   begin

      Check_Validity (Expression, Package_Name & "Membership_Test_Range");

      if not (Arg_Kind in An_In_Membership_Test .. A_Not_In_Membership_Test
            and then
              Is_Range_Memberchip_Test (Expression))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Membership_Test_Range",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      --  we cannot use the auto determination of the result kind
      --  because of the possible rewriting of A'Range as
      --  A'First .. A'Last.

      Result_Node    := Right_Opnd (Arg_Node);
      Result_Or_Node := Original_Node (Result_Node);

      if Nkind (Result_Or_Node) = N_Attribute_Reference then
         Result_Kind := A_Range_Attribute_Reference;
      else
         Result_Kind := A_Simple_Expression_Range;
      end if;

      return Node_To_Element_New (Node             => Result_Node,
                                  Internal_Kind    => Result_Kind,
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Membership_Test_Range");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Membership_Test_Range",
            Ex          => Ex,
            Arg_Element => Expression);
   end Membership_Test_Range;
-----------------------------------------------------------------------------

   function Membership_Test_Subtype_Mark
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Membership_Test_Subtype_Mark");

      if not (Arg_Kind in An_In_Membership_Test .. A_Not_In_Membership_Test
            and then
              Is_Type_Memberchip_Test (Expression))
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Membership_Test_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New (Node             => Right_Opnd (Arg_Node),
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Membership_Test_Subtype_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Membership_Test_Subtype_Mark",
            Ex          => Ex,
            Arg_Element => Expression);
   end Membership_Test_Subtype_Mark;
------------------------------------------------------------------------------

   function Converted_Or_Qualified_Subtype_Mark
      (Expression : Asis.Expression)
       return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Converted_Or_Qualified_Subtype_Mark");

      if not (Arg_Kind = A_Type_Conversion or else
              Arg_Kind = A_Qualified_Expression)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Converted_Or_Qualified_Subtype_Mark",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      --  if Special_Case (Expression) = Type_Conversion_With_Attribute then
      --
      --     return Node_To_Element (
      --              Node    => Arg_Node,
      --              --  that is, the node of N_Attribute_Reference kind!
      --              Check_If_Type_Conversion => False,
      --              --  and it is treated as the base for
      --              --  An_Attribute_Reference Element
      --              In_Unit => Encl_Unit (Expression));
      --  else
      --     return Node_To_Element (
      --              Node    => Sinfo.Subtype_Mark (Arg_Node),
      --              In_Unit => Encl_Unit (Expression));
      --  end if;
      --
      --   ??? It looks like Type_Conversion_With_Attribute is not needed
      --   ??? any more as a Special_Cases value. We'll keep the old code
      --   ??? (commented out) till the next application of the massive
      --   ??? testing procedure

      return Node_To_Element_New (
               Node             => Sinfo.Subtype_Mark (Arg_Node),
               Starting_Element => Expression);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Converted_Or_Qualified_Subtype_Mark");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Converted_Or_Qualified_Subtype_Mark",
            Ex          => Ex,
            Arg_Element => Expression);
   end Converted_Or_Qualified_Subtype_Mark;
------------------------------------------------------------------------------

   function Converted_Or_Qualified_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Expression, Package_Name & "Converted_Or_Qualified_Expression");

      if not (Arg_Kind = A_Type_Conversion or else
              Arg_Kind = A_Qualified_Expression)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Converted_Or_Qualified_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name &
                             "Converted_Or_Qualified_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Converted_Or_Qualified_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Converted_Or_Qualified_Expression;
-----------------------------------------------------------------------------

   function Allocator_Subtype_Indication
     (Expression : Asis.Expression)
      return       Asis.Subtype_Indication
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Expression, Package_Name & "Allocator_Subtype_Indication");

      if not (Arg_Kind = An_Allocation_From_Subtype) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Allocator_Subtype_Indication",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);
      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Internal_Kind    => A_Subtype_Indication,
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Allocator_Subtype_Indication");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Allocator_Subtype_Indication",
            Ex          => Ex,
            Arg_Element => Expression);
   end Allocator_Subtype_Indication;
------------------------------------------------------------------------------

   function Allocator_Qualified_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
   begin
      Check_Validity
        (Expression, Package_Name & "Allocator_Qualified_Expression");

      if not (Arg_Kind = An_Allocation_From_Qualified_Expression) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Allocator_Qualified_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Expression);

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Allocator_Qualified_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Allocator_Qualified_Expression",
            Ex          => Ex,
            Arg_Element => Expression);
   end Allocator_Qualified_Expression;

------------------------------------------------------------------------------
--  Processing of the Ada extensions that most likely will be included in   --
--  Ada 2015 and that are already implemented in GNAT                       --
------------------------------------------------------------------------------

-----------------------------
-- Conditional Expressions --
-----------------------------

   ----------------------
   -- Expression_Paths --
   ----------------------

   function Expression_Paths
     (Expression : Asis.Expression)
      return       Asis.Element_List
   is
      Res_Node            : Node_Id;
      First_Path_Kind     : Internal_Element_Kinds := An_If_Expression_Path;
      Next_Cond_Expr_Node : Node_Id                := Node (Expression);

      procedure Get_If_Paths;
      --  Get expression paths from the Expressions list of
      --  N_Conditional_Expression node that is the current value of
      --  Next_Cond_Expr_Node. It uses the value of First_Path_Kind to set
      --  the kind of the first path element.
      --
      --  For IF expression, The Expressions list may have the following
      --  structure:
      --
      --  (1) expr1 -> expr2, this corresponds to IF expr1 THEN expr2
      --                      no ELSE or ELSIF paths
      --
      --  (2) expr1 -> expr2 -> expr3, this corresponds to
      --                      IF expr1 THEN expr2 ELSE expr3
      --                      no ELSIF paths
      --
      --  (3) expr1 -> expr2 -> cond_expr, this corresponds to a conditional
      --                      expression with ELSIF path
      --
      --  This procedure creates and appends to Asis_Element_Table.Table
      --  elements corresponding to expr1, expr2 and expr3, and in case (3)
      --  sets Next_Cond_Expr_Node to cond_expr. In cases (1) and (2)
      --  Next_Cond_Expr_Node is set to Empty

      procedure Get_If_Paths is
      begin
         Res_Node := First (Sinfo.Expressions (Next_Cond_Expr_Node));
         --  This Element represents the condition from IF/ELSIF path, so
         Res_Node  := Next (Res_Node);

         Asis_Element_Table.Append
           (Node_To_Element_New
              (Node => Res_Node,
               Starting_Element => Expression,
               Internal_Kind    => First_Path_Kind));

         Res_Node := Next (Res_Node);

         if Present (Res_Node)
           and then
            Comes_From_Source (Res_Node)
         then

            if Nkind (Res_Node) = N_Conditional_Expression then
               Next_Cond_Expr_Node := Res_Node;
            else
               Asis_Element_Table.Append
                 (Node_To_Element_New
                    (Node => Res_Node,
                     Starting_Element => Expression,
                     Internal_Kind    => An_Else_Expression_Path));

               Next_Cond_Expr_Node := Empty;
            end if;

         else
            Next_Cond_Expr_Node := Empty;
         end if;

      end Get_If_Paths;

   begin
      Check_Validity (Expression, Package_Name & "Expression_Paths");

      if Expression_Kind (Expression) not in
         A_Case_Expression .. An_If_Expression
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Expression_Paths",
            Wrong_Kind => Int_Kind (Expression));
      end if;

      Asis_Element_Table.Init;

      if Expression_Kind (Expression) = An_If_Expression then
         Next_Cond_Expr_Node := Node (Expression);
         Get_If_Paths;
         First_Path_Kind := An_Elsif_Expression_Path;

         while Present (Next_Cond_Expr_Node) loop
            Get_If_Paths;
         end loop;
      else
         Res_Node := First (Alternatives (Node (Expression)));

         while Present (Res_Node) loop
            Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node => Res_Node,
                  Starting_Element => Expression,
                  Internal_Kind    => A_Case_Expression_Path));

            Res_Node := Next (Res_Node);
         end loop;
      end if;

      return Asis.Element_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Expression_Paths");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Expression_Paths",
            Ex          => Ex,
            Arg_Element => Expression);
   end Expression_Paths;

   -------------------------------
   -- Path_Condition_Expression --
   -------------------------------

--   function Path_Condition_Expression
--     (Path : Asis.Element)
--      return Asis.Expression
--   is
--      Res_Node : Node_Id;
--   begin
--      Check_Validity (Path, Package_Name & "Path_Condition_Expression");

--      if Expression_Path_Kind (Path) not in
--        An_If_Expression_Path .. An_Elsif_Expression_Path
--      then
--         Raise_ASIS_Inappropriate_Element
--           (Diagnosis  => Package_Name & "Path_Condition_Expression",
--            Wrong_Kind => Int_Kind (Path));
--      end if;

--      Res_Node := R_Node (Path);
--      pragma Assert (Is_List_Member (Res_Node));
--      Res_Node := Prev (Res_Node);

--      return Node_To_Element_New
--               (Node => Res_Node,
--                Starting_Element => Path);
--   exception
--      when ASIS_Inappropriate_Element =>
--         raise;
--      when ASIS_Failed =>

--         if Status_Indicator = Unhandled_Exception_Error then
--            Add_Call_Information
--              (Argument   => Path,
--               Outer_Call => Package_Name & "Path_Condition_Expression");
--         end if;

--         raise;
--      when Ex : others =>
--         Report_ASIS_Bug
--           (Query_Name  => Package_Name & "Path_Condition_Expression",
--            Ex          => Ex,
--            Arg_Element => Path);
--   end Path_Condition_Expression;

   --------------------------
   -- Dependent_Expression --
   --------------------------

   function Dependent_Expression
     (Path : Asis.Element)
      return Asis.Expression
   is
      Arg_Kind    : constant Path_Kinds := Path_Kind (Path);
      Result_Node :          Node_Id;
   begin
      Check_Validity (Path, Package_Name & "Dependent_Expression");

      if Arg_Kind not in
           A_Case_Expression_Path .. An_Else_Expression_Path
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Dependent_Expression",
            Wrong_Kind => Int_Kind (Path));
      end if;

      Result_Node := R_Node (Path);

      if Arg_Kind = A_Case_Expression_Path then
         Result_Node := Sinfo.Expression (Result_Node);
      end if;

      return Node_To_Element_New
               (Node             => Result_Node,
                Starting_Element => Path);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Path,
               Outer_Call => Package_Name & "Dependent_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Dependent_Expression",
            Ex          => Ex,
            Arg_Element => Path);
   end Dependent_Expression;

----------------------------------
-- Generalized Membership Tests --
----------------------------------

   function Membership_Test_Choices
     (Expression : Asis.Expression)
      return Asis.Element_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
      Arg_Node : Node_Id;
      Res_Node : Node_Id;
   begin
      if Arg_Kind not in An_In_Membership_Test .. A_Not_In_Membership_Test then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Membership_Test_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Asis_Element_Table.Init;
      Arg_Node := Node (Expression);

      if No (Alternatives (Arg_Node)) then
         Asis_Element_Table.Append
           (Node_To_Element_New
              (Node => Right_Opnd (Arg_Node),
               Starting_Element => Expression));
      else
         Res_Node := First (Alternatives (Arg_Node));

         while Present (Res_Node) loop
            Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node => Res_Node,
                  Starting_Element => Expression));

            Res_Node := Next (Res_Node);
         end loop;
      end if;

      return Asis.Element_List
               (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Membership_Test_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Membership_Test_Choices",
            Ex          => Ex,
            Arg_Element => Expression);
   end Membership_Test_Choices;

----------------------------
-- Quantified Expressions --
----------------------------

   ----------------------------
   -- Iterator_Specification --
   ----------------------------

   function Iterator_Specification
     (Expression : Asis.Expression)
      return      Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Expression);
      Result_Node :          Node_Id;
   begin
      Check_Validity (Expression, Package_Name & "Iterator_Specification");

      if Arg_Kind not in
           A_For_All_Quantified_Expression .. A_For_Some_Quantified_Expression
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Iterator_Specification",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := Loop_Parameter_Specification (Node (Expression));

      if No (Result_Node) then
         Result_Node := Iterator_Specification (Node (Expression));
      end if;

      return Node_To_Element_New (Node             => Result_Node,
                                  Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Iterator_Specification");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Iterator_Specification",
            Ex          => Ex,
            Arg_Element => Expression);
   end Iterator_Specification;

   ---------------
   -- Predicate --
   ---------------

   function Predicate
     (Expression : Asis.Expression)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Expression);
   begin
      Check_Validity (Expression, Package_Name & "Predicate");

      if Arg_Kind not in
           A_For_All_Quantified_Expression .. A_For_Some_Quantified_Expression
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Predicate",
            Wrong_Kind => Arg_Kind);
      end if;

      return Node_To_Element_New
               (Node             => Condition (Node (Expression)),
                Starting_Element => Expression);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Predicate");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Predicate",
            Ex          => Ex,
            Arg_Element => Expression);
   end Predicate;

---------------------------
--  Subpool in allocator --
---------------------------

   function Subpool_Name
     (Expression : Asis.Expression)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Expression);
   begin
      Check_Validity (Expression, Package_Name & "Subpool_Name");

      if Arg_Kind not in An_Allocation_From_Subtype ..
                         An_Allocation_From_Qualified_Expression
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Subpool_Name",
            Wrong_Kind => Arg_Kind);
      end if;

--        return Node_To_Element_New
--                 (Node             => Subpool_Handle_Name (Node (Expression)),
--                  Starting_Element => Expression);
      raise ASIS_Inappropriate_Element;  --  SCz
      return Expression;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Expression,
               Outer_Call => Package_Name & "Subpool_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Subpool_Name",
            Ex          => Ex,
            Arg_Element => Expression);
   end Subpool_Name;

end Asis.Expressions;
