------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                   A 4 G . S P A N _ B E G I N N I N G                    --
--                                                                          --
--                                 S p e c                                  --
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

with Asis;

with A4G.Int_Knds; use A4G.Int_Knds;

with Types;        use Types;

package A4G.Span_Beginning is

   function Set_Image_Beginning (E : Asis.Element) return Source_Ptr;
   --  The driver function for computing the beginning of the element image
   --  in the GNAT source text buffer. In case if the argument is a labeled
   --  statement returns the baginning of the first attachecd label. Otherwise
   --  calls the element-kind-specific function for computing the image
   --  beginning

   --  The functions below compute the beginning of the text image for some
   --  specific set of ELement kinds/

   function Subunit_Beginning (E : Asis.Element) return Source_Ptr;
   function Private_Unit_Beginning (E : Asis.Element) return Source_Ptr;
   --  Assuming that E is an element representing the top Unit element of a
   --  subunit or of a library item representing a private unit, returns the
   --  source pointer to the beginning of the keyword SEPARATE or PRIVATE
   --  accordingly.

   function No_Search (E : Asis.Element) return Source_Ptr;
   --  That is, we can just use Sloc from the node the Element is based upon.

   function Search_Identifier_Beginning
     (E : Asis.Element)
      return Source_Ptr;
   --  A special processing is needed for an identifier representing the
   --  attribute designator in pseudo-attribute-reference in an attribute
   --  definition clause and for 'Class attribute in aspect indication

   function Search_Subtype_Indication_Beginning
     (E : Asis.Element)
      return Source_Ptr;
   --  If the subtype mark is an expanded name, we have to look for its prefix

   function Defining_Identifier_Beginning
     (E :    Asis.Element)
      return Source_Ptr;
   --  Returns the beginning of A_Defining_Identifier elements. In case of
   --  labels we have to get rid of '<<'

   function Short_Circuit_Beginning (E : Asis.Element) return Source_Ptr;
   --  Returns the beginning of short circuit expression

   function Membership_Test_Beginning (E : Asis.Element) return Source_Ptr;
   --  Returns the beginning of membership test expression

   function Null_Component_Beginning (E : Asis.Element) return Source_Ptr;
   --  In case of A_Nil_Component element Sloc of its Node points to
   --  "record" (it's easy) or to variant (it's a pain)

   function Search_Prefix_Beginning (E : Asis.Element) return Source_Ptr;
   --  Is needed when the Argument has a "prefix" in its structure, but Sloc
   --  points somewhere inside the elemen structure

--  --|A2005 start
   function Possible_Null_Exclusion_Beginning
     (E    : Asis.Element)
      return Source_Ptr;
   --  Should be used for constructs that can contain null_exclusion

   function Possible_Overriding_Indicator_Beginning
     (E    : Asis.Element)
      return Source_Ptr;
   --  Is supposed to be used for some of the constructs that can contain
   --  overriding_indicator
--  --|A2005 end

   function Component_And_Parameter_Declaration_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Exception_Declaration_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Derived_Definition_Beginning (E : Asis.Element) return Source_Ptr;

   function Type_Definition_Beginning (E : Asis.Element) return Source_Ptr;

   function Interface_Definition_Beginning
     (E    : Asis.Element)
      return Source_Ptr;

   function Tagged_Type_Definition_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Simple_Expression_Range_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Component_Definition_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Search_Left_Parenthesis_After (E : Asis.Element) return Source_Ptr;
   --  Is needed when the Element does not have its "own" node (in particular,
   --  for A_Known_Discriminant_Part Element)

   function Private_Extension_Definition_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Private_Type_Definition_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Explicit_Dereference_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Function_Call_Beginning (E : Asis.Element) return Source_Ptr;

   function Indexed_Component_Beginning (E : Asis.Element) return Source_Ptr;

   function Component_Association_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Association_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Parenthesized_Expression_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Assignment_Statement_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function Named_Statement_Beginning
     (E :    Asis.Element)
      return Source_Ptr;
   --  Takes care of loop and block names

   function Call_Statement_Beginning (E : Asis.Element) return Source_Ptr;

   function While_Loop_Statement_Beginning
     (E :    Asis.Element)
      return Source_Ptr;

   function For_Loop_Statement_Beginning (E : Asis.Element) return Source_Ptr;

   function Else_Path_Beginning (E : Asis.Element) return Source_Ptr;

   function With_Clause_Beginning (E : Asis.Element) return Source_Ptr;

   function Component_Clause_Beginning (E : Asis.Element) return Source_Ptr;

   function Subprogram_Spec_Beginning (E : Asis.Element) return Source_Ptr;

   function Formal_Object_Declaration_Beginning
     (E : Asis.Element)
      return Source_Ptr;

   function A_Then_Abort_Path_Beginning (E : Asis.Element) return Source_Ptr;

   function Select_Alternative_Beginning (E : Asis.Element) return Source_Ptr;

--  --|A2015 start
   function Case_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr;

   function If_Expression_Beginning
     (E    : Asis.Element)
      return Source_Ptr;

   function Conditional_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr;

   function An_Else_Expression_Path_Beginning
     (E    : Asis.Element)
      return Source_Ptr;
--  --|A2015 end

   --  The look-up table below defines the mapping from Element kinds onto
   --  specific routines for computing the Element image beginning

   type Set_Source_Location_type is
      access function (E : Asis.Element) return Source_Ptr;

   Switch : array (Internal_Element_Kinds) of Set_Source_Location_type := (

      An_All_Calls_Remote_Pragma ..
      --  An_Asynchronous_Pragma
      --  An_Atomic_Pragma
      --  An_Atomic_Components_Pragma
      --  An_Attach_Handler_Pragma
      --  A_Controlled_Pragma
      --  A_Convention_Pragma
      --  An_Elaborate_All_Pragma
      --  An_Elaborate_Body_Pragma
      --  An_Export_Pragma
      --  An_Import_Pragma
      --  An_Inline_Pragma
      --  An_Inspection_Point_Pragma
      --  An_Interrupt_Handler_Pragma
      --  An_Interrupt_Priority_Pragma
      --  A_List_Pragma
      --  A_Locking_Policy_Pragma
      --  A_Normalize_Scalars_Pragma
      --  An_Optimize_Pragma
      --  A_Pack_Pragma
      --  A_Page_Pragma
      --  A_Preelaborate_Pragma
      --  A_Priority_Pragma
      --  A_Pure_Pragma
      --  A_Queuing_Policy_Pragma
      --  A_Remote_Call_Interface_Pragma
      --  A_Remote_Types_Pragma
      --  A_Restrictions_Pragma
      --  A_Reviewable_Pragma
      --  A_Shared_Passive_Pragma
      --  A_Suppress_Pragma
      --  A_Task_Dispatching_Policy_Pragma
      --  A_Volatile_Pragma
      --  A_Volatile_Components_Pragma
      --  An_Implementation_Defined_Pragma
      An_Unknown_Pragma       =>         No_Search'Access,

      A_Defining_Identifier => Defining_Identifier_Beginning'Access,

      A_Defining_Character_Literal ..
      --  A_Defining_Enumeration_Literal
      --  A_Defining_Or_Operator
      --  A_Defining_Xor_Operator
      --  A_Defining_Equal_Operator
      --  A_Defining_Not_Equal_Operator
      --  A_Defining_Less_Than_Operator
      --  A_Defining_Less_Than_Or_Equal_Operator
      --  A_Defining_Greater_Than_Operator
      --  A_Defining_Greater_Than_Or_Equal_Operator
      --  A_Defining_Plus_Operator
      --  A_Defining_Minus_Operator
      --  A_Defining_Concatenate_Operator
      --  A_Defining_Unary_Plus_Operator
      --  A_Defining_Unary_Minus_Operator
      --  A_Defining_Multiply_Operator
      --  A_Defining_Divide_Operator
      --  A_Defining_Mod_Operator
      --  A_Defining_Rem_Operator
      --  A_Defining_Exponentiate_Operator
      --  A_Defining_Abs_Operator
      A_Defining_Not_Operator =>         No_Search'Access,

      A_Defining_Expanded_Name =>        Search_Prefix_Beginning'Access,

      An_Ordinary_Type_Declaration ..
      --  A_Task_Type_Declaration
      --  A_Protected_Type_Declaration
      --  An_Incomplete_Type_Declaration
      --  A_Private_Type_Declaration
      --  A_Private_Extension_Declaration
      --  A_Subtype_Declaration
      --  A_Variable_Declaration
      --  A_Constant_Declaration
      --  A_Deferred_Constant_Declaration
      --  A_Single_Task_Declaration
      --  A_Single_Protected_Declaration
      --  An_Integer_Number_Decl
      --  A_Real_Number_Declaration
      --  An_Enumeration_Literal_Specification
      A_Discriminant_Specification =>    No_Search'Access,

      A_Component_Declaration =>
                         Component_And_Parameter_Declaration_Beginning'Access,

      A_Loop_Parameter_Specification => No_Search'Access,

--  --|A2012 start
      A_Generalized_Iterator_Specification => No_Search'Access,  --  ???
      An_Element_Iterator_Specification    => No_Search'Access,  --  ???
--  --|A2012 end

      A_Procedure_Declaration ..
      A_Function_Declaration =>          Subprogram_Spec_Beginning'Access,

--  --|A2005 start
      A_Null_Procedure_Declaration =>        Subprogram_Spec_Beginning'Access,
--  --|A2005 end

--  --|A2012 start
      An_Expression_Function_Declaration => Subprogram_Spec_Beginning'Access,
--  --|A2012 end

      A_Parameter_Specification =>
                         Component_And_Parameter_Declaration_Beginning'Access,

      A_Procedure_Body_Declaration ..
      A_Function_Body_Declaration => Subprogram_Spec_Beginning'Access,

      A_Package_Declaration ..
      --  A_Package_Body_Declaration
      --  An_Object_Renaming_Declaration
      --  An_Exception_Renaming_Declaration
      A_Package_Renaming_Declaration =>  No_Search'Access,

      A_Procedure_Renaming_Declaration => Subprogram_Spec_Beginning'Access,
      A_Function_Renaming_Declaration  => Subprogram_Spec_Beginning'Access,

      A_Generic_Package_Renaming_Declaration ..
      --  A_Generic_Procedure_Renaming_Declaration
      --  A_Generic_Function_Renaming_Declaration
      --  A_Task_Body_Declaration
      A_Protected_Body_Declaration =>  No_Search'Access,

      An_Entry_Declaration => Possible_Overriding_Indicator_Beginning'Access,

      An_Entry_Body_Declaration ..
      An_Entry_Index_Specification => No_Search'Access,

      A_Procedure_Body_Stub ..
      A_Function_Body_Stub    => Subprogram_Spec_Beginning'Access,

      A_Package_Body_Stub ..
      --  A_Task_Body_Stub
      A_Protected_Body_Stub =>           No_Search'Access,

      An_Exception_Declaration =>
                         Exception_Declaration_Beginning'Access,

      A_Choice_Parameter_Specification ..
      --  A_Generic_Procedure_Declaration
      --  A_Generic_Function_Declaration
      --  A_Generic_Package_Declaration
      A_Package_Instantiation => No_Search'Access,

      A_Procedure_Instantiation ..
      A_Function_Instantiation =>
        Possible_Overriding_Indicator_Beginning'Access,

      A_Formal_Object_Declaration =>
         Formal_Object_Declaration_Beginning'Access,

      A_Formal_Type_Declaration ..
      --  A_Formal_Procedure_Declaration
      --  A_Formal_Function_Declaration
      --  A_Formal_Package_Declaration
      A_Formal_Package_Declaration_With_Box =>   No_Search'Access,

      A_Derived_Type_Definition =>       Derived_Definition_Beginning'Access,

      A_Derived_Record_Extension_Definition =>
                         Derived_Definition_Beginning'Access,

      An_Enumeration_Type_Definition ..
      --  A_Signed_Integer_Type_Definition
      --  A_Modular_Type_Definition

      ---------------------------------------------------------
      --  !!! They all are implicit and cannot have image
      --  |
      --  |->  A_Root_Integer_Definition
      --  |->  A_Root_Real_Definition
      --  |->  A_Root_Fixed_Definition
      --  |->  A_Universal_Integer_Definition
      --  |->  A_Universal_Real_Definition
      --  +->  A_Universal_Fixed_Definition
      ---------------------------------------------------------

      --  A_Floating_Point_Definition
      --  An_Ordinary_Fixed_Point_Definition
      --  A_Decimal_Fixed_Point_Definition
      --  An_Unconstrained_Array_Definition
      A_Constrained_Array_Definition =>  No_Search'Access,

      A_Record_Type_Definition => Type_Definition_Beginning'Access,

      A_Tagged_Record_Type_Definition =>
         Tagged_Type_Definition_Beginning'Access,

--  --|A2005 start

      An_Ordinary_Interface ..
      --  A_Limited_Interface,
      --  A_Task_Interface,
      --  A_Protected_Interface,
      A_Synchronized_Interface => Interface_Definition_Beginning'Access,
--  --|A2005 end

      A_Pool_Specific_Access_To_Variable ..
      --  An_Access_To_Variable
      --  An_Access_To_Constant
      --  An_Access_To_Procedure
      --  An_Access_To_Protected_Procedure
      --  An_Access_To_Function
      An_Access_To_Protected_Function =>  No_Search'Access,
      A_Subtype_Indication      =>  Search_Subtype_Indication_Beginning'Access,

      A_Range_Attribute_Reference =>     Search_Prefix_Beginning'Access,

      A_Simple_Expression_Range => Simple_Expression_Range_Beginning'Access,

      A_Digits_Constraint ..
      --  A_Delta_Constraint
      --  An_Index_Constraint
      A_Discriminant_Constraint =>       No_Search'Access,

      A_Component_Definition =>          Component_Definition_Beginning'Access,

      A_Discrete_Subtype_Indication_As_Subtype_Definition =>
        Search_Subtype_Indication_Beginning'Access,

      A_Discrete_Range_Attribute_Reference_As_Subtype_Definition =>
         Search_Prefix_Beginning'Access,

      A_Discrete_Simple_Expression_Range_As_Subtype_Definition =>
         Simple_Expression_Range_Beginning'Access,

      A_Discrete_Subtype_Indication =>
        Search_Subtype_Indication_Beginning'Access,

      A_Discrete_Range_Attribute_Reference => Search_Prefix_Beginning'Access,

      A_Discrete_Simple_Expression_Range =>
         Simple_Expression_Range_Beginning'Access,

      An_Unknown_Discriminant_Part ..
      A_Known_Discriminant_Part =>       Search_Left_Parenthesis_After'Access,

      A_Record_Definition ..
      A_Null_Record_Definition =>                No_Search'Access,

      A_Null_Component => Null_Component_Beginning'Access,

      A_Variant_Part ..
      --  A_Variant
      An_Others_Choice =>                No_Search'Access,

--  --|A2005 start
      An_Anonymous_Access_To_Variable ..
      --  An_Anonymous_Access_To_Constant,
      --  An_Anonymous_Access_To_Procedure,
      --  An_Anonymous_Access_To_Protected_Procedure,
      --  An_Anonymous_Access_To_Function,
      An_Anonymous_Access_To_Protected_Function =>
        Possible_Null_Exclusion_Beginning'Access,
--  --|A2005 end

      A_Private_Type_Definition =>   Private_Type_Definition_Beginning'Access,

      A_Tagged_Private_Type_Definition =>
         Private_Type_Definition_Beginning'Access,

      A_Private_Extension_Definition =>
         Private_Extension_Definition_Beginning'Access,

      A_Task_Definition =>               No_Search'Access,

      A_Protected_Definition =>          No_Search'Access,

      A_Formal_Private_Type_Definition =>
         Private_Type_Definition_Beginning'Access,

      A_Formal_Tagged_Private_Type_Definition =>
         Private_Type_Definition_Beginning'Access,

      A_Formal_Derived_Type_Definition => Derived_Definition_Beginning'Access,

      A_Formal_Discrete_Type_Definition => No_Search'Access,

      A_Formal_Signed_Integer_Type_Definition ..
      --  A_Formal_Modular_Type_Definition
      --  A_Formal_Floating_Point_Definition
      --  A_Formal_Ordinary_Fixed_Point_Definition
      A_Formal_Decimal_Fixed_Point_Definition => No_Search'Access,

      A_Formal_Ordinary_Interface ..
--      A_Formal_Limited_Interface
--      A_Formal_Task_Interface
--      A_Formal_Protected_Interface
      A_Formal_Synchronized_Interface => Interface_Definition_Beginning'Access,

      A_Formal_Unconstrained_Array_Definition ..
      --  A_Formal_Constrained_Array_Definition
      --  A_Formal_Pool_Specific_Access_To_Variable
      --  A_Formal_Access_To_Variable
      --  A_Formal_Access_To_Constant
      --  A_Formal_Access_To_Procedure
      --  A_Formal_Access_To_Protected_Procedure
      --  A_Formal_Access_To_Function
      A_Formal_Access_To_Protected_Function => No_Search'Access,

      An_Aspect_Specification => No_Search'Access,

      An_Integer_Literal ..
      --  A_Real_Literal
      A_String_Literal =>   No_Search'Access,

      An_Identifier    =>  Search_Identifier_Beginning'Access,

      An_And_Operator ..
      --  An_Or_Operator
      --  An_Xor_Operator
      --  An_Equal_Operator
      --  A_Not_Equal_Operator
      --  A_Less_Than_Operator
      --  A_Less_Than_Or_Equal_Operator
      --  A_Greater_Than_Operator
      --  A_Greater_Than_Or_Equal_Operator
      --  A_Plus_Operator
      --  A_Minus_Operator
      --  A_Concatenate_Operator
      --  A_Unary_Plus_Operator
      --  A_Unary_Minus_Operator
      --  A_Multiply_Operator
      --  A_Divide_Operator
      --  A_Mod_Operator
      --  A_Rem_Operator
      --  An_Exponentiate_Operator
      --  An_Abs_Operator
      --  A_Not_Operator
      --  A_Character_Literal
      An_Enumeration_Literal =>          No_Search'Access,

      An_Explicit_Dereference =>         Explicit_Dereference_Beginning'Access,

      A_Function_Call =>                 Function_Call_Beginning'Access,

      An_Indexed_Component =>            Indexed_Component_Beginning'Access,
      A_Slice              =>            Indexed_Component_Beginning'Access,

      A_Selected_Component ..
      --  An_Access_Attribute
      --  An_Address_Attribute
      --  An_Adjacent_Attribute
      --  An_Aft_Attribute
      --  An_Alignment_Attribute
      --  A_Base_Attribute
      --  A_Bit_Order_Attribute
      --  A_Body_Version_Attribute
      --  A_Callable_Attribute
      --  A_Caller_Attribute
      --  A_Ceiling_Attribute
      --  A_Class_Attribute
      --  A_Component_Size_Attribute
      --  A_Compose_Attribute
      --  A_Constrained_Attribute
      --  A_Copy_Sign_Attribute
      --  A_Count_Attribute
      --  A_Definite_Attribute
      --  A_Delta_Attribute
      --  A_Denorm_Attribute
      --  A_Digits_Attribute
      --  An_Exponent_Attribute
      --  An_External_Tag_Attribute
      --  A_First_Attribute
      --  A_First_Bit_Attribute
      --  A_Floor_Attribute
      --  A_Fore_Attribute
      --  A_Fraction_Attribute
      --  An_Identity_Attribute
      --  An_Image_Attribute
      --  An_Input_Attribute
      --  A_Last_Attribute
      --  A_Last_Bit_Attribute
      --  A_Leading_Part_Attribute
      --  A_Length_Attribute
      --  A_Machine_Attribute
      --  A_Machine_Emax_Attribute
      --  A_Machine_Emin_Attribute
      --  A_Machine_Mantissa_Attribute
      --  A_Machine_Overflows_Attribute
      --  A_Machine_Radix_Attribute
      --  A_Machine_Rounds_Attribute
      --  A_Max_Attribute
      --  A_Max_Size_In_Storage_Elements_Attribute
      --  A_Min_Attribute
      --  A_Model_Attribute
      --  A_Model_Emin_Attribute
      --  A_Model_Epsilon_Attribute
      --  A_Model_Mantissa_Attribute
      --  A_Model_Small_Attribute
      --  A_Modulus_Attribute
      --  An_Output_Attribute
      --  A_Partition_ID_Attribute
      --  A_Pos_Attribute
      --  A_Position_Attribute
      --  A_Pred_Attribute
      --  A_Range_Attribute
      --  A_Read_Attribute
      --  A_Remainder_Attribute
      --  A_Round_Attribute
      --  A_Rounding_Attribute
      --  A_Safe_First_Attribute
      --  A_Safe_Last_Attribute
      --  A_Scale_Attribute
      --  A_Scaling_Attribute
      --  A_Signed_Zeros_Attribute
      --  A_Size_Attribute
      --  A_Small_Attribute
      --  A_Storage_Pool_Attribute
      --  A_Storage_Size_Attribute
      --  A_Succ_Attribute
      --  A_Tag_Attribute
      --  A_Terminated_Attribute
      --  A_Truncation_Attribute
      --  An_Unbiased_Rounding_Attribute
      --  An_Unchecked_Access_Attribute
      --  A_Val_Attribute
      --  A_Valid_Attribute
      --  A_Value_Attribute
      --  A_Version_Attribute
      --  A_Wide_Image_Attribute
      --  A_Wide_Value_Attribute
      --  A_Wide_Width_Attribute
      --  A_Width_Attribute
      --  A_Write_Attribute
      --  An_Implementation_Defined_Attribute
      An_Unknown_Attribute =>            Search_Prefix_Beginning'Access,

      A_Record_Aggregate ..
      --  An_Extension_Aggregate
      --  An_Positional_Array_Aggregate
      A_Named_Array_Aggregate  =>                  No_Search'Access,

      An_And_Then_Short_Circuit ..
      An_Or_Else_Short_Circuit => Short_Circuit_Beginning'Access,

      An_In_Membership_Test ..
      A_Not_In_Membership_Test  => Membership_Test_Beginning'Access,

      A_Null_Literal =>                  No_Search'Access,

      A_Parenthesized_Expression =>
         Parenthesized_Expression_Beginning'Access,

      A_Type_Conversion =>               Search_Prefix_Beginning'Access,

      A_Qualified_Expression =>          Search_Prefix_Beginning'Access,

      An_Allocation_From_Subtype     => No_Search'Access,
      An_Allocation_From_Qualified_Expression => No_Search'Access,
--  --|A2012 start
      A_Case_Expression              => No_Search'Access,
      An_If_Expression               => If_Expression_Beginning'Access,
--  --|A2012 end
      A_Pragma_Argument_Association  => Association_Beginning'Access,
      A_Discriminant_Association     => Association_Beginning'Access,
      A_Record_Component_Association => Component_Association_Beginning'Access,

      An_Array_Component_Association => Component_Association_Beginning'Access,

      A_Parameter_Association       =>  Association_Beginning'Access,
      A_Generic_Association         =>  No_Search'Access,
      A_Null_Statement =>                No_Search'Access,

      An_Assignment_Statement =>         Assignment_Statement_Beginning'Access,

      An_If_Statement =>                 No_Search'Access,

      A_Case_Statement =>                No_Search'Access,

      A_Loop_Statement =>                Named_Statement_Beginning'Access,

      A_While_Loop_Statement =>          While_Loop_Statement_Beginning'Access,

      A_For_Loop_Statement =>            For_Loop_Statement_Beginning'Access,

      A_Block_Statement =>               Named_Statement_Beginning'Access,

      An_Exit_Statement =>   No_Search'Access,
      A_Goto_Statement  =>   No_Search'Access,

      A_Procedure_Call_Statement => Call_Statement_Beginning'Access,

      A_Return_Statement  => No_Search'Access,
      An_Accept_Statement => No_Search'Access,

      An_Entry_Call_Statement =>  Call_Statement_Beginning'Access,

      A_Requeue_Statement ..
      --  A_Requeue_Statement_With_Abort
      --  A_Delay_Until_Statement
      --  A_Delay_Relative_Statement
      --  A_Terminate_Alternative_Statement
      --  A_Selective_Accept_Statement
      --  A_Timed_Entry_Call_Statement
      --  A_Conditional_Entry_Call_Statement
      --  An_Asynchronous_Select_Statement
      --  An_Abort_Statement
      --  A_Raise_Statement
      --  A_Code_Statement

      --  An_If_Path
      --  NOTE: There is no explicit node in GNAT AST tree corresponding to
      --  this Internal_Element_Kind's. It's supposed that corresponding
      --  Element's record field contains Source_Ptr type value that points to
      --  word IF.
      --  If it isn't so, we should change this part.
      --  I believe it's more correct to have A_Then_Path in spite of
      --  An_If_Path which should point to the word THEN.

      An_Elsif_Path => No_Search'Access,
      An_Else_Path  => Else_Path_Beginning'Access,
      A_Case_Path   => No_Search'Access,

      A_Select_Path ..
      --  NOTE: There is no explicit node in GNAT AST tree corresponding to
      --        this Internal_Element_Kind's. It's supposed that corresponding
      --        Element's record field contains Source_Ptr type value that
      --        points to word SELECT.
      --       If it isn't so, we should change this part.

      An_Or_Path => Select_Alternative_Beginning'Access,
      --  NOTE: There is no explicit node in GNAT AST tree corresponding to
      --  this Internal_Element_Kind's. It's supposed that corresponding
      --  Element's record field contains Source_Ptr type value that points to
      --  word OR. If it isn't so, we should change this part.

      A_Then_Abort_Path => A_Then_Abort_Path_Beginning'Access,

--  --|A2015 start
      A_Case_Expression_Path   => Case_Expression_Path_Beginning'Access,

      An_If_Expression_Path ..
      An_Elsif_Expression_Path => Conditional_Expression_Path_Beginning'Access,

      An_Else_Expression_Path => An_Else_Expression_Path_Beginning'Access,
--  --|A2015 end

      A_Use_Package_Clause ..
      --  A_Use_Type_Clause
      A_Use_All_Type_Clause =>  No_Search'Access,  --  Ada 2012

      A_With_Clause =>                   With_Clause_Beginning'Access,

      An_Attribute_Definition_Clause ..
      --  An_Enumeration_Representation_Clause
      --  A_Record_Representation_Clause
      An_At_Clause =>                    No_Search'Access,

      A_Component_Clause =>              Component_Clause_Beginning'Access,

      An_Exception_Handler =>            No_Search'Access,

      others => No_Search'Access);

end A4G.Span_Beginning;
