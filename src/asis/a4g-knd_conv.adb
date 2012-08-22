------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . K N D _ C O N V                         --
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

package body A4G.Knd_Conv is

   use Asis;

   --------------------------------------
   -- Element Classification Functions --
   --------------------------------------

   --  Most of the Conversion Functions use the table-driven switching to
   --  define the result of the conversion. Switches are implemented as
   --  one-dimension arrays indexed by the corresponding Internal_Element_Kinds
   --  subtype and having components of the target type of the conversion

   -------------------------------------------------------
   -- Conversion Switches Definition and Initialization --
   -------------------------------------------------------

   Pragma_Kind_Switch : constant array (Internal_Pragma_Kinds) of Pragma_Kinds
     :=
(An_All_Calls_Remote_Pragma       =>     An_All_Calls_Remote_Pragma,
 An_Asynchronous_Pragma           =>     An_Asynchronous_Pragma,
 An_Atomic_Pragma                 =>     An_Atomic_Pragma,
 An_Atomic_Components_Pragma      =>     An_Atomic_Components_Pragma,
 An_Attach_Handler_Pragma         =>     An_Attach_Handler_Pragma,
 A_Controlled_Pragma              =>     A_Controlled_Pragma,
 A_Convention_Pragma              =>     A_Convention_Pragma,
 A_Discard_Names_Pragma           =>     A_Discard_Names_Pragma, --  added
 An_Elaborate_Pragma              =>     An_Elaborate_Pragma,    --  added
 An_Elaborate_All_Pragma          =>     An_Elaborate_All_Pragma,
 An_Elaborate_Body_Pragma         =>     An_Elaborate_Body_Pragma,
 An_Export_Pragma                 =>     An_Export_Pragma,
 An_Import_Pragma                 =>     An_Import_Pragma,
 An_Inline_Pragma                 =>     An_Inline_Pragma,
 An_Inspection_Point_Pragma       =>     An_Inspection_Point_Pragma,
 An_Interrupt_Handler_Pragma      =>     An_Interrupt_Handler_Pragma,
 An_Interrupt_Priority_Pragma     =>     An_Interrupt_Priority_Pragma,
 A_Linker_Options_Pragma          =>     A_Linker_Options_Pragma, --  added
 A_List_Pragma                    =>     A_List_Pragma,
 A_Locking_Policy_Pragma          =>     A_Locking_Policy_Pragma,
 A_Normalize_Scalars_Pragma       =>     A_Normalize_Scalars_Pragma,
 An_Optimize_Pragma               =>     An_Optimize_Pragma,
 A_Pack_Pragma                    =>     A_Pack_Pragma,
 A_Page_Pragma                    =>     A_Page_Pragma,
 A_Preelaborate_Pragma            =>     A_Preelaborate_Pragma,
 A_Priority_Pragma                =>     A_Priority_Pragma,
 A_Pure_Pragma                    =>     A_Pure_Pragma,
 A_Queuing_Policy_Pragma          =>     A_Queuing_Policy_Pragma,
 A_Remote_Call_Interface_Pragma   =>     A_Remote_Call_Interface_Pragma,
 A_Remote_Types_Pragma            =>     A_Remote_Types_Pragma,
 A_Restrictions_Pragma            =>     A_Restrictions_Pragma,
 A_Reviewable_Pragma              =>     A_Reviewable_Pragma,
 A_Shared_Passive_Pragma          =>     A_Shared_Passive_Pragma,
 A_Storage_Size_Pragma            =>     A_Storage_Size_Pragma, --  added
 A_Suppress_Pragma                =>     A_Suppress_Pragma,
 A_Task_Dispatching_Policy_Pragma =>     A_Task_Dispatching_Policy_Pragma,
 A_Volatile_Pragma                =>     A_Volatile_Pragma,
 A_Volatile_Components_Pragma     =>     A_Volatile_Components_Pragma,

--  --|A2005 start
--  New Ada 2005 pragmas. To be alphabetically ordered later:
 An_Assert_Pragma                 =>     An_Assert_Pragma,
 An_Assertion_Policy_Pragma       =>     An_Assertion_Policy_Pragma,
 A_Detect_Blocking_Pragma         =>     A_Detect_Blocking_Pragma,
 A_No_Return_Pragma               =>     A_No_Return_Pragma,
 A_Partition_Elaboration_Policy_Pragma =>
    A_Partition_Elaboration_Policy_Pragma,
 A_Preelaborable_Initialization_Pragma =>
    A_Preelaborable_Initialization_Pragma,
 A_Priority_Specific_Dispatching_Pragma =>
    A_Priority_Specific_Dispatching_Pragma,
 A_Profile_Pragma                 =>     A_Profile_Pragma,
 A_Relative_Deadline_Pragma       =>     A_Relative_Deadline_Pragma,
 An_Unchecked_Union_Pragma        =>     An_Unchecked_Union_Pragma,
 An_Unsuppress_Pragma             =>     An_Unsuppress_Pragma,
--  --|A2005 end

--  --|A2012 start
 --  New Ada 2012 pragmas. To be alphabetically ordered later
 A_Default_Storage_Pool_Pragma    => A_Default_Storage_Pool_Pragma,
 A_Dispatching_Domain_Pragma      => A_Dispatching_Domain_Pragma,
 A_CPU_Pragma                     => A_CPU_Pragma,
 An_Independent_Pragma            => An_Independent_Pragma,
 A_Independent_Components_Pragma  => A_Independent_Components_Pragma,
   --  To be continued...
--  --|A2012 end

 An_Implementation_Defined_Pragma =>     An_Implementation_Defined_Pragma,
 An_Unknown_Pragma                =>     An_Unknown_Pragma);
------------------------------------------------------------------------------

   Defining_Name_Kind_Switch : constant array (Internal_Defining_Name_Kinds) of
     Defining_Name_Kinds :=

(A_Defining_Identifier          => A_Defining_Identifier,
 A_Defining_Character_Literal   => A_Defining_Character_Literal,
 A_Defining_Enumeration_Literal => A_Defining_Enumeration_Literal,

--  A_Defining_Operator_Symbol

    A_Defining_And_Operator ..
    A_Defining_Not_Operator     => A_Defining_Operator_Symbol,

 A_Defining_Expanded_Name       => A_Defining_Expanded_Name);
------------------------------------------------------------------------------

   Declaration_Kind_Switch : constant array (Internal_Declaration_Kinds) of
                          Declaration_Kinds :=

(An_Ordinary_Type_Declaration            => An_Ordinary_Type_Declaration,
 A_Task_Type_Declaration                 => A_Task_Type_Declaration,
 A_Protected_Type_Declaration            => A_Protected_Type_Declaration,
 An_Incomplete_Type_Declaration          => An_Incomplete_Type_Declaration,

 A_Tagged_Incomplete_Type_Declaration    =>
    A_Tagged_Incomplete_Type_Declaration,

 A_Private_Type_Declaration              => A_Private_Type_Declaration,
 A_Private_Extension_Declaration         => A_Private_Extension_Declaration,
 A_Subtype_Declaration                   => A_Subtype_Declaration,
 A_Variable_Declaration                  => A_Variable_Declaration,
 A_Constant_Declaration                  => A_Constant_Declaration,
 A_Deferred_Constant_Declaration         => A_Deferred_Constant_Declaration,
 A_Single_Task_Declaration               => A_Single_Task_Declaration,
 A_Single_Protected_Declaration          => A_Single_Protected_Declaration,
 An_Integer_Number_Declaration           => An_Integer_Number_Declaration,
 A_Real_Number_Declaration               => A_Real_Number_Declaration,

 An_Enumeration_Literal_Specification    =>
    An_Enumeration_Literal_Specification,

 A_Discriminant_Specification             => A_Discriminant_Specification,
 A_Component_Declaration                  => A_Component_Declaration,
 A_Loop_Parameter_Specification           => A_Loop_Parameter_Specification,

 A_Generalized_Iterator_Specification     =>
   A_Generalized_Iterator_Specification,

 An_Element_Iterator_Specification        => An_Element_Iterator_Specification,
 A_Procedure_Declaration                  => A_Procedure_Declaration,
 A_Function_Declaration                   => A_Function_Declaration,
 A_Parameter_Specification                => A_Parameter_Specification,
 A_Procedure_Body_Declaration             => A_Procedure_Body_Declaration,
 A_Function_Body_Declaration              => A_Function_Body_Declaration,
 A_Return_Variable_Specification          => A_Return_Variable_Specification,
 A_Return_Constant_Specification          => A_Return_Constant_Specification,
 A_Null_Procedure_Declaration             => A_Null_Procedure_Declaration,
 An_Expression_Function_Declaration       =>
   An_Expression_Function_Declaration,
 A_Package_Declaration                    => A_Package_Declaration,
 A_Package_Body_Declaration               => A_Package_Body_Declaration,
 An_Object_Renaming_Declaration           => An_Object_Renaming_Declaration,
 An_Exception_Renaming_Declaration        => An_Exception_Renaming_Declaration,
 A_Package_Renaming_Declaration           => A_Package_Renaming_Declaration,
 A_Procedure_Renaming_Declaration         => A_Procedure_Renaming_Declaration,
 A_Function_Renaming_Declaration          => A_Function_Renaming_Declaration,

 A_Generic_Package_Renaming_Declaration   =>
    A_Generic_Package_Renaming_Declaration,

 A_Generic_Procedure_Renaming_Declaration =>
    A_Generic_Procedure_Renaming_Declaration,

 A_Generic_Function_Renaming_Declaration  =>
    A_Generic_Function_Renaming_Declaration,

 A_Task_Body_Declaration                  => A_Task_Body_Declaration,
 A_Protected_Body_Declaration             => A_Protected_Body_Declaration,
 An_Entry_Declaration                     => An_Entry_Declaration,
 An_Entry_Body_Declaration                => An_Entry_Body_Declaration,
 An_Entry_Index_Specification             => An_Entry_Index_Specification,
 A_Procedure_Body_Stub                    => A_Procedure_Body_Stub,
 A_Function_Body_Stub                     => A_Function_Body_Stub,
 A_Package_Body_Stub                      => A_Package_Body_Stub,
 A_Task_Body_Stub                         => A_Task_Body_Stub,
 A_Protected_Body_Stub                    => A_Protected_Body_Stub,
 An_Exception_Declaration                 => An_Exception_Declaration,
 A_Choice_Parameter_Specification         => A_Choice_Parameter_Specification,
 A_Generic_Procedure_Declaration          => A_Generic_Procedure_Declaration,
 A_Generic_Function_Declaration           => A_Generic_Function_Declaration,
 A_Generic_Package_Declaration            => A_Generic_Package_Declaration,
 A_Package_Instantiation                  => A_Package_Instantiation,
 A_Procedure_Instantiation                => A_Procedure_Instantiation,
 A_Function_Instantiation                 => A_Function_Instantiation,
 A_Formal_Object_Declaration              => A_Formal_Object_Declaration,
 A_Formal_Type_Declaration                => A_Formal_Type_Declaration,
 A_Formal_Incomplete_Type_Declaration     =>
   A_Formal_Incomplete_Type_Declaration,
 A_Formal_Procedure_Declaration           => A_Formal_Procedure_Declaration,
 A_Formal_Function_Declaration            => A_Formal_Function_Declaration,
 A_Formal_Package_Declaration             => A_Formal_Package_Declaration,

 A_Formal_Package_Declaration_With_Box    =>
    A_Formal_Package_Declaration_With_Box);
------------------------------------------------------------------------------

   Definition_Kind_Switch : constant array (Internal_Definition_Kinds) of
                         Definition_Kinds :=

(A_Derived_Type_Definition ..
   An_Access_To_Protected_Function   => A_Type_Definition,

 A_Subtype_Indication               => A_Subtype_Indication,

--  A_Constraint,                     -- 3.2.2   -> Constraint_Kinds

   A_Range_Attribute_Reference ..
   A_Discriminant_Constraint          => A_Constraint,

 A_Component_Definition             => A_Component_Definition,

--  A_Discrete_Subtype_Definition,    -- 3.6     -> Discrete_Range_Kinds

   A_Discrete_Subtype_Indication_As_Subtype_Definition ..
   A_Discrete_Simple_Expression_Range_As_Subtype_Definition
                                      => A_Discrete_Subtype_Definition,

--  A_Discrete_Range,                 -- 3.6.1   -> Discrete_Range_Kinds

       A_Discrete_Subtype_Indication ..
       A_Discrete_Simple_Expression_Range => A_Discrete_Range,

 An_Unknown_Discriminant_Part     => An_Unknown_Discriminant_Part,
 A_Known_Discriminant_Part        => A_Known_Discriminant_Part,
 A_Record_Definition              => A_Record_Definition,
 A_Null_Record_Definition         => A_Null_Record_Definition,
 A_Null_Component                 => A_Null_Component,
 A_Variant_Part                   => A_Variant_Part,
 A_Variant                        => A_Variant,
 An_Others_Choice                 => An_Others_Choice,

--  --|A2005 start
--    An_Access_Definition,          -- 3.10(6/2)   -> Access_Definition_Kinds
 An_Anonymous_Access_To_Variable ..
 An_Anonymous_Access_To_Protected_Function => An_Access_Definition,
--  --|A2005 end

 A_Private_Type_Definition        => A_Private_Type_Definition,
 A_Tagged_Private_Type_Definition => A_Tagged_Private_Type_Definition,
 A_Private_Extension_Definition   => A_Private_Extension_Definition,
 A_Task_Definition                => A_Task_Definition,
 A_Protected_Definition           => A_Protected_Definition,

--  A_Formal_Type_Definition,         -- 12.5    -> Formal_Type_Kinds

   A_Formal_Private_Type_Definition ..
   A_Formal_Access_To_Protected_Function  => A_Formal_Type_Definition,
   An_Aspect_Specification        => An_Aspect_Specification);
------------------------------------------------------------------------------

   Type_Kind_Switch : constant array (Internal_Type_Kinds) of Type_Kinds :=

(A_Derived_Type_Definition             => A_Derived_Type_Definition,

 A_Derived_Record_Extension_Definition =>
    A_Derived_Record_Extension_Definition,

 An_Enumeration_Type_Definition        => An_Enumeration_Type_Definition,
 A_Signed_Integer_Type_Definition      => A_Signed_Integer_Type_Definition,
 A_Modular_Type_Definition             => A_Modular_Type_Definition,

--     A_Root_Type_Definition,                -- 3.5.4(10), 3.5.6(4)
--                                                      -> Root_Type_Kinds
   A_Root_Integer_Definition ..
   A_Universal_Fixed_Definition          => A_Root_Type_Definition,

 A_Floating_Point_Definition        => A_Floating_Point_Definition,
 An_Ordinary_Fixed_Point_Definition => An_Ordinary_Fixed_Point_Definition,
 A_Decimal_Fixed_Point_Definition   => A_Decimal_Fixed_Point_Definition,
 An_Unconstrained_Array_Definition  => An_Unconstrained_Array_Definition,
 A_Constrained_Array_Definition     => A_Constrained_Array_Definition,
 A_Record_Type_Definition           => A_Record_Type_Definition,
 A_Tagged_Record_Type_Definition    => A_Tagged_Record_Type_Definition,

--  --|A2005 start
--    An_Interface_Type_Definition,          -- 3.9.4      -> Interface_Kinds
--  --|A2005 end

 An_Ordinary_Interface .. A_Synchronized_Interface =>
    An_Interface_Type_Definition,
--     An_Access_Type_Definition,             -- 3.10   -> Access_Type_Kinds

   A_Pool_Specific_Access_To_Variable ..
   An_Access_To_Protected_Function     => An_Access_Type_Definition);
------------------------------------------------------------------------------

   Formal_Type_Kind_Switch : constant array (Internal_Formal_Type_Kinds) of
                          Formal_Type_Kinds :=

(A_Formal_Private_Type_Definition         => A_Formal_Private_Type_Definition,

 A_Formal_Tagged_Private_Type_Definition  =>
    A_Formal_Tagged_Private_Type_Definition,

 A_Formal_Derived_Type_Definition         => A_Formal_Derived_Type_Definition,
 A_Formal_Discrete_Type_Definition        => A_Formal_Discrete_Type_Definition,

 A_Formal_Signed_Integer_Type_Definition  =>
    A_Formal_Signed_Integer_Type_Definition,

 A_Formal_Modular_Type_Definition         => A_Formal_Modular_Type_Definition,

 A_Formal_Floating_Point_Definition       =>
    A_Formal_Floating_Point_Definition,

 A_Formal_Ordinary_Fixed_Point_Definition =>
    A_Formal_Ordinary_Fixed_Point_Definition,

 A_Formal_Decimal_Fixed_Point_Definition  =>
    A_Formal_Decimal_Fixed_Point_Definition,

--  --|A2005 start
--    A_Formal_Interface_Type_Definition,      -- 12.5.5(2) -> Interface_Kinds
 A_Formal_Ordinary_Interface .. A_Formal_Synchronized_Interface =>
   A_Formal_Interface_Type_Definition,
--  --|A2005 end

 A_Formal_Unconstrained_Array_Definition  =>
    A_Formal_Unconstrained_Array_Definition,

 A_Formal_Constrained_Array_Definition    =>
    A_Formal_Constrained_Array_Definition,

--    A_Formal_Access_Type_Definition,         -- 12.5.4  -> Access_Type_Kinds
   A_Formal_Pool_Specific_Access_To_Variable ..
   A_Formal_Access_To_Protected_Function => A_Formal_Access_Type_Definition);
------------------------------------------------------------------------------

   Access_Type_Kind_Switch : constant array (Internal_Access_Type_Kinds) of
                          Access_Type_Kinds :=

(A_Pool_Specific_Access_To_Variable => A_Pool_Specific_Access_To_Variable,
 An_Access_To_Variable              => An_Access_To_Variable,
 An_Access_To_Constant              => An_Access_To_Constant,
 An_Access_To_Procedure             => An_Access_To_Procedure,
 An_Access_To_Protected_Procedure   => An_Access_To_Protected_Procedure,
 An_Access_To_Function              => An_Access_To_Function,
 An_Access_To_Protected_Function    => An_Access_To_Protected_Function);
------------------------------------------------------------------------------
--  --|A2005 start

   Access_Definition_Kind_Switch : constant
     array (Internal_Access_Definition_Kinds) of Access_Definition_Kinds :=

(An_Anonymous_Access_To_Variable  => An_Anonymous_Access_To_Variable,
 An_Anonymous_Access_To_Constant  => An_Anonymous_Access_To_Constant,
 An_Anonymous_Access_To_Procedure => An_Anonymous_Access_To_Procedure,
 An_Anonymous_Access_To_Protected_Procedure =>
   An_Anonymous_Access_To_Protected_Procedure,
 An_Anonymous_Access_To_Function  => An_Anonymous_Access_To_Function,
 An_Anonymous_Access_To_Protected_Function =>
  An_Anonymous_Access_To_Protected_Function);

   Interface_Kind_Switch : constant array (Internal_Interface_Kinds) of
                          Interface_Kinds :=

(An_Ordinary_Interface    => An_Ordinary_Interface,
 A_Limited_Interface      => A_Limited_Interface,
 A_Task_Interface         => A_Task_Interface,
 A_Protected_Interface    => A_Protected_Interface,
 A_Synchronized_Interface => A_Synchronized_Interface);

   Formal_Interface_Kind_Switch : constant
     array (Internal_Formal_Interface_Kinds) of Interface_Kinds :=

(A_Formal_Ordinary_Interface    => An_Ordinary_Interface,
 A_Formal_Limited_Interface      => A_Limited_Interface,
 A_Formal_Task_Interface         => A_Task_Interface,
 A_Formal_Protected_Interface    => A_Protected_Interface,
 A_Formal_Synchronized_Interface => A_Synchronized_Interface);

--  --|A2005 end
------------------------------------------------------------------------------

   Formal_Access_Type_Kind_Switch : constant
     array (Internal_Formal_Access_Type_Kinds) of Access_Type_Kinds :=

(A_Formal_Pool_Specific_Access_To_Variable =>
   A_Pool_Specific_Access_To_Variable,

 A_Formal_Access_To_Variable               => An_Access_To_Variable,
 A_Formal_Access_To_Constant               => An_Access_To_Constant,
 A_Formal_Access_To_Procedure              => An_Access_To_Procedure,
 A_Formal_Access_To_Protected_Procedure    => An_Access_To_Protected_Procedure,
 A_Formal_Access_To_Function               => An_Access_To_Function,
 A_Formal_Access_To_Protected_Function     => An_Access_To_Protected_Function);
------------------------------------------------------------------------------

   Root_Type_Kind_Switch : constant array (Internal_Root_Type_Kinds) of
                        Root_Type_Kinds :=

(A_Root_Integer_Definition       => A_Root_Integer_Definition,
 A_Root_Real_Definition          => A_Root_Real_Definition,
 A_Universal_Integer_Definition  => A_Universal_Integer_Definition,
 A_Universal_Real_Definition     => A_Universal_Real_Definition,
 A_Universal_Fixed_Definition    => A_Universal_Fixed_Definition);
------------------------------------------------------------------------------

   Constraint_Kind_Switch : constant array (Internal_Constraint_Kinds) of
                         Constraint_Kinds :=

(A_Range_Attribute_Reference  => A_Range_Attribute_Reference,
 A_Simple_Expression_Range    => A_Simple_Expression_Range,
 A_Digits_Constraint          => A_Digits_Constraint,
 A_Delta_Constraint           => A_Delta_Constraint,
 An_Index_Constraint          => An_Index_Constraint,
 A_Discriminant_Constraint    => A_Discriminant_Constraint);
------------------------------------------------------------------------------

   Discrete_Range_Kind_Switch : constant array (Internal_Element_Kinds) of
                         Discrete_Range_Kinds :=

--  This switch array (as well as Operator_Kind_Switch) differs from all
--  the others, because it is to be used for two different internal
--  classification subtypes:
--  Internal_Discrete_Subtype_Definition_Kinds and
--  Internal_Discrete_Range_Kinds

(A_Discrete_Subtype_Indication_As_Subtype_Definition
| A_Discrete_Subtype_Indication
                                => A_Discrete_Subtype_Indication,
  A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
| A_Discrete_Range_Attribute_Reference
                                => A_Discrete_Range_Attribute_Reference,
  A_Discrete_Simple_Expression_Range_As_Subtype_Definition
| A_Discrete_Simple_Expression_Range
                                => A_Discrete_Simple_Expression_Range,

  others                        => Not_A_Discrete_Range);
------------------------------------------------------------------------------

   Expression_Kind_Switch : constant array (Internal_Expression_Kinds) of
                         Expression_Kinds :=

(An_Integer_Literal                      => An_Integer_Literal,
 A_Real_Literal                          => A_Real_Literal,
 A_String_Literal                        => A_String_Literal,
 An_Identifier                           => An_Identifier,

   --  An_Operator_Symbol
 An_And_Operator ..
 A_Not_Operator                          => An_Operator_Symbol,

 A_Character_Literal                     => A_Character_Literal,
 An_Enumeration_Literal                  => An_Enumeration_Literal,
 An_Explicit_Dereference                 => An_Explicit_Dereference,
 A_Function_Call                         => A_Function_Call,
 An_Indexed_Component                    => An_Indexed_Component,
 A_Slice                                 => A_Slice,
 A_Selected_Component                    => A_Selected_Component,
 An_Access_Attribute ..
 An_Unknown_Attribute                    => An_Attribute_Reference,
 A_Record_Aggregate                      => A_Record_Aggregate,
 An_Extension_Aggregate                  => An_Extension_Aggregate,
 A_Positional_Array_Aggregate            => A_Positional_Array_Aggregate,
 A_Named_Array_Aggregate                 => A_Named_Array_Aggregate,
 An_And_Then_Short_Circuit               => An_And_Then_Short_Circuit,
 An_Or_Else_Short_Circuit                => An_Or_Else_Short_Circuit,
 An_In_Membership_Test                   => An_In_Membership_Test,
 A_Not_In_Membership_Test                => A_Not_In_Membership_Test,

 A_Null_Literal                          => A_Null_Literal,
 A_Parenthesized_Expression              => A_Parenthesized_Expression,
 A_Type_Conversion                       => A_Type_Conversion,
 A_Qualified_Expression                  => A_Qualified_Expression,
 An_Allocation_From_Subtype              => An_Allocation_From_Subtype,

 An_Allocation_From_Qualified_Expression =>
    An_Allocation_From_Qualified_Expression,

 A_Case_Expression                       => A_Case_Expression,
 An_If_Expression                        => An_If_Expression,
 A_For_All_Quantified_Expression         => A_For_All_Quantified_Expression,
 A_For_Some_Quantified_Expression        => A_For_Some_Quantified_Expression);

------------------------------------------------------------------------------

   Operator_Kind_Switch : constant array (Internal_Element_Kinds) of
                       Operator_Kinds :=

--  This switch array (as well as Discrete_Range_Kind_Switch) differs from
--  all the others, because it is to be used for two different internal
--  classification subtypes:
--  Internal_Defining_Operator_Kinds and Internal_Operator_Symbol_Kinds

(A_Defining_And_Operator
| An_And_Operator                         => An_And_Operator,

  A_Defining_Or_Operator
| An_Or_Operator                          => An_Or_Operator,

  A_Defining_Xor_Operator
| An_Xor_Operator                         => An_Xor_Operator,

  A_Defining_Equal_Operator
| An_Equal_Operator                       => An_Equal_Operator,

  A_Defining_Not_Equal_Operator
| A_Not_Equal_Operator                    => A_Not_Equal_Operator,

  A_Defining_Less_Than_Operator
| A_Less_Than_Operator                    => A_Less_Than_Operator,

  A_Defining_Less_Than_Or_Equal_Operator
| A_Less_Than_Or_Equal_Operator           => A_Less_Than_Or_Equal_Operator,

  A_Defining_Greater_Than_Operator
| A_Greater_Than_Operator                 => A_Greater_Than_Operator,

  A_Defining_Greater_Than_Or_Equal_Operator
| A_Greater_Than_Or_Equal_Operator        => A_Greater_Than_Or_Equal_Operator,

  A_Defining_Plus_Operator
| A_Plus_Operator                         => A_Plus_Operator,

  A_Defining_Minus_Operator
| A_Minus_Operator                        => A_Minus_Operator,

  A_Defining_Concatenate_Operator
| A_Concatenate_Operator                  => A_Concatenate_Operator,

  A_Defining_Unary_Plus_Operator
| A_Unary_Plus_Operator                   => A_Unary_Plus_Operator,

  A_Defining_Unary_Minus_Operator
| A_Unary_Minus_Operator                  => A_Unary_Minus_Operator,

  A_Defining_Multiply_Operator
| A_Multiply_Operator                     => A_Multiply_Operator,

  A_Defining_Divide_Operator
| A_Divide_Operator                       => A_Divide_Operator,

  A_Defining_Mod_Operator
| A_Mod_Operator                          => A_Mod_Operator,

  A_Defining_Rem_Operator
| A_Rem_Operator                          => A_Rem_Operator,

  A_Defining_Exponentiate_Operator
| An_Exponentiate_Operator                => An_Exponentiate_Operator,

  A_Defining_Abs_Operator
| An_Abs_Operator                         => An_Abs_Operator,

  A_Defining_Not_Operator
| A_Not_Operator                          => A_Not_Operator,

  others                                  => Not_An_Operator);
------------------------------------------------------------------------------

   Attribute_Kind_Switch : constant array (Internal_Attribute_Reference_Kinds)
     of Attribute_Kinds :=
(
 An_Access_Attribute                      => An_Access_Attribute,
 An_Address_Attribute                     => An_Address_Attribute,
 An_Adjacent_Attribute                    => An_Adjacent_Attribute,
 An_Aft_Attribute                         => An_Aft_Attribute,
 An_Alignment_Attribute                   => An_Alignment_Attribute,
 A_Base_Attribute                         => A_Base_Attribute,
 A_Bit_Order_Attribute                    => A_Bit_Order_Attribute,
 A_Body_Version_Attribute                 => A_Body_Version_Attribute,
 A_Callable_Attribute                     => A_Callable_Attribute,
 A_Caller_Attribute                       => A_Caller_Attribute,
 A_Ceiling_Attribute                      => A_Ceiling_Attribute,
 A_Class_Attribute                        => A_Class_Attribute,
 A_Component_Size_Attribute               => A_Component_Size_Attribute,
 A_Compose_Attribute                      => A_Compose_Attribute,
 A_Constrained_Attribute                  => A_Constrained_Attribute,
 A_Copy_Sign_Attribute                    => A_Copy_Sign_Attribute,
 A_Count_Attribute                        => A_Count_Attribute,
 A_Definite_Attribute                     => A_Definite_Attribute,
 A_Delta_Attribute                        => A_Delta_Attribute,
 A_Denorm_Attribute                       => A_Denorm_Attribute,
 A_Digits_Attribute                       => A_Digits_Attribute,
 An_Exponent_Attribute                    => An_Exponent_Attribute,
 An_External_Tag_Attribute                => An_External_Tag_Attribute,
 A_First_Attribute                        => A_First_Attribute,
 A_First_Bit_Attribute                    => A_First_Bit_Attribute,
 A_Floor_Attribute                        => A_Floor_Attribute,
 A_Fore_Attribute                         => A_Fore_Attribute,
 A_Fraction_Attribute                     => A_Fraction_Attribute,
 An_Identity_Attribute                    => An_Identity_Attribute,
 An_Image_Attribute                       => An_Image_Attribute,
 An_Input_Attribute                       => An_Input_Attribute,
 A_Last_Attribute                         => A_Last_Attribute,
 A_Last_Bit_Attribute                     => A_Last_Bit_Attribute,
 A_Leading_Part_Attribute                 => A_Leading_Part_Attribute,
 A_Length_Attribute                       => A_Length_Attribute,
 A_Machine_Attribute                      => A_Machine_Attribute,
 A_Machine_Emax_Attribute                 => A_Machine_Emax_Attribute,
 A_Machine_Emin_Attribute                 => A_Machine_Emin_Attribute,
 A_Machine_Mantissa_Attribute             => A_Machine_Mantissa_Attribute,
 A_Machine_Overflows_Attribute            => A_Machine_Overflows_Attribute,
 A_Machine_Radix_Attribute                => A_Machine_Radix_Attribute,
 A_Machine_Rounds_Attribute               => A_Machine_Rounds_Attribute,
 A_Max_Attribute                          => A_Max_Attribute,

 A_Max_Size_In_Storage_Elements_Attribute =>
    A_Max_Size_In_Storage_Elements_Attribute,

 A_Min_Attribute                          => A_Min_Attribute,
 A_Model_Attribute                        => A_Model_Attribute,
 A_Model_Emin_Attribute                   => A_Model_Emin_Attribute,
 A_Model_Epsilon_Attribute                => A_Model_Epsilon_Attribute,
 A_Model_Mantissa_Attribute               => A_Model_Mantissa_Attribute,
 A_Model_Small_Attribute                  => A_Model_Small_Attribute,
 A_Modulus_Attribute                      => A_Modulus_Attribute,
 An_Output_Attribute                      => An_Output_Attribute,
 A_Partition_ID_Attribute                 => A_Partition_ID_Attribute,
 A_Pos_Attribute                          => A_Pos_Attribute,
 A_Position_Attribute                     => A_Position_Attribute,
 A_Pred_Attribute                         => A_Pred_Attribute,
 A_Range_Attribute                        => A_Range_Attribute,
 A_Read_Attribute                         => A_Read_Attribute,
 A_Remainder_Attribute                    => A_Remainder_Attribute,
 A_Round_Attribute                        => A_Round_Attribute,
 A_Rounding_Attribute                     => A_Rounding_Attribute,
 A_Safe_First_Attribute                   => A_Safe_First_Attribute,
 A_Safe_Last_Attribute                    => A_Safe_Last_Attribute,
 A_Scale_Attribute                        => A_Scale_Attribute,
 A_Scaling_Attribute                      => A_Scaling_Attribute,
 A_Signed_Zeros_Attribute                 => A_Signed_Zeros_Attribute,
 A_Size_Attribute                         => A_Size_Attribute,
 A_Small_Attribute                        => A_Small_Attribute,
 A_Storage_Pool_Attribute                 => A_Storage_Pool_Attribute,
 A_Storage_Size_Attribute                 => A_Storage_Size_Attribute,
 A_Succ_Attribute                         => A_Succ_Attribute,
 A_Tag_Attribute                          => A_Tag_Attribute,
 A_Terminated_Attribute                   => A_Terminated_Attribute,
 A_Truncation_Attribute                   => A_Truncation_Attribute,
 An_Unbiased_Rounding_Attribute           => An_Unbiased_Rounding_Attribute,
 An_Unchecked_Access_Attribute            => An_Unchecked_Access_Attribute,
 A_Val_Attribute                          => A_Val_Attribute,
 A_Valid_Attribute                        => A_Valid_Attribute,
 A_Value_Attribute                        => A_Value_Attribute,
 A_Version_Attribute                      => A_Version_Attribute,
 A_Wide_Image_Attribute                   => A_Wide_Image_Attribute,
 A_Wide_Value_Attribute                   => A_Wide_Value_Attribute,
 A_Wide_Width_Attribute                   => A_Wide_Width_Attribute,
 A_Width_Attribute                        => A_Width_Attribute,
 A_Write_Attribute                        => A_Write_Attribute,

--  |A2005/2012 start
--  New Ada 2005/2012 attributes. To be alphabetically ordered later
 A_Machine_Rounding_Attribute             => A_Machine_Rounding_Attribute,
 A_Mod_Attribute                          => A_Mod_Attribute,
 A_Priority_Attribute                     => A_Priority_Attribute,
 A_Stream_Size_Attribute                  => A_Stream_Size_Attribute,
 A_Wide_Wide_Image_Attribute              => A_Wide_Wide_Image_Attribute,
 A_Wide_Wide_Value_Attribute              => A_Wide_Wide_Value_Attribute,
 A_Wide_Wide_Width_Attribute              => A_Wide_Wide_Width_Attribute,
 A_Max_Alignment_For_Allocation_Attribute =>
   A_Max_Alignment_For_Allocation_Attribute,
 An_Overlaps_Storage_Attribute            => An_Overlaps_Storage_Attribute,
--  |A2005/2012 end

 An_Implementation_Defined_Attribute      =>
    An_Implementation_Defined_Attribute,

 An_Unknown_Attribute                     => An_Unknown_Attribute);
------------------------------------------------------------------------------

   Association_Kind_Switch : constant array (Internal_Association_Kinds) of
                          Association_Kinds :=
(
 A_Pragma_Argument_Association  => A_Pragma_Argument_Association,
 A_Discriminant_Association     => A_Discriminant_Association,
 A_Record_Component_Association => A_Record_Component_Association,
 An_Array_Component_Association => An_Array_Component_Association,
 A_Parameter_Association        => A_Parameter_Association,
 A_Generic_Association          => A_Generic_Association);
------------------------------------------------------------------------------

   Statement_Kind_Switch : constant array (Internal_Statement_Kinds) of
                        Statement_Kinds :=

(A_Null_Statement                   => A_Null_Statement,
 An_Assignment_Statement            => An_Assignment_Statement,
 An_If_Statement                    => An_If_Statement,
 A_Case_Statement                   => A_Case_Statement,
 A_Loop_Statement                   => A_Loop_Statement,
 A_While_Loop_Statement             => A_While_Loop_Statement,
 A_For_Loop_Statement               => A_For_Loop_Statement,
 A_Block_Statement                  => A_Block_Statement,
 An_Exit_Statement                  => An_Exit_Statement,
 A_Goto_Statement                   => A_Goto_Statement,
 A_Procedure_Call_Statement         => A_Procedure_Call_Statement,
 A_Return_Statement                 => A_Return_Statement,

--  --|A2005 start
 An_Extended_Return_Statement       => An_Extended_Return_Statement,
--  --|A2005 end

 An_Accept_Statement                => An_Accept_Statement,
 An_Entry_Call_Statement            => An_Entry_Call_Statement,
 A_Requeue_Statement                => A_Requeue_Statement,
 A_Requeue_Statement_With_Abort     => A_Requeue_Statement_With_Abort,
 A_Delay_Until_Statement            => A_Delay_Until_Statement,
 A_Delay_Relative_Statement         => A_Delay_Relative_Statement,
 A_Terminate_Alternative_Statement  => A_Terminate_Alternative_Statement,
 A_Selective_Accept_Statement       => A_Selective_Accept_Statement,
 A_Timed_Entry_Call_Statement       => A_Timed_Entry_Call_Statement,
 A_Conditional_Entry_Call_Statement => A_Conditional_Entry_Call_Statement,
 An_Asynchronous_Select_Statement   => An_Asynchronous_Select_Statement,
 An_Abort_Statement                 => An_Abort_Statement,
 A_Raise_Statement                  => A_Raise_Statement,
 A_Code_Statement                   => A_Code_Statement);
------------------------------------------------------------------------------
   Path_Kind_Switch : constant array (Internal_Path_Kinds) of
                          Path_Kinds :=

(An_If_Path               => An_If_Path,
 An_Elsif_Path            => An_Elsif_Path,
 An_Else_Path             => An_Else_Path,
 A_Case_Path              => A_Case_Path,
 A_Select_Path            => A_Select_Path,
 An_Or_Path               => An_Or_Path,
 A_Then_Abort_Path        => A_Then_Abort_Path,
 A_Case_Expression_Path   => A_Case_Expression_Path,
 An_If_Expression_Path    => An_If_Expression_Path,
 An_Elsif_Expression_Path => An_Elsif_Expression_Path,
 An_Else_Expression_Path  => An_Else_Expression_Path);
------------------------------------------------------------------------------

   Clause_Kind_Switch : constant array (Internal_Clause_Kinds) of Clause_Kinds
     :=
(A_Use_Package_Clause      => A_Use_Package_Clause,
 A_Use_Type_Clause         => A_Use_Type_Clause,
 A_Use_All_Type_Clause     => A_Use_All_Type_Clause,  --  Ada 2012
 A_With_Clause             => A_With_Clause,

--  A_Representation_Clause,        -- 13.1     -> Representation_Clause_Kinds

   An_Attribute_Definition_Clause ..
   An_At_Clause              => A_Representation_Clause,

 A_Component_Clause        => A_Component_Clause);
------------------------------------------------------------------------------

   Representation_Clause_Kind_Switch :
           constant array (Internal_Representation_Clause_Kinds) of
           Representation_Clause_Kinds :=

(An_Attribute_Definition_Clause       => An_Attribute_Definition_Clause,
 An_Enumeration_Representation_Clause => An_Enumeration_Representation_Clause,
 A_Record_Representation_Clause       => A_Record_Representation_Clause,
 An_At_Clause                         => An_At_Clause);
------------------------------------------------------------------------------

   Def_Op_Switch : constant array (Internal_Operator_Symbol_Kinds) of
                   Internal_Defining_Operator_Kinds :=

(An_And_Operator                  => A_Defining_And_Operator,
 An_Or_Operator                   => A_Defining_Or_Operator,
 An_Xor_Operator                  => A_Defining_Xor_Operator,
 An_Equal_Operator                => A_Defining_Equal_Operator,
 A_Not_Equal_Operator             => A_Defining_Not_Equal_Operator,
 A_Less_Than_Operator             => A_Defining_Less_Than_Operator,
 A_Less_Than_Or_Equal_Operator    => A_Defining_Less_Than_Or_Equal_Operator,
 A_Greater_Than_Operator          => A_Defining_Greater_Than_Operator,
 A_Greater_Than_Or_Equal_Operator => A_Defining_Greater_Than_Or_Equal_Operator,
 A_Plus_Operator                  => A_Defining_Plus_Operator,
 A_Minus_Operator                 => A_Defining_Minus_Operator,
 A_Concatenate_Operator           => A_Defining_Concatenate_Operator,
 A_Unary_Plus_Operator            => A_Defining_Unary_Plus_Operator,
 A_Unary_Minus_Operator           => A_Defining_Unary_Minus_Operator,
 A_Multiply_Operator              => A_Defining_Multiply_Operator,
 A_Divide_Operator                => A_Defining_Divide_Operator,
 A_Mod_Operator                   => A_Defining_Mod_Operator,
 A_Rem_Operator                   => A_Defining_Rem_Operator,
 An_Exponentiate_Operator         => A_Defining_Exponentiate_Operator,
 An_Abs_Operator                  => A_Defining_Abs_Operator,
 A_Not_Operator                   => A_Defining_Not_Operator);
------------------------------------------------------------------------------

   -------------------------------------------------
   -- Internal Element Kinds Conversion Functions --
   -------------------------------------------------

   ------------------------------------
   -- Access_Type_Kind_From_Internal --
   ------------------------------------

   function Access_Type_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Access_Type_Kinds is
   begin
      if Internal_Kind in Internal_Access_Type_Kinds then
         return Access_Type_Kind_Switch (Internal_Kind);
      elsif Internal_Kind in Internal_Formal_Access_Type_Kinds then

         return Formal_Access_Type_Kind_Switch (Internal_Kind);
      else
         return Not_An_Access_Type_Definition;
      end if;
   end Access_Type_Kind_From_Internal;

   -----------------------------
   -- Asis_From_Internal_Kind --
   -----------------------------

   function Asis_From_Internal_Kind
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Element_Kinds
   is
   begin

      case Internal_Kind is

         when Internal_Pragma_Kinds =>
            return A_Pragma;

         when Internal_Defining_Name_Kinds =>
            return A_Defining_Name;

         when Internal_Declaration_Kinds =>
            return A_Declaration;

         when Internal_Definition_Kinds =>
            return A_Definition;

         when Internal_Expression_Kinds =>
            return An_Expression;

         when Internal_Association_Kinds =>
            return An_Association;

         when Internal_Statement_Kinds =>
            return A_Statement;

         when Internal_Path_Kinds =>
            return A_Path;

         when Internal_Clause_Kinds =>
            return A_Clause;

         when An_Exception_Handler =>
            return An_Exception_Handler;

         when others => -- Not_An_Element and Not_A_XXX values
            return Not_An_Element;

      end case;

   end Asis_From_Internal_Kind;

   ------------------------------------
   -- Association_Kind_From_Internal --
   ------------------------------------

   function Association_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Association_Kinds is
   begin
      if Internal_Kind not in Internal_Association_Kinds then
         return Not_An_Association;
      else
         return Association_Kind_Switch (Internal_Kind);
      end if;
   end Association_Kind_From_Internal;

   ----------------------------------
   -- Attribute_Kind_From_Internal --
   ----------------------------------

   function Attribute_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Attribute_Kinds is
   begin
      if Internal_Kind not in Internal_Attribute_Reference_Kinds then
         return Not_An_Attribute;
      else
         return Attribute_Kind_Switch (Internal_Kind);
      end if;
   end Attribute_Kind_From_Internal;

   -------------------------------
   -- Clause_Kind_From_Internal --
   -------------------------------

   function Clause_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Clause_Kinds is
   begin
      if Internal_Kind not in Internal_Clause_Kinds then
         return Not_A_Clause;
      else
         return Clause_Kind_Switch (Internal_Kind);
      end if;
   end Clause_Kind_From_Internal;

   -----------------------------------
   -- Constraint_Kind_From_Internal --
   -----------------------------------

   function Constraint_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Constraint_Kinds is
   begin
      if Internal_Kind not in Internal_Constraint_Kinds then
         return Not_A_Constraint;
      else
         return Constraint_Kind_Switch (Internal_Kind);
      end if;
   end Constraint_Kind_From_Internal;

   ------------------------------------
   -- Declaration_Kind_From_Internal --
   ------------------------------------

   function Declaration_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Declaration_Kinds is
   begin
      if Internal_Kind not in Internal_Declaration_Kinds then
         return Not_A_Declaration;
      else
         return Declaration_Kind_Switch (Internal_Kind);
      end if;
   end Declaration_Kind_From_Internal;

   -------------------------------------
   -- Defining_Name_Kind_From_Internal--
   -------------------------------------

   function Defining_Name_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Defining_Name_Kinds is
   begin
      if Internal_Kind not in Internal_Defining_Name_Kinds then
         return Not_A_Defining_Name;
      else
         return Defining_Name_Kind_Switch (Internal_Kind);
      end if;
   end Defining_Name_Kind_From_Internal;

   -----------------------------------
   -- Definition_Kind_From_Internal --
   -----------------------------------

   function Definition_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Definition_Kinds is
   begin
      if Internal_Kind not in Internal_Definition_Kinds then
         return Not_A_Definition;
      else
         return Definition_Kind_Switch (Internal_Kind);
      end if;
   end Definition_Kind_From_Internal;

   ---------------------------------------
   -- Discrete_Range_Kind_From_Internal --
   ---------------------------------------

   function Discrete_Range_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Discrete_Range_Kinds is
   begin
         return Discrete_Range_Kind_Switch (Internal_Kind);
   end Discrete_Range_Kind_From_Internal;

   -----------------------------------
   -- Expression_Kind_From_Internal --
   -----------------------------------

   function Expression_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Expression_Kinds is
   begin
      if Internal_Kind not in Internal_Expression_Kinds then
         return Not_An_Expression;
      else
         return Expression_Kind_Switch (Internal_Kind);
      end if;
   end Expression_Kind_From_Internal;

   ------------------------------------
   -- Formal_Type_Kind_From_Internal --
   ------------------------------------

   function Formal_Type_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Formal_Type_Kinds is
   begin
      if Internal_Kind not in Internal_Formal_Type_Kinds then
         return Not_A_Formal_Type_Definition;
      else
         return Formal_Type_Kind_Switch (Internal_Kind);
      end if;
   end Formal_Type_Kind_From_Internal;

--  --|A2005 start

   ------------------------------------------
   -- Access_Definition_Kind_From_Internal --
   ------------------------------------------

   function Access_Definition_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Access_Definition_Kinds
   is
   begin
      if Internal_Kind not in Internal_Access_Definition_Kinds then
         return Not_An_Access_Definition;
      else
         return Access_Definition_Kind_Switch (Internal_Kind);
      end if;
   end Access_Definition_Kind_From_Internal;

   ----------------------------------
   -- Interface_Kind_From_Internal --
   ----------------------------------

   function Interface_Kind_From_Internal
     (Internal_Kind : Internal_Element_Kinds)
      return Asis.Interface_Kinds
   is
   begin

      if Internal_Kind in Internal_Interface_Kinds then
         return Interface_Kind_Switch (Internal_Kind);
      elsif Internal_Kind in Internal_Formal_Interface_Kinds then
         return Formal_Interface_Kind_Switch (Internal_Kind);
      else
         return Not_An_Interface;
      end if;

   end Interface_Kind_From_Internal;

--  --|A2005 end

   ---------------------------------
   -- Operator_Kind_From_Internal --
   ---------------------------------

   function Operator_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Operator_Kinds is
   begin
         return Operator_Kind_Switch (Internal_Kind);
   end Operator_Kind_From_Internal;

   -----------------------------
   -- Path_Kind_From_Internal --
   -----------------------------

   function Path_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Path_Kinds is
   begin
      if Internal_Kind not in Internal_Path_Kinds then
         return Not_A_Path;
      else
         return Path_Kind_Switch (Internal_Kind);
      end if;
   end Path_Kind_From_Internal;

   -------------------------------
   -- Pragma_Kind_From_Internal --
   -------------------------------

   function Pragma_Kind_From_Internal (Internal_Kind : Internal_Element_Kinds)
             return Asis.Pragma_Kinds is
   begin
      if Internal_Kind not in Internal_Pragma_Kinds then
         return Not_A_Pragma;
      else
         return Pragma_Kind_Switch (Internal_Kind);
      end if;
   end Pragma_Kind_From_Internal;

   ----------------------------------------------
   -- Representation_Clause_Kind_From_Internal --
   -----------------------------------------------

   function Representation_Clause_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Representation_Clause_Kinds is
   begin
      if Internal_Kind not in Internal_Representation_Clause_Kinds then
         return Not_A_Representation_Clause;
      else
         return Representation_Clause_Kind_Switch (Internal_Kind);
      end if;
   end Representation_Clause_Kind_From_Internal;

   ----------------------------------
   -- Root_Type_Kind_From_Internal --
   ----------------------------------

   function Root_Type_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Root_Type_Kinds is
   begin
      if Internal_Kind not in Internal_Root_Type_Kinds then
         return Not_A_Root_Type_Definition;
      else
         return Root_Type_Kind_Switch (Internal_Kind);
      end if;
   end Root_Type_Kind_From_Internal;

   ----------------------------------
   -- Statement_Kind_From_Internal --
   ----------------------------------

   function Statement_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Statement_Kinds is
   begin
      if Internal_Kind not in Internal_Statement_Kinds then
         return Not_A_Statement;
      else
         return Statement_Kind_Switch (Internal_Kind);
      end if;
   end Statement_Kind_From_Internal;

   -----------------------------
   -- Type_Kind_From_Internal --
   -----------------------------

   function Type_Kind_From_Internal
         (Internal_Kind : Internal_Element_Kinds)
          return Asis.Type_Kinds is
   begin
      if Internal_Kind not in Internal_Type_Kinds then
         return Not_A_Type_Definition;
      else
         return Type_Kind_Switch (Internal_Kind);
      end if;
   end Type_Kind_From_Internal;

   -------------------------------------
   -- Additional Classification items --
   -------------------------------------

   -----------------------
   -- Def_Operator_Kind --
   -----------------------

   function Def_Operator_Kind
     (Op_Kind : Internal_Element_Kinds)
      return Internal_Element_Kinds is
   begin
      return Def_Op_Switch (Op_Kind);
   end Def_Operator_Kind;

------------------------------------------------------------------------------
end A4G.Knd_Conv;
