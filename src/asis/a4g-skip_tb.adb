------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . S K I P _ T B                           --
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

with Asis.Definitions; use Asis.Definitions;
with Asis.Elements;    use Asis.Elements;
with Asis.Expressions; use Asis.Expressions;

with Asis.Set_Get;     use Asis.Set_Get;

with A4G.A_Debug;      use A4G.A_Debug;
with A4G.A_Sinput;     use A4G.A_Sinput;
with A4G.A_Types;      use A4G.A_Types;
with A4G.Int_Knds;     use A4G.Int_Knds;

with Atree;            use Atree;
with Output;           use Output;
with Sinfo;            use Sinfo;

package body A4G.Skip_TB is

   --  In fact, there is a very few different cases of skipping the "syntax
   --  sugar", but we implement this skipping by means of look-up table
   --  for ease of maintenance.

   -------------------------------
   -- Specific Serach Functions --
   -------------------------------

   type Find_Source_Location_Type is access
       function (E : Asis.Element; S : Source_Ptr) return Source_Ptr;
   --  All the specific search functions are of the designated profile of
   --  this access-to-function type. We need the Element parameter to
   --  define the presennce or absence of optional component, because
   --  such optional components may have trailing brackets specific for
   --  them

   function No_Brackets (E : Asis.Element; S : Source_Ptr) return Source_Ptr;
   --  No seaqrch is made, the argument is returned

   function Search_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  The location of the right-most ')' is returned. The search is started
   --  the position S + 1, so if S itself points to ')', S will not be
   --  returned

   function Search_QE_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  Version of the previous function for a qualified expression. If the
   --  argument is an aggregate which does not Needs_Extra_Parentheses, this
   --  function is equivalent to No_Brackets, otherwise is the same as
   --  Search_Right_Parenthesis

   function Search_Attr_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  If E has a static expression as part of  attribute designator,
   --  this function looks for the  location of the right-most ')'.
   --  otherwise it makes no search.
   --  E should be of A_First_Attribute, A_Last_Attribute, A_Length_Attribute,
   --  A_Range_Attribute, An_Implementation_Defined_Attribute or
   --  An_Unknown_Attribute kind only, otherwise ASIS_Inappropriate_Element
   --  will be raised

   function Search_Before_Semicolon
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  This is a somewhat special case: a task/protected declaration ends
   --  with "end [identifier];", but the corresponding definition ends
   --  with "end [identifier]", that is, *without* ";" .
   --  When this function is called, it is supposed, that S points
   --  to the trailing semicolon of the last component of a task/protected
   --  definition or to the first character of the trailing end in case
   --  if there is no component

   function Search_Semicolon
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  The location of the rigt-most ';' is returned, starting from the
   --  position next to S, but not S itself

   function Clear_After_Funcion_Call
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;
   --  For a function call we have to make a differenve between an infix
   --  call (no trailing brackets to skip) and prefix call,  and in the
   --  latter case - between the case when there is no parameter (no
   --  trailing brackets to skip) and the case when there is(are) a
   --  parameter(s) (a ')' should be skipped)

   function Skip_WP (E : Asis.Element; S : Source_Ptr) return Source_Ptr;
   --  This function handles a special case with
   --  A_Formal_Derived_Type_Definition: in skips 'with private"
   --  keywords, if any

   function Search_End_Of_Box
     (E    : Asis.Element;
      S    : Source_Ptr)
      return Source_Ptr;
   --  If E may contain a box as the end of its text image, this function
   --  returns the location of the '>' that is the end of this box, otherwise
   --  it returns S. It is supposed that there is no other '<' between S and
   --  the end of box except '<' that is the beginning of the box to look for.

   function A_Bug (E : Asis.Element; S : Source_Ptr) return Source_Ptr;
   --  This function should never be called. We need it for "others"
   --  choise in the aggregate initializing the look-up table for those
   --  values of Internal_Element_Kinds which should never be processed
   --  (they correspond to implicit root and universal types). This
   --  function raises Internal_Implementation_Error

   ------------------
   -- Serach Array --
   ------------------

   Skip_Switch : constant array (Internal_Element_Kinds) of
     Find_Source_Location_Type :=
     (An_All_Calls_Remote_Pragma ..
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
      An_Unknown_Pragma =>               Search_Semicolon'Access,

      A_Defining_Identifier ..
      --  A_Defining_Character_Literal
      --  A_Defining_Enumeration_Literal
      --  A_Defining_And_Operator
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
      --  A_Defining_Not_Operator
      A_Defining_Expanded_Name =>        No_Brackets'Access,

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
      --  An_Integer_Number_Declaration
      A_Real_Number_Declaration            => Search_Semicolon'Access,

      An_Enumeration_Literal_Specification => No_Brackets'Access,
      A_Discriminant_Specification         => No_Brackets'Access,
      A_Component_Declaration              => Search_Semicolon'Access,
      A_Loop_Parameter_Specification       => No_Brackets'Access,

      A_Generalized_Iterator_Specification => No_Brackets'Access,
      An_Element_Iterator_Specification    => No_Brackets'Access,

      A_Procedure_Declaration              => Search_Semicolon'Access,
      A_Function_Declaration               => Search_Semicolon'Access,
      A_Parameter_Specification            => No_Brackets'Access,

      A_Procedure_Body_Declaration ..
      A_Function_Body_Declaration          => Search_Semicolon'Access,

      A_Return_Variable_Specification      => No_Brackets'Access,
      A_Return_Constant_Specification      => No_Brackets'Access,

      A_Null_Procedure_Declaration ..
      --  An_Expression_Function_Declaration
      --  A_Package_Declaration
      --  A_Package_Body_Declaration
      --  A_Package_Body_Declaration
      --  An_Object_Renaming_Declaration
      --  An_Exception_Renaming_Declaration
      --  A_Package_Renaming_Declaration
      --  A_Procedure_Renaming_Declaration
      --  A_Function_Renaming_Declaration
      --  A_Generic_Package_Renaming_Declaration
      --  A_Generic_Procedure_Renaming_Declaration
      --  A_Generic_Function_Renaming_Declaration
      --  A_Task_Body_Declaration
      --  A_Protected_Body_Declaration
      --  An_Entry_Declaration
      An_Entry_Body_Declaration        => Search_Semicolon'Access,

      An_Entry_Index_Specification     => No_Brackets'Access,

      A_Procedure_Body_Stub ..
      --  A_Function_Body_Stub
      --  A_Package_Body_Stub
      --  A_Task_Body_Stub
      --  A_Protected_Body_Stub
      An_Exception_Declaration         => Search_Semicolon'Access,

      A_Choice_Parameter_Specification => No_Brackets'Access,

      A_Generic_Procedure_Declaration ..
      --  A_Generic_Function_Declaration
      --  A_Generic_Package_Declaration
      --  A_Package_Instantiation
      --  A_Procedure_Instantiation
      --  A_Function_Instantiation
      --  A_Formal_Object_Declaration
      --  A_Formal_Type_Declaration
      --  A_Formal_Procedure_Declaration
      --  A_Formal_Function_Declaration
      --  A_Formal_Package_Declaration
      A_Formal_Package_Declaration_With_Box => Search_Semicolon'Access,

      A_Derived_Type_Definition             => No_Brackets'Access,
      A_Derived_Record_Extension_Definition => No_Brackets'Access,
      An_Enumeration_Type_Definition        => Search_Right_Parenthesis'Access,

      A_Signed_Integer_Type_Definition ..
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
      --  A_Constrained_Array_Definition
      --  A_Record_Type_Definition
      --  A_Tagged_Record_Type_Definition
      --  A_Pool_Specific_Access_To_Variable
      --  An_Access_To_Variable
      An_Access_To_Constant            => No_Brackets'Access,

      An_Access_To_Procedure           => Search_Right_Parenthesis'Access,
      An_Access_To_Protected_Procedure => Search_Right_Parenthesis'Access,

      An_Access_To_Function ..
      --  An_Access_To_Protected_Function
      --  A_Subtype_Indication
      --  A_Range_Attribute_Reference
      --  A_Simple_Expression_Range
      --  A_Digits_Constraint
      A_Delta_Constraint               => No_Brackets'Access,

      An_Index_Constraint              => Search_Right_Parenthesis'Access,
      A_Discriminant_Constraint        => Search_Right_Parenthesis'Access,

      A_Component_Definition ..
      --  A_Discrete_Subtype_Indication_As_Subtype_Definition
      --  A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
      --  A_Discrete_Simple_Expression_Range_As_Subtype_Definition
      --  A_Discrete_Subtype_Indication
      --  A_Discrete_Range_Attribute_Reference
      --  A_Discrete_Simple_Expression_Range
      An_Unknown_Discriminant_Part     => No_Brackets'Access,

      A_Known_Discriminant_Part        => Search_Right_Parenthesis'Access,
      A_Record_Definition              => No_Brackets'Access,
      A_Null_Record_Definition         => No_Brackets'Access,
      A_Null_Component                 => Search_Semicolon'Access,
      A_Variant_Part                   => Search_Semicolon'Access,

      A_Variant ..
      An_Others_Choice                 => No_Brackets'Access,

--  --|A2005 start

      An_Anonymous_Access_To_Variable ..
      An_Anonymous_Access_To_Constant  => No_Brackets'Access,

      An_Anonymous_Access_To_Procedure ..
      An_Anonymous_Access_To_Protected_Procedure =>
        Search_Right_Parenthesis'Access,

      An_Anonymous_Access_To_Function ..
      An_Anonymous_Access_To_Protected_Function => No_Brackets'Access,
--  --|A2005 end

      A_Private_Type_Definition ..
      --  A_Tagged_Private_Type_Definition
      A_Private_Extension_Definition   => No_Brackets'Access,

      A_Task_Definition ..
      A_Protected_Definition           => Search_Before_Semicolon'Access,

      A_Formal_Private_Type_Definition ..
      A_Formal_Tagged_Private_Type_Definition => No_Brackets'Access,

      A_Formal_Derived_Type_Definition        => Skip_WP'Access,

      A_Formal_Discrete_Type_Definition ..
      --  A_Formal_Signed_Integer_Type_Definition
      --  A_Formal_Modular_Type_Definition
      --  A_Formal_Floating_Point_Definition
      --  A_Formal_Ordinary_Fixed_Point_Definition
      --  A_Formal_Decimal_Fixed_Point_Definition
      --  A_Formal_Unconstrained_Array_Definition
      --  A_Formal_Constrained_Array_Definition
      --  A_Formal_Pool_Specific_Access_To_Variable
      --  A_Formal_Access_To_Variable
      A_Formal_Access_To_Constant            => No_Brackets'Access,

      A_Formal_Access_To_Procedure           =>
         Search_Right_Parenthesis'Access,

      A_Formal_Access_To_Protected_Procedure =>
         Search_Right_Parenthesis'Access,

      A_Formal_Access_To_Function ..
      --  A_Formal_Access_To_Protected_Function
      --  An_Integer_Literal
      --  A_Real_Literal
      --  A_String_Literal
      --  An_Identifier
      --  An_And_Operator
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
      --  An_Enumeration_Literal
      An_Explicit_Dereference          => No_Brackets'Access,

      A_Function_Call                  => Clear_After_Funcion_Call'Access,

      An_Indexed_Component ..
      A_Slice                          => Search_Right_Parenthesis'Access,

      A_Selected_Component             => No_Brackets'Access,

      An_Access_Attribute ..
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
      An_External_Tag_Attribute        => No_Brackets'Access,

      A_First_Attribute                => Search_Attr_Right_Parenthesis'Access,

      A_First_Bit_Attribute ..
      --  A_Floor_Attribute
      --  A_Fore_Attribute
      --  A_Fraction_Attribute
      --  An_Identity_Attribute
      --  An_Image_Attribute
      An_Input_Attribute               => No_Brackets'Access,
      A_Last_Attribute                 => Search_Attr_Right_Parenthesis'Access,
      A_Last_Bit_Attribute             => No_Brackets'Access,
      A_Leading_Part_Attribute         => No_Brackets'Access,
      A_Length_Attribute               => Search_Attr_Right_Parenthesis'Access,

      A_Machine_Attribute ..
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
      A_Pred_Attribute                 => No_Brackets'Access,

      A_Range_Attribute                => Search_Attr_Right_Parenthesis'Access,

      A_Read_Attribute ..
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
--  |A2006 start
--  New Ada 2005 attributes. To be alphabetically ordered later
      --  A_Machine_Rounding_Attribute
      --  A_Mod_Attribute
      --  A_Priority_Attribute
      --  A_Stream_Size_Attribute
      --  A_Wide_Wide_Image_Attribute
      --  A_Wide_Wide_Value_Attribute
      A_Wide_Wide_Width_Attribute => No_Brackets'Access,
--  |A2006 end

      An_Implementation_Defined_Attribute ..
      An_Unknown_Attribute             => Search_Attr_Right_Parenthesis'Access,

      A_Record_Aggregate ..
      --  An_Extension_Aggregate
      --  A_Positional_Array_Aggregate
      A_Named_Array_Aggregate          => Search_Right_Parenthesis'Access,

      An_And_Then_Short_Circuit ..
      --  An_Or_Else_Short_Circuit
      --  An_In_Range_Membership_Test
      --  A_Not_In_Range_Membership_Test
      --  An_In_Type_Membership_Test
      --  A_Not_In_Type_Membership_Test
      A_Null_Literal                   => No_Brackets'Access,

      A_Parenthesized_Expression ..
      A_Type_Conversion                => Search_Right_Parenthesis'Access,

      A_Qualified_Expression => Search_QE_Right_Parenthesis'Access,

      An_Allocation_From_Subtype ..
      An_Allocation_From_Qualified_Expression => No_Brackets'Access,

      A_Case_Expression                => No_Brackets'Access, --  Ada 2012
      An_If_Expression                 => No_Brackets'Access, --  Ada 2012
      A_For_All_Quantified_Expression  => No_Brackets'Access, --  Ada 2012
      A_For_Some_Quantified_Expression => No_Brackets'Access, --  Ada 2012

      A_Pragma_Argument_Association ..
      --  A_Discriminant_Association
      --  A_Record_Component_Association
      --  An_Array_Component_Association
      A_Parameter_Association          => No_Brackets'Access,

      A_Generic_Association            => Search_End_Of_Box'Access,

      A_Null_Statement ..
      --  An_Assignment_Statement
      --  An_If_Statement
      --  A_Case_Statement
      --  A_Loop_Statement
      --  A_While_Loop_Statement
      --  A_For_Loop_Statement
      --  A_Block_Statement
      --  An_Exit_Statement
      --  A_Goto_Statement
      --  A_Procedure_Call_Statement
      --  A_Return_Statement
      --  An_Accept_Statement
      --  An_Entry_Call_Statement
      --  A_Requeue_Statement
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
      A_Code_Statement                 => Search_Semicolon'Access,

      An_If_Path ..
      --  An_Elsif_Path
      --  An_Else_Path
      --  A_Case_Path
      --  A_Select_Path
      --  An_Or_Path
      --  A_Then_Abort_Path
      --  A_Case_Expression_Path      --  ASIS 2012
      --  An_If_Expression_Path       --  ASIS 2012
      --  An_Elsif_Expression_Path    --  ASIS 2012
      An_Else_Expression_Path          => No_Brackets'Access,  -- Ada 2012

      A_Use_Package_Clause ..
      --  A_Use_Type_Clause
      --  A_Use_All_Type_Clause  --  Ada 2012
      --  A_With_Clause
      --  An_Attribute_Definition_Clause
      --  An_Enumeration_Representation_Clause
      --  A_Record_Representation_Clause
      --  An_At_Clause
      A_Component_Clause               => Search_Semicolon'Access,

      An_Exception_Handler             => No_Brackets'Access,

      others                           => A_Bug'Access);

   -----------
   -- A_Bug --
   -----------

   function A_Bug (E : Asis.Element; S : Source_Ptr) return Source_Ptr is
   begin
      pragma Unreferenced (E);

      --  This function should never be called
      raise Internal_Implementation_Error;
      return S;
   end A_Bug;

   ------------------------------
   -- Clear_After_Funcion_Call --
   ------------------------------

   function Clear_After_Funcion_Call
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
   begin
      if Is_Prefix_Call (E) then
         if Is_Nil (Function_Call_Parameters (E)) then
            return No_Brackets (E, S);
         else
            return Search_Right_Parenthesis (E, S);
         end if;
      else
         return No_Brackets (E, S);
      end if;
   end Clear_After_Funcion_Call;

   -----------------------------
   -- Needs_Extra_Parentheses --
   -----------------------------

   function Needs_Extra_Parentheses (E : Asis.Element) return Boolean is
      Result : Boolean := False;
      N      : Node_Id;
   begin

      if Int_Kind (E) in A_Record_Aggregate .. A_Named_Array_Aggregate then
         N := R_Node (E);

         if Paren_Count (N) = 1 and then
            Nkind (Parent (N)) = N_Qualified_Expression
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Needs_Extra_Parentheses;

   -----------------
   -- No_Brackets --
   -----------------

   function No_Brackets (E : Asis.Element; S : Source_Ptr) return Source_Ptr is
   begin
      pragma Unreferenced (E);

      return S;
   end No_Brackets;

   -----------------------------------
   -- Search_Attr_Right_Parenthesis --
   -----------------------------------

   function Search_Attr_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
   begin
      if Is_Nil (Attribute_Designator_Expressions (E)) then
         return No_Brackets (E, S);
      else
         return Search_Right_Parenthesis (E, S);
      end if;
   end Search_Attr_Right_Parenthesis;

   -----------------------------
   -- Search_Before_Semicolon --
   -----------------------------

   function Search_Before_Semicolon
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
      S_P    : Source_Ptr;
      Result : Source_Ptr;
   begin
      --  first, we have to check where we are: at ';' or at the beginning of
      --  the reserved word "end" in case if there is no components,
      --  and if we are at ';', we have to go to "end":
      if Get_Character (S) = ';' then
         S_P := Next_Identifier (S);

      elsif Asis.Definitions.Is_Private_Present (E) then
         --  patological case of empty efinition with private part
         S_P := S + 7;
      else
         S_P := S;
         S_P := Rightmost_Non_Blank (S_P);
      end if;

      Result := S_P + 2;
      --  the last character of "end"
      S_P := S_P + 3;
      --  the first character after "end"
      S_P := Rightmost_Non_Blank (S_P);

      --  and the final check - what follows the final "end"
      if Get_Character (S_P) /= ';' then
         --  there is an identifier after "end"
         Result := Get_Word_End (P       => S_P,
                                 In_Word => In_Identifier'Access);
      end if;

      return Result;

   end Search_Before_Semicolon;

   -----------------------
   -- Search_End_Of_Box --
   -----------------------

   function Search_End_Of_Box
     (E    : Asis.Element;
      S    : Source_Ptr)
      return Source_Ptr
   is
      Result : Source_Ptr            := S;
   begin

      case Int_Kind (E) is
         when A_Generic_Association =>

            if Is_Nil (Actual_Parameter (E)) then
               Result := Search_Rightmost_Symbol (S, '<');
               Result := Result + 1;
            end if;

         when others =>
            null;
      end case;

      return Result;
   end Search_End_Of_Box;

   ---------------------------------
   -- Search_QE_Right_Parenthesis --
   ---------------------------------

   function Search_QE_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
      Result : Source_Ptr            := S;
      Tmp    : constant Asis.Element := Converted_Or_Qualified_Expression (E);
   begin

      if Int_Kind (Tmp) not in A_Record_Aggregate .. A_Named_Array_Aggregate
        or else Needs_Extra_Parentheses (Tmp)
      then
         Result := Search_Rightmost_Symbol (S + 1, ')');
      end if;

      return Result;

   end Search_QE_Right_Parenthesis;

   ------------------------------
   -- Search_Right_Parenthesis --
   ------------------------------

   function Search_Right_Parenthesis
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
   begin
      pragma Unreferenced (E);

      return Search_Rightmost_Symbol (S + 1, ')');
   end Search_Right_Parenthesis;

   ----------------------
   -- Search_Semicolon --
   ----------------------

   function Search_Semicolon
     (E : Asis.Element;
      S : Source_Ptr)
   return Source_Ptr
   is
      pragma Unreferenced (E);

      S_P : Source_Ptr;
   begin
      S_P := Search_Rightmost_Symbol (S + 1, ';');
      return S_P;
   end Search_Semicolon;

   -------------
   -- Skip_WP --
   -------------

   function Skip_WP (E : Asis.Element; S : Source_Ptr) return Source_Ptr is
      S_P : Source_Ptr;
   begin
      --  just jump to the trailing semicolon, and then - to the end of
      --  the previous word, this should handle properly cases where there
      --  are and where there are not "with private" keywords
      S_P := Search_Semicolon (E, S);
      S_P := Search_Prev_Word (S_P);
      return S_P;
   end Skip_WP;

   ----------------------------
   -- Skip_Trailing_Brackets --
   ----------------------------

   function Skip_Trailing_Brackets
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr
   is
   begin
      --  all that this function does is switching to the function
      --  implementing the specific processing for the given element
      --  kind

      if Debug_Flag_X then
         Write_Str ("  Skip_Trailing_Brackets - called for ");
         Write_Str (Internal_Element_Kinds'Image (Int_Kind (E)));
         Write_Eol;
         Write_Eol;
      end if;

      return Skip_Switch (Int_Kind (E)) (E, S);

   end Skip_Trailing_Brackets;

end A4G.Skip_TB;
