------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . E N C L _ E L                          --
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

with Asis;              use Asis;
with Asis.Declarations; use Asis.Declarations;
with Asis.Elements;     use Asis.Elements;
with Asis.Extensions;

with Asis.Set_Get;      use Asis.Set_Get;

with A4G.A_Sem;         use A4G.A_Sem;
with A4G.A_Types;       use A4G.A_Types;
with A4G.Int_Knds;      use A4G.Int_Knds;
with A4G.Mapping;       use A4G.Mapping;
with A4G.Queries;       use A4G.Queries;
with A4G.Vcheck;        use A4G.Vcheck;

with Atree;             use Atree;
with Einfo;             use Einfo;
with Namet;             use Namet;
with Nlists;            use Nlists;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;
with Snames;
with Stand;             use Stand;
with Types;             use Types;

package body A4G.Encl_El is

   ------------------------------------------------
   -- The general approach to the implementation --
   -- of the Enclosing_Element query             --
   ------------------------------------------------

   --  There are important differences in the ways how an Enclosing_Element
   --  is retrieved for explicit and implicit Elements, and for the elements
   --  from expanded generics. For explicit Elements, the general way to get
   --  the enclosing Element is to do the necessary bottom-up tree traversing,
   --  for most of the cases all what we need if one step up the front-end
   --  tree, but sometimes the differences between front-end and ASIS trees
   --  require some non-trivial traversing.
   --
   --  For implicit Elements, there is a semantic link between a top Element
   --  of an ASIS implicit sub-hierarchy and some explicit Element that
   --  "generates" this subhierarchy. For example, an implicit declaration of
   --  an inherited supprogram is "generated" by some derived type definition,
   --  so inside the implicit subhierarchy we use the same approach for
   --  retrieving the enclosing Element as for explicit Elements, but the
   --  enclosing Element for subhierarchy is the construct that "generates" the
   --  subhierarchy, and to get to this construct, the link stored as a part
   --  of implicit elements structure is used.
   --
   --  For Elements from generic instantiations, we do bottom-up traversing of
   --  the ASIS/front-end tree structure corresponding to the expanded code
   --  in the same way as for explicit Elements, but when we are at the top of
   --  an expanded spec or body, the next Enclosing_Element step should go
   --  to the corresponding instantiation, so here we also do something
   --  different that bottom-up tree traversing
   --
   --  But for most of the cases the way to get the enclosing Element is to
   --  map the bottom-up traversing of the compiler tree onto the ASIS Elements
   --  hierarchy. This is performed by Enclosing_Element_For_Explicit function,
   --  and all the other routines defined in this package detect and process
   --  various special cases. For implicit Elements and for Elements that are
   --  components of expanded generic structure the first thing is to check if
   --  this Element can be processed as if it is a usual explicit Element, and
   --  then correct result, if needed.

   ---------------------------------------------------------------------
   -- Mapping the bottom-up traversing of the compiler tree onto ASIS --
   ---------------------------------------------------------------------

   --  Each ASIS Element contains the reference to the tree node it has been
   --  built from. In many cases the enclosing Element should be built on
   --  the parent node. In some cases the enclosing Element may be built on the
   --  same node. And there are some cases when we have to do some traversing
   --  that is specific to this particular Element to get to the compiler tree
   --  node corresponding to its enclosing Element.

   --  The way of getting the enclosing Element is implemented on the base of
   --  two look-up tables (switches). The first table defines if for the given
   --  element (that is, for the given Element kind, and the internal flat
   --  Element classification is used here) some regular way of constructing
   --  enclosing Element should be used, or some non-trivial traversing is
   --  needed. This non-trivial traversing is specific to the Element kind, and
   --  the corresponding routine is defined by the second look-up table.

   -------------------------------------------------
   --  The general structure of this package body --
   -------------------------------------------------

   --  The rest of this package body has the following structure:
   --
   --  Section 1 - definition of the first Enclosing_Element switch (makes
   --              the difference between trivial and non-trivial cases of
   --              mapping the bottom up compiler tree traversing onto ASIS
   --
   --  Section 2 - declarations of routines implementing various cases of
   --              non-trivial bottom up compiler tree traversing
   --
   --  Section 3 - definition of the second Enclosing_Element switch (maps
   --              Element kind requiring non-trivial actions onto
   --              corresponding routines
   --
   --  Section 4 - (general-purpose) local subprograms
   --
   --  Section 5 - bodies of the routines declared in Section 2
   --
   --  Section 6 - bodies of the routines declared in the package spec

   ---------------------------------------------------------------------
   --  Section 1 - Enclosing_Element first switch, separating trivial --
   --              and non-trivial cases                              --
   ---------------------------------------------------------------------

   --  This switch maps each value of the Internal_Element_Kinds onto one
   --  of the following values of the same type, and this mapping has the
   --  following meaning:

   --  Not_An_Element       => Asis.Nil_Element should be returned as
   --                          Enclosed Element;
   --
   --  Trivial_Mapping      => A standard Enclosing_Element constructor should
   --                          be used, it is implemented by General_Encl_Elem
   --                          function
   --
   --  No_Mapping           => is set for the special values added to the
   --                          Internal_Element_Kinds literals to organize the
   --                          Node_to_Element and Enclosing Element switches.
   --
   --  Non_Trivial_Mapping  => a special function is needed for this Element
   --                          kind to get the Enclosing Element. This function
   --                          is selected by second switch,
   --
   --  Not_Implemented_Mapping => it means what is sounds
   --
   --  any the other value => the Enclosing Element for the Element of the
   --                         corresponding kind is based on the same node, but
   --                         is of the specified kind

   Enclosing_Element_For_Explicits_First_Switch : constant
     array (Internal_Element_Kinds) of Internal_Element_Kinds :=
(
--   type Internal_Element_Kinds is (
--
Not_An_Element   => Not_An_Element,
--  Asis.Nil_Element should be returned as the
--  Enclosing for the Asis.Nil_Element, should not it???
--
------------------------------------------------------------------------------
--
--  --  A_Pragma,                  -- Asis.Elements
--
------------------------------------------------------------------------------
--
An_All_Calls_Remote_Pragma ..
--     An_Asynchronous_Pragma,
--     An_Atomic_Pragma,
--     An_Atomic_Components_Pragma,
--     An_Attach_Handler_Pragma,
--     A_Controlled_Pragma,
--     A_Convention_Pragma,
--     A_Discard_Names_Pragma,
--     An_Elaborate_Pragma,
--     An_Elaborate_All_Pragma,
--     An_Elaborate_Body_Pragma,
--     An_Export_Pragma,
--     An_Import_Pragma,
--     An_Inline_Pragma,
--     An_Inspection_Point_Pragma,
--     An_Interrupt_Handler_Pragma,
--     An_Interrupt_Priority_Pragma,
--     A_Linker_Options_Pragma
--     A_List_Pragma,
--     A_Locking_Policy_Pragma,
--     A_Normalize_Scalars_Pragma,
--     An_Optimize_Pragma,
--     A_Pack_Pragma,
--     A_Page_Pragma,
--     A_Preelaborate_Pragma,
--     A_Priority_Pragma,
--     A_Pure_Pragma,
--     A_Queuing_Policy_Pragma,
--     A_Remote_Call_Interface_Pragma,
--     A_Remote_Types_Pragma,
--     A_Restrictions_Pragma,
--     A_Reviewable_Pragma,
--     A_Shared_Passive_Pragma,
--     A_Storage_Size_Pragma,
--     A_Suppress_Pragma,
--     A_Task_Dispatching_Policy_Pragma,
--     A_Volatile_Pragma,
--     A_Volatile_Components_Pragma,
--
--     An_Implementation_Defined_Pragma,
--
An_Unknown_Pragma        => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--
--  --  A_Defining_Name,           -- Asis.Declarations
--
------------------------------------------------------------------------------
--
A_Defining_Identifier          => Non_Trivial_Mapping,
A_Defining_Character_Literal   => An_Enumeration_Literal_Specification,
A_Defining_Enumeration_Literal => An_Enumeration_Literal_Specification,
--
--  --  A_Defining_Operator_Symbol  => Non_Trivial_Mapping
--
A_Defining_And_Operator ..
--        A_Defining_Or_Operator,
--        A_Defining_Xor_Operator,
--        A_Defining_Equal_Operator,
--        A_Defining_Not_Equal_Operator,
--        A_Defining_Less_Than_Operator,
--        A_Defining_Less_Than_Or_Equal_Operator,
--        A_Defining_Greater_Than_Operator,
--        A_Defining_Greater_Than_Or_Equal_Operator,
--        A_Defining_Plus_Operator,
--        A_Defining_Minus_Operator,
--        A_Defining_Concatenate_Operator,
--        A_Defining_Unary_Plus_Operator,
--        A_Defining_Unary_Minus_Operator,
--        A_Defining_Multiply_Operator,
--        A_Defining_Divide_Operator,
--        A_Defining_Mod_Operator,
--        A_Defining_Rem_Operator,
--        A_Defining_Exponentiate_Operator,
--        A_Defining_Abs_Operator,
A_Defining_Not_Operator            => Non_Trivial_Mapping,

A_Defining_Expanded_Name           => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--
--  --  A_Declaration,             -- Asis.Declarations
--
------------------------------------------------------------------------------
--
An_Ordinary_Type_Declaration ..
--     A_Task_Type_Declaration,
--     A_Protected_Type_Declaration,
--     An_Incomplete_Type_Declaration,
--     A_Private_Type_Declaration,
--     A_Private_Extension_Declaration,
--     A_Subtype_Declaration,
A_Variable_Declaration                   => Trivial_Mapping,

A_Constant_Declaration                   => Trivial_Mapping,

A_Deferred_Constant_Declaration ..
--     A_Single_Task_Declaration,
--     A_Single_Protected_Declaration,
--
--     An_Integer_Number_Declaration,
A_Real_Number_Declaration                => Trivial_Mapping,
--
An_Enumeration_Literal_Specification     => Non_Trivial_Mapping,
--  is it really so?
--
A_Discriminant_Specification             => Non_Trivial_Mapping,

A_Component_Declaration                  => Non_Trivial_Mapping,

A_Loop_Parameter_Specification ..
--  A_Generalized_Iterator_Specification,
An_Element_Iterator_Specification        => Non_Trivial_Mapping,

A_Procedure_Declaration                  => Non_Trivial_Mapping,
A_Function_Declaration                   => Non_Trivial_Mapping,
--
A_Parameter_Specification                => Non_Trivial_Mapping,
--
A_Procedure_Body_Declaration             => Non_Trivial_Mapping,
A_Function_Body_Declaration              => Non_Trivial_Mapping,

A_Return_Variable_Specification          => Trivial_Mapping,
A_Return_Constant_Specification          => Trivial_Mapping,
A_Null_Procedure_Declaration             => Trivial_Mapping,
An_Expression_Function_Declaration       => Trivial_Mapping,

A_Package_Declaration                    => Non_Trivial_Mapping,
A_Package_Body_Declaration               => Non_Trivial_Mapping,

An_Object_Renaming_Declaration           => Trivial_Mapping,
An_Exception_Renaming_Declaration        => Trivial_Mapping,
A_Package_Renaming_Declaration           => Non_Trivial_Mapping,
A_Procedure_Renaming_Declaration         => Non_Trivial_Mapping,
A_Function_Renaming_Declaration          => Non_Trivial_Mapping,
A_Generic_Package_Renaming_Declaration   => Non_Trivial_Mapping,
A_Generic_Procedure_Renaming_Declaration => Non_Trivial_Mapping,
A_Generic_Function_Renaming_Declaration  => Non_Trivial_Mapping,

A_Task_Body_Declaration                  => Non_Trivial_Mapping,
A_Protected_Body_Declaration             => Non_Trivial_Mapping,

An_Entry_Declaration                     => Non_Trivial_Mapping,

An_Entry_Body_Declaration                => Trivial_Mapping,
An_Entry_Index_Specification             => Trivial_Mapping,

A_Procedure_Body_Stub                    => Trivial_Mapping,
A_Function_Body_Stub                     => Trivial_Mapping,
A_Package_Body_Stub                      => Trivial_Mapping,
A_Task_Body_Stub                         => Trivial_Mapping,
A_Protected_Body_Stub                    => Trivial_Mapping,

An_Exception_Declaration                 => Trivial_Mapping,
A_Choice_Parameter_Specification         => Trivial_Mapping,
--
A_Generic_Procedure_Declaration          => Non_Trivial_Mapping,
A_Generic_Function_Declaration           => Non_Trivial_Mapping,
A_Generic_Package_Declaration            => Non_Trivial_Mapping,

A_Package_Instantiation                  => Non_Trivial_Mapping,
A_Procedure_Instantiation                => Non_Trivial_Mapping,
A_Function_Instantiation                 => Non_Trivial_Mapping,

A_Formal_Object_Declaration              => Trivial_Mapping,
A_Formal_Type_Declaration                => Trivial_Mapping,
A_Formal_Incomplete_Type_Declaration     => Trivial_Mapping,
A_Formal_Procedure_Declaration           => Trivial_Mapping,
A_Formal_Function_Declaration            => Trivial_Mapping,
A_Formal_Package_Declaration             => Trivial_Mapping,
A_Formal_Package_Declaration_With_Box    => Trivial_Mapping,

------------------------------------------------------------------------------
--
--  --  A_Definition,              -- Asis.Definitions
--
------------------------------------------------------------------------------
--
--  --  A_Type_Definition,
--
A_Derived_Type_Definition               => Trivial_Mapping,
A_Derived_Record_Extension_Definition   => Trivial_Mapping,

--
An_Enumeration_Type_Definition          => Non_Trivial_Mapping,
--
A_Signed_Integer_Type_Definition        => Trivial_Mapping,
A_Modular_Type_Definition               => Trivial_Mapping,
--
--  --     A_Root_Type_Definition,             ----- #########
--
--           A_Root_Integer_Definition,       ----- #########
--           A_Root_Real_Definition,          ----- #########
--           A_Root_Fixed_Definition,         ----- #########
--
--           A_Universal_Integer_Definition,  ----- #########
--           A_Universal_Real_Definition,     ----- #########
--           A_Universal_Fixed_Definition,    ----- #########
--
--
A_Floating_Point_Definition             => Trivial_Mapping,
--
An_Ordinary_Fixed_Point_Definition      => Trivial_Mapping,
A_Decimal_Fixed_Point_Definition        => Trivial_Mapping,
--
An_Unconstrained_Array_Definition       => Trivial_Mapping,
A_Constrained_Array_Definition          => Trivial_Mapping,
--
A_Record_Type_Definition                => Trivial_Mapping, -- ???
A_Tagged_Record_Type_Definition         => Trivial_Mapping, -- ???

--  --|A2005 start
--      An_Interface_Type_Definition,

An_Ordinary_Interface ..
--   A_Limited_Interface,
--   A_Task_Interface,
--   A_Protected_Interface,
A_Synchronized_Interface                => Trivial_Mapping,
--  --|A2005 end

--  --     An_Access_Type_Definition,
--
A_Pool_Specific_Access_To_Variable      => Trivial_Mapping,
An_Access_To_Variable                   => Trivial_Mapping,
An_Access_To_Constant                   => Trivial_Mapping,
--
An_Access_To_Procedure                  => Trivial_Mapping,
An_Access_To_Protected_Procedure        => Trivial_Mapping,
An_Access_To_Function                   => Trivial_Mapping,
An_Access_To_Protected_Function         => Trivial_Mapping,
--
--
A_Subtype_Indication                    => Non_Trivial_Mapping,
--
--  --  A_Constraint,
--
A_Range_Attribute_Reference             => Non_Trivial_Mapping,    --  ???
A_Simple_Expression_Range               => Non_Trivial_Mapping,
A_Digits_Constraint                     => Trivial_Mapping,
A_Delta_Constraint                      => Trivial_Mapping,
An_Index_Constraint                     => Non_Trivial_Mapping,
A_Discriminant_Constraint               => Non_Trivial_Mapping,
--
A_Component_Definition                  => Trivial_Mapping,
--
--  --  A_Discrete_Subtype_Definition,
--
A_Discrete_Subtype_Indication_As_Subtype_Definition        => Trivial_Mapping,
A_Discrete_Range_Attribute_Reference_As_Subtype_Definition => Trivial_Mapping,
A_Discrete_Simple_Expression_Range_As_Subtype_Definition   => Trivial_Mapping,
--
--  --  A_Discrete_Range,
--
A_Discrete_Subtype_Indication        => Non_Trivial_Mapping,
A_Discrete_Range_Attribute_Reference => Non_Trivial_Mapping,
A_Discrete_Simple_Expression_Range   => Non_Trivial_Mapping,
--
--
An_Unknown_Discriminant_Part     => Non_Trivial_Mapping,
A_Known_Discriminant_Part        => Non_Trivial_Mapping,
--
A_Record_Definition              => Non_Trivial_Mapping,
A_Null_Record_Definition         => Non_Trivial_Mapping,
--
A_Null_Component                 => Non_Trivial_Mapping,
A_Variant_Part                   => Non_Trivial_Mapping,
A_Variant                        => Trivial_Mapping,
An_Others_Choice                 => Non_Trivial_Mapping,

--  --|A2005 start
An_Anonymous_Access_To_Variable ..
--    An_Anonymous_Access_To_Constant
--    An_Anonymous_Access_To_Procedure
--    An_Anonymous_Access_To_Protected_Procedure
--    An_Anonymous_Access_To_Function
An_Anonymous_Access_To_Protected_Function => Trivial_Mapping,
--  --|A2005 end

A_Private_Type_Definition        => A_Private_Type_Declaration,
A_Tagged_Private_Type_Definition => A_Private_Type_Declaration,
A_Private_Extension_Definition   => A_Private_Extension_Declaration,
--
A_Task_Definition                => Trivial_Mapping,
A_Protected_Definition           => Non_Trivial_Mapping,
--
--  --  A_Formal_Type_Definition,
--
A_Formal_Private_Type_Definition ..
--        A_Formal_Tagged_Private_Type_Definition,
--
--        A_Formal_Derived_Type_Definition,
--
--        A_Formal_Discrete_Type_Definition,
--
--        A_Formal_Signed_Integer_Type_Definition,
--        A_Formal_Modular_Type_Definition,
--
--        A_Formal_Floating_Point_Definition,
--
--        A_Formal_Ordinary_Fixed_Point_Definition,
--        A_Formal_Decimal_Fixed_Point_Definition,
--
--        A_Formal_Unconstrained_Array_Definition,
--        A_Formal_Constrained_Array_Definition,
--
--  --     A_Formal_Access_Type_Definition,
--
--           A_Formal_Pool_Specific_Access_To_Variable,
--           A_Formal_Access_To_Variable,
--           A_Formal_Access_To_Constant,
--
--           A_Formal_Access_To_Procedure,
--           A_Formal_Access_To_Protected_Procedure,
--           A_Formal_Access_To_Function,
--           A_Formal_Access_To_Protected_Function
An_Aspect_Specification => Trivial_Mapping,
--
------------------------------------------------------------------------------
--
--  --  An_Expression,             -- Asis.Expressions        --##########
--
------------------------------------------------------------------------------
--
An_Integer_Literal     => Non_Trivial_Mapping,
A_Real_Literal         => Non_Trivial_Mapping,

A_String_Literal       => Non_Trivial_Mapping,
An_Identifier          => Non_Trivial_Mapping,
--
--  --  An_Operator_Symbol,
--
An_And_Operator ..
--           An_Or_Operator,
--           An_Xor_Operator,
--           An_Equal_Operator,
--           A_Not_Equal_Operator,
--           A_Less_Than_Operator,
--           A_Less_Than_Or_Equal_Operator,
--           A_Greater_Than_Operator,
--           A_Greater_Than_Or_Equal_Operator,
--           A_Plus_Operator,
--           A_Minus_Operator,
--           A_Concatenate_Operator,
--           A_Unary_Plus_Operator,
--           A_Unary_Minus_Operator,
--           A_Multiply_Operator,
--           A_Divide_Operator,
--           A_Mod_Operator,
--           A_Rem_Operator,
--           An_Exponentiate_Operator,
--           An_Abs_Operator,
--   A_Not_Operator        => A_Function_Call,
A_Not_Operator        => Non_Trivial_Mapping,
--
--  A_Character_Literal ..
--  --     An_Enumeration_Literal,
--  An_Explicit_Dereference  => Trivial_Mapping,
--
--  A_Function_Call          => Non_Trivial_Mapping,
--  --
--  An_Indexed_Component  ..
--  A_Slice                  => Trivial_Mapping,
--  A_Selected_Component     => Non_Trivial_Mapping,
--  --
--  --     -- ??? Not_An_Attribute,
--  --  -- An_Attribute_Reference  => Non_Trivial_Mapping,
--  --
--  An_Access_Attribute ..
--  --          An_Address_Attribute,
--  --          An_Adjacent_Attribute,
--  --          An_Aft_Attribute,
--  --          An_Alignment_Attribute,
--  --          A_Base_Attribute,
--  --          A_Bit_Order_Attribute,
--  --          A_Body_Version_Attribute,
--  --          A_Callable_Attribute,
--  --          A_Caller_Attribute,
--  --          A_Ceiling_Attribute,
--  --          A_Class_Attribute,
--  --          A_Component_Size_Attribute,
--  --          A_Compose_Attribute,
--  --          A_Constrained_Attribute,
--  --          A_Copy_Sign_Attribute,
--  --          A_Count_Attribute,
--  --          A_Definite_Attribute,
--  --          A_Delta_Attribute,
--  --          A_Denorm_Attribute,
--  --          A_Digits_Attribute,
--  --          An_Exponent_Attribute,
--  --          An_External_Tag_Attribute,
--  --          A_First_Attribute,
--  --          A_First_Bit_Attribute,
--  --          A_Floor_Attribute,
--  --          A_Fore_Attribute,
--  --          A_Fraction_Attribute,
--  --          An_Identity_Attribute,
--  --          An_Image_Attribute,
--  --          An_Input_Attribute,
--  --          A_Last_Attribute,
--  --          A_Last_Bit_Attribute,
--  --          A_Leading_Part_Attribute,
--  --          A_Length_Attribute,
--  --          A_Machine_Attribute,
--  --          A_Machine_Emax_Attribute,
--  --          A_Machine_Emin_Attribute,
--  --          A_Machine_Mantissa_Attribute,
--  --          A_Machine_Overflows_Attribute,
--  --          A_Machine_Radix_Attribute,
--  --          A_Machine_Rounds_Attribute,
--  --          A_Max_Attribute,
--  --          A_Max_Size_In_Storage_Elements_Attribute,
--  --          A_Min_Attribute,
--  --          A_Model_Attribute,
--  --          A_Model_Emin_Attribute,
--  --          A_Model_Epsilon_Attribute,
--  --          A_Model_Mantissa_Attribute,
--  --          A_Model_Small_Attribute,
--  --          A_Modulus_Attribute,
--  --          An_Output_Attribute,
--  --          A_Partition_ID_Attribute,
--  --          A_Pos_Attribute,
--  --          A_Position_Attribute,
--  --          A_Pred_Attribute,
--  --          A_Range_Attribute,
--  --          A_Read_Attribute,
--  --          A_Remainder_Attribute,
--  --          A_Round_Attribute,
--  --          A_Rounding_Attribute,
--  --          A_Safe_First_Attribute,
--  --          A_Safe_Last_Attribute,
--  --          A_Scale_Attribute,
--  --          A_Scaling_Attribute,
--  --          A_Signed_Zeros_Attribute,
--  --          A_Size_Attribute,
--  --          A_Small_Attribute,
--  --          A_Storage_Pool_Attribute,
--  --          A_Storage_Size_Attribute,
--  --
--  --          A_Succ_Attribute,
--  --          A_Tag_Attribute,
--  --          A_Terminated_Attribute,
--  --          A_Truncation_Attribute,
--  --          An_Unbiased_Rounding_Attribute,
--  --          An_Unchecked_Access_Attribute,
--  --          A_Val_Attribute,
--  --          A_Valid_Attribute,
--  --          A_Value_Attribute,
--  --          A_Version_Attribute,
--  --          A_Wide_Image_Attribute,
--  --          A_Wide_Value_Attribute,
--  --          A_Wide_Width_Attribute,
--  --          A_Width_Attribute,
--  --          A_Write_Attribute,
--  --
--  --          An_Implementation_Defined_Attribute,  -- Vendor Annex M
--  An_Unknown_Attribute   => Non_Trivial_Mapping,
--
--  --     A_Record_Aggregate,
--  --     An_Extension_Aggregate,
--  --     A_Positional_Array_Aggregate,
--  --     A_Named_Array_Aggregate,
--  --
--  --     An_And_Then_Short_Circuit,
--  --     An_Or_Else_Short_Circuit,
--  --
--  --     An_In_Range_Membership_Test,
--  --     A_Not_In_Range_Membership_Test,
--  --     An_In_Type_Membership_Test,
--   --     A_Not_In_Type_Membership_Test,
--   --
--   --     A_Null_Literal,
--   --     A_Parenthesized_Expression,
--   --
--   --     A_Type_Conversion,
--   --     A_Qualified_Expression,
--   --
--   --     An_Allocation_From_Subtype,
--  An_Allocation_From_Qualified_Expression,
--  A_Case_Expression,                         -- Ada 2012
--  An_If_Expression,                          -- Ada 2012
--  A_For_All_Quantified_Expression,           -- Ada 2012
--  A_For_Some_Quantified_Expression);         -- Ada 2012

A_Character_Literal ..
A_For_Some_Quantified_Expression => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--
--  --  An_Association,            -- Asis.Expressions
--
------------------------------------------------------------------------------
--
A_Pragma_Argument_Association      => Trivial_Mapping,
A_Discriminant_Association         => Non_Trivial_Mapping,
A_Record_Component_Association     => Trivial_Mapping,
An_Array_Component_Association     => Non_Trivial_Mapping,

A_Parameter_Association            => Non_Trivial_Mapping,

A_Generic_Association              => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--
--  --  A_Statement,               -- Asis.Statements
--
--  All subordinates of A_Statement kind require non trivial processing,
--  this processing is the same for all of them except
--  A_Terminate_Alternative_Statement
------------------------------------------------------------------------------
--
A_Null_Statement ..
--     An_Assignment_Statement,
--     An_If_Statement,
--     A_Case_Statement,
--
--     A_Loop_Statement,
--     A_While_Loop_Statement,
--     A_For_Loop_Statement,
--
--     A_Block_Statement,
--     An_Exit_Statement,
--     A_Goto_Statement,
--
--     A_Procedure_Call_Statement,
--     A_Return_Statement,
--
--     An_Accept_Statement,
--     An_Entry_Call_Statement,
--
--     A_Requeue_Statement,
--     A_Requeue_Statement_With_Abort,
--
--     A_Delay_Until_Statement,
--     A_Delay_Relative_Statement,
--
--     A_Terminate_Alternative_Statement,
--     A_Selective_Accept_Statement,
--     A_Timed_Entry_Call_Statement,
--     A_Conditional_Entry_Call_Statement,
--     An_Asynchronous_Select_Statement,
--
--     An_Abort_Statement,
--     A_Raise_Statement,
A_Code_Statement               => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--  Path_Kinds
--  Literals                        -- Ada RM 95
--
--  Detailed classification for
--  ASIS_Element_Kinds.Element_Kinds(A_Path) literal
--  corresponds to subtype Internal_Path_Kinds
------------------------------------------------------------------------------

An_If_Path        => An_If_Statement,
An_Elsif_Path     => Trivial_Mapping,
An_Else_Path      => Non_Trivial_Mapping,
A_Case_Path       => Trivial_Mapping,
A_Select_Path     => Trivial_Mapping,
An_Or_Path        => Trivial_Mapping,
A_Then_Abort_Path => Trivial_Mapping,
--
------------------------------------------------------------
--  An_Expression_Path,        -- Asis.Expressions  Ada 2015

--  Detailed classification for Asis.Element_Kinds (An_Expression_Path)
--  literal corresponds to subtype Internal_Expression_Path_Kinds
------------------------------------------------------------

An_If_Expression_Path ..
--    An_Elsif_Expression_Path,
An_Else_Expression_Path => Non_Trivial_Mapping,
------------------------------------------------------------------------
--
--  -- A_Clause,                  -- Asis.Clauses
--
------------------------------------------------------------------------------
--
A_Use_Package_Clause  => Non_Trivial_Mapping,
A_Use_Type_Clause     => Non_Trivial_Mapping,
A_Use_All_Type_Clause => Non_Trivial_Mapping,
A_With_Clause         => Not_An_Element,
--
--  --  A_Representation_Clause,
--
An_Attribute_Definition_Clause       => Non_Trivial_Mapping,
An_Enumeration_Representation_Clause => Trivial_Mapping,
A_Record_Representation_Clause       => Trivial_Mapping,
An_At_Clause                         => Trivial_Mapping,
--
--
A_Component_Clause                   => Trivial_Mapping,
--
------------------------------------------------------------------------------
--
An_Exception_Handler      => Non_Trivial_Mapping,
--
------------------------------------------------------------------------------
--  Special values added for Node -> Element and
--  Element -> Enclosing Element switching,
------------------------------------------------------------------------------

Non_Trivial_Mapping     => No_Mapping,
Not_Implemented_Mapping => No_Mapping,
Trivial_Mapping         => No_Mapping,
No_Mapping              => No_Mapping,

others                  => Not_Implemented_Mapping
   );

   -------------------------------------------------------------------------
   --  Section 2 - declarations of routines implementing various cases of --
   --              non-trivial bottom up compiler tree traversing and     --
   --              accessed though the second switch                      --
   -------------------------------------------------------------------------

   function Not_Implemented_Enclosing_Element_Construction
       (Element : Asis.Element) return Asis.Element;
   --  Placeholders for "others" choice

   --  The functions below computes Enclosing_Elememnt for specific Element
   --  kinds; the corresponding situations cannot be covered by
   --  General_Encl_Elem

   function A_Pragma_Enclosing (Element : Asis.Element) return Asis.Element;

   function A_Defining_Expanded_Name_Enclosing
     (Element : Asis.Element)
      return    Asis.Element;

   function A_Defining_Identifier_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Defining_Operator_Symbol_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Constant_Declaration_Enclosing
     (Element : Asis.Element)
      return    Asis.Element;

   function An_Enumeration_Literal_Specification_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Discriminant_Specification_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Loop_Parameter_Specification_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Parameter_Specification_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function An_Enumeration_Type_Definition_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Subtype_Indication_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Range_Attribute_Reference_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Simple_Expression_Range_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Discrete_Range_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Discriminant_Part_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Record_Definition_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Null_Component_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Variant_Part_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function An_Others_Choice_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function A_Statement_Enclosing
     (Element : Asis.Element)
      return Asis.Element;
   function A_Terminate_Alternative_Statement_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function An_Else_Path_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function An_Attribute_Definition_Clause_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function An_Exception_Handler_Enclosing
     (Element : Asis.Element)
      return Asis.Element;

   function Possible_C_U_Enclosing
     (Element : Asis.Element)
      return Asis.Element;
   --  Called in a situation when Enclosing_Element may have to be Nil_Element,
   --  because we may reach the very top of the Element hierarchy of an ASIS
   --  Compilation_Unit, so logically the next step up should be from Elements
   --  into enclosing unit.

   function An_Association_Enclosing
     (Element : Asis.Element)
      return Asis.Element;
   --  Computes the Enclosing Element for parameter associations. The main
   --  difference with An_Expression_Enclosing is that here we may have to deal
   --  with normalized associations

   function An_Expression_Enclosing
     (Element : Asis.Element)
      return Asis.Element;
   --  This function implements the part of the semantic of the
   --  Asis.Elements.Enclosing_Element function corresponding to the
   --  enclosing element retrieving for elements representing Ada explicit
   --  constructs. It deals only with expressions - the hardest part
   --  for Enclosing_Element.

   --------------------------------------------------------------------
   --  Section 3 - definition of the second Enclosing_Element switch --
   --              (maps Element kind requiring non-trivial actions --
   --              onto corresponding routines                       --
   --------------------------------------------------------------------
   type Enclosing_Element_Construction_For_Explicits_Items is access
     function (Element : Asis.Element) return Asis.Element;
   --  access to the local items of the constructing the Enclosing Elements
   --  for Explicit constructs

   Enclosing_Element_For_Explicits_Second_Switch : constant
      array (Internal_Element_Kinds)
         of Enclosing_Element_Construction_For_Explicits_Items :=
(

--  type Internal_Element_Kinds is (
--
--    Not_An_Element,            -- Asis.Nil_Element
--
------------------------------------------------------------------------------

--  A_Pragma,                  -- Asis.Elements

------------------------------------------------------------------------------

An_All_Calls_Remote_Pragma ..
--     An_Asynchronous_Pragma,
--     An_Atomic_Pragma,
--     An_Atomic_Components_Pragma,
--     An_Attach_Handler_Pragma,
--     A_Controlled_Pragma,
--     A_Convention_Pragma,
--     A_Discard_Names_Pragma,
--     An_Elaborate_Pragma,
--     An_Elaborate_All_Pragma,
--     An_Elaborate_Body_Pragma,
--     An_Export_Pragma,
--     An_Import_Pragma,
--     An_Inline_Pragma,
--     An_Inspection_Point_Pragma,
--     An_Interrupt_Handler_Pragma,
--     An_Interrupt_Priority_Pragma,
--     A_Linker_Options_Pragma
--     A_List_Pragma,
--     A_Locking_Policy_Pragma,
--     A_Normalize_Scalars_Pragma,
--     An_Optimize_Pragma,
--     A_Pack_Pragma,
--     A_Page_Pragma,
--     A_Preelaborate_Pragma,
--     A_Priority_Pragma,
--     A_Pure_Pragma,
--     A_Queuing_Policy_Pragma,
--     A_Remote_Call_Interface_Pragma,
--     A_Remote_Types_Pragma,
--     A_Restrictions_Pragma,
--     A_Reviewable_Pragma,
--     A_Shared_Passive_Pragma,
--     A_Storage_Size_Pragma,
--     A_Suppress_Pragma,
--     A_Task_Dispatching_Policy_Pragma,
--     A_Volatile_Pragma,
--     A_Volatile_Components_Pragma,
--
--     An_Implementation_Defined_Pragma,
--
An_Unknown_Pragma                => A_Pragma_Enclosing'Access,

------------------------------------------------------------------------------

--  A_Defining_Name,           -- Asis.Declarations

------------------------------------------------------------------------------

A_Defining_Identifier            => A_Defining_Identifier_Enclosing'Access,

--    A_Defining_Character_Literal,    -- an Enclosing Element is based
--    A_Defining_Enumeration_Literal,  -- on the same Node
--
--  --  A_Defining_Operator_Symbol
--
A_Defining_And_Operator ..
--       A_Defining_Or_Operator,
--       A_Defining_Xor_Operator,
--       A_Defining_Equal_Operator,
--       A_Defining_Not_Equal_Operator,
--       A_Defining_Less_Than_Operator,
--       A_Defining_Less_Than_Or_Equal_Operator,
--       A_Defining_Greater_Than_Operator,
--       A_Defining_Greater_Than_Or_Equal_Operator,
--       A_Defining_Plus_Operator,
--       A_Defining_Minus_Operator,
--       A_Defining_Concatenate_Operator,
--       A_Defining_Unary_Plus_Operator,
--       A_Defining_Unary_Minus_Operator,
--       A_Defining_Multiply_Operator,
--       A_Defining_Divide_Operator,
--       A_Defining_Mod_Operator,
--       A_Defining_Rem_Operator,
--       A_Defining_Exponentiate_Operator,
--       A_Defining_Abs_Operator,
A_Defining_Not_Operator         => A_Defining_Operator_Symbol_Enclosing'Access,
A_Defining_Expanded_Name        => A_Defining_Expanded_Name_Enclosing'Access,
--
-------------------------------------------------------------------------------
--
--  --  A_Declaration,             -- Asis.Declarations
--
-------------------------------------------------------------------------------
--
--    An_Ordinary_Type_Declaration,              -- 3.2.1
--    A_Task_Type_Declaration,                   -- 3.2.1
--    A_Protected_Type_Declaration,              -- 3.2.1
--    An_Incomplete_Type_Declaration,            -- 3.2.1
--    A_Private_Type_Declaration,                -- 3.2.1
--    A_Private_Extension_Declaration,           -- 3.2.1
--
--    A_Subtype_Declaration,                     -- 3.2.2
--
--    A_Variable_Declaration,                    -- 3.3.1 -> Trait_Kinds

A_Constant_Declaration            => A_Constant_Declaration_Enclosing'Access,
--  This is turned off, see G416-009

--    A_Deferred_Constant_Declaration,           -- 3.3.1 -> Trait_Kinds
--    A_Single_Task_Declaration,                 -- 3.3.1
--    A_Single_Protected_Declaration,            -- 3.3.1
--
--    An_Integer_Number_Declaration,             -- 3.3.2
--    A_Real_Number_Declaration,                 -- 3.3.2
--
An_Enumeration_Literal_Specification =>
   An_Enumeration_Literal_Specification_Enclosing'Access,
--

A_Discriminant_Specification => A_Discriminant_Specification_Enclosing'Access,
--
--  A_Component_Declaration      => A_Component_Declaration_Enclosing'Access,
A_Component_Declaration      => An_Expression_Enclosing'Access,
--
A_Loop_Parameter_Specification =>
                         A_Loop_Parameter_Specification_Enclosing'Access,

A_Generalized_Iterator_Specification ..
An_Element_Iterator_Specification =>
                         A_Loop_Parameter_Specification_Enclosing'Access,

--
A_Procedure_Declaration       => Possible_C_U_Enclosing'Access,
A_Function_Declaration        => Possible_C_U_Enclosing'Access,
--
A_Parameter_Specification     => A_Parameter_Specification_Enclosing'Access,

A_Procedure_Body_Declaration  => Possible_C_U_Enclosing'Access,
A_Function_Body_Declaration   => Possible_C_U_Enclosing'Access,
--
A_Package_Declaration         => Possible_C_U_Enclosing'Access,
A_Package_Body_Declaration    => Possible_C_U_Enclosing'Access,
--
--    An_Object_Renaming_Declaration,            -- 8.5.1
--    An_Exception_Renaming_Declaration,         -- 8.5.2

A_Package_Renaming_Declaration           => Possible_C_U_Enclosing'Access,
A_Procedure_Renaming_Declaration         => Possible_C_U_Enclosing'Access,
A_Function_Renaming_Declaration          => Possible_C_U_Enclosing'Access,
A_Generic_Package_Renaming_Declaration   => Possible_C_U_Enclosing'Access,
A_Generic_Procedure_Renaming_Declaration => Possible_C_U_Enclosing'Access,
A_Generic_Function_Renaming_Declaration  => Possible_C_U_Enclosing'Access,

A_Task_Body_Declaration                  => Possible_C_U_Enclosing'Access,
A_Protected_Body_Declaration             => Possible_C_U_Enclosing'Access,
--
An_Entry_Declaration                     => An_Expression_Enclosing'Access,
--  for entry declarations, the problem is for single task declarations
--  rewritten as anonymous task type declaration and task object declaration,
--  that's why we have to use An_Expression_Enclosing

--    An_Entry_Body_Declaration,                 -- 9.5.2
--    An_Entry_Index_Specification,              -- 9.5.2
--
--    A_Procedure_Body_Stub,                     -- 10.1.3
--    A_Function_Body_Stub,                      -- 10.1.3
--    A_Package_Body_Stub,                       -- 10.1.3
--    A_Task_Body_Stub,                          -- 10.1.3
--    A_Protected_Body_Stub,                     -- 10.1.3
--
--    An_Exception_Declaration,                  -- 11.1
--    A_Choice_Parameter_Specification,          -- 11.2
--
A_Generic_Procedure_Declaration => Possible_C_U_Enclosing'Access,
A_Generic_Function_Declaration  => Possible_C_U_Enclosing'Access,
A_Generic_Package_Declaration   => Possible_C_U_Enclosing'Access,

A_Package_Instantiation         => Possible_C_U_Enclosing'Access,
A_Procedure_Instantiation       => Possible_C_U_Enclosing'Access,
A_Function_Instantiation        => Possible_C_U_Enclosing'Access,
--
--    A_Formal_Object_Declaration,               -- 12.4  -> Mode_Kinds
--
--    A_Formal_Type_Declaration,                 -- 12.5
--    A_Formal_Procedure_Declaration,            -- 12.6  -> Default_Kinds
--
--    A_Formal_Function_Declaration,             -- 12.6  -> Default_Kinds
--
--    A_Formal_Package_Declaration,              -- 12.7
--    A_Formal_Package_Declaration_With_Box,     -- 12.7
--
-------------------------------------------------------------------------------
--
--  --  A_Definition,              -- Asis.Definitions
--
-------------------------------------------------------------------------------
--
--  --  A_Type_Definition,                -- 3.2.1   -> Type_Kinds
--
--       A_Derived_Type_Definition,             -- 3.4    -> Trait_Kinds
--       A_Derived_Record_Extension_Definition, -- 3.4    -> Trait_Kinds
--

An_Enumeration_Type_Definition =>
   An_Enumeration_Type_Definition_Enclosing'Access,

--
--       A_Signed_Integer_Type_Definition,      -- 3.5.4
--       A_Modular_Type_Definition,             -- 3.5.4
--
--  --     A_Root_Type_Definition,                -- 3.5.4(10), 3.5.6(4)
--                                              --        -> Root_Type_Kinds
--          A_Root_Integer_Definition,             -- 3.5.4(9)
--          A_Root_Real_Definition,                -- 3.5.6(2)
--          A_Root_Fixed_Definition,               -- 3.5.6(2)
--
--          A_Universal_Integer_Definition,        -- 3.5.4(10)
--          A_Universal_Real_Definition,           -- 3.5.6(4)
--          A_Universal_Fixed_Definition,          -- 3.5.6(4)
--
--
--       A_Floating_Point_Definition,           -- 3.5.7
--
--       An_Ordinary_Fixed_Point_Definition,    -- 3.5.9
--       A_Decimal_Fixed_Point_Definition,      -- 3.5.9
--
--       An_Unconstrained_Array_Definition,     -- 3.6
--       A_Constrained_Array_Definition,        -- 3.6
--
--       A_Record_Type_Definition,              -- 3.8    -> Trait_Kinds
--       A_Tagged_Record_Type_Definition,       -- 3.8    -> Trait_Kinds
--
--  --     An_Access_Type_Definition,           -- 3.10   -> Access_Type_Kinds
--
--          A_Pool_Specific_Access_To_Variable,
--          An_Access_To_Variable,
--          An_Access_To_Constant,
--
--          An_Access_To_Procedure,
--          An_Access_To_Protected_Procedure,
--          An_Access_To_Function,
--          An_Access_To_Protected_Function,
--
--
A_Subtype_Indication      => A_Subtype_Indication_Enclosing'Access,
--
--  --  A_Constraint,                     -- 3.2.2   -> Constraint_Kinds
--
A_Range_Attribute_Reference => A_Range_Attribute_Reference_Enclosing'Access,

A_Simple_Expression_Range => A_Simple_Expression_Range_Enclosing'Access,

--       A_Digits_Constraint,                   -- 3.2.2, 3.5.9
--       A_Delta_Constraint,                    -- 3.2.2, N.3

--  An_Index_Constraint      => An_Index_Constraint_Enclosing'Access,
An_Index_Constraint      => An_Expression_Enclosing'Access,

A_Discriminant_Constraint => An_Expression_Enclosing'Access,
--
--    A_Component_Definition,           -- 3.6
--
--  --  A_Discrete_Subtype_Definition,    -- 3.6     -> Discrete_Range_Kinds
--
--       A_Discrete_Subtype_Indication_As_Subtype_Definition,
--       A_Discrete_Range_Attribute_Reference_As_Subtype_Definition,
--       A_Discrete_Simple_Expression_Range_As_Subtype_Definition,
--
--  --  A_Discrete_Range,                 -- 3.6.1   -> Discrete_Range_Kinds
--
A_Discrete_Subtype_Indication        => A_Discrete_Range_Enclosing'Access,
A_Discrete_Range_Attribute_Reference => A_Discrete_Range_Enclosing'Access,
A_Discrete_Simple_Expression_Range   => A_Discrete_Range_Enclosing'Access,
--
--
An_Unknown_Discriminant_Part         => A_Discriminant_Part_Enclosing'Access,
A_Known_Discriminant_Part            => A_Discriminant_Part_Enclosing'Access,
--
A_Record_Definition                  => A_Record_Definition_Enclosing'Access,
A_Null_Record_Definition             => A_Record_Definition_Enclosing'Access,
--
A_Null_Component                     => A_Null_Component_Enclosing'Access,
A_Variant_Part                       => A_Variant_Part_Enclosing'Access,
--    A_Variant,                        -- 3.8
--
An_Others_Choice                     => An_Others_Choice_Enclosing'Access,
--    A_Private_Type_Definition,        -- 7.3     -> Trait_Kinds
--    A_Tagged_Private_Type_Definition, -- 7.3     -> Trait_Kinds
--    A_Private_Extension_Definition,   -- 7.3     -> Trait_Kinds
--
--    A_Task_Definition,                -- 9.1

A_Protected_Definition               => An_Expression_Enclosing'Access,

--
--  --  A_Formal_Type_Definition,         -- 12.5    -> Formal_Type_Kinds
--
--       A_Formal_Private_Type_Definition,         -- 12.5.1  -> Trait_Kinds
--       A_Formal_Tagged_Private_Type_Definition,  -- 12.5.1  -> Trait_Kinds
--
--       A_Formal_Derived_Type_Definition,         -- 12.5.1  -> Trait_Kinds
--
--       A_Formal_Discrete_Type_Definition,        -- 12.5.2
--
--       A_Formal_Signed_Integer_Type_Definition,  -- 12.5.2
--       A_Formal_Modular_Type_Definition,         -- 12.5.2
--
--       A_Formal_Floating_Point_Definition,       -- 12.5.2
--
--       A_Formal_Ordinary_Fixed_Point_Definition, -- 12.5.2
--       A_Formal_Decimal_Fixed_Point_Definition,  -- 12.5.2
--
--       A_Formal_Unconstrained_Array_Definition,  -- 12.5.3
--       A_Formal_Constrained_Array_Definition,    -- 12.5.3
--
--  --     A_Formal_Access_Type_Definition,
--
--          A_Formal_Pool_Specific_Access_To_Variable,
--          A_Formal_Access_To_Variable,
--          A_Formal_Access_To_Constant,
--
--          A_Formal_Access_To_Procedure,
--          A_Formal_Access_To_Protected_Procedure,
--          A_Formal_Access_To_Function,
--          A_Formal_Access_To_Protected_Function,
--
-------------------------------------------------------------------------------
--
--  --  An_Expression,             -- Asis.Expressions
--
-------------------------------------------------------------------------------
--  --
An_Integer_Literal ..
--  --    A_Real_Literal,                            -- 2.4.1
--  A_String_Literal      => A_Literal_Enclosing'Access,
--  --
An_Identifier    => An_Expression_Enclosing'Access,
--  An_Identifier    => An_Identifier_Enclosing'Access,
--  --
--  ----  An_Operator_Symbol,                        -- 4.1
--  --
An_And_Operator ..
--  --          An_Or_Operator,                      -- or
--  --          An_Xor_Operator,                      -- xor
--  --          An_Equal_Operator,                   -- =
--  --          A_Not_Equal_Operator,                -- /=
--  --          A_Less_Than_Operator,                -- <
--  --          A_Less_Than_Or_Equal_Operator,       -- <=
--  --          A_Greater_Than_Operator,             -- >
--  --          A_Greater_Than_Or_Equal_Operator,    -- >=
--  --          A_Plus_Operator,                     -- +
--  --          A_Minus_Operator,                    -- -
--  --          A_Concatenate_Operator,              -- &
--  --          A_Unary_Plus_Operator,               -- +
--  --          A_Unary_Minus_Operator,              -- -
--  --          A_Multiply_Operator,                 -- *
--  --          A_Divide_Operator,                   -- /
--  --          A_Mod_Operator,                      -- mod
--  --          A_Rem_Operator,                      -- rem
--  --          An_Exponentiate_Operator,            -- **
--  --          An_Abs_Operator,                     -- abs
--  A_Not_Operator => An_Operator_Symbol_Enclosing'Access,
--  ??? Do we need An_Operator_Symbol_Enclosing???

A_Not_Operator => An_Expression_Enclosing'Access,
--  --
A_Character_Literal ..
--  --    An_Enumeration_Literal,                    -- 4.1
--  --    An_Explicit_Dereference,                   -- 4.1
--
--  A_Function_Call    => A_Function_Call_Enclosing'Access,
--  --
--  --    An_Indexed_Component,                      -- 4.1.1
--  --    A_Slice,                                   -- 4.1.2
--  A_Selected_Component  => An_Identifier_Enclosing'Access,
--  --
--  --    An_Attribute_Reference,  -- 4.1.4  -> Attribute_Kinds
--  --
--  An_Access_Attribute ..
--  --          An_Address_Attribute,
--  --          An_Adjacent_Attribute,
--  --          An_Aft_Attribute,
--  --          An_Alignment_Attribute,
--  --          A_Base_Attribute,
--  --          A_Bit_Order_Attribute,
--  --          A_Body_Version_Attribute,
--  --          A_Callable_Attribute,
--  --          A_Caller_Attribute,
--  --          A_Ceiling_Attribute,
--  --          A_Class_Attribute,
--  --          A_Component_Size_Attribute,
--  --          A_Compose_Attribute,
--  --          A_Constrained_Attribute,
--  --          A_Copy_Sign_Attribute,
--  --          A_Count_Attribute,
--  --          A_Definite_Attribute,
--  --          A_Delta_Attribute,
--  --          A_Denorm_Attribute,
--  --          A_Digits_Attribute,
--  --          An_Exponent_Attribute,
--  --          An_External_Tag_Attribute,
--  --          A_First_Attribute,
--  --          A_First_Bit_Attribute,
--  --          A_Floor_Attribute,
--  --          A_Fore_Attribute,
--  --          A_Fraction_Attribute,
--  --          An_Identity_Attribute,
--  --          An_Image_Attribute,
--  --          An_Input_Attribute,
--  --          A_Last_Attribute,
--  --          A_Last_Bit_Attribute,
--  --          A_Leading_Part_Attribute,
--  --          A_Length_Attribute,
--  --          A_Machine_Attribute,
--  --          A_Machine_Emax_Attribute,
--  --          A_Machine_Emin_Attribute,
--  --          A_Machine_Mantissa_Attribute,
--  --          A_Machine_Overflows_Attribute,
--  --          A_Machine_Radix_Attribute,
--  --          A_Machine_Rounds_Attribute,
--  --          A_Max_Attribute,
--  --          A_Max_Size_In_Storage_Elements_Attribute,
--  --          A_Min_Attribute,
--  --          A_Model_Attribute,
--  --          A_Model_Emin_Attribute,
--  --          A_Model_Epsilon_Attribute,
--  --          A_Model_Mantissa_Attribute,
--  --          A_Model_Small_Attribute,
--  --          A_Modulus_Attribute,
--  --          An_Output_Attribute,
--  --          A_Partition_ID_Attribute,
--  --          A_Pos_Attribute,
--  --          A_Position_Attribute,
--  A_Pred_Attribute  => An_Attribute_Reference_Enclosing'Access,
--
--  A_Range_Attribute => A_Range_Attribute_Enclosing'Access,
--
--  A_Read_Attribute ..
--  --          A_Remainder_Attribute,
--  --          A_Round_Attribute,
--  --          A_Rounding_Attribute,
--  --          A_Safe_First_Attribute,
--  --          A_Safe_Last_Attribute,
--  --          A_Scale_Attribute,
--  --          A_Scaling_Attribute,
--  --          A_Signed_Zeros_Attribute,
--  --          A_Size_Attribute,
--  --          A_Small_Attribute,
--  --          A_Storage_Pool_Attribute,
--  --          A_Storage_Size_Attribute,
--  --
--  --          A_Succ_Attribute,
--  --          A_Tag_Attribute,
--  --          A_Terminated_Attribute,
--  --          A_Truncation_Attribute,
--  --          An_Unbiased_Rounding_Attribute,
--  --          An_Unchecked_Access_Attribute,
--  --          A_Val_Attribute,
--  --          A_Valid_Attribute,
--  --          A_Value_Attribute,
--  --          A_Version_Attribute,
--  --          A_Wide_Image_Attribute,
--  --          A_Wide_Value_Attribute,
--  --          A_Wide_Width_Attribute,
--  --          A_Width_Attribute,
--  --          A_Write_Attribute,
--  --
--  --          An_Implementation_Defined_Attribute,  -- Vendor Annex M
--  An_Unknown_Attribute     => An_Attribute_Reference_Enclosing'Access,
--  --
--  --    A_Record_Aggregate,                        -- 4.3
--  --    An_Extension_Aggregate,                    -- 4.3
--  --    A_Positional_Array_Aggregate,              -- 4.3
--  --    A_Named_Array_Aggregate,                   -- 4.3
--  --
--  --    An_And_Then_Short_Circuit,                 -- 4.4
--  --    An_Or_Else_Short_Circuit,                  -- 4.4
--  --
--  --    An_In_Range_Membership_Test,               -- 4.4
--  --    A_Not_In_Range_Membership_Test,            -- 4.4
--  --    An_In_Type_Membership_Test,                -- 4.4
--  --    A_Not_In_Type_Membership_Test,             -- 4.4
--  --
--  --    A_Null_Literal,                            -- 4.4
--  --    A_Parenthesized_Expression,                -- 4.4
--  --
--  --    A_Type_Conversion,                         -- 4.6
--  --    A_Qualified_Expression,                    -- 4.7
--  --
--  --    An_Allocation_From_Subtype,                -- 4.8
--  --    An_Allocation_From_Qualified_Expression,   -- 4.8
--        A_Case_Expression,                         -- Ada 2012
--        An_If_Expression,                          -- Ada 2012
--        A_For_All_Quantified_Expression,           -- Ada 2012
--        A_For_Some_Quantified_Expression);         -- Ada 2012

A_For_Some_Quantified_Expression => An_Expression_Enclosing'Access,
-------------------------------------------------------------------------------
--
--  --  An_Association,            -- Asis.Expressions
--
-------------------------------------------------------------------------------
--
--    A_Pragma_Argument_Association,         -- 2.8

A_Discriminant_Association => An_Expression_Enclosing'Access,

--    A_Record_Component_Association,        -- 4.3.1

An_Array_Component_Association => An_Expression_Enclosing'Access,

A_Parameter_Association .. A_Generic_Association  =>
   An_Association_Enclosing'Access,
--
-------------------------------------------------------------------------------
--
--  --  A_Statement,               -- Asis.Statements
--
-------------------------------------------------------------------------------
--
A_Null_Statement  ..
--    An_Assignment_Statement,             -- 5.2
--    An_If_Statement,                     -- 5.3
--    A_Case_Statement,                    -- 5.4
--
--    A_Loop_Statement,                    -- 5.5
--    A_While_Loop_Statement,              -- 5.5
--    A_For_Loop_Statement,                -- 5.5
--
--    A_Block_Statement,                   -- 5.6
--    An_Exit_Statement,                   -- 5.7
--    A_Goto_Statement,                    -- 5.8
--
--    A_Procedure_Call_Statement,          -- 6.4
--    A_Return_Statement,                  -- 6.5
--
--    An_Accept_Statement,                 -- 9.5.2
--    An_Entry_Call_Statement,             -- 9.5.3
--
--    A_Requeue_Statement,                 -- 9.5.4
--    A_Requeue_Statement_With_Abort,      -- 9.5.4
--
--    A_Delay_Until_Statement,             -- 9.6
A_Delay_Relative_Statement        => A_Statement_Enclosing'Access,
--
A_Terminate_Alternative_Statement =>
                           A_Terminate_Alternative_Statement_Enclosing'Access,
--
A_Selective_Accept_Statement   ..
--    A_Timed_Entry_Call_Statement,        -- 9.7.3
--    A_Conditional_Entry_Call_Statement,  -- 9.7.3
--    An_Asynchronous_Select_Statement,    -- 9.7.4
--
--    An_Abort_Statement,                  -- 9.8
--    A_Raise_Statement,                   -- 11.3
A_Code_Statement                  => A_Statement_Enclosing'Access,
--
-------------------------------------------------------------------------------
--  Path_Kinds
--  Literals                        -- RM 95
------------------------------------------------------------------------------
--
--    An_If_Path,
--    An_Elsif_Path,
--
An_Else_Path   => An_Else_Path_Enclosing'Access,
--
--    A_Case_Path,
--              -- when discrete_choice_list =>
--              --   sequence_of_statements
--
--    A_Select_Path,
--              -- select [guard] select_alternative
--              -- 9.7.2, 9.7.3:
--              -- select entry_call_alternative
--              -- 9.7.4:
--              -- select triggering_alternative
--
--    An_Or_Path,
--              -- or [guard] select_alternative  9.7.2:
--              -- or delay_alternative
--
--    A_Then_Abort_Path,        -- 9.7.4
--                                    -- then abort sequence_of_statements
--
--
------------------------------------------------------------
--  An_Expression_Path,        -- Asis.Expressions  Ada 2015
------------------------------------------------------------

An_If_Expression_Path ..
--    An_Elsif_Expression_Path,
An_Else_Expression_Path => An_Expression_Enclosing'Access,
-------------------------------------------------------------------------------
--
--  -- A_Clause,                  -- Asis.Clauses
--
-------------------------------------------------------------------------------
--
    A_Use_Package_Clause  => Possible_C_U_Enclosing'Access,  -- 8.4
    A_Use_Type_Clause     => Possible_C_U_Enclosing'Access,  -- 8.4
    A_Use_All_Type_Clause => Possible_C_U_Enclosing'Access,  -- 8.4 Ada 2012
--
--    A_With_Clause,                  -- 10.1.2
--
--  --  A_Representation_Clause,        -- 13.1 -> Representation_Clause_Kinds
--
An_Attribute_Definition_Clause =>
                               An_Attribute_Definition_Clause_Enclosing'Access,
--       An_Enumeration_Representation_Clause,     -- 13.4
--       A_Record_Representation_Clause,           -- 13.5.3
--       An_At_Clause,                             -- N.7
--
--
--    A_Component_Clause,             -- 13.5.3
--
-------------------------------------------------------------------------------
--
An_Exception_Handler   => An_Exception_Handler_Enclosing'Access,
--
-------------------------------------------------------------------------------
--  -- Special values added for Node -> Element switching,
--  -- see Asis_Vendor_Primitives.GNAT_to_Asis_Mapping body for
--  -- more details
-------------------------------------------------------------------------------
--
--    Non_Trivial_Mapping,
--    Not_Implemented_Mapping,
--    No_Mapping
--
others => Not_Implemented_Enclosing_Element_Construction'Access);

   ------------------------------------------------------
   --  Section 4 - (general-purpose) local subprograms --
   ------------------------------------------------------

   procedure Skip_Implicit_Subtype (Constr : in out Node_Id);
   --  Supposing that Constr is a constraint, this procedure checks if the
   --  parent node for it points to implicit subtype created in case if
   --  this constraint is used directly in object declaration, and if
   --  so, resets Constr to point to the constraint from the object
   --  declaration

   function Parent (Node : Node_Id) return Node_Id;
   --  this function is the modification of Atree.Parent. It is able
   --  to deal in the "ASIS mode" with the sequences of one-identifier
   --  declarations/with clauses resulting from the normalization of
   --  multi-name declarations/with clauses which is done by the
   --  compiler

   function General_Encl_Elem (Element : Asis.Element) return Asis.Element;
   --  Computes Enclosing_Element for most common cases

   procedure No_Enclosing_Element (Element_Kind : Internal_Element_Kinds);
   --  Should be called only in erroneous situations, when no Enclosing_Element
   --  can correspond to a given Element. Raises ASIS_Failed with the
   --  corresponding Diagnosis

   procedure Not_Implemented_Enclosing_Element_Construction
     (Element : Asis.Element);
   --  Generates Element-specific diagnosis about non-implemented case

   function Is_Top_Of_Expanded_Generic (N : Node_Id) return Boolean;
   --  Checks if N is the top node of the tree structure corresponding to
   --  expanded generic spec or body

   function Get_Rough_Enclosing_Node (Element : Asis.Element) return Node_Id;
   --  This function finds the node, which is the base for a "rough"
   --  enclosing element for the argument Element. Starting from the
   --  argument R_Node, we go up through the chain of Parent nodes
   --  till the first node, which is a member of some Node_List or to the node
   --  representing the unit declaration in a compilation unit

   function Get_Enclosing
     (Approximation : Asis.Element;
      Element       : Asis.Element)
      return Asis.Element;
   --  This function finds the Enclosing Element for Element by traversing
   --  Approximation which is  considered as a rough estimation for
   --  enclosing element.

   procedure Skip_Normalized_Declarations_Back (Node : in out Node_Id);
   --  this procedure is applied in case when the compiler may normalize a
   --  multi-identifier declaration (or multi-name with clause) in a set of
   --  equivalent one-identifier (one-name) declarations (clauses). It is
   --  intended to be called for Node representing any  declaration
   --  (clause) in this normalized sequence, and it resets its parameter
   --  to point to the first declaration (clause) in this sequence
   --
   --  There is no harm to call this procedure for Node which does not
   --  represent a normalized declaration (or even which does not represent
   --  any declaration at all), or for Node which represents the first
   --  declaration in a normalized chain - the procedure simply leaves
   --  its parameter intact.
   --
   --  (In some sense this procedure may be considered as an "inversion
   --  of the local procedure Skip_Normalized_Declarations defined in
   --  the body of the A4G.Mapping package)

   ---------------------------------------------------------------
   --  Section 5 - bodies of the routines declared in Section 2 --
   ---------------------------------------------------------------

   --------------------------------------
   -- A_Constant_Declaration_Enclosing --
   --------------------------------------

   function A_Constant_Declaration_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result   : Asis.Element := General_Encl_Elem (Element);
      Res_Node : Node_Id;
   begin
      --  The problem with constant declarations exists for a declarations
      --  created by the front-end to pass the actual expressions for generic
      --  IN parameters, see EC16-004 and EC22-007

      Res_Node := Node (Element);

      if Present (Corresponding_Generic_Association (Res_Node)) then
         Res_Node := Parent (Res_Node);

         if No (Generic_Parent (Res_Node)) then
            --  This IF statement prevents us from doing this special
            --  processing for expanded package declarations, we have to do it
            --  only for wrapper packages created for subprogram instantiation

            Res_Node := Parent (Res_Node);
            Res_Node := Corresponding_Body (Res_Node);
            Res_Node := Parent (Res_Node);
            Res_Node := First (Sinfo.Declarations (Res_Node));

            while Nkind (Res_Node) /= N_Subprogram_Body loop
               Res_Node := Next (Res_Node);
            end loop;

            Result := Node_To_Element_New (Node             => Res_Node,
                                           Starting_Element => Element);

         end if;

      end if;

      return Result;

   end A_Constant_Declaration_Enclosing;

   ----------------------------------------
   -- A_Defining_Expanded_Name_Enclosing --
   ---------------------------------------

   function A_Defining_Expanded_Name_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      Parent_Node      : Node_Id            := Parent (R_Node (Element));
      Parent_Node_Kind : constant Node_Kind := Nkind (Parent_Node);
   begin
      if Parent_Node_Kind = N_Function_Specification  or else
         Parent_Node_Kind = N_Procedure_Specification or else
         Parent_Node_Kind = N_Package_Specification
      then -- one more step up required
         Parent_Node := Parent (Parent_Node);
      end if;
      return Node_To_Element_New (Node             => Parent_Node,
                                  Starting_Element => Element);
   end A_Defining_Expanded_Name_Enclosing;

   -------------------------------------
   -- A_Defining_Identifier_Enclosing --
   -------------------------------------

   function A_Defining_Identifier_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      --  A_Defining_Identifier may be processed just in the same way as
      --  A_Defining_Expanded_Name, except the following cases:

      --  - A_Defining_Identifier obtained as the child of
      --    A_Choice_Parameter_Specification element (by means of Names
      --    query) - both these elements are based on the same
      --    N_Defining_Identifier node
      --
      --  - A_Defining_Identifier representing a statement label, it is
      --    obtained by means of Label_Names query, and it is based on
      --    N_Label node which is the member of the node list representing
      --    the corresponding statement sequence (or it can be based on
      --    N_Identifier node in case if the front-end rewrites a sequence of
      --    statement implementing the infinite loop by goto into
      --    N_Loop_Statement node. The Enclosing_Element of such
      --    A_Defining_Name element will be the statement labeled by it, see
      --    Asis_Statements.Label_Names.
      --
      --  - A_Defining_Identifier representing a statement identifier, it is
      --    obtained by means of Statement_Identifier query, and it is based
      --    on N_Identifier node. The Enclosing_Element of the name is the
      --    named statement, see Asis_Statements.Statement_Identifier. But
      --    there is no difference in computing the Enclosing Element
      --    (compared to A_Defining_Expanded_Name) in this case.
      --
      --  - A special processing is needed for a formal package defining name
      --
      --  - A_Defining_Identifier is from a single task/protected declaration

      Parent_Node      : Node_Id                := Parent (R_Node (Element));
      Parent_Node_Kind : constant Node_Kind     := Nkind  (Parent_Node);
      Result_Kind      : Internal_Element_Kinds := Not_An_Element;
   begin

      if Nkind (Node (Element)) = N_Label then
         Parent_Node    := Next (R_Node (Element));
         --  R_Node (Element) definitely is a list member

         while not Is_Statement (Parent_Node) loop
            Parent_Node := Next (Parent_Node);
         end loop;

      elsif Nkind (Node (Element)) = N_Identifier
         and then
            Parent_Node_Kind = N_Loop_Statement
         and then
            Is_Rewrite_Substitution (Parent_Node)
         and then
            Nkind (Original_Node (Parent_Node)) = N_Goto_Statement
      then

         if Is_Empty_List (Sinfo.Statements (Parent_Node)) then
            --  Pathological case of
            --
            --   <<Target>> goto target;
            Result_Kind := A_Goto_Statement;
         else
            Parent_Node := First (Sinfo.Statements (Parent_Node));
         end if;

      elsif Parent_Node_Kind = N_Exception_Handler then
         Parent_Node := R_Node (Element);
         Result_Kind := A_Choice_Parameter_Specification;

      elsif Nkind (Parent_Node) = N_Generic_Package_Declaration
          and then
            Nkind (Original_Node (Parent_Node)) = N_Formal_Package_Declaration
          and then
            R_Node (Element) =
            Defining_Identifier (Original_Node (Parent_Node))
      then
         --  A formal package with a box (but not its expanded spec!)
         Result_Kind := A_Formal_Package_Declaration_With_Box;

      elsif not Comes_From_Source (Parent_Node)
        and then
            Nkind (Parent_Node) = N_Object_Declaration
        and then
            Present (Etype (R_Node (Element)))
        and then
            Ekind (Etype (R_Node (Element))) in
              E_Task_Type .. E_Protected_Type
      then
         --  The case of a single task/protected definition - the problem here
         --  is that Parent field of the argument node points into artificial
         --  object declaration, see G214-005

         Parent_Node := Etype (R_Node (Element));

         if Ekind (Parent_Node) = E_Protected_Type then
            Result_Kind := A_Single_Protected_Declaration;
         else
            Result_Kind := A_Single_Task_Declaration;
         end if;

         Parent_Node := Parent (Parent_Node);
      else
         return A_Defining_Expanded_Name_Enclosing (Element);
      end if;

      return Node_To_Element_New (Node             => Parent_Node,
                                  Internal_Kind    => Result_Kind,
                                  Starting_Element => Element);

   end A_Defining_Identifier_Enclosing;

   ------------------------------------------
   -- A_Defining_Operator_Symbol_Enclosing --
   ------------------------------------------

   function A_Defining_Operator_Symbol_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node      : Node_Id            := Parent (R_Node (Element));
      Parent_Node_Kind : constant Node_Kind := Nkind (Parent_Node);
   begin

      if Parent_Node_Kind = N_Function_Specification

      then -- one more step up required

         Parent_Node := Parent (Parent_Node);

      end if;

      return Node_To_Element_New (Node             => Parent_Node,
                                  Starting_Element => Element);

   end A_Defining_Operator_Symbol_Enclosing;

   --------------------------------
   -- A_Discrete_Range_Enclosing --
   --------------------------------

   function A_Discrete_Range_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node      : Node_Id                := Parent (R_Node (Element));
      Result_Node_Kind : constant Node_Kind     := Nkind  (Result_Node);
      Result_Elem_Kind : Internal_Element_Kinds := Not_An_Element;
   begin

      if not Comes_From_Source (Result_Node) or else
         not Comes_From_Source (Parent (Result_Node))
      then
         return An_Expression_Enclosing (Element);
      end if;

      if Nkind (Node (Element)) = N_Component_Clause then
         Result_Node      := R_Node (Element);
         Result_Elem_Kind := A_Component_Clause;
      elsif Result_Node_Kind = N_Component_Association then
         Result_Elem_Kind := An_Array_Component_Association;
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => Result_Elem_Kind,
         Considering_Parent_Count => False);
   end A_Discrete_Range_Enclosing;

   -----------------------------------
   -- A_Discriminant_Part_Enclosing --
   -----------------------------------

   function A_Discriminant_Part_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
   begin
      return Node_To_Element_New (Node             => R_Node (Element),
                                  Starting_Element => Element);
   end A_Discriminant_Part_Enclosing;

   --------------------------------------------
   -- A_Discriminant_Specification_Enclosing --
   --------------------------------------------

   function A_Discriminant_Specification_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
   begin

      return Node_To_Element_New (
                Node          => Parent (R_Node (Element)),
                Internal_Kind => A_Known_Discriminant_Part,
                Starting_Element => Element);

   end A_Discriminant_Specification_Enclosing;

   ----------------------------------------------
   -- A_Loop_Parameter_Specification_Enclosing --
   ----------------------------------------------

   function A_Loop_Parameter_Specification_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node : Node_Id;
      Tmp         : Node_Id;
      Result      : Asis.Element;
   begin
      Result_Node := Parent (R_Node (Element));

      if Nkind (Result_Node) /= N_Quantified_Expression then
         --  We have got to N_Ineration_Sceme node only
         Result_Node := Parent (Result_Node);
      end if;

      if Declaration_Kind (Element) in
        A_Generalized_Iterator_Specification ..
         An_Element_Iterator_Specification
      then
         --  Here we may have to get to an artificial block statement the
         --  needed loop node is rewritten into
         Tmp := Parent (Parent (Result_Node));

         if Nkind (Tmp) = N_Block_Statement
           and then
            Is_Rewrite_Substitution (Tmp)
           and then
            Nkind (Original_Node (Tmp)) = N_Loop_Statement
         then
            Result_Node := Tmp;
         end if;

      end if;

      Result := Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Element);

      if Int_Kind (Result) = A_Parenthesized_Expression then
         --  This is the case when an iteration scheme is used in a
         --  conditional or quantified expression. We go in bottom-up
         --  direction, so we can have A_Parenthesized_Expression only as the
         --  next enclosing Element
         Result := An_Expression_Enclosing (Element);
      end if;

      return Result;

   end A_Loop_Parameter_Specification_Enclosing;

   --------------------------------
   -- A_Null_Component_Enclosing --
   --------------------------------

   function A_Null_Component_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      Parent_Node          : constant Node_Id := Node (Element);
      Parent_Internal_Kind : Internal_Element_Kinds;
   begin

      if Nkind (Parent_Node) = N_Record_Definition then
         Parent_Internal_Kind := A_Record_Definition;
      else
         Parent_Internal_Kind := A_Variant;
      end if;

      return Node_To_Element_New (Node             => Parent_Node,
                                  Internal_Kind    => Parent_Internal_Kind,
                                  Starting_Element => Element);

   end A_Null_Component_Enclosing;

   -----------------------------------------
   -- A_Parameter_Specification_Enclosing --
   -----------------------------------------

   function A_Parameter_Specification_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node      : Node_Id            := Parent (R_Node (Element));
      Result_Node_Kind : constant Node_Kind := Nkind (Result_Node);
   begin
      if not (Result_Node_Kind = N_Entry_Declaration           or else
              Result_Node_Kind = N_Access_Function_Definition  or else
              Result_Node_Kind = N_Access_Procedure_Definition or else
              Result_Node_Kind = N_Accept_Statement)

--  --|A2005 start
        or else
          (Nkind (Parent (Result_Node)) = N_Identifier
           and then
          Is_Rewrite_Substitution (Parent (Result_Node))
           and then
          Nkind (Original_Node (Parent (Result_Node))) = N_Access_Definition)
        or else
          Nkind (Parent (Result_Node)) = N_Access_Definition
--  --|A2005 end

      then
         Result_Node := Parent (Result_Node);
         --  the first Parent gives N_Function/Procedure_Specification only
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Considering_Parent_Count => False);
   end A_Parameter_Specification_Enclosing;

   ------------------------
   -- A_Pragma_Enclosing --
   ------------------------

   function A_Pragma_Enclosing (Element : Asis.Element) return Asis.Element is

      Parent_Node          : Node_Id   := Atree.Parent (R_Node (Element));
      Parent_Node_Kind     : Node_Kind := Nkind (Parent_Node);
      Parent_Internal_Kind : Internal_Element_Kinds;

   begin

      if Parent_Node_Kind = N_Loop_Statement
        and then
         Is_Rewrite_Substitution (Parent_Node)
        and then
         Nkind (Original_Node (Parent_Node)) = N_Goto_Statement
      then
         --  This is the case when infinite loop implemented as
         --
         --    <<Target>> ...
         --       ...
         --    goto Target;
         --
         --  is rewritten into N_Loop_Statement
         Parent_Node      := Parent (Parent_Node);
         Parent_Node_Kind := Nkind (Parent_Node);
      end if;

      --  filtering out compilation pragmas and correcting Parent_Node,
      --  if necessary

      case Parent_Node_Kind is

         when   N_Handled_Sequence_Of_Statements
              | N_Package_Specification
              | N_Component_List                  =>

            Parent_Node      := Atree.Parent (Parent_Node);
            Parent_Node_Kind := Nkind (Parent_Node);

         when N_Compilation_Unit =>
            return Asis.Nil_Element;
         when others =>
            null;

      end case;

      --  special processing for Nodes requiring by-hand Enclosing Element
      --  kind determination and returning the result for all other Nodes

      case Parent_Node_Kind is

         when N_If_Statement =>

            if List_Containing (R_Node (Element)) =
               Then_Statements (Parent_Node)
            then
               Parent_Internal_Kind := An_If_Path;
            else
               Parent_Internal_Kind := An_Else_Path;
            end if;
            --  ???    List_Containing (Node (Element))   ??
            --  ??? or List_Containing (R_Node (Element)) ??

         when   N_Conditional_Entry_Call
              | N_Selective_Accept       =>

            Parent_Internal_Kind := An_Else_Path;

         when N_Record_Definition =>

            Parent_Internal_Kind := An_Else_Path;

         when others => --  auto determination of the Enclosing Element kind:

            return Node_To_Element_New (Node             => Parent_Node,
                                        Starting_Element => Element);
      end case;

      --  returning the Enclosing Element with the by-hand-defined kind:

      return Node_To_Element_New (Node             => Parent_Node,
                                  Internal_Kind    => Parent_Internal_Kind,
                                  Starting_Element => Element);

   end A_Pragma_Enclosing;

   -------------------------------------------
   -- A_Range_Attribute_Reference_Enclosing --
   -------------------------------------------

   function A_Range_Attribute_Reference_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node : Node_Id := Parent (R_Node (Element));
      Tmp         : Node_Id := Parent (Result_Node);
   begin

      if Nkind (Result_Node) = N_Subtype_Indication  and then
         Nkind (Tmp)         = N_Subtype_Declaration and then
         not Comes_From_Source (Tmp)
      then
         --  This N_Subtype_Declaration is from the tree structure created
         --  for an artificial subtype declaration, see C208-003

         Tmp         := Next (Tmp);
         Result_Node := Object_Definition (Tmp);
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Considering_Parent_Count => False);

   end A_Range_Attribute_Reference_Enclosing;

   -----------------------------------
   -- A_Record_Definition_Enclosing --
   -----------------------------------

   function A_Record_Definition_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node          : Node_Id;
      Parent_Internal_Kind : Internal_Element_Kinds;
   begin

      if Nkind (Parent (R_Node (Element))) = N_Derived_Type_Definition then

         Parent_Node          := Parent (R_Node (Element));
         Parent_Internal_Kind := A_Derived_Record_Extension_Definition;

      else

         Parent_Node := Node (Element);

         if Tagged_Present (Parent_Node) then
            Parent_Internal_Kind := A_Tagged_Record_Type_Definition;
         else
            Parent_Internal_Kind := A_Record_Type_Definition;
         end if;

      end if;

      return Node_To_Element_New (Node             => Parent_Node,
                                  Internal_Kind    => Parent_Internal_Kind,
                                  Starting_Element => Element);

   end A_Record_Definition_Enclosing;

   -----------------------------------------
   -- A_Simple_Expression_Range_Enclosing --
   ----------------------------------------

   function A_Simple_Expression_Range_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      Enclosing_Node         : Node_Id   := Node (Element);
      Enclosing_Node_Kind    : Node_Kind := Nkind (Enclosing_Node);
      Context                : Node_Id;
      Context_Kind           : Node_Kind;
      Enclosing_Element_Kind : Internal_Element_Kinds;
   begin
      if Enclosing_Node_Kind = N_Signed_Integer_Type_Definition then
         --  back from Integer_Constraint
         return Node_To_Element_New
                 (Starting_Element         => Element,
                  Node                     => R_Node (Element),
                  Internal_Kind            => A_Signed_Integer_Type_Definition,
                  Considering_Parent_Count => False);
      else -- one step up
         Enclosing_Node      := Parent (R_Node (Element));
         Enclosing_Node_Kind := Nkind  (Enclosing_Node);
   --  possible values of                corresponding kinds of
   --  Enclosing_Node_Kind:              Enclosing Element:
   --
   --  N_Floating_Point_Definition      A_Floating_Point_Definition        (*)
   --  N_Ordinary_Fixed_Point_Definition An_Ordinary_Fixed_Point_Definition (*)
   --  N_Decimal_Fixed_Point_Definition  A_Decimal_Fixed_Point_Definition   (*)
   --
   --  A_Constraint
   --      N_Digits_Constraint          A_Digits_Constraint                (*)
   --      N_Delta_Constraint           A_Delta_Constraint                 (*)
   --
   --
   --  A_Subtype_Indication
   --       N_Subtype_Indication   A_Discrete_Subtype_Indication
   --                                         (_As_Subtype_Definition)
   --                              A_Subtype_Indication
   --
   --  A_Discrete_Range
   --       N_Subtype_Indication   A_Discrete_Subtype_Indication
   --
   --
   --
   --  N_In      An_In_Range_Membership_Test                              (*)
   --  N_Not_In  A_Not_In_Range_Membership_Test                           (*)
   --
   --  (*) means that the Enclosing Element can be obtained by Node_To_Elemen
   --    constructor with auto determination of the Element kind

         if Enclosing_Node_Kind /= N_Subtype_Indication then
            return Node_To_Element_New
                    (Starting_Element         => Element,
                     Node                     => Enclosing_Node,
                     Considering_Parent_Count => False);
         else
            --  A_Discrete_Subtype_Indication or
            --  A_Discrete_Subtype_Indication_As_Subtype_Definition
            --  or A_Subtype_Indication?

            --  First, we have to skip implicit subtype created for
            --  constraint directly included in object declaration,
            --  if any

            Skip_Implicit_Subtype (Enclosing_Node);

            Context      := Parent (Enclosing_Node);
            Context_Kind := Nkind (Context);

            if Context_Kind = N_Subtype_Indication then
               --  it's impossible to make a decision on the base
               --  of this node, we shall go one more step up
               Context      := Parent (Context);
               Context_Kind := Nkind (Context);
            end if;

            if Context_Kind = N_Subtype_Declaration           or else
               ((Context_Kind = N_Constrained_Array_Definition  or else
                  Context_Kind = N_Unconstrained_Array_Definition)
                and then
                 Enclosing_Node = Sinfo.Component_Definition (Context))
               or else
               --   is it enough or should we add:
               --   and then Enclosing_Node = Subtype_Indication (Context)?
               Context_Kind = N_Derived_Type_Definition or else
               Context_Kind = N_Access_To_Object_Definition
            then
               Enclosing_Element_Kind := A_Subtype_Indication;

            elsif Context_Kind = N_Constrained_Array_Definition or else
                  Context_Kind = N_Entry_Declaration            or else
                  Context_Kind = N_Entry_Index_Specification    or else
                  Context_Kind = N_Loop_Parameter_Specification
            then
               Enclosing_Element_Kind :=
                    A_Discrete_Subtype_Indication_As_Subtype_Definition;
            elsif Context_Kind = N_Component_Declaration or else
                  Context_Kind = N_Object_Declaration    or else
                  Context_Kind = N_Component_Definition
            then
               Enclosing_Element_Kind := A_Subtype_Indication;
            else
               Enclosing_Element_Kind :=
                    A_Discrete_Subtype_Indication;
            end if;

            return Node_To_Element_New
                 (Starting_Element         => Element,
                  Node                     => Enclosing_Node,
                  Internal_Kind            => Enclosing_Element_Kind,
                  Considering_Parent_Count => False);
         end if;
      end if;
   end A_Simple_Expression_Range_Enclosing;

   ---------------------------
   -- A_Statement_Enclosing --
   ---------------------------

   function A_Statement_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node          : Node_Id   := Parent (R_Node (Element));
      Parent_Node_Kind     : Node_Kind := Nkind (Parent_Node);
      Parent_Internal_Kind : Internal_Element_Kinds;
   begin

      if Parent_Node_Kind = N_Loop_Statement
        and then
         Is_Rewrite_Substitution (Parent_Node)
        and then
         Nkind (Original_Node (Parent_Node)) = N_Goto_Statement
      then
         --  This is the case when infinite loop implemented as
         --
         --    <<Target>> ...
         --       ...
         --    goto Target;
         --
         --  is rewritten into N_Loop_Statement
         Parent_Node      := Parent (Parent_Node);
         Parent_Node_Kind := Nkind (Parent_Node);
      end if;

      if Parent_Node_Kind = N_If_Statement then

         if List_Containing (R_Node (Element)) =
            Then_Statements (Parent_Node)
         then
            Parent_Internal_Kind := An_If_Path;
         else
            Parent_Internal_Kind := An_Else_Path;
         end if;
         --  ???    List_Containing (Node (Element))   ??
         --  ?? or List_Containing (R_Node (Element)) ??

      elsif Parent_Node_Kind = N_Conditional_Entry_Call or else
            Parent_Node_Kind = N_Selective_Accept
      then

         Parent_Internal_Kind := An_Else_Path;

      else

         if Parent_Node_Kind = N_Handled_Sequence_Of_Statements then
            --  to go to N_Block_Statement, N_Accept_Statement,
            --  N_Subprogram_Body, N_Package_Body, N_Task_Body or
            --  N_Entry_Body node
            Parent_Node := Parent (Parent_Node);
         end if;

         return Node_To_Element_New (Node             => Parent_Node,
                                     Starting_Element => Element);
      end if;

      return Node_To_Element_New (Node             => Parent_Node,
                                  Internal_Kind    => Parent_Internal_Kind,
                                  Starting_Element => Element);

   end A_Statement_Enclosing;

   ------------------------------------
   -- A_Subtype_Indication_Enclosing --
   ------------------------------------

   function A_Subtype_Indication_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node      : Node_Id            := Parent (R_Node (Element));
      Result_Node      : Node_Id            := R_Node (Element);
      Parent_Node_Kind : constant Node_Kind := Nkind  (Parent_Node);
      Result_Kind      : Internal_Element_Kinds;
   begin

      if Parent_Node_Kind = N_Component_Definition then
         Parent_Node := Parent (Parent_Node);
         --  This skips the normalized component declarations back!
         Parent_Node := Sinfo.Component_Definition (Parent_Node);
      end if;

      if Parent_Node_Kind = N_Allocator and then
         Nkind (Parent (Parent_Node)) = N_Component_Association and then
         not Comes_From_Source (Parent (Parent_Node))
      then
         return An_Expression_Enclosing (Element);
      end if;

      if Parent_Node_Kind = N_Unconstrained_Array_Definition or else
         Parent_Node_Kind = N_Constrained_Array_Definition   or else
         Parent_Node_Kind = N_Component_Declaration
      then
         Result_Kind := A_Component_Definition;

      elsif Parent_Node_Kind = N_Private_Extension_Declaration then
         Result_Kind := A_Private_Extension_Definition;
         Result_Node := Parent_Node;
      else
         return Node_To_Element_New
          (Starting_Element         => Element,
           Node                     => Parent_Node,
           Considering_Parent_Count => False);
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => Result_Kind,
         Considering_Parent_Count => False);

   end A_Subtype_Indication_Enclosing;

   -------------------------------------------------
   -- A_Terminate_Alternative_Statement_Enclosing --
   -------------------------------------------------

   function A_Terminate_Alternative_Statement_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
   begin
      return Node_To_Element_New (Node             => R_Node (Element),
                                  Starting_Element => Element);
   end A_Terminate_Alternative_Statement_Enclosing;

   ------------------------------
   -- A_Variant_Part_Enclosing --
   ------------------------------

   function A_Variant_Part_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      Result_Node : constant Node_Id := Parent (Parent (R_Node (Element)));
      Result_Kind : Internal_Element_Kinds;
   begin
      if Nkind (Result_Node) = N_Record_Definition then
         Result_Kind := A_Record_Definition;
      else
         Result_Kind := A_Variant;
      end if;
      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => Result_Kind,
         Considering_Parent_Count => False);
   end A_Variant_Part_Enclosing;

   ------------------------------
   -- An_Association_Enclosing --
   ------------------------------

   function An_Association_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result : Asis.Element;
   begin

      if Normalization_Case (Element) = Is_Not_Normalized then
         Result := An_Expression_Enclosing (Element);
      else
         Result := Node_To_Element_New
                     (Node             => R_Node (Element),
                      Starting_Element => Element);

         Set_From_Implicit (Result, False);
      end if;

      return Result;
   end An_Association_Enclosing;

   ----------------------------------------------
   -- An_Attribute_Definition_Clause_Enclosing --
   ----------------------------------------------

   function An_Attribute_Definition_Clause_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node      : Node_Id   := Parent (R_Node (Element));
      Result_Node_Kind : Node_Kind := Nkind  (Result_Node);
      Result_Kind     : Internal_Element_Kinds := Not_An_Element;
   begin
      if Result_Node_Kind = N_Component_List or else
         Result_Node_Kind = N_Package_Specification
      then
         Result_Node      := Parent (Result_Node);
         Result_Node_Kind := Nkind  (Result_Node);
      end if;

      if Result_Node_Kind = N_Record_Definition then
         Result_Kind := A_Record_Definition;
      elsif Result_Node_Kind = N_Variant then
         Result_Kind := A_Variant;
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => Result_Kind,
         Considering_Parent_Count => False);
   end An_Attribute_Definition_Clause_Enclosing;

   ----------------------------
   -- An_Else_Path_Enclosing --
   ----------------------------

   function An_Else_Path_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
   begin
      return Node_To_Element_New (Node             => R_Node (Element),
                                  Starting_Element => Element);
   end An_Else_Path_Enclosing;

   ----------------------------------------------------
   -- An_Enumeration_Literal_Specification_Enclosing --
   ----------------------------------------------------

   function An_Enumeration_Literal_Specification_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Result_Node : Node_Id;
      Start_Elem  : Asis.Element := Element;
   begin
      if Special_Case (Element) = Stand_Char_Literal then
         Result_Node  := R_Node (Element);

         Set_Character_Code (Start_Elem, 0);
         Set_Special_Case   (Start_Elem, Explicit_From_Standard);
      else
         Result_Node  := Parent (R_Node (Element));
      end if;

      return Node_To_Element_New
        (Starting_Element         => Start_Elem,
         Node                     => Result_Node,
         Internal_Kind            => An_Enumeration_Type_Definition);

   end An_Enumeration_Literal_Specification_Enclosing;

   ----------------------------------------------
   -- An_Enumeration_Type_Definition_Enclosing --
   ----------------------------------------------

   function An_Enumeration_Type_Definition_Enclosing
     (Element : Asis.Element)
      return Asis.Element
   is
      Result_Node : Node_Id;
   begin
      if Special_Case (Element) = Stand_Char_Literal then
         Result_Node  := R_Node (Element);

         if Nkind (Result_Node) = N_Defining_Identifier then
            --  we are in the definition of Standard.Boolean:
            Result_Node := Parent (Etype (Result_Node));
         end if;

      else
         Result_Node  := Parent (R_Node (Element));
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => An_Ordinary_Type_Declaration);

   end An_Enumeration_Type_Definition_Enclosing;

   ------------------------------------
   -- An_Exception_Handler_Enclosing --
   ------------------------------------

   function An_Exception_Handler_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
   begin
      return Node_To_Element_New
               (Node             => Parent (Parent (R_Node (Element))),
                Starting_Element => Element);
   end An_Exception_Handler_Enclosing;

   -----------------------------
   -- An_Expression_Enclosing --
   -----------------------------

   function An_Expression_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Start_Elem           : Asis.Element  := Element;
      Rough_Result_Node    : Node_Id;
      Res_Entity           : Entity_Id;
      Rough_Result_Element : Asis.Element;
      Rough_Res_Spec_Case  : Special_Cases;
      Result_Element       : Asis.Element;
   begin
      Rough_Result_Node    := Get_Rough_Enclosing_Node (Element);

      if not (Sloc (Node (Start_Elem)) <= Standard_Location or else
              Special_Case (Start_Elem) = Configuration_File_Pragma)
      then
         Set_Special_Case (Start_Elem, Not_A_Special_Case);
      end if;

      Rough_Result_Element := Node_To_Element_New
                                (Node             => Rough_Result_Node,
                                 Starting_Element => Start_Elem);

      if Is_Top_Of_Expanded_Generic (Rough_Result_Node)
        and then
         Is_From_Instance (Element)
        and then
         (Nkind (Original_Node (Rough_Result_Node)) /=
            N_Formal_Package_Declaration
         or else
          Instantiation_Depth (Sloc (R_Node (Element))) >
            Instantiation_Depth (Sloc (Rough_Result_Node)))
      then
         --  ??? The content of this if statement is just a slightly edited
         --  ??? fragment of Enclosing_For_Explicit_Instance_Component

         if Nkind (Rough_Result_Node) = N_Package_Declaration or else
            Nkind (Rough_Result_Node) = N_Package_Body
         then
            Rough_Res_Spec_Case := Expanded_Package_Instantiation;
            --  and here we have to correct the result:
            Set_Node (Rough_Result_Element, R_Node (Rough_Result_Element));

            if Nkind (Rough_Result_Node) = N_Package_Declaration then
               Set_Int_Kind (Rough_Result_Element, A_Package_Declaration);
            else
               Set_Int_Kind (Rough_Result_Element, A_Package_Body_Declaration);
            end if;

         else
            Rough_Res_Spec_Case := Expanded_Subprogram_Instantiation;
         end if;

         Set_Special_Case (Rough_Result_Element, Rough_Res_Spec_Case);

      end if;

      if Special_Case (Element) = Is_From_Gen_Association
        and then
         Is_Top_Of_Expanded_Generic (Node (Rough_Result_Element))
        and then
         Instantiation_Depth (Sloc (Node (Rough_Result_Element))) =
         Instantiation_Depth (Sloc (Node (Element)))
      then
         Rough_Result_Element := Enclosing_Element (Rough_Result_Element);
      end if;

      if Nkind (Rough_Result_Node) = N_Subprogram_Declaration
        and then
         not Comes_From_Source (Rough_Result_Node)
      then
         Res_Entity := Defining_Unit_Name (Specification (Rough_Result_Node));

         if (Ekind (Res_Entity) = E_Function
           and then not Comes_From_Source (Res_Entity)
           and then Chars (Res_Entity) = Snames.Name_Op_Ne)
           and then Present (Corresponding_Equality (Res_Entity))
         then
            Set_Special_Case
              (Rough_Result_Element, Is_From_Imp_Neq_Declaration);
         end if;

      end if;

      Result_Element :=  Get_Enclosing
                           (Approximation => Rough_Result_Element,
                            Element       => Element);
      return Result_Element;
   end An_Expression_Enclosing;

   --------------------------------
   -- An_Others_Choice_Enclosing --
   --------------------------------

   function An_Others_Choice_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node : constant Node_Id := Parent (Parent (R_Node (Element)));
      Result_Node : Node_Id := Parent (R_Node (Element));
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      if Nkind (Result_Node) = N_Component_Association then
         --  we have to find out, is it record or array component
         --  association. Parent_Node points to the enclosing aggregate

         if No (Etype (Parent_Node)) or else
            Is_Array_Type (Etype (Parent_Node))
         then
            --  the first condition in 'or else' is true for multi-dimensional
            --  array aggregates
            Result_Kind := An_Array_Component_Association;
         else
            Result_Kind := A_Record_Component_Association;
         end if;

      elsif Nkind (Result_Node) = N_Package_Declaration
         and then
            Nkind (Original_Node (Result_Node)) = N_Formal_Package_Declaration
      then
         Result_Node :=
           Last_Non_Pragma
             (Generic_Associations (Original_Node (Result_Node)));
         Result_Kind := A_Generic_Association;
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Internal_Kind            => Result_Kind,
         Considering_Parent_Count => False);
   end An_Others_Choice_Enclosing;

   ----------------------------------------------------
   -- Not_Implemented_Enclosing_Element_Construction --
   ----------------------------------------------------

   function Not_Implemented_Enclosing_Element_Construction
       (Element : Asis.Element) return Asis.Element is
   begin
      Not_Implemented_Yet (Diagnosis =>
                        "Enclosing Element retrieval for the explicit Element "
                      &  "of the " &  Internal_Element_Kinds'Image
                      (Int_Kind (Element)) & " kind "
                      & "has not been implemented yet");
      return Asis.Nil_Element; -- to make the code syntactically correct;
   end Not_Implemented_Enclosing_Element_Construction;

   ----------------------------
   -- Possible_C_U_Enclosing --
   ----------------------------

   function Possible_C_U_Enclosing
     (Element : Asis.Element)
      return    Asis.Element
   is
      Parent_Node      : Node_Id            := Parent (R_Node (Element));
      Parent_Node_Kind : constant Node_Kind := Nkind (Parent_Node);
   begin
      if Parent_Node_Kind = N_Compilation_Unit or else
         Parent_Node_Kind = N_Subunit
      then
         return Asis.Nil_Element;

      elsif Parent_Node_Kind = N_Package_Specification then
         Parent_Node := Parent (Parent_Node);

      elsif Parent_Node_Kind = N_Protected_Definition then
         Parent_Node := Parent (Parent_Node);
         Parent_Node := Protected_Definition (Original_Node (Parent_Node));
      end if;

      return Node_To_Element_New
        (Starting_Element => Element,
         Node             => Parent_Node);

   end Possible_C_U_Enclosing;

   -----------------------------------------------------------------
   --  Section 6 - bodies for the routines defined in the package --
   --              spec and local subprograms                     --
   -----------------------------------------------------------------

   ---------------------------------
   -- Corresponding_Instantiation --
   ---------------------------------

   function Corresponding_Instantiation
     (Element : Asis.Element)
      return Asis.Element
   is
      Argument_Node : Node_Id                         := R_Node (Element);
      Argument_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Result_Node   : Node_Id                         := Argument_Node;
      Result_Kind   : Internal_Element_Kinds;
      Result_Unit   : constant Asis.Compilation_Unit  := Encl_Unit (Element);
   begin

      if Argument_Kind = A_Package_Declaration or else
         Argument_Kind = A_Package_Body_Declaration
      then

         --  A formal package with box needs a special processing - it is
         --  based on the same node as the argument

         if Nkind (Original_Node (Argument_Node)) =
            N_Formal_Package_Declaration
           and then
            Box_Present (Original_Node (Argument_Node))
         then
            Result_Kind := A_Formal_Package_Declaration_With_Box;
         else
            Argument_Node := Parent (Argument_Node);

            if Nkind (Argument_Node) in N_Generic_Declaration and then
               Is_List_Member (Result_Node)                   and then
               List_Containing (Result_Node) =
                  Generic_Formal_Declarations (Argument_Node)
            then
               Result_Kind := A_Formal_Package_Declaration;
            else
               Result_Kind := A_Package_Instantiation;
            end if;

         end if;

      else

         if Argument_Kind = A_Procedure_Declaration or else
            Argument_Kind = A_Procedure_Body_Declaration
         then
            Result_Kind := A_Procedure_Instantiation;
         else
            Result_Kind := A_Function_Instantiation;
         end if;

         --  we have to go the N_Package_Decalaration node of an
         --  artificial package created by the compiler for a subprogram
         --  instantiation - two steps up the tree are needed:
         Result_Node := Parent (Result_Node);

         if Argument_Kind = A_Procedure_Declaration or else
            Argument_Kind = A_Function_Declaration
         then
            Result_Node := Parent (Result_Node);
         end if;

      end if;

      if Nkind (Parent (Result_Node)) = N_Compilation_Unit then

         --  For library-level subprogram instantiations we may have a
         --  problem in the tree created for the instantiation itself.

         if Nkind (Result_Node) = N_Package_Declaration and then
            not Is_Rewrite_Substitution (Result_Node)
         then
            Result_Node := Parent (Corresponding_Body (Result_Node));

            if Nkind (Result_Node) = N_Defining_Program_Unit_Name then
               Result_Node := Parent (Result_Node);
            end if;

         end if;

      elsif Nkind (Original_Node (Result_Node)) /=
            N_Formal_Package_Declaration
      then
         --  "local" instantiation, therefore - one or two steps down the
         --  declaration list to get in the instantiation node, a formal
         --  package with a box is an exception:

         Result_Node := Next_Non_Pragma (Result_Node);

         if Nkind (Result_Node) = N_Package_Body then
            --  This is an expanded generic body
            Result_Node := Next_Non_Pragma (Result_Node);
         end if;

      end if;

      if Is_Rewrite_Substitution (Result_Node) and then
         Is_Rewrite_Substitution (Original_Node (Result_Node))
      then
         Result_Node := Original_Node (Result_Node);
      end if;

      return Node_To_Element_New
               (Node          => Result_Node,
                Internal_Kind => Result_Kind,
                In_Unit       => Result_Unit);
   end Corresponding_Instantiation;

   ------------------------------------
   -- Enclosing_Element_For_Explicit --
   ------------------------------------

   function Enclosing_Element_For_Explicit
     (Element : Asis.Element)
      return    Asis.Element
   is
      Enclosing_Construction_Case : Internal_Element_Kinds;
      Element_Internal_Kind       : Internal_Element_Kinds;
      Result_Element              : Asis.Element;

      Res_Node      : constant Node_Id       := Standard_Package_Node;
      Res_Kind      : Internal_Element_Kinds := Not_An_Element;
      Res_Spec_Case : Special_Cases;
   begin
      Element_Internal_Kind := Int_Kind (Element);

      --  A special case of fake Numeric_Error renaming is handled
      --  separately (see B712-0050)

      if Special_Case (Element) = Numeric_Error_Renaming then

         case Element_Internal_Kind is
            when An_Exception_Renaming_Declaration =>
               Res_Kind      := A_Package_Declaration;
               Res_Spec_Case := Explicit_From_Standard;
            when A_Defining_Identifier |
                 An_Identifier         =>
               Res_Kind      := An_Exception_Renaming_Declaration;
               Res_Spec_Case := Numeric_Error_Renaming;
            when others =>
               null;
         end case;

         Result_Element := Node_To_Element_New
           (Starting_Element => Element,
            Node             => Res_Node,
            Internal_Kind    => Res_Kind,
            Spec_Case        => Res_Spec_Case);

         return Result_Element;

      end if;

      --  A special case of a configuration pragma is handled separately
      --  (BA07-013)

      if Element_Internal_Kind in Internal_Pragma_Kinds and then
         Special_Case (Element) = Configuration_File_Pragma
      then
         return Asis.Nil_Element;
      end if;

      Enclosing_Construction_Case :=
         Enclosing_Element_For_Explicits_First_Switch (Element_Internal_Kind);

      case Enclosing_Construction_Case is
         when Not_An_Element =>
            return Asis.Nil_Element;
         when Trivial_Mapping =>
            Result_Element := General_Encl_Elem (Element);
         when Non_Trivial_Mapping =>
            Result_Element :=
               Enclosing_Element_For_Explicits_Second_Switch
                 (Element_Internal_Kind) (Element);
         when No_Mapping =>
               No_Enclosing_Element (Element_Kind => Element_Internal_Kind);
            return Asis.Nil_Element; -- to avoid GNAT warning
         when  Not_Implemented_Mapping =>
               Not_Implemented_Enclosing_Element_Construction
                  (Element => Element);
         when others =>
            --  others means here that the Enclosing Element should
            --  based on the same node.
            Result_Element := Node_To_Element_New
              (Starting_Element         => Element,
               Node                     => R_Node (Element),
               Internal_Kind            => Enclosing_Construction_Case,
               Considering_Parent_Count => False);

            if Element_Internal_Kind = A_Defining_Character_Literal then
               Set_Character_Code (Result_Element, Character_Code (Element));
            end if;

      end case;

      if Is_From_Implicit (Element)
        and then
         Statement_Kind (Element) = A_Null_Statement
      then
         --  Case of an implicit NULL statement needed for 'floating' labels,
         --  Ada 2012
         Set_From_Implicit (Result_Element, False);
      end if;

      return Result_Element;
   end Enclosing_Element_For_Explicit;

   -----------------------------------------------
   -- Enclosing_For_Explicit_Instance_Component --
   -----------------------------------------------

   function Enclosing_For_Explicit_Instance_Component
     (Element : Asis.Element)
      return Asis.Element
   is
      Result_Element   : Asis.Element;
      Result_Node      : Node_Id;
      Tmp_Node         : Node_Id;
      Res_Spec_Case    : Special_Cases;

      function Is_Top_Exp_Form_Pack_With_Box
        (Potential_Enclosing_Element : Asis.Element;
         Arg_Element                 : Asis.Element)
         return                        Boolean;
      --  Checks if Potential_Enclosing_Element is the top expanded spec
      --  (??? what about body???) for a formal package declaration with box.
      --  The problem here is that when going up the tree, we can get into this
      --  argument both from components of the formal package declaration with
      --  box and from the corresponding expanded spec. So we have to check
      --  if Potential_Enclosing_Element and Arg_Element are the same level
      --  of instantiating (nested instances may be a pain! The function needs
      --  more testing ???)
      --  See the discussion in E425-007

      function Is_Top_Exp_Form_Pack_With_Box
        (Potential_Enclosing_Element : Asis.Element;
         Arg_Element                 : Asis.Element)
         return                        Boolean
      is
         EE_Inst_Level  : Natural := 0;
         Arg_Inst_Level : Natural := 0;

         Src : Source_Ptr :=
           Instantiation
             (Get_Source_File_Index
                (Sloc (R_Node (Potential_Enclosing_Element))));

         function May_Be_Exp_Pack_Def_Name
           (N_Pack : Node_Id;
            N_Name : Node_Id)
         return Boolean;
         --  In case of the defining name of an expanded package created for a
         --  formal package with the box, we have the instantiation chain one
         --  link shorter then the rest of the expanded package, so we have
         --  to detect this situation.

         function May_Be_Nested_FP_Instantiation
           (N_Pack : Node_Id;
            N_Name : Node_Id)
         return Boolean;
         --  See E430-A01. We try to detect the situation when we go out of
         --  a chain of nested instantiations created by formal packages with
         --  the box

         function May_Be_Exp_Pack_Def_Name
           (N_Pack : Node_Id;
            N_Name : Node_Id)
         return Boolean
         is
            Result : Boolean := False;
         begin

            if Nkind (N_Name) = N_Defining_Identifier
              and then
               Nkind (Original_Node (N_Pack)) = N_Formal_Package_Declaration
              and then
               Box_Present (Original_Node (N_Pack))
            then
               Result := N_Name = Defining_Unit_Name (Specification (N_Pack));
            end if;

            return Result;
         end May_Be_Exp_Pack_Def_Name;

         function May_Be_Nested_FP_Instantiation
           (N_Pack : Node_Id;
            N_Name : Node_Id)
         return Boolean
         is
            Result : Boolean := False;
         begin
            if Nkind (N_Pack) = N_Generic_Package_Declaration
              and then
               Nkind (Original_Node (N_Pack)) = N_Formal_Package_Declaration
              and then
               Box_Present (Original_Node (N_Pack))
              and then
               Nkind (N_Name) = N_Generic_Package_Declaration
              and then
               Nkind (Original_Node (N_Name)) = N_Formal_Package_Declaration
              and then
               Box_Present (Original_Node (N_Name))
            then
               Result := True;
            end if;

            return Result;
         end May_Be_Nested_FP_Instantiation;

      begin

         if not (Nkind (Node (Potential_Enclosing_Element)) =
                 N_Formal_Package_Declaration
               and then
                 Nkind (R_Node (Potential_Enclosing_Element)) =
                 N_Generic_Package_Declaration)
           or else
            (Int_Kind (Potential_Enclosing_Element) =
             A_Formal_Package_Declaration_With_Box
            and then
             Node (Arg_Element) =
             Defining_Identifier (Node (Potential_Enclosing_Element)))
         then
            return False;
         end if;

         while Src /= No_Location loop
            EE_Inst_Level := EE_Inst_Level + 1;
            Src           := Instantiation (Get_Source_File_Index (Src));
         end loop;

         Src :=
           Instantiation (Get_Source_File_Index (Sloc (R_Node (Arg_Element))));

         while Src /= No_Location loop
            Arg_Inst_Level := Arg_Inst_Level + 1;
            Src           := Instantiation (Get_Source_File_Index (Src));
         end loop;

         return (May_Be_Exp_Pack_Def_Name
                 (R_Node (Potential_Enclosing_Element),
                  R_Node (Arg_Element))
               and then
                 EE_Inst_Level = Arg_Inst_Level + 1)
              or else
                 (May_Be_Nested_FP_Instantiation
                 (R_Node (Potential_Enclosing_Element),
                  R_Node (Arg_Element))
               and then
                  EE_Inst_Level + 1 = Arg_Inst_Level)
              or else
                EE_Inst_Level = Arg_Inst_Level;

      end Is_Top_Exp_Form_Pack_With_Box;

   begin
      Result_Element := Enclosing_Element_For_Explicit (Element);

      if Is_Nil (Result_Element) then
         --  There is a special case corresponding to the defining name in an
         --  artificial subtype declaration that is a means to pass an actual
         --  type in the expanded instantiation (see K811-006). Under some
         --  conditions the corresponding node in the tree is an Itype node,
         --  and it does not have  a Parent reference set.

         Tmp_Node := Node (Element);

         if Nkind (Tmp_Node) = N_Defining_Identifier and then
            Is_Itype (Tmp_Node)
         then
            Tmp_Node := Associated_Node_For_Itype (Tmp_Node);

            Result_Element :=
              Node_To_Element_New
                (Node             => Tmp_Node,
                 Starting_Element => Element,
                 Internal_Kind    => A_Subtype_Declaration);
         end if;

      end if;

      --  In case if the result argument is an artificial declaration
      --  used to pass an actual into expanded subprogram, we are
      --  in the spec of the artificial  wrapper package. So we have to get
      --  to the expanded subprogram declaration (see G416-009)

      if Is_Top_Of_Expanded_Generic (R_Node (Result_Element)) then
         Tmp_Node := (R_Node (Result_Element));

         if Nkind (Tmp_Node) = N_Package_Declaration
           and then
            No (Generic_Parent (Specification (Tmp_Node))) then
               --  This IF statement prevents us from doing this special
               --  processing for expanded package declarations, we have to do
               --  it only for wrapper packages created for subprogram
               --  instantiation

               Tmp_Node :=
                 Last (Visible_Declarations (Specification (Tmp_Node)));

               Result_Element :=
                 Node_To_Element_New
                   (Node             => Tmp_Node,
                    Spec_Case        => Expanded_Subprogram_Instantiation,
                    Starting_Element => Element);
         end if;

      end if;

      --  and now we have to check if we are in the whole expanded
      --  declaration
      Result_Node := R_Node (Result_Element);

      if not (Is_Rewrite_Substitution (Result_Node)
             and then
              Nkind (Original_Node (Result_Node)) =
                 N_Formal_Package_Declaration
             and then
                (Node (Element) =
                  Defining_Identifier (Original_Node (Result_Node))
               or else
                 (Node (Element) /=
                    Defining_Unit_Name (Specification (Result_Node))
                  and then
                    Instantiation_Depth (Sloc (R_Node (Element))) =
                    Instantiation_Depth (Sloc (Result_Node)))))
        and then
         Is_Top_Of_Expanded_Generic (Result_Node)
      then
         --  this is an artificial package or subprogram declaration
         --  created by the compiler as an expanded generic declaration

         if Nkind (Result_Node) = N_Package_Declaration or else
            Nkind (Result_Node) = N_Package_Body
         then
            Res_Spec_Case := Expanded_Package_Instantiation;
            --  and here we have to correct the result:
            Set_Node (Result_Element, R_Node (Result_Element));

            if Nkind (Result_Node) = N_Package_Declaration then
               Set_Int_Kind (Result_Element, A_Package_Declaration);
            else
               Set_Int_Kind (Result_Element, A_Package_Body_Declaration);
            end if;

         else
            Res_Spec_Case := Expanded_Subprogram_Instantiation;
         end if;

         Set_Special_Case (Result_Element, Res_Spec_Case);

      elsif Is_Top_Exp_Form_Pack_With_Box (Result_Element, Element) then
         --  This case is somewhat special - we have not a package, but a
         --  generic package declaration as expanded code here
         Set_Int_Kind     (Result_Element, A_Package_Declaration);
         Set_Special_Case (Result_Element, Expanded_Package_Instantiation);
         --  ??? What about expanded bodies for formal packages with a box?
      end if;

      --  and we have to correct Is_Part_Of_Instance field of the result -
      --  just in case. May be, it will not be necessary, if (and when)
      --  Enclosing_Element_For_Explicit takes the corresponding fields
      --  from its argument

      if not Is_Nil (Result_Element) then
         Set_From_Instance (Result_Element, True);
      end if;

      return Result_Element;

   end Enclosing_For_Explicit_Instance_Component;

   ------------------------------------
   -- Enclosing_Element_For_Implicit --
   ------------------------------------

   function Enclosing_Element_For_Implicit
     (Element : Asis.Element)
      return Asis.Element
   is
      Arg_Kind        : constant Internal_Element_Kinds := Int_Kind (Element);
      Result_Node     : Node_Id                         := Empty;
      Result_Element  : Asis.Element;
      Result_Kind     : Internal_Element_Kinds          := Not_An_Element;
      Res_Spec_Case   : Special_Cases                   := Not_A_Special_Case;

   begin

      --  Special treatment for the declaration of implicit "/=", see F903-002:

      if Asis.Extensions.Is_Implicit_Neq_Declaration (Element) then
         Result_Element :=
           Enclosing_Element
             (Asis.Declarations.Corresponding_Equality_Operator (Element));
      else

         case Arg_Kind is

            when A_Procedure_Declaration          |
                 A_Function_Declaration           |
                 A_Procedure_Body_Declaration     |
                 A_Function_Body_Declaration      |
                 A_Procedure_Renaming_Declaration |
                 A_Function_Renaming_Declaration  |
                 A_Discriminant_Specification     |
                 A_Component_Declaration          =>

               Result_Node := Original_Node (Node_Field_1 (Element));

               if Nkind (Result_Node) in N_Entity
                 and then
                  (Arg_Kind in A_Procedure_Declaration ..
                               A_Function_Declaration
                   or else
                   Arg_Kind in A_Procedure_Body_Declaration ..
                               A_Function_Body_Declaration
                   or else
                   Arg_Kind in A_Procedure_Renaming_Declaration ..
                               A_Function_Renaming_Declaration)
               then
                  Result_Node :=
                    Original_Node (Parent (Node_Field_1 (Element)));
               end if;

               case Nkind (Result_Node) is

                  when N_Private_Extension_Declaration =>
                     Result_Kind := A_Private_Extension_Definition;

                  when N_Formal_Type_Declaration =>
                     Result_Node := Sinfo.Formal_Type_Definition (Result_Node);

                  when others =>
                     Result_Node := Sinfo.Type_Definition (Result_Node);
               end case;

               Result_Element := Node_To_Element_New (
                  Node             => Result_Node,
                  Starting_Element => Element,
                  Internal_Kind    => Result_Kind);

               Set_From_Implicit  (Result_Element, False);
               Set_From_Inherited (Result_Element, False);
               Set_Node_Field_1   (Result_Element, Empty);

            when Internal_Root_Type_Kinds =>
               Result_Element := Element;
               Set_Int_Kind (Result_Element, An_Ordinary_Type_Declaration);

            when An_Ordinary_Type_Declaration =>
               --  The only possible case is the declaration of a root or
               --  universal numeric type
               Result_Node   := Standard_Package_Node;
               Res_Spec_Case := Explicit_From_Standard;
               Result_Kind   := A_Package_Declaration;

               Result_Element :=
                  Node_To_Element_New (Node      => Result_Node,
                                       Spec_Case => Res_Spec_Case,
                                       In_Unit   => Encl_Unit (Element));

            when An_Enumeration_Literal_Specification |
                 An_Entry_Declaration                 =>

               Result_Node   :=
                  Sinfo.Type_Definition
                    (Original_Node (Node_Field_1 (Element)));
               Result_Kind   := A_Derived_Type_Definition;

               Result_Element := Node_To_Element_New (
                  Node             => Result_Node,
                  Starting_Element => Element,
                  Internal_Kind    => Result_Kind);

               Set_From_Implicit  (Result_Element, False);
               Set_From_Inherited (Result_Element, False);
               Set_Node_Field_1   (Result_Element, Empty);

            when A_Generic_Association =>

               Result_Element :=
                 Node_To_Element_New (Node    => R_Node (Element),
                                      In_Unit => Encl_Unit (Element));

            when others =>
               if Normalization_Case (Element) =
                  Is_Normalized_Defaulted_For_Box
               then
                  Result_Node    := Parent (Parent (Parent (Node (Element))));

                  while not (Nkind (Result_Node) in N_Generic_Instantiation
                          or else
                             Nkind (Original_Node (Result_Node)) =
                               N_Formal_Package_Declaration)
                  loop

                     if Nkind (Parent (Result_Node)) = N_Compilation_Unit then
                        --  Library level instantiation

                        if Is_Rewrite_Substitution (Result_Node) then
                           --  Package instantiation, the package does not have
                           --  a body
                           exit;
                        else
                           Result_Node := Corresponding_Body (Result_Node);

                           while Nkind (Result_Node) /= N_Package_Body loop
                              Result_Node := Parent (Result_Node);
                           end loop;
                        end if;

                        exit;
                     else
                        Result_Node := Next (Result_Node);
                     end if;

                  end loop;

                  Result_Element := Node_To_Element_New
                                      (Node    => Result_Node,
                                       In_Unit => Encl_Unit (Element));
               else
                  Result_Element := Enclosing_Element_For_Explicit (Element);
               end if;
         end case;

      end if;

      if Int_Kind (Result_Element) = A_Function_Renaming_Declaration then
         --  See C125-002
         Set_Int_Kind (Result_Element, A_Function_Declaration);
      elsif Int_Kind (Result_Element) = A_Procedure_Renaming_Declaration then
         Set_Int_Kind (Result_Element, A_Procedure_Declaration);
      end if;

      return Result_Element;

   end Enclosing_Element_For_Implicit;

   ----------------------------------------
   -- Enclosing_Element_For_Limited_View --
   ----------------------------------------

   function Enclosing_Element_For_Limited_View
     (Element : Asis.Element)
      return Asis.Element
   is
      Result   : Asis.Element := Enclosing_Element_For_Explicit (Element);
   begin
      if not Is_Nil (Result) then
         Set_Special_Case  (Result, From_Limited_View);
         Set_From_Implicit (Result, True);
         Set_Int_Kind (Result, Limited_View_Kind (Result));
      end if;

      return Result;
   end Enclosing_Element_For_Limited_View;

   -----------------------
   -- General_Encl_Elem --
   -----------------------

   function General_Encl_Elem (Element : Asis.Element) return Asis.Element is
      Result_Node  : Node_Id;
      Result_Nkind : Node_Kind;
   begin
      Result_Node  := Parent (R_Node (Element));
      Result_Nkind := Nkind (Result_Node);

      --  and now - special processing for some node kinds to skip nodes which
      --  are of no use in ASIS
      if Result_Nkind = N_Package_Specification   or else
         Result_Nkind = N_Function_Specification  or else
         Result_Nkind = N_Procedure_Specification or else
         Result_Nkind = N_Entry_Body_Formal_Part
      then
         Result_Node := Parent (Result_Node);
      end if;

      return Node_To_Element_New
        (Starting_Element         => Element,
         Node                     => Result_Node,
         Considering_Parent_Count => False);
   end General_Encl_Elem;

   -------------------
   -- Get_Enclosing --
   -------------------

   function Get_Enclosing
     (Approximation : Asis.Element;
      Element       : Asis.Element)
      return Asis.Element
   is
      --  we need two-level traversing for searching for Enclosing Element:
      --  first, we go through the direct children of an approximate
      --  result, and none of them Is_Identical to Element, we repeat
      --  the search process for each direct child. We may implement
      --  this on top of Traverse_Element, but we prefer to code
      --  it manually on top of A4G.Queries

      Result_Element : Asis.Element;
      Result_Found   : Boolean := False;
      --  needed to simulate the effect of Terminate_Immediatelly

      procedure Check_Possible_Enclosing (Appr_Enclosing : Asis.Element);
      --  implements the first level of the search. Appr_Enclosing is
      --  the "approximate" Enclosing Element, and this procedure
      --  checks if some of its components Is_Identical to Element
      --  (Element here is the parameter of Get_Enclosing function,
      --  as a global constant value inside Get_Enclosing, it is the
      --  same for all the (recursive) calls of Check_Possible_Enclosing

      ------------------------------
      -- Check_Possible_Enclosing --
      -------------------------------
      procedure Check_Possible_Enclosing (Appr_Enclosing : Asis.Element) is
         Child_Access : constant Query_Array :=
           Appropriate_Queries (Appr_Enclosing);
         --  this is the way to traverse the direct children
         Next_Child : Asis.Element;

         procedure Check_List (L : Asis.Element_List);
         --  checks if L contains a component which Is_Identical
         --  to (global) Element. Sets Result_Found ON if such a
         --  component is found

         procedure Check_List_Down (L : Asis.Element_List);
         --  calls Get_Enclosing for every component of L, by
         --  this the recursion and the second level of the search
         --  is implemented

         procedure Check_List (L : Asis.Element_List) is
         begin

            for L_El_Index in L'Range loop

               if Is_Identical (Element, L (L_El_Index)) then
                  Result_Found := True;

                  return;
               end if;

            end loop;

         end Check_List;

         procedure Check_List_Down (L : Asis.Element_List) is
         begin

            if Result_Found then

               return;
               --  it seems that we do not need this if... ???
            end if;

            for L_El_Index in L'Range loop
               Check_Possible_Enclosing (L (L_El_Index));

               if Result_Found then

                  return;
               end if;

            end loop;

         end Check_List_Down;

      begin  -- Check_Possible_Enclosing

         if Result_Found then

            return;
            --  now the only goal is to not disturb the setting of the
            --  global variable Result_Element to be returned as a result
         end if;

         --  first, setting the (global for this procedure) Result_Element:
         Result_Element := Appr_Enclosing;
         --  the first level of the search - checking all the direct
         --  children:
         for Each_Query in Child_Access'Range loop

            case Child_Access (Each_Query).Query_Kind is
               when Bug =>
                  null;
               when Single_Element_Query =>
                  Next_Child :=
                     Child_Access (Each_Query).Func_Simple (Appr_Enclosing);

                  if Is_Identical (Element, Next_Child) then
                     Result_Found := True;

                     return;
                  end if;

               when Element_List_Query =>

                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List (Appr_Enclosing);
                  begin
                     Check_List (Child_List);

                     if Result_Found then

                        return;
                     end if;

                  end;
               when Element_List_Query_With_Boolean =>

                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List_Boolean
                           (Appr_Enclosing, Child_Access (Each_Query).Bool);
                  begin
                     Check_List (Child_List);

                     if Result_Found then

                        return;
                     end if;

                  end;
            end case;
         end loop;

         --  if we are here, we have hot found Element among the direct
         --  children of Appr_Enclosing. So we have to traverse the direct
         --  children again, but this time we have to go one step down,
         --  so here we have the second level of the search:

         for Each_Query in Child_Access'Range loop

            case Child_Access (Each_Query).Query_Kind is
               when Bug =>
                  null;
               when Single_Element_Query =>
                  Next_Child :=
                     Child_Access (Each_Query).Func_Simple (Appr_Enclosing);

                  --  and here - recursively one step down

                  if not Is_Nil (Next_Child) then
                     Check_Possible_Enclosing (Next_Child);

                     if Result_Found then

                        return;
                     end if;

                  end if;

               when Element_List_Query =>

                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List (Appr_Enclosing);
                  begin
                     --  and here - recursively one step down
                     Check_List_Down (Child_List);

                     if Result_Found then

                        return;
                     end if;

                  end;
               when Element_List_Query_With_Boolean =>

                  declare
                     Child_List : constant Asis.Element_List :=
                        Child_Access (Each_Query).Func_List_Boolean
                           (Appr_Enclosing, Child_Access (Each_Query).Bool);
                  begin
                     --  and here - recursively one step down
                     Check_List_Down (Child_List);

                     if Result_Found then

                        return;
                     end if;

                  end;

            end case;
         end loop;
      end Check_Possible_Enclosing;

   begin  -- Get_Enclosing
      Check_Possible_Enclosing (Approximation);
      pragma Assert (Result_Found);

      return Result_Element;

   end Get_Enclosing;

   ------------------------------
   -- Get_Rough_Enclosing_Node --
   ------------------------------

   function Get_Rough_Enclosing_Node (Element : Asis.Element) return Node_Id
   is
      Arg_Node    : constant Node_Id := R_Node (Element);
      Result_Node : Node_Id;
      Res_Nkind   : Node_Kind;

      function Is_Acceptable_As_Rough_Enclosing_Node
        (N : Node_Id)
         return Boolean;
      --  this function encapsulates the condition for choosing
      --  the rough enclosing node

      function Is_Acceptable_Impl_Neq_Decl (N : Node_Id) return Boolean;
      --  Implements a special check for Is_Acceptable_As_Rough_Enclosing_Node:
      --  in case if Element is a subcomponenet of an implicit declaration of
      --  "/=", checks that N represents the whole declaration of this "/="

      -------------------------------------------
      -- Is_Acceptable_As_Rough_Enclosing_Node --
      -------------------------------------------

      function Is_Acceptable_As_Rough_Enclosing_Node
        (N : Node_Id)
         return Boolean
      is
         N_K    : constant Node_Kind := Nkind (N);
         Result : Boolean            := True;
      begin

         if not (Is_Acceptable_Impl_Neq_Decl (N)
              or else
                 Is_List_Member (N)
              or else
                (Nkind (Parent (N)) = N_Compilation_Unit or else
                 Nkind (Parent (N)) = N_Subunit))
            or else
              (Nkind (N) in N_Subexpr
              and then Nkind (N) /=
               N_Procedure_Call_Statement)
            or else
              Nkind (N) =  N_Parameter_Association
         then

            Result := False;

         elsif N_K = N_Range                 or else
--               N_K = N_Component_Association or else
               N_K = N_Subtype_Indication
         then
            Result := False;

         elsif N_K = N_Component_Association then
            if Special_Case (Element) = Is_From_Gen_Association
              or else
               Is_From_Rewritten_Aggregate (N)
            then
               Result := False;
            end if;
         elsif N_K = N_Procedure_Call_Statement and then
               Nkind (Parent (N)) = N_Pragma
         then
            Result := False;

         elsif not Comes_From_Source (N)
             and then
               Sloc (N) > Standard_Location
             and then
               not Is_Acceptable_Impl_Neq_Decl (N)
         then

            if not (Is_From_Instance (Element)
                and then
                    Is_Top_Of_Expanded_Generic (N))
            then
               Result := False;
            end if;

         end if;

         return Result;

      end Is_Acceptable_As_Rough_Enclosing_Node;

      ---------------------------------
      -- Is_Acceptable_Impl_Neq_Decl --
      ---------------------------------

      function Is_Acceptable_Impl_Neq_Decl (N : Node_Id) return Boolean is
         Result : Boolean := False;
      begin

         if Special_Case (Element) = Is_From_Imp_Neq_Declaration
           and then
            Nkind (N) = N_Subprogram_Declaration
           and then
            not Comes_From_Source (N)
           and then
            Present (Corresponding_Equality
                        (Defining_Unit_Name (Specification (N))))
         then
            Result := True;
         end if;

         return Result;
      end Is_Acceptable_Impl_Neq_Decl;

   begin  --  Get_Rough_Enclosing_Node
      Result_Node := Parent (Arg_Node);

      if Nkind (Result_Node) = N_Object_Renaming_Declaration
        and then
         Special_Case (Element) = Is_From_Gen_Association
        and then
         Present (Corresponding_Generic_Association (Result_Node))
      then
         Result_Node := Corresponding_Generic_Association (Result_Node);
      elsif (Nkind (Result_Node) = N_Attribute_Definition_Clause
           or else
             Nkind (Result_Node) = N_Pragma)
         and then
            From_Aspect_Specification (Result_Node)
      then
         --           Result_Node := Corresponding_Aspect (Result_Node);
         null;  -- SCz
      end if;

      while Present (Result_Node) and then
            not Is_Acceptable_As_Rough_Enclosing_Node (Result_Node)
      loop
         Result_Node := Parent (Result_Node);

         if Nkind (Result_Node) = N_Object_Renaming_Declaration
           and then
            Special_Case (Element) = Is_From_Gen_Association
           and then
            Present (Corresponding_Generic_Association (Result_Node))
         then
            Result_Node := Corresponding_Generic_Association (Result_Node);
         elsif (Nkind (Result_Node) = N_Attribute_Definition_Clause
              or else
                Nkind (Result_Node) = N_Pragma)
            and then
               From_Aspect_Specification (Result_Node)
         then
            --              Result_Node := Corresponding_Aspect (Result_Node);
            null; -- SCz
         end if;

         if Nkind (Result_Node) = N_Compilation_Unit then
            --  this means that there is no node list on the way up
            --  the tree, and we have to go back to the node
            --  for the unit declaration:
            if Is_Standard (Encl_Unit (Element)) then
               Result_Node := Standard_Package_Node;
            else
               Result_Node := Unit (Result_Node);
            end if;

            if Nkind (Result_Node) = N_Subunit then
               Result_Node := Proper_Body (Result_Node);
            end if;

            exit;
         end if;

      end loop;

      --  and here we have to take into account possible normalization
      --  of multi-identifier declarations:
      Res_Nkind := Nkind (Result_Node);

      if Res_Nkind = N_Object_Declaration         or else
         Res_Nkind = N_Number_Declaration         or else
         Res_Nkind = N_Discriminant_Specification or else
         Res_Nkind = N_Component_Declaration      or else
         Res_Nkind = N_Parameter_Specification    or else
         Res_Nkind = N_Exception_Declaration      or else
         Res_Nkind = N_Formal_Object_Declaration  or else
         Res_Nkind = N_With_Clause
      then
         Skip_Normalized_Declarations_Back (Result_Node);
      end if;

      --  If we've got Result_Node pointing to the artificial package
      --  declaration created for library-level generic instantiation,
      --  we have to the body for which we have this instantiation as
      --  the original node

      if Nkind  (Result_Node) = N_Package_Declaration      and then
         not Comes_From_Source (Result_Node)               and then
         Nkind (Parent (Result_Node)) = N_Compilation_Unit and then
         not Is_From_Instance (Element)                    and then
         not Is_Rewrite_Substitution (Result_Node)
      then
         Result_Node := Corresponding_Body (Result_Node);

         while Nkind (Result_Node) /= N_Package_Body loop
            Result_Node := Parent (Result_Node);
         end loop;

      end if;

      --  Below is the patch for 8706-003. It is needed when we are looking
      --  for the enclosing element for actual parameter in subprogram
      --  instantiation. In this case Result_Node points to the spec of a
      --  wrapper package, so we have to go to the instantiation.

      if Special_Case (Element) = Is_From_Gen_Association
        and then
         Nkind (Result_Node) = N_Package_Declaration
        and then
          not (Nkind (Original_Node (Result_Node)) = N_Package_Instantiation
           or else
               Nkind (Original_Node (Result_Node)) = N_Package_Body
           or else
               (Present (Generic_Parent (Specification (Result_Node)))
               and then
                Ekind (Generic_Parent (Specification (Result_Node))) =
                 E_Generic_Package))
        and then
         not Comes_From_Source (Result_Node)
        and then
         (Nkind (Parent (Arg_Node)) = N_Subprogram_Renaming_Declaration
        and then
         not Comes_From_Source (Parent (Arg_Node)))
        and then
         Instantiation_Depth (Sloc (Result_Node)) =
         Instantiation_Depth (Sloc (Arg_Node))
      then

         if Is_Rewrite_Substitution (Result_Node)
           and then
            Nkind (Original_Node (Result_Node)) in N_Generic_Instantiation
         then
            Result_Node := Original_Node (Result_Node);
         else

            while not Comes_From_Source (Result_Node) loop
               Result_Node := Next_Non_Pragma (Result_Node);
            end loop;

         end if;

      end if;

      return Result_Node;

   end Get_Rough_Enclosing_Node;

   --------------------------------
   -- Is_Top_Of_Expanded_Generic --
   --------------------------------

   function Is_Top_Of_Expanded_Generic (N : Node_Id) return Boolean is
      N_Kind : constant Node_Kind := Nkind (N);
      Result : Boolean            := False;
   begin

      Result :=

         ((not Comes_From_Source (N) or else
           Is_Rewrite_Insertion (N))
         and then
          (N_Kind = N_Package_Declaration    or else
           N_Kind = N_Package_Body           or else
           N_Kind = N_Subprogram_Declaration or else
           N_Kind = N_Subprogram_Body)
         and then
           Nkind (Original_Node (N)) not in  N_Renaming_Declaration)
       or else

         (Nkind (Parent (N)) = N_Package_Body and then
          not Comes_From_Source (Parent (N)))

        or else

         (Is_Rewrite_Substitution (N) and then
          Nkind (Original_Node (N)) = N_Package_Instantiation);
      --  Library-level package instantiation

      return Result;

   end Is_Top_Of_Expanded_Generic;

   --------------------------
   -- No_Enclosing_Element --
   --------------------------

   procedure No_Enclosing_Element (Element_Kind : Internal_Element_Kinds) is
   begin
      Raise_ASIS_Failed
        ("No Enclosing Element can correspond " &
         "to the Element with Internal_Element_Kinds value of " &
          Internal_Element_Kinds'Image (Element_Kind));
   end No_Enclosing_Element;

   ----------------------------------------------------
   -- Not_Implemented_Enclosing_Element_Construction --
   ----------------------------------------------------

   procedure Not_Implemented_Enclosing_Element_Construction
      (Element : Asis.Element)
   is
   begin
      Not_Implemented_Yet (Diagnosis =>
                        "Enclosing Element retrieval for the explicit Element "
                      &  "of the " &  Internal_Element_Kinds'Image
                      (Int_Kind (Element)) & " kind "
                      & "has not been implemented yet");
   end Not_Implemented_Enclosing_Element_Construction;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Node_Id) return Node_Id is
      Result_Node : Node_Id;
   begin
      Result_Node := Atree.Parent (Node);
      Skip_Normalized_Declarations_Back (Result_Node);
      return Result_Node;
   end Parent;

   ---------------------------
   -- Skip_Implicit_Subtype --
   ---------------------------

   procedure Skip_Implicit_Subtype (Constr : in out Node_Id) is
   begin

      if not Comes_From_Source (Parent (Constr)) then

         Constr := Parent (Constr);

         while Nkind (Constr) /= N_Object_Declaration loop
            Constr := Next_Non_Pragma (Constr);
         end loop;

         Constr := Object_Definition (Constr);

      end if;
   end Skip_Implicit_Subtype;

   ---------------------------------------
   -- Skip_Normalized_Declarations_Back --
   ---------------------------------------

   procedure Skip_Normalized_Declarations_Back (Node : in out Node_Id) is
      Arg_Kind : constant Node_Kind := Nkind (Node);
   begin
      loop
         if Arg_Kind = N_Object_Declaration         or else
            Arg_Kind = N_Number_Declaration         or else
            Arg_Kind = N_Discriminant_Specification or else
            Arg_Kind = N_Component_Declaration      or else
            Arg_Kind = N_Parameter_Specification    or else
            Arg_Kind = N_Exception_Declaration      or else
            Arg_Kind = N_Formal_Object_Declaration
         then

            if Prev_Ids (Node) then
               Node := Prev (Node);

               while Nkind (Node) /= Arg_Kind loop
                  --  some implicit subtype declarations may be inserted by
                  --  the compiler in between the normalized declarations, so:
                  Node := Prev (Node);
               end loop;

            else

               return;
            end if;
         elsif Arg_Kind = N_With_Clause then

            if First_Name (Node) then

               return;
            else
               Node := Prev (Node);
            end if;

         else

            return;
            --  nothing to do!
         end if;

      end loop;

   end Skip_Normalized_Declarations_Back;

end A4G.Encl_El;
