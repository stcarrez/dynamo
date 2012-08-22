------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--             A S I S . E X T E N S I O N S . F L A T _ K I N D S          --
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

--  This package provides the "flat" Element classification as a part of ASIS
--  extensions provided by ASIS-for-GNAT. This classification is equivalent
--  to the element classification hierarchy defined in the Asis package.

with A4G.Int_Knds;

package Asis.Extensions.Flat_Kinds is

   type Flat_Element_Kinds is new A4G.Int_Knds.Internal_Element_Kinds
      range Not_An_Element .. An_Exception_Handler;
   --  This type provides the flat Element classification for using in ASIS
   --  applications. The main reasons to define it as a type derived from
   --  the type providing similar flat classification for implementation
   --  purposes are to hide the implementation-specific values added to
   --  the internal flat classification (they are of no use for applications)
   --  and to simplify the possible maintenance problems by having only one
   --  set of enumeration literals representing the flat Element
   --  classification. The set of enumeration literals inherited by the
   --  Flat_Element_Kinds type is given below as a "commented" copy of
   --  a part of the declaration of the A4G.Int_Knds.Internal_Element_Kinds
   --  type.
   --
   --  The way how the flat classification is obtained from the hierarchy is
   --  strightforward: all the enumeration values of the ASIS Element_Kinds
   --  type, which represent kinds having subordinate subkinds are recursively
   --  replaced with the enumeration values of the subordinate kinds
   --  (excluding Not_A_... starting values). A very few exceptions corresponds
   --  to the situation where in the ASIS-defined Element classification we
   --  have not  hierarchy, but some kind of a net (for example, there is a
   --  classification of access types applied bot to ordinary type definitions
   --  and to formal type definitions. For such cases, the flat classification
   --  introduces new (self-descriptive) names.
   --
   --  In the definition of Internal_Element_Kinds we keep most of the
   --  documentation from the Element classification hierarchy as it is defined
   --  in the Asis package, slightly reformatted to staicfy the GNAT coding
   --  style.
   --
   --  Below is the commented copy of a part of the definition of the
   --  Internal_Element_Kinds type.

   ----------------------------------------------------------
   -- Element_Kinds Hierarchy, as defined by Asis package: --
   ----------------------------------------------------------
   --
   --        Element_Kinds         -> Subordinate Kinds
   ---------------------------------------------------------------------
   --
   --   Key: Read "->" as "can be further categorized by its"
   --
   --        A_Pragma              -> Pragma_Kinds
   --
   --        A_Defining_Name       -> Defining_Name_Kinds
   --                                         -> Operator_Kinds
   --
   --        A_Declaration         -> Declaration_Kinds
   --                                         -> Trait_Kinds
   --                                         -> Declaration_Origin
   --                                         -> Mode_Kinds
   --                                         -> Default_Kinds
   --
   --        A_Definition          -> Definition_Kinds
   --                                         -> Trait_Kinds
   --                                         -> Type_Kinds
   --                                                    -> Trait_Kinds
   --                                         -> Formal_Type_Kinds
   --                                                    -> Trait_Kinds
   --                                         -> Access_Type_Kinds
   --                                         -> Root_Type_Kinds
   --                                         -> Constraint_Kinds
   --                                         -> Discrete_Range_Kinds
   --
   --        An_Expression         -> Expression_Kinds
   --                                         -> Operator_Kinds
   --                                         -> Attribute_Kinds
   --
   --        An_Association        -> Association_Kinds
   --
   --        A_Statement           -> Statement_Kinds
   --
   --        A_Path                -> Path_Kinds
   --
   --        A_Clause              -> Clause_Kinds
   --                                         -> Representation_Clause_Kinds
   --
   --        An_Exception_Handler
   --
   -------------------------------------------------------------------------

   --  type Internal_Element_Kinds is

   --    (Not_An_Element,            -- Nil_Element

      --  All the other Not_A_XXX values for subordinate kinds are not included
      --  in the flat element classification - they just are not needed.

      -------------------------------------------------------------

      --  A_Pragma,                  -- Asis.Elements

      --  Detailed classification for Asis.Element_Kinds (A_Pragma) literal,
      --  corresponds to subtype Internal_Pragma_Kinds
      ------------------------------------------------------------

      --  An_All_Calls_Remote_Pragma,       -- E.2.3(4)
      --  An_Asynchronous_Pragma,           -- E.4.1(2)
      --  An_Atomic_Pragma,                 -- C.6(2)
      --  An_Atomic_Components_Pragma,      -- C.6(2)
      --  An_Attach_Handler_Pragma,         -- C.3.1(3)
      --  A_Controlled_Pragma,              -- 13.11.3(2)
      --  A_Convention_Pragma,              -- B.1(4)
      --  A_Discard_Names_Pragma,           -- C.5(2),
      --  An_Elaborate_Pragma,              -- 10.2.1(19),
      --  An_Elaborate_All_Pragma,          -- 10.2.1(19)
      --  An_Elaborate_Body_Pragma,         -- 10.2.1(19)
      --  An_Export_Pragma,                 -- B.1(4)
      --  An_Import_Pragma,                 -- B.1(4)
      --  An_Inline_Pragma,                 -- 6.3.2(2)
      --  An_Inspection_Point_Pragma,       -- H.3.2(2)
      --  An_Interrupt_Handler_Pragma,      -- C.3.1(1)
      --  An_Interrupt_Priority_Pragma,     -- D.1(4)
      --  A_Linker_Options_Pragma,          -- B.1(4),
      --  A_List_Pragma,                    -- 2.8(20)
      --  A_Locking_Policy_Pragma,          -- D.3(2)
      --  A_Normalize_Scalars_Pragma,       -- H.1(2)
      --  An_Optimize_Pragma,               -- 2.8(20)
      --  A_Pack_Pragma,                    -- 13.2(2)
      --  A_Page_Pragma,                    -- 2.8(20)
      --  A_Preelaborate_Pragma,            -- 10.2.1(2)
      --  A_Priority_Pragma,                -- D.1(2)
      --  A_Pure_Pragma,                    -- 10.2.1(13)
      --  A_Queuing_Policy_Pragma,          -- D.4(2)
      --  A_Remote_Call_Interface_Pragma,   -- E.2.3(2)
      --  A_Remote_Types_Pragma,            -- E.2.2(2)
      --  A_Restrictions_Pragma,            -- 13.12(2)
      --  A_Reviewable_Pragma,              -- H.3.1(2)
      --  A_Shared_Passive_Pragma,          -- E.2.1(2)
      --  A_Storage_Size_Pragma,            -- 13.3(62),
      --  A_Suppress_Pragma,                -- 11.5(3)
      --  A_Task_Dispatching_Policy_Pragma, -- D.2.2(2)
      --  A_Volatile_Pragma,                -- C.6(2)
      --  A_Volatile_Components_Pragma,     -- C.6(2)
      --
      --  An_Implementation_Defined_Pragma, -- Vendor Appendix M

      --  An_Unknown_Pragma,                -- Unknown to the compiler.

   ------------------------------------------------------------

   --  A_Defining_Name,           -- Asis.Declarations

   --  Detailed classification for Asis.Element_Kinds (A_Defining_Name)
   --  literal, corresponds to subtype Internal_Defining_Name_Kinds
   ------------------------------------------------------------

      --  A_Defining_Identifier,                     -- 3.1
      --  A_Defining_Character_Literal,              -- 3.5.1
      --  A_Defining_Enumeration_Literal,            -- 3.5.1

      --  A_Defining_Operator_Symbol

      --  Detailed classification for
      --  Asis.Operator_Kinds (A_Defining_Operator_Symbol) literal,
      --  corresponds to subtype Internal_Defining_Operator_Kinds

      --  A_Defining_And_Operator,                    -- and
      --  A_Defining_Or_Operator,                     -- or
      --  A_Defining_Xor_Operator,                    -- xor
      --  A_Defining_Equal_Operator,                  -- =
      --  A_Defining_Not_Equal_Operator,              -- /=
      --  A_Defining_Less_Than_Operator,              -- <
      --  A_Defining_Less_Than_Or_Equal_Operator,     -- <=
      --  A_Defining_Greater_Than_Operator,           -- >
      --  A_Defining_Greater_Than_Or_Equal_Operator,  -- >=
      --  A_Defining_Plus_Operator,                   -- +
      --  A_Defining_Minus_Operator,                  -- -
      --  A_Defining_Concatenate_Operator,            -- &
      --  A_Defining_Unary_Plus_Operator,             -- +
      --  A_Defining_Unary_Minus_Operator,            -- -
      --  A_Defining_Multiply_Operator,               -- *
      --  A_Defining_Divide_Operator,                 -- /
      --  A_Defining_Mod_Operator,                    -- mod
      --  A_Defining_Rem_Operator,                    -- rem
      --  A_Defining_Exponentiate_Operator,           -- **
      --  A_Defining_Abs_Operator,                    -- abs
      --  A_Defining_Not_Operator,                    -- not
      --
      --  A_Defining_Expanded_Name,
      --                        6.1 program_unit_name.defining_identifier
      --
      ------------------------------------------------------------

      --  A_Declaration,             -- Asis.Declarations

      --  Detailed classification for
      --  Asis.Element_Kinds (A_Declaration) literal
      --  corresponds to subtype Internal_Declaration_Kinds
      ------------------------------------------------------------

      --  An_Ordinary_Type_Declaration,              -- 3.2.1
      --  A_Task_Type_Declaration,                   -- 3.2.1
      --  A_Protected_Type_Declaration,              -- 3.2.1
      --  An_Incomplete_Type_Declaration,            -- 3.2.1
      --  A_Private_Type_Declaration,                -- 3.2.1
      --  A_Private_Extension_Declaration,           -- 3.2.1
      --
      --  A_Subtype_Declaration,                     -- 3.2.2
      --
      --  A_Variable_Declaration,                    -- 3.3.1 -> Trait_Kinds
      --  A_Constant_Declaration,                    -- 3.3.1 -> Trait_Kinds
      --  A_Deferred_Constant_Declaration,           -- 3.3.1 -> Trait_Kinds
      --  A_Single_Task_Declaration,                 -- 3.3.1
      --  A_Single_Protected_Declaration,            -- 3.3.1
      --
      --  An_Integer_Number_Declaration,             -- 3.3.2
      --  A_Real_Number_Declaration,                 -- 3.3.2
      --
      --  An_Enumeration_Literal_Specification,      -- 3.5.1
      --
      --  A_Discriminant_Specification,              -- 3.7   -> Trait_Kinds
      --
      --  A_Component_Declaration,                   -- 3.8
      --
      --  A_Loop_Parameter_Specification,            -- 5.5   -> Trait_Kinds
      --  A_Generalized_Iterator_Specification,      -- 5.5.2 -> Trait_Kinds
      --  An_Element_Iterator_Specification,         -- 5.5.2 -> Trait_Kinds
      --
      --  A_Procedure_Declaration,                   -- 6.1   -> Trait_Kinds
      --  A_Function_Declaration,                    -- 6.1   -> Trait_Kinds
      --
      --  A_Parameter_Specification,                 -- 6.1   -> Trait_Kinds
      --  --                                                  -> Mode_Kinds
      --
      --  A_Procedure_Body_Declaration,              -- 6.3
      --  A_Function_Body_Declaration,               -- 6.3
      --
      --  A_Package_Declaration,                     -- 7.1
      --  A_Package_Body_Declaration,                -- 7.2
      --
      --  An_Object_Renaming_Declaration,            -- 8.5.1
      --  An_Exception_Renaming_Declaration,         -- 8.5.2
      --  A_Package_Renaming_Declaration,            -- 8.5.3
      --  A_Procedure_Renaming_Declaration,          -- 8.5.4
      --  A_Function_Renaming_Declaration,           -- 8.5.4
      --  A_Generic_Package_Renaming_Declaration,    -- 8.5.5
      --  A_Generic_Procedure_Renaming_Declaration,  -- 8.5.5
      --  A_Generic_Function_Renaming_Declaration,   -- 8.5.5
      --
      --  A_Task_Body_Declaration,                   -- 9.1
      --  A_Protected_Body_Declaration,              -- 9.4
      --
      --  An_Entry_Declaration,                      -- 9.5.2
      --  An_Entry_Body_Declaration,                 -- 9.5.2
      --  An_Entry_Index_Specification,              -- 9.5.2
      --
      --  A_Procedure_Body_Stub,                     -- 10.1.3
      --  A_Function_Body_Stub,                      -- 10.1.3
      --  A_Package_Body_Stub,                       -- 10.1.3
      --  A_Task_Body_Stub,                          -- 10.1.3
      --  A_Protected_Body_Stub,                     -- 10.1.3
      --
      --  An_Exception_Declaration,                  -- 11.1
      --  A_Choice_Parameter_Specification,          -- 11.2
      --
      --  A_Generic_Procedure_Declaration,           -- 12.1
      --  A_Generic_Function_Declaration,            -- 12.1
      --  A_Generic_Package_Declaration,             -- 12.1
      --
      --  A_Package_Instantiation,                   -- 12.3
      --  A_Procedure_Instantiation,                 -- 12.3
      --  A_Function_Instantiation,                  -- 12.3
      --
      --  A_Formal_Object_Declaration,               -- 12.4  -> Mode_Kinds
      --  A_Formal_Type_Declaration,                 -- 12.5
      --  A_Formal_Procedure_Declaration,            -- 12.6  -> Default_Kinds
      --  A_Formal_Function_Declaration,             -- 12.6  -> Default_Kinds
      --  A_Formal_Package_Declaration,              -- 12.7
      --  A_Formal_Package_Declaration_With_Box,     -- 12.7
      --
      ------------------------------------------------------------

      --  A_Definition,              --  Asis.Definitions
      --
      --  Detailed classification for Asis.Element_Kinds (A_Definition)
      --  literal, corresponds to subtype Internal_Definition_Kinds
      ------------------------------------------------------------

      --  A_Type_Definition,                -- 3.2.1   -> Type_Kinds

      --  Detailed classification for Asis.Definition_Kinds (A_Type_Definition)
      --  literal, corresponds to subtype Internal_Type_Kinds

      --  A_Derived_Type_Definition,             -- 3.4    -> Trait_Kinds
      --  A_Derived_Record_Extension_Definition, -- 3.4    -> Trait_Kinds
      --
      --  An_Enumeration_Type_Definition,        -- 3.5.1
      --
      --  A_Signed_Integer_Type_Definition,      -- 3.5.4
      --  A_Modular_Type_Definition,             -- 3.5.4
      --
   --     A_Root_Type_Definition,                -- 3.5.4(10), 3.5.6(4)
   --                                                      -> Root_Type_Kinds
   --  Detailed classification for Asis.Type_Kinds (A_Root_Type_Definition)
   --  literal, corresponds to subtype Internal_Root_Type_Kinds

      --  A_Root_Integer_Definition,             -- 3.5.4(9)
      --  A_Root_Real_Definition,                -- 3.5.6(2)
      --
      --  A_Universal_Integer_Definition,        -- 3.5.4(10)
      --  A_Universal_Real_Definition,           -- 3.5.6(4)
      --  A_Universal_Fixed_Definition,          -- 3.5.6(4)
      --
      --
      --  A_Floating_Point_Definition,           -- 3.5.7
      --
      --  An_Ordinary_Fixed_Point_Definition,    -- 3.5.9
      --  A_Decimal_Fixed_Point_Definition,      -- 3.5.9
      --
      --  An_Unconstrained_Array_Definition,     -- 3.6
      --  A_Constrained_Array_Definition,        -- 3.6
      --
      --  A_Record_Type_Definition,              -- 3.8    -> Trait_Kinds
      --  A_Tagged_Record_Type_Definition,       -- 3.8    -> Trait_Kinds

   --     An_Access_Type_Definition,             -- 3.10   -> Access_Type_Kinds

   --  Detailed classification for
   --  Asis.Type_Kinds (An_Access_Type_Definition) literal,
   --  corresponds to subtype Internal_Access_Type_Kinds

      --  A_Pool_Specific_Access_To_Variable, -- access subtype_indication
      --  An_Access_To_Variable,              -- access all subtype_indication
      --  An_Access_To_Constant,          -- access constant subtype_indication
      --
      --  An_Access_To_Procedure,             -- access procedure
      --  An_Access_To_Protected_Procedure,   -- access protected procedure
      --  An_Access_To_Function,              -- access function
      --  An_Access_To_Protected_Function,    -- access protected function

      --  A_Subtype_Indication,             -- 3.2.2

   --  A_Constraint,                     -- 3.2.2   -> Constraint_Kinds

   --  Detailed classification for Asis.Definition_Kinds (A_Constraint)
   --  literal, corresponds to subtype Internal_Constraint_Kinds

      --  A_Range_Attribute_Reference,           -- 3.2.2, 3.5
      --  A_Simple_Expression_Range,             -- 3.2.2, 3.5
      --  A_Digits_Constraint,                   -- 3.2.2, 3.5.9
      --  A_Delta_Constraint,                    -- 3.2.2, N.3
      --  An_Index_Constraint,                   -- 3.2.2, 3.6.1
      --  A_Discriminant_Constraint,            -- 3.2.2
      --
      --  A_Component_Definition,           -- 3.6

      --  A_Discrete_Subtype_Definition,    -- 3.6     -> Discrete_Range_Kinds

      --  Detailed classification for
      --  Asis.Definition_Kinds (A_Discrete_Subtype_Definition) literal,
      --  corresponds to subtype Internal_Discrete_Subtype_Definition_Kinds

      --  A_Discrete_Subtype_Indication_As_Subtype_Definition, -- 3.6.1, 3.2.2
      --  A_Discrete_Range_Attribute_Reference_As_Subtype_Definition,
      --                                                          -- 3.6.1, 3.5
      --  A_Discrete_Simple_Expression_Range_As_Subtype_Definition,
      --                                                          -- 3.6.1, 3.5

      --  There is no syntactical difference between
      --  A_Discrete_Subtype_Definition kinds and A_Discrete_Range kinds, the
      --  difference is in the context in which they occur - the first defines
      --  subtype in an array type definition, the second should corresponds to
      --  some existing subtype in a range constraint
      --
      --  This is the case when in the ASIS-defined Element classification
      --  we have a net instead of a hierarchy

      --  A_Discrete_Range,                 -- 3.6.1   -> Discrete_Range_Kinds

      --  Detailed classification for Asis.Definition_Kinds (A_Discrete_Range)
      --  literal, corresponds to subtype Internal_Discrete_Range_Kinds

      --  A_Discrete_Subtype_Indication,         -- 3.6.1, 3.2.2
      --  A_Discrete_Range_Attribute_Reference,  -- 3.6.1, 3.5
      --  A_Discrete_Simple_Expression_Range,    -- 3.6.1, 3.5
      --
      --
      --  An_Unknown_Discriminant_Part,     -- 3.7
      --  A_Known_Discriminant_Part,        -- 3.7
      --
      --  A_Record_Definition,              -- 3.8
      --  A_Null_Record_Definition,         -- 3.8
      --
      --  A_Null_Component,                 -- 3.8
      --  A_Variant_Part,                   -- 3.8
      --  A_Variant,                        -- 3.8
      --
      --  An_Others_Choice,                 -- 3.8.1, 4.3.1, 4.3.3, 11.2
      --
      --  A_Private_Type_Definition,        -- 7.3     -> Trait_Kinds
      --  A_Tagged_Private_Type_Definition, -- 7.3     -> Trait_Kinds
      --  A_Private_Extension_Definition,   -- 7.3     -> Trait_Kinds
      --
      --  A_Task_Definition,                -- 9.1
      --  A_Protected_Definition,           -- 9.4
      --
      --  A_Formal_Type_Definition,         -- 12.5    -> Formal_Type_Kinds

      --  Detailed classification for
      --  Asis.Definition_Kinds (A_Formal_Type_Definition) literal,
      --  corresponds to subtype Internal_Formal_Type_Kinds

      --  A_Formal_Private_Type_Definition,         -- 12.5.1  -> Trait_Kinds
      --  A_Formal_Tagged_Private_Type_Definition,  -- 12.5.1  -> Trait_Kinds
      --
      --  A_Formal_Derived_Type_Definition,         -- 12.5.1  -> Trait_Kinds
      --
      --  A_Formal_Discrete_Type_Definition,        -- 12.5.2
      --
      --  A_Formal_Signed_Integer_Type_Definition,  -- 12.5.2
      --  A_Formal_Modular_Type_Definition,         -- 12.5.2
      --
      --  A_Formal_Floating_Point_Definition,       -- 12.5.2
      --
      --  A_Formal_Ordinary_Fixed_Point_Definition, -- 12.5.2
      --  A_Formal_Decimal_Fixed_Point_Definition,  -- 12.5.2
      --
      --  A_Formal_Unconstrained_Array_Definition,  -- 12.5.3
      --  A_Formal_Constrained_Array_Definition,    -- 12.5.3

      --   A_Formal_Access_Type_Definition,     -- 12.5.4  -> Access_Type_Kinds

      --  Detailed classification for
      --  Asis.Definition_Kinds.Internal_Formal_Type_Kinds
      --  (A_Formal_Access_Type_Definition) literal,
      --  corresponds to subtype Internal_Formal_Access_Type_Kinds

      --  A_Formal_Pool_Specific_Access_To_Variable,
      --                                    -- access subtype_indication
      --  A_Formal_Access_To_Variable,        -- access all subtype_indication
      --  A_Formal_Access_To_Constant,   -- access constant subtype_indication
      --
      --  A_Formal_Access_To_Procedure,              -- access procedure
      --  A_Formal_Access_To_Protected_Procedure, -- access protected procedure
      --  A_Formal_Access_To_Function,               -- access function
      --  A_Formal_Access_To_Protected_Function, -- access protected function
      --  An_Aspect_Specification, --  Ada 2012

      ------------------------------------------------------------

      --  An_Expression,             -- Asis.Expressions

      --  Detailed classification for Asis.Element_Kinds (An_Expression)
      --  literal corresponds to subtype Internal_Expression_Kinds
      ------------------------------------------------------------

      --  An_Integer_Literal,                        -- 2.4
      --  A_Real_Literal,                            -- 2.4.1
      --  A_String_Literal,                          -- 2.6

      --  An_Identifier,                             -- 4.1

      --  An_Operator_Symbol,                        -- 4.1

      --  Detailed classification for
      --  Asis.Expression_Kinds (An_Operator_Symbol) literal
      --  corresponds to subtype Internal_Operator_Symbol_Kinds

      --  An_And_Operator,                     -- and
      --  An_Or_Operator,                      -- or
      --  An_Xor_Operator,                     -- xor
      --  An_Equal_Operator,                   -- =
      --  A_Not_Equal_Operator,                -- /=
      --  A_Less_Than_Operator,                -- <
      --  A_Less_Than_Or_Equal_Operator,       -- <=
      --  A_Greater_Than_Operator,             -- >
      --  A_Greater_Than_Or_Equal_Operator,    -- >=
      --  A_Plus_Operator,                     -- +
      --  A_Minus_Operator,                    -- -
      --  A_Concatenate_Operator,              -- &
      --  A_Unary_Plus_Operator,               -- +
      --  A_Unary_Minus_Operator,              -- -
      --  A_Multiply_Operator,                 -- *
      --  A_Divide_Operator,                   -- /
      --  A_Mod_Operator,                      -- mod
      --  A_Rem_Operator,                      -- rem
      --  An_Exponentiate_Operator,            -- **
      --  An_Abs_Operator,                     -- abs
      --  A_Not_Operator,                      -- not
      --
      --  A_Character_Literal,                       -- 4.1
      --  An_Enumeration_Literal,                    -- 4.1
      --  An_Explicit_Dereference,                   -- 4.1
      --  A_Function_Call,                           -- 4.1
      --
      --  An_Indexed_Component,                      -- 4.1.1
      --  A_Slice,                                   -- 4.1.2
      --  A_Selected_Component,                      -- 4.1.3
      --
      --  An_Attribute_Reference,                    -- 4.1.4

      --  Detailed classification for
      --  Asis.Expression_Kinds (An_Attribute_Reference) literal
      --  corresponds to subtype Internal_Attribute_Reference_Kinds

      --  An_Access_Attribute,           -- 3.10.2(24), 3.10.2(32), K(2), K(4)
      --  An_Address_Attribute,          -- 13.3(11), J.7.1(5), K(6)
      --  An_Adjacent_Attribute,         -- A.5.3(48), K(8)
      --  An_Aft_Attribute,              -- 3.5.10(5), K(12)
      --  An_Alignment_Attribute,        -- 13.3(23), K(14)
      --  A_Base_Attribute,              -- 3.5(15), K(17)
      --  A_Bit_Order_Attribute,         -- 13.5.3(4), K(19)
      --  A_Body_Version_Attribute,      -- E.3(4), K(21)
      --  A_Callable_Attribute,          -- 9.9(2), K(23)
      --  A_Caller_Attribute,            -- C.7.1(14), K(25)
      --  A_Ceiling_Attribute,           -- A.5.3(33), K(27)
      --  A_Class_Attribute,             -- 3.9(14), 7.3.1(9), K(31), K(34)
      --  A_Component_Size_Attribute,    -- 13.3(69), K(36)
      --  A_Compose_Attribute,           -- A.5.3(24), K(38)
      --  A_Constrained_Attribute,       -- 3.7.2(3), J.4(2), K(42)
      --  A_Copy_Sign_Attribute,         -- A.5.3(51), K(44)
      --  A_Count_Attribute,             -- 9.9(5), K(48)
      --  A_Definite_Attribute,          -- 12.5.1(23), K(50)
      --  A_Delta_Attribute,             -- 3.5.10(3), K(52)
      --  A_Denorm_Attribute,            -- A.5.3(9), K(54)
      --  A_Digits_Attribute,            -- 3.5.8(2), 3.5.10(7), K(56), K(58)
      --  An_Exponent_Attribute,         -- A.5.3(18), K(60)
      --  An_External_Tag_Attribute,     -- 13.3(75), K(64)
      --  A_First_Attribute,             -- 3.5(12), 3.6.2(3), K(68), K(70)
      --  A_First_Bit_Attribute,         -- 13.5.2(3), K(72)
      --  A_Floor_Attribute,             -- A.5.3(30), K(74)
      --  A_Fore_Attribute,              -- 3.5.10(4), K(78)
      --  A_Fraction_Attribute,          -- A.5.3(21), K(80)
      --  An_Identity_Attribute,         -- 11.4.1(9), C.7.1(12), K(84), K(86)
      --  An_Image_Attribute,            -- 3.5(35), K(88)
      --  An_Input_Attribute,        -- 13.13.2(22), 13.13.2(32), K(92), K(96)
      --  A_Last_Attribute,              -- 3.5(13), 3.6.2(5), K(102), K(104)
      --  A_Last_Bit_Attribute,          -- 13.5.2(4), K(106)
      --  A_Leading_Part_Attribute,      -- A.5.3(54), K(108)
      --  A_Length_Attribute,            -- 3.6.2(9), K(117)
      --  A_Machine_Attribute,           -- A.5.3(60), K(119)
      --  A_Machine_Emax_Attribute,      -- A.5.3(8), K(123)
      --  A_Machine_Emin_Attribute,      -- A.5.3(7), K(125)
      --  A_Machine_Mantissa_Attribute,  -- A.5.3(6), K(127)
      --  A_Machine_Overflows_Attribute, -- A.5.3(12), A.5.4(4), K(129), K(131)
      --  A_Machine_Radix_Attribute,     -- A.5.3(2), A.5.4(2), K(133), K(135)
      --  A_Machine_Rounds_Attribute,    -- A.5.3(11), A.5.4(3), K(137), K(139)
      --  A_Max_Attribute,               -- 3.5(19), K(141)
      --  A_Max_Size_In_Storage_Elements_Attribute, --   13.11.1(3), K(145)
      --  A_Min_Attribute,               -- 3.5(16), K(147)
      --  A_Model_Attribute,             -- A.5.3(68), G.2.2(7), K(151)
      --  A_Model_Emin_Attribute,        -- A.5.3(65), G.2.2(4), K(155)
      --  A_Model_Epsilon_Attribute,     -- A.5.3(66), K(157)
      --  A_Model_Mantissa_Attribute,    -- A.5.3(64), G.2.2(3), K(159)
      --  A_Model_Small_Attribute,       -- A.5.3(67), K(161)
      --  A_Modulus_Attribute,           -- 3.5.4(17), K(163)
      --  An_Output_Attribute,     -- 13.13.2(19), 13.13.2(29), K(165), K(169)
      --  A_Partition_ID_Attribute,      -- E.1(9), K(173)
      --  A_Pos_Attribute,               -- 3.5.5(2), K(175)
      --  A_Position_Attribute,          -- 13.5.2(2), K(179)
      --  A_Pred_Attribute,              -- 3.5(25), K(181)
      --  A_Range_Attribute,             -- 3.5(14), 3.6.2(7), K(187), K(189)
      --  A_Read_Attribute,         -- 13.13.2(6), 13.13.2(14), K(191), K(195)
      --  A_Remainder_Attribute,         -- A.5.3(45), K(199)
      --  A_Round_Attribute,             -- 3.5.10(12), K(203)
      --  A_Rounding_Attribute,          -- A.5.3(36), K(207)
      --  A_Safe_First_Attribute,        -- A.5.3(71), G.2.2(5), K(211)
      --  A_Safe_Last_Attribute,         -- A.5.3(72), G.2.2(6), K(213)
      --  A_Scale_Attribute,             -- 3.5.10(11), K(215)
      --  A_Scaling_Attribute,           -- A.5.3(27), K(217)
      --  A_Signed_Zeros_Attribute,      -- A.5.3(13), K(221)
      --  A_Size_Attribute,              -- 13.3(40), 13.3(45), K(223), K(228)
      --  A_Small_Attribute,             -- 3.5.10(2), K(230)
      --  A_Storage_Pool_Attribute,      -- 13.11(13), K(232)
      --  A_Storage_Size_Attribute,     -- 13.3(60), 13.11(14), J.9(2), K(234),
      --  --                                K(236)
      --  A_Succ_Attribute,              -- 3.5(22), K(238)
      --  A_Tag_Attribute,               -- 3.9(16), 3.9(18), K(242), K(244)
      --  A_Terminated_Attribute,        -- 9.9(3), K(246)
      --  A_Truncation_Attribute,        -- A.5.3(42), K(248)
      --  An_Unbiased_Rounding_Attribute, -- A.5.3(39), K(252)
      --  An_Unchecked_Access_Attribute, -- 13.10(3), H.4(18), K(256)
      --  A_Val_Attribute,               -- 3.5.5(5), K(258)
      --  A_Valid_Attribute,             -- 13.9.2(3), H(6), K(262)
      --  A_Value_Attribute,             -- 3.5(52), K(264)
      --  A_Version_Attribute,           -- E.3(3), K(268)
      --  A_Wide_Image_Attribute,        -- 3.5(28), K(270)
      --  A_Wide_Value_Attribute,        -- 3.5(40), K(274)
      --  A_Wide_Width_Attribute,        -- 3.5(38), K(278)
      --  A_Width_Attribute,             -- 3.5(39), K(280)
      --  A_Write_Attribute,       -- 13.13.2(3), 13.13.2(11), K(282), K(286)
      --
      --  An_Implementation_Defined_Attribute,  -- Vendor Annex M
      --  An_Unknown_Attribute,
      --
      --  A_Record_Aggregate,                        -- 4.3
      --  An_Extension_Aggregate,                    -- 4.3
      --  A_Positional_Array_Aggregate,              -- 4.3
      --  A_Named_Array_Aggregate,                   -- 4.3
      --
      --  An_And_Then_Short_Circuit,                 -- 4.4
      --  An_Or_Else_Short_Circuit,                  -- 4.4
      --
      --  An_In_Range_Membership_Test,               -- 4.4
      --  A_Not_In_Range_Membership_Test,            -- 4.4
      --  An_In_Type_Membership_Test,                -- 4.4
      --  A_Not_In_Type_Membership_Test,             -- 4.4
      --
      --  A_Null_Literal,                            -- 4.4
      --  A_Parenthesized_Expression,                -- 4.4
      --
      --  A_Type_Conversion,                         -- 4.6
      --  A_Qualified_Expression,                    -- 4.7
      --
      --  An_Allocation_From_Subtype,                -- 4.8
      --  An_Allocation_From_Qualified_Expression,   -- 4.8
      --
      ------------------------------------------------------------

      --  An_Association,            -- Asis.Expressions

      --  Detailed classification for Asis.Element_Kinds (An_Association)
      --  literal corresponds to subtype Internal_Association_Kinds
      ------------------------------------------------------------

      --  A_Pragma_Argument_Association,         -- 2.8
      --  A_Discriminant_Association,            -- 3.7.1
      --  A_Record_Component_Association,        -- 4.3.1
      --  An_Array_Component_Association,        -- 4.3.3
      --  A_Parameter_Association,               -- 6.4
      --  A_Generic_Association,                 -- 12.3
      --
      ------------------------------------------------------------

      --  A_Statement,               -- Asis.Statements

      --  Detailed classification for Asis.Element_Kinds (A_Statement) literal
      --  corresponds to subtype Internal_Statement_Kinds
      ------------------------------------------------------------

      --  A_Null_Statement,                    -- 5.1
      --  An_Assignment_Statement,             -- 5.2
      --  An_If_Statement,                     -- 5.3
      --  A_Case_Statement,                    -- 5.4
      --
      --  A_Loop_Statement,                    -- 5.5
      --  A_While_Loop_Statement,              -- 5.5
      --  A_For_Loop_Statement,                -- 5.5

      --  A_Block_Statement,                   -- 5.6
      --  An_Exit_Statement,                   -- 5.7
      --  A_Goto_Statement,                    -- 5.8
      --
      --  A_Procedure_Call_Statement,          -- 6.4
      --  A_Return_Statement,                  -- 6.5
      --
      --  An_Accept_Statement,                 -- 9.5.2
      --  An_Entry_Call_Statement,             -- 9.5.3
      --
      --  A_Requeue_Statement,                 -- 9.5.4
      --  A_Requeue_Statement_With_Abort,      -- 9.5.4
      --
      --  A_Delay_Until_Statement,             -- 9.6
      --  A_Delay_Relative_Statement,          -- 9.6
      --
      --  A_Terminate_Alternative_Statement,   -- 9.7.1
      --
      --  A_Selective_Accept_Statement,        -- 9.7.2
      --  A_Timed_Entry_Call_Statement,        -- 9.7.3
      --  A_Conditional_Entry_Call_Statement,  -- 9.7.3
      --  An_Asynchronous_Select_Statement,    -- 9.7.4
      --
      --  An_Abort_Statement,                  -- 9.8
      --  A_Raise_Statement,                   -- 11.3
      --  A_Code_Statement,                    -- 13.8

      ------------------------------------------------------------
      --  Path_Kinds
      --  Literals                        -- Ada RM
      --
      --  Detailed classification for Asis.Element_Kinds (A_Path) literal
      --  corresponds to subtype Internal_Path_Kinds
      ------------------------------------------------------------

      --  An_If_Path,                     -- 5.3:
      --                                 if condition then
      --                                   sequence_of_statements

      --  An_Elsif_Path,                  -- 5.3:
      --                                 elsif condition then
      --                                   sequence_of_statements

      --  An_Else_Path,                   -- 5.3, 9.7.1, 9.7.3:
      --                                 else sequence_of_statements

      --  A_Case_Path,                    -- 5.4:
      --                                 when discrete_choice_list =>
      --                                   sequence_of_statements

      --  A_Select_Path,                  -- 9.7.1:
      --                                 select [guard] select_alternative
      --                                 9.7.2, 9.7.3:
      --                                 select entry_call_alternative
      --                                 9.7.4:
      --                                 select triggering_alternative

      --  An_Or_Path,                     -- 9.7.1:
      --                                 or [guard] select_alternative
      --                                 9.7.2:
      --                                 or delay_alternative

      --  A_Then_Abort_Path,              -- 9.7.4
      --                                 then abort sequence_of_statements

      ------------------------------------------------------------

      --  A_Clause,                  -- Asis.Clauses

      --  Detailed classification for Asis.Element_Kinds (A_Clause) literal
      --  corresponds to subtype Internal_Clause_Kinds
      ------------------------------------------------------------

      --  A_Use_Package_Clause,           -- 8.4
      --  A_Use_Type_Clause,              -- 8.4
      --  A_With_Clause,                  -- 10.1.2

      --  A_Representation_Clause,  -- 13.1     -> Representation_Clause_Kinds

      --  Detailed classification for
      --  Asis.Clause_Kinds (A_Representation_Clause) literal,
      --  corresponds to subtype Internal_Representation_Clause_Kinds

      --  An_Attribute_Definition_Clause,           -- 13.3.1
      --  An_Enumeration_Representation_Clause,     -- 13.4
      --  A_Record_Representation_Clause,           -- 13.5.3
      --  An_At_Clause,                             -- N.7
      --
      --  A_Component_Clause,             -- 13.5.3
      --
      ------------------------------------------------------------

      --  An_Exception_Handler,    -- Asis.Statements

      --  The rest of the A4G.Int_Knds.Internal_Element_Kinds values are the
      --  special values added the ASIS implementation needs.

   --  The subtype definitions below define ranges corresponding to the
   --  positions (subordinate kinds) in the original Element kinds
   --  classification hierarchy defined in the Asis package, as well as
   --  some other ranges which may be useful for applications

   subtype Flat_Pragma_Kinds is Flat_Element_Kinds
      range An_All_Calls_Remote_Pragma .. An_Unknown_Pragma;

   subtype Flat_Defining_Name_Kinds is Flat_Element_Kinds
      range A_Defining_Identifier .. A_Defining_Expanded_Name;

   subtype Flat_Defining_Operator_Kinds is Flat_Defining_Name_Kinds
      range A_Defining_And_Operator .. A_Defining_Not_Operator;

   subtype Flat_Declaration_Kinds is Flat_Element_Kinds range
      An_Ordinary_Type_Declaration .. A_Formal_Package_Declaration_With_Box;

   subtype A_Flat_Type_Declaration is Flat_Declaration_Kinds
      range An_Ordinary_Type_Declaration .. A_Private_Extension_Declaration;

   subtype A_Flat_Full_Type_Declaration is Flat_Declaration_Kinds
      range An_Ordinary_Type_Declaration .. A_Protected_Type_Declaration;

   subtype A_Flat_Object_Declaration is Flat_Declaration_Kinds
      range A_Variable_Declaration .. A_Single_Protected_Declaration;

   subtype A_Flat_Number_Declaration is Flat_Declaration_Kinds
      range An_Integer_Number_Declaration .. A_Real_Number_Declaration;

   subtype A_Flat_Renaming_Declaration is Flat_Declaration_Kinds
      range An_Object_Renaming_Declaration ..
            A_Generic_Function_Renaming_Declaration;

   subtype A_Flat_Body_Stub is Flat_Declaration_Kinds
      range A_Procedure_Body_Stub .. A_Protected_Body_Stub;

   subtype A_Flat_Generic_Declaration is Flat_Declaration_Kinds
      range A_Generic_Procedure_Declaration .. A_Generic_Package_Declaration;

   subtype A_Flat_Generic_Instantiation is Flat_Declaration_Kinds
      range A_Package_Instantiation .. A_Function_Instantiation;

   subtype A_Flat_Formal_Declaration is Flat_Declaration_Kinds range
      A_Formal_Object_Declaration .. A_Formal_Package_Declaration_With_Box;

   subtype Flat_Definition_Kinds is Flat_Element_Kinds range
      A_Derived_Type_Definition .. An_Aspect_Specification;

   subtype Flat_Type_Kinds is Flat_Definition_Kinds
      range A_Derived_Type_Definition .. An_Access_To_Protected_Function;

   subtype Flat_Interface_Kinds is Flat_Type_Kinds range
      An_Ordinary_Interface .. A_Synchronized_Interface;

   subtype Flat_Access_Type_Kinds is Flat_Type_Kinds range
      A_Pool_Specific_Access_To_Variable .. An_Access_To_Protected_Function;

   subtype Flat_Access_To_Object_Definition is Flat_Access_Type_Kinds
      range A_Pool_Specific_Access_To_Variable .. An_Access_To_Constant;

   subtype Flat_Access_To_Subprogram_Definition is Flat_Access_Type_Kinds
      range An_Access_To_Procedure ..  An_Access_To_Protected_Function;

   subtype Flat_Access_Definition_Kinds is Flat_Definition_Kinds
      range An_Anonymous_Access_To_Variable ..
        An_Anonymous_Access_To_Protected_Function;

   subtype Flat_Root_Type_Kinds is Flat_Type_Kinds
      range A_Root_Integer_Definition ..  A_Universal_Fixed_Definition;

   subtype Flat_Constraint_Kinds is Flat_Definition_Kinds
      range A_Range_Attribute_Reference .. A_Discriminant_Constraint;

   subtype Flat_Discrete_Subtype_Definition_Kinds is Flat_Definition_Kinds
      range A_Discrete_Subtype_Indication_As_Subtype_Definition ..
            A_Discrete_Simple_Expression_Range_As_Subtype_Definition;

   subtype Flat_Discrete_Range_Kinds is Flat_Definition_Kinds range
      A_Discrete_Subtype_Indication .. A_Discrete_Simple_Expression_Range;

   subtype Flat_Formal_Type_Kinds is Flat_Definition_Kinds
      range A_Formal_Private_Type_Definition ..
            A_Formal_Access_To_Protected_Function;

   subtype Flat_Formal_Interface_Kinds is Flat_Formal_Type_Kinds
      range A_Formal_Ordinary_Interface .. A_Formal_Synchronized_Interface;

   subtype Flat_Formal_Access_Type_Kinds is Flat_Formal_Type_Kinds
      range A_Formal_Pool_Specific_Access_To_Variable ..
            A_Formal_Access_To_Protected_Function;

   subtype Flat_Association_Kinds is Flat_Element_Kinds
      range A_Pragma_Argument_Association .. A_Generic_Association;

   subtype Flat_Expression_Kinds is Flat_Element_Kinds
      range An_Integer_Literal .. An_Allocation_From_Qualified_Expression;

   subtype Flat_Operator_Symbol_Kinds is Flat_Expression_Kinds
      range An_And_Operator .. A_Not_Operator;

   subtype Flat_Attribute_Reference_Kinds is Flat_Expression_Kinds
      range An_Access_Attribute .. An_Unknown_Attribute;

   subtype Flat_Statement_Kinds is Flat_Element_Kinds
      range A_Null_Statement .. A_Code_Statement;

   subtype Flat_Loop_Statement is Flat_Statement_Kinds
      range A_Loop_Statement .. A_For_Loop_Statement;

   subtype Flat_Path_Kinds is Flat_Element_Kinds
      range An_If_Path .. A_Then_Abort_Path;

   subtype Flat_Expression_Path_Kinds is Flat_Element_Kinds
      range An_If_Expression_Path .. An_Else_Expression_Path;

   subtype Flat_Clause_Kinds is Flat_Element_Kinds
      range A_Use_Package_Clause .. A_Component_Clause;

   subtype Flat_Use_Clause_Kinds is Flat_Clause_Kinds
      range A_Use_Package_Clause .. A_Use_All_Type_Clause;

   subtype Flat_Context_Clause_Kinds is Flat_Clause_Kinds
      range A_Use_Package_Clause .. A_With_Clause;

   subtype Flat_Representation_Clause_Kinds is Flat_Clause_Kinds
      range An_Attribute_Definition_Clause .. An_At_Clause;

   ---------------------------------------------------------------
   -- Queries implementing the flat Element classification and  --
   -- mapping it onto the ASIS-defined classification hierarchy --
   ---------------------------------------------------------------

   function Flat_Element_Kind
     (Element : Asis.Element)
      return Flat_Element_Kinds;
   --  Returns the Flat_Element_Kinds value of Element. Not_An_Element is
   --  returned if and only if Element is Nil_Element

   --  The following functions convert the value of Flat_Element_Kinds
   --  given as their argument into the corresponding value of the
   --  corresponding Asis Element Classification subordinate kind.
   --  Not_A_XXX is returned if the argument does not belong to the
   --  corresponding subordinate kind of the Element classification
   --  hierarchy (and, thereforo to the corresponding flat classification
   --  subtype.

   function Asis_From_Flat_Kind
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Element_Kinds;

   function Pragma_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Pragma_Kinds;

   function Defining_Name_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Defining_Name_Kinds;

   function Declaration_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Declaration_Kinds;
   function Definition_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Definition_Kinds;

   function Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Type_Kinds;

   function Formal_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Formal_Type_Kinds;

   function Access_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Access_Type_Kinds;

   function Root_Type_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Root_Type_Kinds;

   function Constraint_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Constraint_Kinds;

   function Discrete_Range_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Discrete_Range_Kinds;

   function Expression_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Expression_Kinds;

   function Operator_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Operator_Kinds;

   function Attribute_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Attribute_Kinds;

   function Association_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Association_Kinds;

   function Statement_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Statement_Kinds;

   function Path_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Path_Kinds;

   function Clause_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Clause_Kinds;

   function Representation_Clause_Kind_From_Flat
     (Flat_Kind : Flat_Element_Kinds)
      return Asis.Representation_Clause_Kinds;

   -------------------------------------
   -- Additional Classification items --
   -------------------------------------

   function Def_Operator_Kind
     (Op_Kind : Flat_Element_Kinds)
      return Flat_Element_Kinds;
   --  this function "converts" the value of Flat_Operator_Symbol_Kinds
   --  into the corresponding value of Flat_Defining_Operator_Kinds,
   --  implementing the "mapping" between usage and defining occurences of tthe
   --  operator signs. It is an error to call it to an Flat_Element_Kinds
   --  value which does not belong to Flat_Operator_Symbol_Kinds

end Asis.Extensions.Flat_Kinds;
