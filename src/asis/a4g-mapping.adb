------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . M A P P I N G                            --
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
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
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

with Asis;                    use Asis;
with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Elements;           use Asis.Elements;

with Asis.Set_Get;            use Asis.Set_Get;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Output;            use A4G.A_Output;
with A4G.A_Sem;               use A4G.A_Sem;
with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.Asis_Tables;         use A4G.Asis_Tables;
with A4G.Contt;               use A4G.Contt;
with A4G.Norm;                use A4G.Norm;
with A4G.Vcheck;              use A4G.Vcheck;

with Atree;                   use Atree;
with Einfo;                   use Einfo;
with Elists;                  use Elists;
with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Output;                  use Output;
with Snames;                  use Snames;
with Stand;                   use Stand;
with Uintp;                   use Uintp;
with Urealp;                  use Urealp;

package body A4G.Mapping is

   -------------------------------------------
   -- Tree nodes onto ASIS Elements Mapping --
   -------------------------------------------

   --  The kernel of the mapping from tree nodes onto ASIS Elements is
   --  determining the ASIS kind of the Element which should be built on top
   --  of a given node. We are computing the Element position in the internal
   --  flat classification, that is, the corresponding value of
   --  Internal_Element_Kinds (which is further referred simply as Element
   --  kind in this unit).
   --
   --  Mapping of tree nodes onto Element kinds is implemented as two-level
   --  switching based on look-up tables. Both look-up tables are one-dimension
   --  arrays indexed by Node_Kind type).
   --
   --  The first table has Internal_Element_Kinds as its component type. It
   --  defines the mapping of Node_Kind values onto Internal_Element_Kinds
   --  values as pairs index_value -> component_value, the semantics of this
   --  mapping depends on the component value in the following way:
   --
   --    Component value                      First switch mapping semantics
   --
   --    A_Xxx, where A_Xxx corresponds   - Element which should be based on
   --    to some position in the original   any node having the corresponding
   --    ASIS element classification        Node_Kind will always be of A_Xxx
   --    hierarchy (such as An_Identifier)  ASIS kind, no more computation
   --                                       of Element kind is needed
   --
   --    Non_Trivial_Mapping              - Elements which can be built on
   --                                       nodes having the corresponding
   --                                       Node_Kind may have different ASIS
   --                                       kinds, therefore a special function
   --                                       computing the ASIS kind should be
   --                                       used, this function is defined by
   --                                       the second look-up table
   --
   --    No_Mapping                       - no ASIS Element kind corresponds to
   --                                       nodes of the corresponding
   --                                       Node_Kind in the framework of the
   --                                       given node-to-Element mapping
   --
   --    Not_Implemented_Mapping          - this value was used during the
   --                                       development phase and now it is
   --                                       kept as the value for 'others'
   --                                       choice in the initialization
   --                                       aggregate just in case if a new
   --                                       value appear in Node_Kind type
   --
   --  No_Mapping does not mean, that for a given Node_Kind value no Element
   --  can be created at all, it means, that automatic Element kind
   --  determination is impossible for these nodes because of any reason.
   --
   --  Both No_Mapping and Not_Implemented_Mapping mapping items, when chosen.
   --  resulted in raising the ASIS_Failed exception. The reason because of
   --  which we keep Not_Implemented_Mapping value after finishing the
   --  development stage is to catch possible changes in Node_Kind definition.
   --  No_Mapping means that for sure there is no mapping for a given Node_Kind
   --  value, and Not_Implemented_Mapping means that processing of the given
   --  Node_Kind value is missed in the existing code.
   --
   --  The second look-up table defines functions to be used to compute
   --  Element kind for those Node_Kind values for which the first table
   --  defines Non_Trivial_Mapping. All these functions are supposed to be
   --  called for nodes of the Node_Kind from the corresponding mapping item
   --  defined by the second table, it is erroneous to call them for other
   --  nodes.
   --
   --  The structure, documentation and naming policy for look-up tables
   --  implementing note-to-Element mapping are based on the GNAT Sinfo
   --  package, and, in particular, on the Sinfo.Node_Kind type definition.
   --  Rather old version of the spec of Sinfo is used, so some deviations
   --  with the latest version may be possible

   ------------------------------------------------------
   -- Tree nodes lists onto ASIS Element lists Mapping --
   ------------------------------------------------------

   --  ASIS Element lists are built from tree node lists: when constructing an
   --  ASIS Element_List value, the corresponding routine goes trough the
   --  corresponding tree node list, checks which nodes should be used as a
   --  basis for ASIS Elements to be placed in the result Element_List, and
   --  which should not, and then calls node-to-Element conversion function
   --  for the selected nodes. Therefore, two main components of node list to
   --  Element list mapping are filters for the nodes in the argument node
   --  list and node-to-Element mapping

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Normalize_Name (Capitalized : Boolean := False);
   --  This procedure "normalizes" a name stored in Namet.Name_Buffer by
   --  capitalizing its firs letter and all the letters following underscores
   --  (if any). If Capitalized is set ON, all the letters are converted to
   --  upper case, this is used for some defining names from Standard (such as
   --  ASCII)

   function Is_Protected_Procedure_Call (N : Node_Id) return Boolean;
   --  In case if N is of N_Entry_Call_Statement, it checks if this is a call
   --  to a protected subprogram (if it is, the corresponding ASIS Element
   --  should be classified as A_Procedure_Call_Statement, but not as
   --  An_Entry_Call_Statement

   function Is_Stub_To_Body_Instanse_Replacement
     (N :    Node_Id)
      return Boolean;
   --  Checks if the node corresponds to the body which replaces the body stub
   --  within the instance. The reason why we need this is that Sloc
   --  field is not set to point into the instance copy if the source for
   --  such node, so the ordinary Is_From_Instance check does not work
   --  for this node (see 8930-001)

   function Is_Config_Pragma (N : Node_Id) return Boolean;
   --  Checks if N represents a configuration pragma

   function Requires_Parentheses
     (N    : Node_Id)
      return Boolean;
   --  Checks if N represents a conditional or quantified expression in the
   --  context that requires the expression to be in parentheses. The problem
   --  is that in this case Parent_Count is 0. This function is supposed to be
   --  called if Parent_Count (N) = 0

   --------------------------------------------------------------
   -- Subprograms for the second Note-to-Element Look-Up Table --
   --------------------------------------------------------------

   procedure No_Mapping (Node : Node_Id);

   function Not_Implemented_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   procedure Not_Implemented_Mapping (Source_Node_Kind : Node_Kind);
   --  These three subprograms raise ASIS_Failed with the appropriate
   --  Diagnosis string

   pragma No_Return (Not_Implemented_Mapping);
   pragma No_Return (No_Mapping);

   --  Individual mapping components:

   function N_Pragma_Mapping (Node : Node_Id) return Internal_Element_Kinds;

   function N_Defining_Identifier_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Defining_Operator_Symbol_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Expanded_Name_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Identifier_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds renames N_Expanded_Name_Mapping;

   function N_Attribute_Reference_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Function_Call_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Range_Mapping (Node : Node_Id) return Internal_Element_Kinds;

   function N_Allocator_Mapping (Node : Node_Id) return Internal_Element_Kinds;

   function N_Aggregate_Mapping (Node : Node_Id) return Internal_Element_Kinds;

--  |A2005 start
   function N_Incomplete_Type_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;
--  |A2005 end

   function N_Subtype_Indication_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

--  |A2012 start
   function N_Formal_Type_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Iterator_Specification_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Quantified_Expression_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;
--  |A2012 end

   function N_Object_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Access_Function_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Access_Procedure_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Subprogram_Body_Stub_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Subprogram_Body_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Generic_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Constrained_Array_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Unconstrained_Array_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Subprogram_Renaming_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Loop_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Requeue_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Abstract_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Accept_Alternative_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

--  --|A2005 start
   function N_Access_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;
--  --|A2005 end

   function N_Access_To_Object_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Component_Association_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Derived_Type_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Delay_Alternative_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Formal_Package_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Formal_Private_Type_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Formal_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Index_Or_Discriminant_Constraint_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Number_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Procedure_Call_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Range_Constraint_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Record_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Use_Type_Clause_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;

   function N_Terminate_Alternative_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds renames N_Accept_Alternative_Mapping;

   -----------------------------------------
   -- Node lists to Element lists filters --
   -----------------------------------------

   function May_Be_Included (Node : Node_Id) return Boolean;
   --  Top-level filter for selecting nodes from a node list to be used to
   --  create ASIS Elements which are members of some ASIS Element List.

   function Ordinary_Inclusion_Condition (Node : Node_Id) return Boolean;
   --  Defines the general condition for a separate node list member to be
   --  used to construct an Element to be returned by some ASIS query. This
   --  function does not make the final decision, because the node may be
   --  duplicated, and this is checked in the May_Be_Included function

   procedure Skip_Normalized_Declarations (Node : in out Node_Id);
   --  This procedure is applied in case when the compiler normalizes a
   --  multi-identifier declaration (or multi-name with clause) in a set of
   --  equivalent one-identifier (one-name) declarations (clauses). It is
   --  intended to be called for Node representing the first declaration
   --  (clause) in this normalized sequence, and it resets its parameter
   --  to point to the last declaration (clause) in this sequence

   -----------------------------------------
   -- Node-to-Element First Look-Up Table --
   -----------------------------------------

   Node_To_Element_Kind_Mapping_First_Switch :
      constant array (Node_Kind) of Internal_Element_Kinds := (

   N_Unused_At_Start  => No_Mapping,
   N_At_Clause                         => An_At_Clause,
   N_Component_Clause                  => A_Component_Clause,
   N_Enumeration_Representation_Clause => An_Enumeration_Representation_Clause,
   N_Mod_Clause                        => No_Mapping,
   N_Record_Representation_Clause      => A_Record_Representation_Clause,
   N_Attribute_Definition_Clause       => An_Attribute_Definition_Clause,
   N_Empty                             => No_Mapping,
   N_Error                             => No_Mapping,
   N_Pragma                            => Non_Trivial_Mapping,
   N_Pragma_Argument_Association       => A_Pragma_Argument_Association,
   N_Defining_Character_Literal        => A_Defining_Character_Literal,
   N_Defining_Identifier               => Non_Trivial_Mapping,
   N_Defining_Operator_Symbol          => Non_Trivial_Mapping,
   N_Expanded_Name                     => Non_Trivial_Mapping,
   N_Identifier                        => Non_Trivial_Mapping,
   N_Character_Literal                 => A_Character_Literal,
   N_Operator_Symbol                   => Non_Trivial_Mapping,
   N_Op_Add                            => A_Function_Call,
   N_Op_And                            => A_Function_Call,
   N_And_Then                          => An_And_Then_Short_Circuit,
   N_Op_Concat                         => A_Function_Call,
   N_Op_Divide                         => A_Function_Call,
   N_Op_Eq                             => A_Function_Call,
   N_Op_Expon                          => A_Function_Call,
   N_Op_Ge                             => A_Function_Call,
   N_Op_Gt                             => A_Function_Call,
--   N_In                                => Non_Trivial_Mapping,
   N_In                                => An_In_Membership_Test,
   N_Op_Le                             => A_Function_Call,
   N_Op_Lt                             => A_Function_Call,
   N_Op_Mod                            => A_Function_Call,
   N_Op_Multiply                       => A_Function_Call,
   N_Op_Ne                             => A_Function_Call,
--   N_Not_In                            => Non_Trivial_Mapping,
   N_Not_In                            => A_Not_In_Membership_Test,
   N_Op_Or                             => A_Function_Call,
   N_Or_Else                           => An_Or_Else_Short_Circuit,
   N_Op_Rem                            => A_Function_Call,
   N_Op_Subtract                       => A_Function_Call,
   N_Op_Xor                            => A_Function_Call,
   N_Op_Abs                            => A_Function_Call,
   N_Op_Minus                          => A_Function_Call,
   N_Op_Not                            => A_Function_Call,
   N_Op_Plus                           => A_Function_Call,
   N_Attribute_Reference               => Non_Trivial_Mapping,
   N_Conditional_Expression            => An_If_Expression,
   N_Explicit_Dereference              => An_Explicit_Dereference,
   N_Function_Call                     => Non_Trivial_Mapping,
   N_Indexed_Component                 => An_Indexed_Component,
   N_Integer_Literal                   => An_Integer_Literal,
   N_Null                              => A_Null_Literal,
   N_Procedure_Call_Statement          => Non_Trivial_Mapping,
   N_Qualified_Expression              => A_Qualified_Expression,
   N_Quantified_Expression             => Non_Trivial_Mapping,
   N_Raise_Constraint_Error            => No_Mapping,
   N_Range                             => Non_Trivial_Mapping,
   N_Real_Literal                      => A_Real_Literal,
   N_Selected_Component                => A_Selected_Component,
   N_Type_Conversion                   => A_Type_Conversion,
   N_Allocator                         => Non_Trivial_Mapping,
   N_Case_Expression                   => A_Case_Expression,    --  ASIS 2012
   N_Aggregate                         => Non_Trivial_Mapping,
   N_Extension_Aggregate               => An_Extension_Aggregate,
   N_Slice                             => A_Slice,
   N_String_Literal                    => A_String_Literal,
   N_Subtype_Indication                => Non_Trivial_Mapping,
   N_Component_Declaration             => A_Component_Declaration,
   N_Entry_Body                        => An_Entry_Body_Declaration,
   N_Entry_Declaration                 => An_Entry_Declaration,
--     N_Expression_Function               => An_Expression_Function_Declaration, (SCz)
   N_Entry_Index_Specification         => An_Entry_Index_Specification,
   N_Formal_Object_Declaration         => A_Formal_Object_Declaration,
   N_Formal_Type_Declaration           => Non_Trivial_Mapping,
   N_Freeze_Entity                     => No_Mapping,
   N_Full_Type_Declaration             => An_Ordinary_Type_Declaration,
--  --|A2005 start
--   N_Incomplete_Type_Declaration       => An_Incomplete_Type_Declaration,
   N_Incomplete_Type_Declaration       => Non_Trivial_Mapping,
--  --|A2005 end

--  --|A2012 start
   N_Iterator_Specification            => Non_Trivial_Mapping,
--  --|A2012 end

   N_Loop_Parameter_Specification      => A_Loop_Parameter_Specification,
   N_Object_Declaration                => Non_Trivial_Mapping,
   N_Private_Extension_Declaration     => A_Private_Extension_Declaration,
   N_Private_Type_Declaration          => A_Private_Type_Declaration,
   N_Subtype_Declaration               => A_Subtype_Declaration,
   N_Protected_Type_Declaration        => A_Protected_Type_Declaration,
   N_Accept_Statement                  => An_Accept_Statement,
   N_Function_Specification            => No_Mapping,
   N_Procedure_Specification           => No_Mapping,
   N_Access_Function_Definition        => Non_Trivial_Mapping,
   N_Access_Procedure_Definition       => Non_Trivial_Mapping,
   N_Task_Type_Declaration             => A_Task_Type_Declaration,
   N_Package_Body_Stub                 => A_Package_Body_Stub,
   N_Protected_Body_Stub               => A_Protected_Body_Stub,
   N_Subprogram_Body_Stub              => Non_Trivial_Mapping,
   N_Task_Body_Stub                    => A_Task_Body_Stub,
   N_Function_Instantiation            => A_Function_Instantiation,
   N_Package_Instantiation             => A_Package_Instantiation,
   N_Procedure_Instantiation           => A_Procedure_Instantiation,
   N_Package_Body                      => A_Package_Body_Declaration,
   N_Subprogram_Body                   => Non_Trivial_Mapping,
   N_Implicit_Label_Declaration        => No_Mapping,
   N_Package_Declaration               => A_Package_Declaration,
   N_Single_Task_Declaration           => A_Single_Task_Declaration,
   N_Subprogram_Declaration            => Non_Trivial_Mapping,
   N_Task_Body                         => A_Task_Body_Declaration,
   N_Use_Package_Clause                => A_Use_Package_Clause,
   N_Generic_Package_Declaration       => A_Generic_Package_Declaration,
   N_Generic_Subprogram_Declaration    => Non_Trivial_Mapping,
   N_Constrained_Array_Definition      => Non_Trivial_Mapping,
   N_Unconstrained_Array_Definition    => Non_Trivial_Mapping,
   N_Exception_Renaming_Declaration    => An_Exception_Renaming_Declaration,
   N_Object_Renaming_Declaration       => An_Object_Renaming_Declaration,
   N_Package_Renaming_Declaration      => A_Package_Renaming_Declaration,
   N_Subprogram_Renaming_Declaration   => Non_Trivial_Mapping,

   N_Generic_Function_Renaming_Declaration  =>
      A_Generic_Function_Renaming_Declaration,

   N_Generic_Package_Renaming_Declaration   =>
      A_Generic_Package_Renaming_Declaration,

   N_Generic_Procedure_Renaming_Declaration =>
      A_Generic_Procedure_Renaming_Declaration,

   N_Abort_Statement                   => An_Abort_Statement,
   N_Assignment_Statement              => An_Assignment_Statement,
   N_Block_Statement                   => A_Block_Statement,
   N_Case_Statement                    => A_Case_Statement,
   N_Code_Statement                    => A_Code_Statement,
   N_Delay_Relative_Statement          => A_Delay_Relative_Statement,
   N_Delay_Until_Statement             => A_Delay_Until_Statement,
   N_Entry_Call_Statement              => An_Entry_Call_Statement,
   N_Exit_Statement                    => An_Exit_Statement,
   N_Free_Statement                    => No_Mapping,
   N_Goto_Statement                    => A_Goto_Statement,
   N_If_Statement                      => An_If_Statement,
   N_Loop_Statement                    => Non_Trivial_Mapping,
   N_Null_Statement                    => A_Null_Statement,
   N_Raise_Statement                   => A_Raise_Statement,
   N_Requeue_Statement                 => Non_Trivial_Mapping,
   N_Return_Statement                  => A_Return_Statement,
   N_Extended_Return_Statement         => An_Extended_Return_Statement,
   N_Abortable_Part                    => A_Then_Abort_Path,
   N_Abstract_Subprogram_Declaration   => Non_Trivial_Mapping,
   N_Accept_Alternative                => Non_Trivial_Mapping,

--  --|A2005 start
   N_Access_Definition                 => Non_Trivial_Mapping,
--  --|A2005 end

   N_Access_To_Object_Definition       => Non_Trivial_Mapping,

--  --|A2012 start
   N_Aspect_Specification              => An_Aspect_Specification,
   N_Case_Expression_Alternative       => A_Case_Expression_Path,
--  --|A2012 end

   N_Asynchronous_Select               => An_Asynchronous_Select_Statement,
   N_Case_Statement_Alternative        => A_Case_Path,
   N_Compilation_Unit                  => No_Mapping,
   N_Component_Association             => Non_Trivial_Mapping,
   N_Component_Definition              => A_Component_Definition,
   N_Conditional_Entry_Call            => A_Conditional_Entry_Call_Statement,
   N_Derived_Type_Definition           => Non_Trivial_Mapping,
   N_Decimal_Fixed_Point_Definition    => A_Decimal_Fixed_Point_Definition,
   N_Defining_Program_Unit_Name        => A_Defining_Expanded_Name,
   N_Delay_Alternative                 => Non_Trivial_Mapping,
   N_Delta_Constraint                  => A_Delta_Constraint,
   N_Digits_Constraint                 => A_Digits_Constraint,
   N_Discriminant_Association          => A_Discriminant_Association,
   N_Discriminant_Specification        => A_Discriminant_Specification,
   N_Elsif_Part                        => An_Elsif_Path,
   N_Enumeration_Type_Definition       => An_Enumeration_Type_Definition,
   N_Entry_Call_Alternative            => A_Select_Path,
   N_Exception_Declaration             => An_Exception_Declaration,
   N_Exception_Handler                 => An_Exception_Handler,
   N_Floating_Point_Definition         => A_Floating_Point_Definition,

   N_Formal_Decimal_Fixed_Point_Definition  =>
      A_Formal_Decimal_Fixed_Point_Definition,

   N_Formal_Derived_Type_Definition    => A_Formal_Derived_Type_Definition,
   N_Formal_Discrete_Type_Definition   => A_Formal_Discrete_Type_Definition,
   N_Formal_Floating_Point_Definition  => A_Formal_Floating_Point_Definition,
   N_Formal_Modular_Type_Definition    => A_Formal_Modular_Type_Definition,

   N_Formal_Ordinary_Fixed_Point_Definition =>
      A_Formal_Ordinary_Fixed_Point_Definition,

   N_Formal_Package_Declaration        => Non_Trivial_Mapping,
   N_Formal_Private_Type_Definition    => Non_Trivial_Mapping,

   N_Formal_Signed_Integer_Type_Definition  =>
      A_Formal_Signed_Integer_Type_Definition,

   N_Formal_Subprogram_Declaration     => Non_Trivial_Mapping,
   N_Generic_Association               => A_Generic_Association,
   N_Index_Or_Discriminant_Constraint  => Non_Trivial_Mapping,
   N_Label                             => No_Mapping,
   N_Modular_Type_Definition           => A_Modular_Type_Definition,
   N_Number_Declaration                => Non_Trivial_Mapping,
   N_Ordinary_Fixed_Point_Definition   => An_Ordinary_Fixed_Point_Definition,
   N_Others_Choice                     => An_Others_Choice,
   N_Package_Specification             => No_Mapping,
   N_Parameter_Association             => A_Parameter_Association,
   N_Parameter_Specification           => A_Parameter_Specification,
   N_Protected_Body                    => A_Protected_Body_Declaration,
   N_Protected_Definition              => A_Protected_Definition,
   N_Range_Constraint                  => Non_Trivial_Mapping,
   N_Real_Range_Specification          => A_Simple_Expression_Range,
   N_Record_Definition                 => Non_Trivial_Mapping,
   N_Selective_Accept                  => A_Selective_Accept_Statement,
   N_Signed_Integer_Type_Definition    => A_Signed_Integer_Type_Definition,
   N_Single_Protected_Declaration      => A_Single_Protected_Declaration,
   N_Subunit                           => No_Mapping,
   N_Task_Definition                   => A_Task_Definition,
   N_Terminate_Alternative             => Non_Trivial_Mapping,
   N_Timed_Entry_Call                  => A_Timed_Entry_Call_Statement,
   N_Triggering_Alternative            => A_Select_Path,
   N_Use_Type_Clause                   => Non_Trivial_Mapping,
   N_Variant                           => A_Variant,
   N_Variant_Part                      => A_Variant_Part,
   N_With_Clause                       => A_With_Clause,
   N_Unused_At_End                     => No_Mapping,

   others                          => Not_Implemented_Mapping);

   ------------------------------------------
   -- Node-to-Element Second Look-Up Table --
   ------------------------------------------

   type Mapping_Item is access function (Node : Node_Id)
         return Internal_Element_Kinds;

   Node_To_Element_Kind_Mapping_Second_Switch :
      constant array (Node_Kind) of Mapping_Item := (

      N_Pragma                   => N_Pragma_Mapping'Access,
      N_Defining_Identifier      => N_Defining_Identifier_Mapping'Access,
      N_Defining_Operator_Symbol => N_Defining_Operator_Symbol_Mapping'Access,
      N_Expanded_Name            => N_Expanded_Name_Mapping'Access,
      N_Identifier               => N_Identifier_Mapping'Access,
      N_Operator_Symbol          => N_Operator_Symbol_Mapping'Access,
      N_Attribute_Reference      => N_Attribute_Reference_Mapping'Access,
      N_Function_Call            => N_Function_Call_Mapping'Access,
      N_Quantified_Expression    => N_Quantified_Expression_Mapping'Access,
      N_Range                    => N_Range_Mapping'Access,
      N_Allocator                => N_Allocator_Mapping'Access,
      N_Aggregate                => N_Aggregate_Mapping'Access,
      N_Subtype_Indication       => N_Subtype_Indication_Mapping'Access,

--  --|A2012 start
      N_Formal_Type_Declaration  => N_Formal_Type_Declaration_Mapping'Access,
--  --|A2012 end

--  --|A2005 start
--   N_Incomplete_Type_Declaration       => An_Incomplete_Type_Declaration,
      N_Incomplete_Type_Declaration =>
        N_Incomplete_Type_Declaration_Mapping'Access,
--  --|A2005 end

--  --|A2012 start
      N_Iterator_Specification   => N_Iterator_Specification_Mapping'Access,
--  --|A2012 end

      N_Object_Declaration       => N_Object_Declaration_Mapping'Access,

      N_Access_Function_Definition  =>
         N_Access_Function_Definition_Mapping'Access,

      N_Access_Procedure_Definition =>
         N_Access_Procedure_Definition_Mapping'Access,

      N_Subprogram_Body_Stub     => N_Subprogram_Body_Stub_Mapping'Access,
      N_Subprogram_Body          => N_Subprogram_Body_Mapping'Access,
      N_Subprogram_Declaration   => N_Subprogram_Declaration_Mapping'Access,

      N_Generic_Subprogram_Declaration =>
         N_Generic_Subprogram_Declaration_Mapping'Access,

      N_Constrained_Array_Definition =>
         N_Constrained_Array_Definition_Mapping'Access,

      N_Unconstrained_Array_Definition =>
         N_Unconstrained_Array_Definition_Mapping'Access,

      N_Subprogram_Renaming_Declaration =>
         N_Subprogram_Renaming_Declaration_Mapping'Access,

      N_Loop_Statement           => N_Loop_Statement_Mapping'Access,
      N_Requeue_Statement        => N_Requeue_Statement_Mapping'Access,

      N_Abstract_Subprogram_Declaration =>
         N_Abstract_Subprogram_Declaration_Mapping'Access,

      N_Accept_Alternative       => N_Accept_Alternative_Mapping'Access,

--  --|A2005 start
      N_Access_Definition        => N_Access_Definition_Mapping'Access,
--  --|A2005 end

      N_Access_To_Object_Definition =>
         N_Access_To_Object_Definition_Mapping'Access,

      N_Component_Association    => N_Component_Association_Mapping'Access,
      N_Derived_Type_Definition  => N_Derived_Type_Definition_Mapping'Access,
      N_Delay_Alternative        => N_Delay_Alternative_Mapping'Access,

      N_Formal_Package_Declaration     =>
         N_Formal_Package_Declaration_Mapping'Access,

      N_Formal_Private_Type_Definition =>
         N_Formal_Private_Type_Definition_Mapping'Access,

      N_Formal_Subprogram_Declaration  =>
         N_Formal_Subprogram_Declaration_Mapping'Access,

      N_Index_Or_Discriminant_Constraint =>
         N_Index_Or_Discriminant_Constraint_Mapping'Access,

      N_Number_Declaration       => N_Number_Declaration_Mapping'Access,
      N_Range_Constraint         => N_Range_Constraint_Mapping'Access,
      N_Record_Definition        => N_Record_Definition_Mapping'Access,
      N_Terminate_Alternative    => N_Terminate_Alternative_Mapping'Access,
      N_Use_Type_Clause          => N_Use_Type_Clause_Mapping'Access,
      N_Procedure_Call_Statement => N_Procedure_Call_Statement_Mapping'Access,

      others                     => Not_Implemented_Mapping'Access);

   ----------------------------------------------------
   -- Node List to Element List Filter Look-Up Table --
   ----------------------------------------------------

   --  The following look-up table defines the first, very rough filter for
   --  selecting node list elements to be used as a basis for ASIS Element
   --  list components: it defines which Node_Kind values could never be used
   --  for creating Elements in Element List (for them False is set in the
   --  table)

   May_Be_Included_Switch : constant array (Node_Kind) of Boolean := (

      N_Unused_At_Start             => False,
      N_Freeze_Entity               => False,
      N_Implicit_Label_Declaration  => False,
      N_Label                       => False,

      others                        => True);

   --------------------------------
   -- Asis_Internal_Element_Kind --
   --------------------------------

   function Asis_Internal_Element_Kind
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Mapping_Case  : Internal_Element_Kinds;
      Source_Node_Kind : Node_Kind;

   begin -- two-level switching only!

      Source_Node_Kind := Nkind (Node);
      Mapping_Case  := Node_To_Element_Kind_Mapping_First_Switch
                          (Source_Node_Kind);

      case Mapping_Case is

      when Non_Trivial_Mapping =>

         return Node_To_Element_Kind_Mapping_Second_Switch
                (Source_Node_Kind) (Node);

      when Not_Implemented_Mapping =>

         Not_Implemented_Mapping (Source_Node_Kind);

      when No_Mapping =>

         No_Mapping (Node);

      when others => -- all trivial cases!

         return Mapping_Case;

      end case;

   end Asis_Internal_Element_Kind;

   --------------------------------------
   -- Defining_Id_List_From_Normalized --
   --------------------------------------

   function Defining_Id_List_From_Normalized
     (N                : Node_Id;
      From_Declaration : Asis.Element)
      return Asis.Defining_Name_List
   is
      Res_Max_Len : constant Natural :=
        Natural (List_Length (List_Containing (N)));
      --  to avoid two loops through the list of declarations/specifications,
      --  we use the rough estimation of the length of the result
      --  Defining_Name_List - it cannot contain more elements that the
      --  number of nodes in the tree node list containing (normalized)
      --  declarations
      Res_Act_Len : Natural := 1;
      --  the actual number of defining identifiers in the normalized
      --  declaration
      Result_List : Defining_Name_List (1 .. Res_Max_Len);
      Decl_Node   : Node_Id := N;
      Decl_Nkind  : constant Node_Kind := Nkind (Decl_Node);
      Def_Id_Node : Node_Id;
   begin
      Def_Id_Node := Defining_Identifier (Decl_Node);

      Result_List (Res_Act_Len) :=
         Node_To_Element_New (Node             => Def_Id_Node,
                              Starting_Element => From_Declaration,
                              Internal_Kind    => A_Defining_Identifier);

      while More_Ids (Decl_Node) loop
         Decl_Node   := Next (Decl_Node);

         while Nkind (Decl_Node) /= Decl_Nkind loop
            --  some implicit subtype declarations may be inserted by
            --  the compiler in between the normalized declarations, so:
            Decl_Node := Next (Decl_Node);
         end loop;

         Def_Id_Node := Defining_Identifier (Decl_Node);
         Res_Act_Len := Res_Act_Len + 1;

         Result_List (Res_Act_Len) :=
            Node_To_Element_New (Node             => Def_Id_Node,
                                 Starting_Element => From_Declaration,
                                 Internal_Kind    => A_Defining_Identifier);
      end loop;

      return Result_List (1 .. Res_Act_Len);

   end Defining_Id_List_From_Normalized;

   ------------------------------------------
   -- Discrete_Choice_Node_To_Element_List --
   ------------------------------------------

   function Discrete_Choice_Node_To_Element_List
     (Choice_List      : List_Id;
      Starting_Element : Asis.Element)
   return Asis.Element_List
   is
      Result_List  : Asis.Element_List
                     (1 .. ASIS_Integer (List_Length (Choice_List)));
      --  List_Length (Choice_List) cannot be 0 for the DISCRETE_CHOICE_LIST!

      Current_Node             : Node_Id;
      Current_Original_Node    : Node_Id;
      Element_Already_Composed : Boolean;
      Result_Kind              : Internal_Element_Kinds;
   begin

      Current_Node :=  First (Choice_List);
      --  first list element to process cannot be Empty!

      Current_Original_Node := Original_Node (Current_Node);

      for I in 1 .. ASIS_Integer (List_Length (Choice_List)) loop

         Element_Already_Composed := False;

         if Paren_Count (Current_Original_Node) > 0 then
            --  Corner but legal case of discrete choice like
            --
            --    when (1) =>
            --
            --  or
            --
            --     When (A.B.C) =>
            Result_Kind := Not_An_Element;
         else

            case Nkind (Current_Original_Node) is

            --  DISCRETE_CHOICE_LIST ::= DISCRETE_CHOICE {| DISCRETE_CHOICE}
            --  DISCRETE_CHOICE ::= EXPRESSION | DISCRETE_RANGE | others

            when N_Others_Choice =>  --  DISCRETE_CHOICE ::= ... | others

               Result_Kind := An_Others_Choice;

            --  DISCRETE_CHOICE ::= ... | DISCRETE_RANGE | ...

            --  DISCRETE_RANGE ::= discrete_SUBTYPE_INDICATION | RANGE

            when N_Subtype_Indication =>
               --  DISCRETE_RANGE ::= discrete_SUBTYPE_INDICATION | ...
               --
               --  The problem is that GNAT reduces the subtype_indication
               --  having NO constraint directly to subtype_mark
               --  (-> N_Identifier, N_Expanded_Name). May be, it is a
               --  pathological case, but it can also be represented by
               --  ...'Base construction (-> N_Attribute_Reference)

               Result_Kind := A_Discrete_Subtype_Indication;

            when N_Identifier =>

               if Ekind (Entity (Current_Original_Node)) in  Discrete_Kind then
                  --  discrete subtype mark!!
                  Result_Kind := A_Discrete_Subtype_Indication;

               elsif Ekind (Entity (Current_Original_Node)) =
                     E_Enumeration_Literal
               then
                  Result_Kind := An_Enumeration_Literal;
               else
                  Result_Kind := An_Identifier;
               end if;

            when N_Expanded_Name =>

               if Ekind (Entity (Current_Original_Node)) in  Discrete_Kind then
                  --  discrete subtype mark!!
                  Result_Kind := A_Discrete_Subtype_Indication;
               else
                  --  expression
                  Result_Kind := A_Selected_Component;
               end if;

               --  DISCRETE_RANGE ::= ... | RANGE

               --  RANGE ::=
               --    RANGE_ATTRIBUTE_REFERENCE
               --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

            when N_Range =>
               --  RANGE ::= ...
               --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

               Result_Kind := A_Discrete_Simple_Expression_Range;

            when N_Attribute_Reference =>
               --  RANGE ::= RANGE_ATTRIBUTE_REFERENCE | ...
               --  Sinfo.ads:
               --            A range attribute designator is represented
               --            in the tree using the normal N_Attribute_Reference
               --            node.
               --  But if the tree corresponds to the compilable Compilation
               --  Unit, the RANGE_ATTRIBUTE_REFERENCE is the only construct
               --  which could be in this position --W_R_O_N_G !!! T'Base!!!

               if Attribute_Name (Current_Original_Node) = Name_Range then
                  Result_Kind := A_Discrete_Range_Attribute_Reference;
               else
                  --  attribute denoting a type/subtype or yielding a value:

                  Result_List (I) := Node_To_Element_New (
                        Node             => Current_Node,
                        Starting_Element => Starting_Element);

                  Element_Already_Composed := True;

               end if;

            --  DISCRETE_CHOICE ::= EXPRESSION | ...

            when others =>
               --  In the tree corresponding to the compilable Compilation
               --  Unit the only possibility in the others choice is the
               --  EXPRESSION as the DISCRETE_CHOICE.

               Result_List (I) := Node_To_Element_New (
                     Node             => Current_Node,
                     Starting_Element => Starting_Element);

               Element_Already_Composed := True;

            end case;
         end if;

         if not Element_Already_Composed then

            Result_List (I) := Node_To_Element_New (
                  Node             => Current_Node,
                  Internal_Kind    => Result_Kind,
                  Starting_Element => Starting_Element);
         end if;

         Current_Node          := Next (Current_Node);
         Current_Original_Node := Original_Node (Current_Node);

      end loop;

      return Result_List;

   end Discrete_Choice_Node_To_Element_List;

   ----------------------
   -- Is_Config_Pragma --
   ----------------------

   function Is_Config_Pragma (N : Node_Id) return Boolean is
   begin
      return True
        and then Nkind (N) = N_Pragma
        and then Pragma_Name (N) in
          First_Pragma_Name .. Last_Configuration_Pragma_Name;
   end Is_Config_Pragma;

   -------------------------------
   -- Is_GNAT_Attribute_Routine --
   -------------------------------

   function Is_GNAT_Attribute_Routine (N : Node_Id) return Boolean is
      Attribute_Chars : Name_Id;
      Result          : Boolean := False;
   begin

      Attribute_Chars := Attribute_Name (N);

      if Attribute_Chars = Name_Asm_Input     or else
         Attribute_Chars = Name_Asm_Output    or else
         Attribute_Chars = Name_Enum_Rep      or else
         Attribute_Chars = Name_Enum_Val      or else
         Attribute_Chars = Name_Fixed_Value   or else
         Attribute_Chars = Name_Integer_Value or else
         Attribute_Chars = Name_Ref
      then
         Result := True;
      end if;

      return Result;

   end Is_GNAT_Attribute_Routine;

   -------------------------------------------
   -- Is_Rewritten_Function_Prefix_Notation --
   -------------------------------------------

   function Is_Rewritten_Function_Prefix_Notation
     (N     : Node_Id)
      return Boolean
   is
      Result : Boolean := False;
   begin

      if Nkind (N) = N_Function_Call
        and then
         Is_Rewrite_Substitution (N)
        and then
         Nkind (Original_Node (N)) not in N_Op
        and then
         Present (Parameter_Associations (N))
        and then
         not Is_Empty_List (Parameter_Associations (N))
        and then
         Sloc (First (Parameter_Associations (N))) < Sloc (Sinfo.Name (N))
      then
         Result := True;
      end if;

      return Result;
   end Is_Rewritten_Function_Prefix_Notation;

   ------------------------------------------------------
   -- Is_Rewritten_Impl_Deref_Function_Prefix_Notation --
   ------------------------------------------------------

   function Is_Rewritten_Impl_Deref_Function_Prefix_Notation
     (N     : Node_Id)
      return Boolean
   is
      Result : Boolean := False;
   begin

      if Nkind (N) = N_Function_Call
        and then
         Present (Parameter_Associations (N))
        and then
         not Is_Empty_List (Parameter_Associations (N))
        and then
         Sloc (First (Parameter_Associations (N))) < Sloc (Sinfo.Name (N))
      then
         Result := True;
      end if;

      return Result;
   end Is_Rewritten_Impl_Deref_Function_Prefix_Notation;

   -----------------------------------
   -- Get_Next_Configuration_Pragma --
   -----------------------------------

   function Get_Next_Configuration_Pragma (N : Node_Id) return Node_Id is
      Result : Node_Id := N;
   begin

      while not (Is_Config_Pragma (Result) or else
                 No (Result))
      loop
         Result := Next (Result);
      end loop;

      return Result;
   end Get_Next_Configuration_Pragma;

   ----------------------------
   -- Is_Not_Duplicated_Decl --
   ----------------------------

   function Is_Not_Duplicated_Decl (Node : Node_Id) return Boolean is
      Prev_List_Elem : Node_Id;
   begin
      --  the idea is to check if the previous list member (and we are
      --  sure that Node itself is a list member) to be included
      --  in the list is the rewritten tree structure representing
      --  just the same Ada construct
      --
      --  If we change Prev_Non_Pragma to Next_Non_Pragma, then this
      --  function will return False for the first, but not for the second
      --  of the two duplicated declarations

      if not (Nkind (Node) = N_Full_Type_Declaration or else
              Nkind (Node) = N_Private_Type_Declaration or else
              Nkind (Node) = N_Subtype_Declaration)
      then
         return True;
         --  as far as we know for now, the problem of duplicated
         --  declaration exists only for type declarations
      end if;

      Prev_List_Elem := Prev_Non_Pragma (Node);

      while Present (Prev_List_Elem) loop

         if Ordinary_Inclusion_Condition (Prev_List_Elem)
           and then
            (Nkind (Prev_List_Elem) = N_Full_Type_Declaration or else
             Nkind (Prev_List_Elem) = N_Subtype_Declaration)
           and then
            Chars (Defining_Identifier (Prev_List_Elem)) =
            Chars (Defining_Identifier (Node))
         then
            return False;
         end if;

         Prev_List_Elem := Prev_Non_Pragma (Prev_List_Elem);
      end loop;

      return True;

   end Is_Not_Duplicated_Decl;

   ---------------------------------
   -- Is_Protected_Procedure_Call --
   ---------------------------------

   function Is_Protected_Procedure_Call (N : Node_Id) return Boolean is
      Result   : Boolean := False;
      Tmp_Node : Node_Id;
   begin
      Tmp_Node := Sinfo.Name (N);

      if Nkind (Tmp_Node) = N_Indexed_Component then
         --  Call to an entry from an entry family,
         Tmp_Node := Prefix (Tmp_Node);
      end if;

      if Nkind (Tmp_Node) = N_Selected_Component then
         Tmp_Node := Selector_Name (Tmp_Node);
      end if;

      Tmp_Node := Entity (Tmp_Node);

      Result := Ekind (Tmp_Node) = E_Procedure;

      return Result;

   end Is_Protected_Procedure_Call;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (N : Node_Id) return Boolean is
      Arg_Kind : constant Node_Kind := Nkind (N);
   begin
      return Arg_Kind in N_Statement_Other_Than_Procedure_Call or else
             Arg_Kind = N_Procedure_Call_Statement;
   end Is_Statement;

   ------------------------------------------
   -- Is_Stub_To_Body_Instanse_Replacement --
   ------------------------------------------

   function Is_Stub_To_Body_Instanse_Replacement
     (N :    Node_Id)
      return Boolean
   is
      Arg_Kind : constant Node_Kind := Nkind (N);
      Result   : Boolean            := False;
   begin

      if Arg_Kind in N_Proper_Body and then
         Was_Originally_Stub (N)
      then

         case Arg_Kind is
            when N_Subprogram_Body =>
               Result := Is_From_Instance (Sinfo.Specification (N));
            when N_Package_Body =>
               Result := Is_From_Instance (Sinfo.Defining_Unit_Name (N));
            when others =>
               Result := Is_From_Instance (Sinfo.Defining_Identifier (N));
         end case;

      end if;

      return Result;
   end Is_Stub_To_Body_Instanse_Replacement;

   ---------------------
   -- May_Be_Included --
   ---------------------

   function May_Be_Included (Node : Node_Id) return Boolean is

   begin
      return Ordinary_Inclusion_Condition (Node) and then
             Is_Not_Duplicated_Decl       (Node);

   end May_Be_Included;

   -----------------------------------------------
   -- N_Abstract_Subprogram_Declaration_Mapping --
   -----------------------------------------------

   function N_Abstract_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    A_Procedure_Declaration
      --    A_Function_Declaration

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Function_Declaration;
         else
            return A_Procedure_Declaration;
         end if;

   end N_Abstract_Subprogram_Declaration_Mapping;

   ----------------------------------
   -- N_Accept_Alternative_Mapping --
   ----------------------------------

   function N_Accept_Alternative_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    A_Select_Path
      --    An_Or_Path

      if No (Prev (Node)) then
         return A_Select_Path;
      else
         return An_Or_Path;
      end if;

   end N_Accept_Alternative_Mapping;

   ------------------------------------------
   -- N_Access_Function_Definition_Mapping --
   ------------------------------------------

   function N_Access_Function_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Four Internal_Element_Kinds values may be possible:
      --
      --   An_Access_To_Function
      --   An_Access_To_Protected_Function
      --
      --   A_Formal_Access_To_Function
      --   A_Formal_Access_To_Protected_Function

         if Nkind (Parent (Node)) = N_Formal_Type_Declaration then

            if Protected_Present (Node) then
               return A_Formal_Access_To_Protected_Function;
            else
               return A_Formal_Access_To_Function;
            end if;

         else

            if Protected_Present (Node) then
               return An_Access_To_Protected_Function;
            else
               return An_Access_To_Function;
            end if;

         end if;

   end N_Access_Function_Definition_Mapping;

   -------------------------------------------
   -- N_Access_Procedure_Definition_Mapping --
   -------------------------------------------

   function N_Access_Procedure_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Four Internal_Element_Kinds values may be possible:
      --
      --   An_Access_To_Procedure
      --   An_Access_To_Protected_Procedure
      --
      --   A_Formal_Access_To_Procedure
      --   A_Formal_Access_To_Protected_Procedure

         if Nkind (Parent (Node)) = N_Formal_Type_Declaration then
            if Protected_Present (Node) then
               return A_Formal_Access_To_Protected_Procedure;
            else
               return A_Formal_Access_To_Procedure;
            end if;
         else
            if Protected_Present (Node) then
               return An_Access_To_Protected_Procedure;
            else
               return An_Access_To_Procedure;
            end if;
         end if;

   end N_Access_Procedure_Definition_Mapping;

--  --|A2005 start
   ---------------------------------
   -- N_Access_Definition_Mapping --
   ---------------------------------

   function N_Access_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result :          Internal_Element_Kinds := Not_An_Element;
      Tmp    : constant Node_Id                :=
        Sinfo.Access_To_Subprogram_Definition (Node);
   begin

      case Nkind (Tmp) is
         when N_Empty =>
            if Constant_Present (Node) then
               Result := An_Anonymous_Access_To_Constant;
            else
               Result := An_Anonymous_Access_To_Variable;
            end if;

         when N_Access_Function_Definition =>

            if Protected_Present (Tmp) then
               Result := An_Anonymous_Access_To_Protected_Function;
            else
               Result := An_Anonymous_Access_To_Function;
            end if;

         when N_Access_Procedure_Definition =>

            if Protected_Present (Tmp) then
               Result := An_Anonymous_Access_To_Protected_Procedure;
            else
               Result := An_Anonymous_Access_To_Procedure;
            end if;

         when others =>
            pragma Assert (False);
            null;
      end case;

      return Result;
   end N_Access_Definition_Mapping;
--  --|A2005 end

   -------------------------------------------
   -- N_Access_To_Object_Definition_Mapping --
   -------------------------------------------

   function N_Access_To_Object_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Six Internal_Element_Kinds values may be possible:
      --
      --   A_Pool_Specific_Access_To_Variable
      --   An_Access_To_Variable
      --   An_Access_To_Constant
      --
      --   A_Formal_Pool_Specific_Access_To_Variable
      --   A_Formal_Access_To_Variable
      --   A_Formal_Access_To_Constant

      if Nkind (Parent (Node)) = N_Formal_Type_Declaration then

         if All_Present (Node) then
            return A_Formal_Access_To_Variable;
         elsif Constant_Present (Node) then
            return A_Formal_Access_To_Constant;
         else
            return A_Formal_Pool_Specific_Access_To_Variable;
         end if;

      else

         if All_Present (Node) then
            return An_Access_To_Variable;
         elsif Constant_Present (Node) then
            return An_Access_To_Constant;
         else
            return A_Pool_Specific_Access_To_Variable;
         end if;

      end if;

   end N_Access_To_Object_Definition_Mapping;

   -------------------------
   -- N_Aggregate_Mapping --
   -------------------------

   function N_Aggregate_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Aggregate_Type : Node_Id := Etype (Node);
   begin
      --  Three Internal_Element_Kinds values may be possible:
      --     A_Record_Aggregate
      --     A_Positional_Array_Aggregate
      --     A_Named_Array_Aggregate

         --  the following fragment is a result of the current setting
         --  of Etype field in the tree, see open problems #77

         --  for multi-dimensional array aggregates, Etype field for
         --  inner aggregates is set to Empty!!

         if Present (Aggregate_Type)                and then
            Ekind (Aggregate_Type) in Private_Kind  and then
            not (Ekind (Aggregate_Type) in Array_Kind    or else
                  Ekind (Aggregate_Type) in Record_Kind)
         then
            --  we need a full view of the type!
            Aggregate_Type := Full_View (Aggregate_Type);
         end if;

         --  Special case:
         if No (Aggregate_Type)
           and then
            (Nkind (Parent (Node)) = N_Pragma_Argument_Association
            or else
             Nkind (Parent (Node)) = N_Aspect_Specification)
         then
            return A_Record_Aggregate;
         end if;

         if Present (Aggregate_Type) and then
            Ekind (Aggregate_Type) in Record_Kind
         then
            return A_Record_Aggregate;
         else

            if Present (Expressions (Node)) then
               return A_Positional_Array_Aggregate;
            else
               return A_Named_Array_Aggregate;
            end if;

         end if;

   end N_Aggregate_Mapping;

   -------------------------
   -- N_Allocator_Mapping --
   -------------------------

   function N_Allocator_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --     An_Allocation_From_Subtype
      --     An_Allocation_From_Qualified_Expression

         if Nkind (Sinfo.Expression (Node)) = N_Qualified_Expression then
            return An_Allocation_From_Qualified_Expression;
         else
            return An_Allocation_From_Subtype;
         end if;

   end N_Allocator_Mapping;

   -----------------------------------
   -- N_Attribute_Reference_Mapping --
   -----------------------------------

   function N_Attribute_Reference_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Attribute_Chars : constant Name_Id   := Attribute_Name (Node);
      Context_Kind    : constant Node_Kind :=
        Nkind  (Original_Node (Parent_Node));
   begin
      --  The following Internal_Element_Kinds values may be possible:
      --
      --  For range attribute reference:
      --
      --     A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
      --     A_Discrete_Range_Attribute_Reference
      --     A_Range_Attribute_Reference
      --
      --  For attribute reference corresponding to the attributes which are
      --  functions
      --
      --     Adjacent
      --     Ceiling
      --     Compose
      --     Copy_Sign
      --     Exponent
      --     Floor
      --     Fraction
      --     Image
      --     Input
      --     Leading_Part
      --     Machine
      --     Max
      --     Min
      --     Model
      --     Pos
      --     Pred
      --     Remainder
      --     Round
      --     Rounding
      --     Scaling
      --     Succ
      --     Truncation
      --     Unbiased_Rounding
      --     Val
      --     Value
      --     Wide_Image
      --     Wide_Value
--  |A2005 start
      --
      --  Ada 2005 attributes that are functions:
      --
      --      Machine_Rounding
      --      Mod
      --      Wide_Wide_Image
--  |A2005 end
      --
      --  plus GNAT-specific attributes:
      --     Enum_Rep
      --     Fixed_Value
      --     Integer_Value
      --     Result
      --
      --  the Element of A_Function_Call Internal_Element_Kinds value should be
      --  created, and the determination of the prefix kind should further be
      --  done by hand (function Asis_Expressions.Prefix)
      --
      --  For attribute reference corresponding to the attributes which are
      --  procedures
      --
      --     Output
      --     Read
      --     Write
      --
      --  the Element of A_Procedure_Call_Statement kind should be created
      --
      --  For attributes returning types:
      --     Base
      --     Class
      --  the Element of A_Type_Conversion should be created if the node is
      --  rewritten into N_Type_Conversion node. But this is done by the
      --  Node_To_Element function (together with setting the Special_Case
      --  Element field, which is then taken into account by functions
      --  decomposing A_Type_Conversion Element.

      if Attribute_Chars = Name_Range then
      --  processing the range attribute reference
      --  range attribute reference is the part of the RANGE
      --  Syntax Cross Reference extraction for RANGE:
      --
      --  range
      --    discrete_range                       3.6.1
      --       discrete_choice                      3.8.1
      --          discrete_choice_list                 3.8.1
      --             array_component_association          4.3.3
      --                named_array_aggregate                4.3.3
      --             case_statement_alternative           5.4
      --             variant                              3.8.1
      --       index_constraint                     3.6.1
      --       slice                                4.1.2
      --    discrete_subtype_definition          3.6
      --       constrained_array_definition         3.6
      --       entry_declaration                    9.5.2
      --       entry_index_specification            9.5.2
      --       loop_parameter_specification         5.5
      --    range_constraint                     3.5
      --       delta_constraint                     J.3
      --       digits_constraint                    3.5.9
      --       scalar_constraint                    3.2.2
      --    relation                             4.4

         case Context_Kind is      --  should be reorganized when complete

         when
            --  discrete_range
               N_Component_Association
             | N_Case_Statement_Alternative
             | N_Variant
             | N_Index_Or_Discriminant_Constraint
             | N_Slice
             =>

            return A_Discrete_Range_Attribute_Reference;

         when                                -- discrete_subtype_definition
               N_Constrained_Array_Definition
             | N_Entry_Index_Specification
             | N_Loop_Parameter_Specification
             | N_Entry_Declaration
            =>

            return A_Discrete_Range_Attribute_Reference_As_Subtype_Definition;

         when  N_Range_Constraint =>    -- range_constraint ???

            return A_Range_Attribute_Reference;

         when  N_In       -- relation
             | N_Not_In
            =>

            return A_Range_Attribute_Reference;

         when others => -- impossible cases:
            raise Internal_Implementation_Error;
         end case;

      elsif -- language-defined attributes which are functions:

            Attribute_Chars = Name_Adjacent          or else
            Attribute_Chars = Name_Ceiling           or else
            Attribute_Chars = Name_Compose           or else
            Attribute_Chars = Name_Copy_Sign         or else
            Attribute_Chars = Name_Exponent          or else
            Attribute_Chars = Name_Floor             or else
            Attribute_Chars = Name_Fraction          or else
            Attribute_Chars = Name_Image             or else
            Attribute_Chars = Name_Input             or else
            Attribute_Chars = Name_Leading_Part      or else
            Attribute_Chars = Name_Machine           or else
            Attribute_Chars = Name_Max               or else
            Attribute_Chars = Name_Min               or else
            Attribute_Chars = Name_Model             or else
            Attribute_Chars = Name_Pos               or else
            Attribute_Chars = Name_Pred              or else
            Attribute_Chars = Name_Remainder         or else
            Attribute_Chars = Name_Round             or else
            Attribute_Chars = Name_Rounding          or else
            Attribute_Chars = Name_Scaling           or else
            Attribute_Chars = Name_Succ              or else
            Attribute_Chars = Name_Truncation        or else
            Attribute_Chars = Name_Unbiased_Rounding or else
            Attribute_Chars = Name_Val               or else
            Attribute_Chars = Name_Value             or else
            Attribute_Chars = Name_Wide_Image        or else
            Attribute_Chars = Name_Wide_Value        or else

--  |A2005 start

            --  Ada 2005 attributes that are functions:
            Attribute_Chars = Name_Machine_Rounding  or else
            Attribute_Chars = Name_Mod               or else
            Attribute_Chars = Name_Wide_Wide_Image   or else
            Attribute_Chars = Name_Wide_Wide_Value   or else

--  |A2012 start
            --  Ada 2012 attributes that are functions:
--              Attribute_Chars = Name_Overlaps_Storage  or else  (SCz)
--  |A2012 end

--  |A2005 end
            --  Implementation Dependent Attributes-Functions:

            Attribute_Chars = Name_Asm_Input         or else
            Attribute_Chars = Name_Asm_Output        or else
            Attribute_Chars = Name_Enum_Rep          or else
            Attribute_Chars = Name_Enum_Val          or else
            Attribute_Chars = Name_Fixed_Value       or else
            Attribute_Chars = Name_Integer_Value     or else
            Attribute_Chars = Name_Ref
      then

         return A_Function_Call;

      elsif --  language-defined attributes which are procedures:

            Attribute_Chars = Name_Output    or else
            Attribute_Chars = Name_Read      or else
            Attribute_Chars = Name_Write

      then

         return A_Procedure_Call_Statement;

      --  language-defined attributes:

      elsif Attribute_Chars = Name_Access then
         return  An_Access_Attribute;
      elsif Attribute_Chars = Name_Address then
         return  An_Address_Attribute;
      elsif Attribute_Chars = Name_Aft then
         return  An_Aft_Attribute;
      elsif Attribute_Chars = Name_Alignment then
         return  An_Alignment_Attribute;
      elsif Attribute_Chars = Name_Base then
         return  A_Base_Attribute;
      elsif Attribute_Chars = Name_Bit_Order then
         return  A_Bit_Order_Attribute;
      elsif Attribute_Chars = Name_Body_Version then
         return  A_Body_Version_Attribute;
      elsif Attribute_Chars = Name_Callable then
         return  A_Callable_Attribute;
      elsif Attribute_Chars = Name_Caller then
         return  A_Caller_Attribute;
      elsif Attribute_Chars = Name_Class then
         return  A_Class_Attribute;
      elsif Attribute_Chars = Name_Component_Size then
         return  A_Component_Size_Attribute;
      elsif Attribute_Chars = Name_Constrained then
         return  A_Constrained_Attribute;
      elsif Attribute_Chars = Name_Count then
         return  A_Count_Attribute;
      elsif Attribute_Chars = Name_Definite then
         return  A_Definite_Attribute;
      elsif Attribute_Chars = Name_Delta then
         return  A_Delta_Attribute;
      elsif Attribute_Chars = Name_Denorm then
         return  A_Denorm_Attribute;
      elsif Attribute_Chars = Name_Digits then
         return  A_Digits_Attribute;
      elsif Attribute_Chars = Name_External_Tag then
         return  An_External_Tag_Attribute;
      elsif Attribute_Chars = Name_First then
         return  A_First_Attribute;
      elsif Attribute_Chars = Name_First_Bit then
         return  A_First_Bit_Attribute;
      elsif Attribute_Chars = Name_Fore then
         return  A_Fore_Attribute;
      elsif Attribute_Chars = Name_Identity then
         return  An_Identity_Attribute;
      elsif Attribute_Chars = Name_Last then
         return  A_Last_Attribute;
      elsif Attribute_Chars = Name_Last_Bit then
         return  A_Last_Bit_Attribute;
      elsif Attribute_Chars = Name_Length then
         return  A_Length_Attribute;
      elsif Attribute_Chars = Name_Machine_Emax then
         return  A_Machine_Emax_Attribute;
      elsif Attribute_Chars = Name_Machine_Emin then
         return  A_Machine_Emin_Attribute;
      elsif Attribute_Chars = Name_Machine_Mantissa then
         return  A_Machine_Mantissa_Attribute;
      elsif Attribute_Chars = Name_Machine_Overflows then
         return  A_Machine_Overflows_Attribute;
      elsif Attribute_Chars = Name_Machine_Radix then
         return  A_Machine_Radix_Attribute;
      elsif Attribute_Chars = Name_Machine_Rounds then
         return  A_Machine_Rounds_Attribute;
      elsif Attribute_Chars = Name_Max_Size_In_Storage_Elements then
         return  A_Max_Size_In_Storage_Elements_Attribute;
      elsif Attribute_Chars = Name_Model_Emin then
         return  A_Model_Emin_Attribute;
      elsif Attribute_Chars = Name_Model_Epsilon then
         return  A_Model_Epsilon_Attribute;
      elsif Attribute_Chars = Name_Model_Mantissa then
         return  A_Model_Mantissa_Attribute;
      elsif Attribute_Chars = Name_Model_Small then
         return  A_Model_Small_Attribute;
      elsif Attribute_Chars = Name_Modulus then
         return  A_Modulus_Attribute;
      elsif Attribute_Chars = Name_Partition_ID then
         return  A_Partition_ID_Attribute;
      elsif Attribute_Chars = Name_Position then
         return  A_Position_Attribute;
      elsif Attribute_Chars = Name_Range then   --  this alternative
         return  A_Range_Attribute;             --  never works!
      elsif Attribute_Chars = Name_Safe_First then
         return  A_Safe_First_Attribute;
      elsif Attribute_Chars = Name_Safe_Last then
         return  A_Safe_Last_Attribute;
      elsif Attribute_Chars = Name_Scale then
         return  A_Scale_Attribute;
      elsif Attribute_Chars = Name_Signed_Zeros then
         return  A_Signed_Zeros_Attribute;
      elsif Attribute_Chars = Name_Size then
         return  A_Size_Attribute;
      elsif Attribute_Chars = Name_Small then
         return  A_Small_Attribute;
      elsif Attribute_Chars = Name_Storage_Pool then
         return  A_Storage_Pool_Attribute;
      elsif Attribute_Chars = Name_Storage_Size then
         return  A_Storage_Size_Attribute;
      elsif Attribute_Chars = Name_Tag then
         return  A_Tag_Attribute;
      elsif Attribute_Chars = Name_Terminated then
         return  A_Terminated_Attribute;
      elsif Attribute_Chars = Name_Unchecked_Access then
         return  An_Unchecked_Access_Attribute;
      elsif Attribute_Chars = Name_Valid then
         return  A_Valid_Attribute;
      elsif Attribute_Chars = Name_Version then
         return  A_Version_Attribute;
      elsif Attribute_Chars = Name_Wide_Width then
         return  A_Wide_Width_Attribute;
      elsif Attribute_Chars = Name_Width then
         return  A_Width_Attribute;

   --  New Ada 2005/2012 attributes:
      elsif Attribute_Chars = Name_Priority then
         return  A_Priority_Attribute;
      elsif Attribute_Chars = Name_Stream_Size    then
         return  A_Stream_Size_Attribute;
      elsif Attribute_Chars = Name_Wide_Wide_Width then
         return  A_Wide_Wide_Width_Attribute;
      elsif Attribute_Chars = Name_Max_Alignment_For_Allocation then
         return  A_Max_Alignment_For_Allocation_Attribute;

   --  New Ada 2005/2012 attributes:

   --  Implementation Dependent Attributes:

      elsif Attribute_Chars = Name_Abort_Signal             or else
            Attribute_Chars = Name_Address_Size             or else
            Attribute_Chars = Name_Asm_Input                or else
            Attribute_Chars = Name_Asm_Output               or else
            Attribute_Chars = Name_AST_Entry                or else  -- VMS
            Attribute_Chars = Name_Bit                      or else
            Attribute_Chars = Name_Bit_Position             or else
            Attribute_Chars = Name_Code_Address             or else
            Attribute_Chars = Name_Compiler_Version         or else
            Attribute_Chars = Name_Default_Bit_Order        or else
--              Attribute_Chars = Name_Elab_Subp_Body           or else  (SCz)
            Attribute_Chars = Name_Elaborated               or else
            Attribute_Chars = Name_Emax                     or else  -- Ada 83
            Attribute_Chars = Name_Enabled                  or else
            Attribute_Chars = Name_Enum_Rep                 or else
            Attribute_Chars = Name_Enum_Val                 or else
            Attribute_Chars = Name_Epsilon                  or else  -- Ada 83
            Attribute_Chars = Name_Fixed_Value              or else
            Attribute_Chars = Name_Has_Access_Values        or else
            Attribute_Chars = Name_Has_Discriminants        or else
            Attribute_Chars = Name_Img                      or else
            Attribute_Chars = Name_Integer_Value            or else
            Attribute_Chars = Name_Invalid_Value            or else
            Attribute_Chars = Name_Large                    or else  -- Ada 83
            Attribute_Chars = Name_Machine_Size             or else
            Attribute_Chars = Name_Mantissa                 or else  -- Ada 83
            Attribute_Chars = Name_Maximum_Alignment        or else
            Attribute_Chars = Name_Mechanism_Code           or else
            Attribute_Chars = Name_Null_Parameter           or else
            Attribute_Chars = Name_Object_Size              or else
            Attribute_Chars = Name_Old                      or else
            Attribute_Chars = Name_Passed_By_Reference      or else
            Attribute_Chars = Name_Range_Length             or else
            Attribute_Chars = Name_Ref                      or else
            Attribute_Chars = Name_Result                   or else
            Attribute_Chars = Name_Safe_Emax                or else  -- Ada 83
            Attribute_Chars = Name_Safe_Large               or else  -- Ada 83
            Attribute_Chars = Name_Safe_Small               or else  -- Ada 83
            Attribute_Chars = Name_Storage_Unit             or else
--              Attribute_Chars = Name_System_Allocator_Alignment or else (SCz)
            Attribute_Chars = Name_Target_Name              or else
            Attribute_Chars = Name_To_Address               or else
            Attribute_Chars = Name_Type_Class               or else
            Attribute_Chars = Name_UET_Address              or else
            Attribute_Chars = Name_Universal_Literal_String or else
            Attribute_Chars = Name_Unrestricted_Access      or else
            Attribute_Chars = Name_VADS_Size                or else
            Attribute_Chars = Name_Value_Size               or else
            Attribute_Chars = Name_Wchar_T_Size             or else
            Attribute_Chars = Name_Word_Size                or else
            Attribute_Chars = Name_Elab_Body                or else
            Attribute_Chars = Name_Elab_Spec
      then
         return An_Implementation_Defined_Attribute;
      else
         return An_Unknown_Attribute;
      end if;

   end N_Attribute_Reference_Mapping;

   -------------------------------------
   -- N_Component_Association_Mapping --
   -------------------------------------

   function N_Component_Association_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Special cases first:

      case Nkind (Parent (Parent (Node))) is
         when N_Enumeration_Representation_Clause =>
            return An_Array_Component_Association;
         when N_Pragma_Argument_Association |
              N_Aspect_Specification        =>
            return A_Record_Component_Association;
         when others =>
            null;
      end case;

      --  Regular case:
      if Nkind (Parent (Parent (Node))) = N_Enumeration_Representation_Clause
       or else
         Ekind (Etype (Parent (Node))) in Array_Kind
      then
         return An_Array_Component_Association;
      else
         return A_Record_Component_Association;
      end if;
   end N_Component_Association_Mapping;

   --------------------------------------------
   -- N_Constrained_Array_Definition_Mapping --
   --------------------------------------------

   function N_Constrained_Array_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Constrained_Array_Definition
      --   A_Formal_Constrained_Array_Definition

      if Nkind (Parent (Node)) = N_Formal_Type_Declaration then
         return A_Formal_Constrained_Array_Definition;
      else
         return A_Constrained_Array_Definition;
      end if;

   end N_Constrained_Array_Definition_Mapping;

   -----------------------------------
   -- N_Defining_Identifier_Mapping --
   -----------------------------------

   function N_Defining_Identifier_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Template_Node : Node_Id := Empty;
   begin
      --  Several Internal_Element_Kinds values may be possible for automatic
      --  Internal Kind Determination:
      --
      --    A_Defining_Identifier
      --    A_Defining_Enumeration_Literal
      --    Internal Defining Name kinds, this happens for the extended
      --    code of the function instantiation in case if the instance
      --    defines an operator symbol - in this case the defining name of
      --    the extended function spec and body is represented by
      --    N_Defining_Identifier node.
      --
      --  Some other Internal_Element_Kinds values could be set "by-hand",
      --  for example:
      --
      --     An_Enumeration_Literal_Specification

      if Sloc (Node) > Standard_Location and then
         Get_Character (Sloc (Node)) = '"'
      then
         Template_Node := Parent (Parent (Node));

         if Pass_Generic_Actual (Template_Node) then
            --  This is the case of subprogram renaming that passes an actual
            --  for a formal operator function
            Template_Node := Corresponding_Formal_Spec (Template_Node);
         else
            --  This is the case of expanded generic subprogram having a
            --  defining operator symbol, it comes as the defining name from
            --  the corresponding instance, but is represented by
            --  N_Defining_Identifier node in the expanded code. We just
            --  go to the defining name from the instance and call
            --  N_Defining_Operator_Symbol_Mapping for it
            Template_Node := Parent (Template_Node);

            if Nkind (Template_Node) = N_Package_Specification then
               Template_Node := Parent (Template_Node);
            end if;

            Template_Node := Next (Template_Node);

            if Nkind (Template_Node) = N_Package_Body then
               --  skipping the body of wrapper package for extended subprogram
               --  body
               Template_Node := Next (Template_Node);
            end if;

            Template_Node := Defining_Unit_Name (Template_Node);
         end if;

         pragma Assert (Nkind (Template_Node) = N_Defining_Operator_Symbol);

         return N_Defining_Operator_Symbol_Mapping (Template_Node);
      else

         if Nkind (Parent (Node)) = N_Enumeration_Type_Definition then
            return A_Defining_Enumeration_Literal;
         else
            return A_Defining_Identifier;
         end if;
      end if;

   end N_Defining_Identifier_Mapping;

   ----------------------------------------
   -- N_Defining_Operator_Symbol_Mapping --
   ----------------------------------------

   function N_Defining_Operator_Symbol_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is

      Operator_Chars   : constant Name_Id := Chars (Node);
      Parameter_Number : Nat range 1 .. 2;

      --  see the GNAT components Namet.ads and Snames.ads

      --  N_Operator_Symbol_Mapping uses just the same approach,
      --  (except computing the Parameter_Number value)
      --  so if there is any error in it, then both functions contain it

   begin
      --  The following Internal_Element_Kinds values may be possible:
      --
      --    A_Defining_And_Operator,                    -- and
      --    A_Defining_Or_Operator,                     -- or
      --    A_Defining_Xor_Operator,                    -- xor
      --    A_Defining_Equal_Operator,                  -- =
      --    A_Defining_Not_Equal_Operator,              -- /=
      --    A_Defining_Less_Than_Operator,              -- <
      --    A_Defining_Less_Than_Or_Equal_Operator,     -- <=
      --    A_Defining_Greater_Than_Operator,           -- >
      --    A_Defining_Greater_Than_Or_Equal_Operator,  -- >=
      --    A_Defining_Plus_Operator,                   -- +
      --    A_Defining_Minus_Operator,                  -- -
      --    A_Defining_Concatenate_Operator,            -- &
      --    A_Defining_Unary_Plus_Operator,             -- +
      --    A_Defining_Unary_Minus_Operator,            -- -
      --    A_Defining_Multiply_Operator,               -- *
      --    A_Defining_Divide_Operator,                 -- /
      --    A_Defining_Mod_Operator,                    -- mod
      --    A_Defining_Rem_Operator,                    -- rem
      --    A_Defining_Exponentiate_Operator,           -- **
      --    A_Defining_Abs_Operator,                    -- abs
      --    A_Defining_Not_Operator,                    -- not

         if Operator_Chars = Name_Op_And then
            return A_Defining_And_Operator;

         elsif Operator_Chars = Name_Op_Or then
            return A_Defining_Or_Operator;

         elsif Operator_Chars = Name_Op_Xor then
            return A_Defining_Xor_Operator;

         elsif Operator_Chars = Name_Op_Eq then
            return A_Defining_Equal_Operator;

         elsif Operator_Chars = Name_Op_Ne then
            return A_Defining_Not_Equal_Operator;

         elsif Operator_Chars = Name_Op_Lt then
            return A_Defining_Less_Than_Operator;

         elsif Operator_Chars = Name_Op_Le then
            return A_Defining_Less_Than_Or_Equal_Operator;

         elsif Operator_Chars = Name_Op_Gt then
            return A_Defining_Greater_Than_Operator;

         elsif Operator_Chars = Name_Op_Ge then
            return A_Defining_Greater_Than_Or_Equal_Operator;

         elsif Operator_Chars = Name_Op_Concat then
            return A_Defining_Concatenate_Operator;

         elsif Operator_Chars = Name_Op_Multiply then
            return A_Defining_Multiply_Operator;

         elsif Operator_Chars = Name_Op_Divide then
            return A_Defining_Divide_Operator;

         elsif Operator_Chars = Name_Op_Mod then
            return A_Defining_Mod_Operator;

         elsif Operator_Chars = Name_Op_Rem then
            return A_Defining_Rem_Operator;

         elsif Operator_Chars = Name_Op_Expon then
            return A_Defining_Exponentiate_Operator;

         elsif Operator_Chars = Name_Op_Abs then
            return A_Defining_Abs_Operator;

         elsif Operator_Chars = Name_Op_Not then
            return A_Defining_Not_Operator;

         else
            --  for + and - operator signs binary and unary cases
            --  should be distinguished

            if Nkind (Parent_Node) = N_Function_Instantiation then
               --  we have to compute the number of parameters
               --  from the declaration of the corresponding generic
               --  function
               Parent_Node := Parent (Entity (Sinfo.Name (Parent_Node)));

               if Nkind (Parent_Node) = N_Defining_Program_Unit_Name then
                  Parent_Node := Parent (Parent_Node);
               end if;

            end if;

            Parameter_Number :=
              List_Length (Parameter_Specifications (Parent_Node));

            if Operator_Chars = Name_Op_Add then

               if Parameter_Number = 1 then
                  return A_Defining_Unary_Plus_Operator;
               else
                  return A_Defining_Plus_Operator;
               end if;

            else -- Operator_Chars = "-"

               if Parameter_Number = 1 then
                  return A_Defining_Unary_Minus_Operator;
               else
                  return A_Defining_Minus_Operator;
               end if;

            end if;

         end if;

   end N_Defining_Operator_Symbol_Mapping;

   ---------------------------------
   -- N_Delay_Alternative_Mapping --
   ---------------------------------

   function N_Delay_Alternative_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    A_Select_Path
      --    An_Or_Path

      if Is_List_Member (Node) then
         --  a delay alternative in a selective accept statement,
         --  processing is the same as for N_Accept_Alternative

         return N_Accept_Alternative_Mapping (Node);

      else
         --  a relay alternative in a timed entry call

         return An_Or_Path;
      end if;
   end N_Delay_Alternative_Mapping;

   ---------------------------------------
   -- N_Derived_Type_Definition_Mapping --
   ---------------------------------------

   function N_Derived_Type_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result : Internal_Element_Kinds := A_Derived_Type_Definition;
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Derived_Type_Definition
      --   A_Derived_Record_Extension_Definition

--  --|A2005 start
      --  Plus the following values for Ada 2005:
      --   An_Ordinary_Interface
      --   A_Limited_Interface
      --   A_Task_Interface
      --   A_Protected_Interface
      --   A_Synchronized_Interface
--  --|A2005 end

      --  Implementation revised for Ada 2005

      if Interface_Present (Node) then

         if Limited_Present (Node) then
            Result := A_Limited_Interface;
         elsif Task_Present (Node) then
            Result := A_Task_Interface;
         elsif Protected_Present (Node) then
            Result := A_Protected_Interface;
         elsif Synchronized_Present (Node) then
            Result := A_Synchronized_Interface;
         else
            Result := An_Ordinary_Interface;
         end if;

      elsif Present (Record_Extension_Part (Node)) then
         Result := A_Derived_Record_Extension_Definition;
      end if;

      return Result;
--  --|A2005 end

   end N_Derived_Type_Definition_Mapping;

   -----------------------------
   -- N_Expanded_Name_Mapping --
   -----------------------------

   function N_Expanded_Name_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Context      : constant Node_Id   := Parent_Node;
      Context_Kind : constant Node_Kind := Nkind  (Context);
      Temp_Node    : Node_Id;

   begin
      --  The following Internal_Element_Kinds values may be possible:
      --
      --     A_Subtype_Indication
      --     A_Discrete_Subtype_Indication_As_Subtype_Definition
      --     A_Discrete_Subtype_Indication
      --     A_Selected_Component
      --     An_Identifier (we use this routine for An_Identifier node as well)
      --     A_Function_Call (F in F.X)
      --     An_Attribute_Reference

         case Context_Kind is
            --  special cases should be reorganized when complete ???

            when  N_Object_Declaration =>

               if Node = Object_Definition (Context) then
                  return A_Subtype_Indication;
               else
                  --  an initializing expression in an object declaration
                  goto Expr;
               end if;

            when  N_Derived_Type_Definition
                | N_Access_To_Object_Definition
                                                =>
--  --|A2005 start
               if Is_List_Member (Node) then
                  --  The node represents an interface name from some interface
                  --  list
                  if Nkind (Node) = N_Expanded_Name then
                     return A_Selected_Component;
                  else
                     return An_Identifier;
                  end if;

               else
                  return A_Subtype_Indication;
               end if;
--  --|A2005 end

            when N_Constrained_Array_Definition |
                 N_Entry_Declaration =>

               return A_Discrete_Subtype_Indication_As_Subtype_Definition;

            when N_Unconstrained_Array_Definition =>

               if Is_List_Member (Node) then
               --  this for sure means, that node represents one of index
               --  subtype definitions

                  --  CODE SHARING WITH N_IDENTIFIER MAPPING ITEM :-[#]

                  if Nkind (Node) = N_Expanded_Name then
                     return A_Selected_Component;
                  else
                     return An_Identifier;
                  end if;
               else
                  --  this is a component definition!
                  return A_Component_Definition;
               end if;

            when N_Index_Or_Discriminant_Constraint =>

               if Asis_Internal_Element_Kind (Context) =
                  An_Index_Constraint
               then
                  return A_Discrete_Subtype_Indication;
               end if;

            when N_Slice =>

               --       A.B(C.D) <- Context
               --       /     \
               --  Prefix    Discrete_Range

               if Node = Sinfo.Discrete_Range (Context) then
                  return A_Discrete_Subtype_Indication;
               end if;

            --  when N_??? => should be implemented

            when N_Selected_Component =>

               --  This corresponds to a special case: F.A, where F is
               --  a function call
               Temp_Node := Prefix (Context);

               if Is_Rewrite_Substitution (Temp_Node) and then
                  Nkind (Temp_Node) = N_Function_Call and then
                  Original_Node (Temp_Node) = Node
               then
                  return A_Function_Call;
               end if;

            when N_Parameter_Specification =>
               --  See FA13-008. In case if we have an implicit "/="
               --  declaration, and in the corresponding explicit "="
               --  declaration parameter type is defined by a 'Class attribute,
               --  in the parameter specification of "/=" the front-end uses
               --  the reference to the internal type entity node

               if Nkind (Node) = N_Identifier
                and then
                 not Comes_From_Source (Node)
                and then
                 Ekind (Entity (Node)) = E_Class_Wide_Type
               then
                  return A_Class_Attribute;
               end if;

            when others =>
               null;
         end case;

         --  general case, the following if statement is necessary because
         --  of sharing of this code between N_Expanded_Name and N_Identifier
         --  mapping items.

         --  IS THIS CODE SHARING A REALLY GOOD THING???

         <<Expr>> --  here we are analyzing the "ordinary" expression
         if Nkind (Node) = N_Expanded_Name then
            return A_Selected_Component;

         elsif Context_Kind /= N_Defining_Program_Unit_Name then

            if (Nkind (Node) in N_Has_Entity
               or else
                Nkind (Node) = N_Attribute_Definition_Clause)
              and then
                Entity_Present (Node)
              and then
               Ekind (Entity (Node)) = E_Enumeration_Literal
            then
               return An_Enumeration_Literal;
            else
               return An_Identifier;
            end if;

         else

            return An_Identifier;
         end if;

   end N_Expanded_Name_Mapping;

   ---------------------------------------
   -- N_Formal_Type_Declaration_Mapping --
   ---------------------------------------

   function N_Formal_Type_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
--        if Nkind (Sinfo.Formal_Type_Definition (Node)) = (SCz)
--           N_Formal_Incomplete_Type_Definition
--        then
--           return A_Formal_Incomplete_Type_Declaration;
--        else
         return A_Formal_Type_Declaration;
--        end if;
   end N_Formal_Type_Declaration_Mapping;

   ------------------------------------------
   -- N_Formal_Package_Declaration_Mapping --
   ------------------------------------------

   function N_Formal_Package_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Formal_Package_Declaration,
      --   A_Formal_Package_Declaration_With_Box

         if Box_Present (Node) then
            return A_Formal_Package_Declaration_With_Box;
         else
            return A_Formal_Package_Declaration;
         end if;

   end N_Formal_Package_Declaration_Mapping;

   ----------------------------------------------
   -- N_Formal_Private_Type_Definition_Mapping --
   ----------------------------------------------

   function N_Formal_Private_Type_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Formal_Private_Type_Definition
      --   A_Formal_Tagged_Private_Type_Definition

         if Tagged_Present (Node) then
            return A_Formal_Tagged_Private_Type_Definition;
         else
            return A_Formal_Private_Type_Definition;
         end if;

   end N_Formal_Private_Type_Definition_Mapping;

   ---------------------------------------------
   -- N_Formal_Subprogram_Declaration_Mapping --
   ---------------------------------------------

   function N_Formal_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    A_Formal_Procedure_Declaration
      --    A_Formal_Function_Declaration.

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Formal_Function_Declaration;
         else
            return A_Formal_Procedure_Declaration;
         end if;

   end N_Formal_Subprogram_Declaration_Mapping;

   -----------------------------
   -- N_Function_Call_Mapping --
   -----------------------------

   function N_Function_Call_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Called_Name     : Node_Id;
      Called_Function : Node_Id                := Empty;
      Result          : Internal_Element_Kinds := A_Function_Call;
   begin
      --  Three Internal_Element_Kinds values may be possible:
      --     A_Function_Call (usual situation)
      --     A_Selected_Component
      --     An_Enumeration_Literal
      --  The last two cases correspond to a reference to an overloaded
      --  enumeration literal (either qualified or direct)

      if No (Parameter_Associations (Node)) then

         Called_Name := Sinfo.Name (Node);

         if Nkind (Called_Name) in N_Has_Entity then
            Called_Function := Entity (Called_Name);
         end if;

         if Nkind (Parent (Called_Function)) =
            N_Enumeration_Type_Definition
         then

            if Nkind (Called_Name) = N_Selected_Component or else -- ???
               Nkind (Called_Name) = N_Expanded_Name
            then
               Result := A_Selected_Component;
            elsif Nkind (Called_Name) = N_Identifier then
               Result := An_Enumeration_Literal;
            end if;

         end if;

      end if;

      return Result;

   end N_Function_Call_Mapping;

   ----------------------------------------------
   -- N_Generic_Subprogram_Declaration_Mapping --
   ----------------------------------------------

   function N_Generic_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --  A_Generic_Procedure_Declaration and A_Generic_Function_Declaration

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Generic_Function_Declaration;
         else
            return A_Generic_Procedure_Declaration;
         end if;

   end N_Generic_Subprogram_Declaration_Mapping;

--  |A2005 start
   -------------------------------------------
   -- N_Incomplete_Type_Declaration_Mapping --
   -------------------------------------------

   function N_Incomplete_Type_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result : Internal_Element_Kinds := An_Incomplete_Type_Declaration;
   begin
      if Tagged_Present (Node) then
         Result := A_Tagged_Incomplete_Type_Declaration;
      end if;

      return Result;
   end N_Incomplete_Type_Declaration_Mapping;
--  |A2005 end

   ------------------------------------------------
   -- N_Index_Or_Discriminant_Constraint_Mapping --
   ------------------------------------------------

   function N_Index_Or_Discriminant_Constraint_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is

      First_Item : Node_Id;
      --  the first element in the list of discrete ranges or discriminant
      --  associations, this element is the only one used to determine the kind
      --  of the constraint being analyzed

      First_Item_Kind : Node_Kind;

      Type_Entity : Node_Id;
      --  Needed in case when we can not make the decision using the syntax
      --  information only. Represents the type or subtype entity to which the
      --  constraint is applied

   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    An_Index_Constraint
      --    A_Discriminant_Constraint

      First_Item      := First (Constraints (Node));
      First_Item_Kind := Nkind (First_Item);

      --  analyzing the syntax structure of First_Item:

      if First_Item_Kind = N_Discriminant_Association then
         return A_Discriminant_Constraint;

      elsif First_Item_Kind = N_Subtype_Indication or else
            First_Item_Kind = N_Range
      then
         return An_Index_Constraint;

      elsif First_Item_Kind = N_Attribute_Reference then

         --  analyzing the attribute designator:
         if Attribute_Name (First_Item) = Name_Range then
            return An_Index_Constraint;
         else
            return A_Discriminant_Constraint;
         end if;

      elsif not (First_Item_Kind = N_Identifier or else
                 First_Item_Kind = N_Expanded_Name)
      then
         --  First_Item is an expression and it could not be interpreted as a
         --  subtype_mark from a discrete_subtype_indication, so what we have
         --  in this case is:
         return A_Discriminant_Constraint;
      end if;

      --  First_Item is of N_Identifier or N_Expanded_Name kind, and it may be
      --  either an expression in index constraint or a subtype mark in a
      --  discriminant constraint. In this case it is easier to analyze the
      --  type to which the constraint is applied, but not the constraint
      --  itself.

      Type_Entity := Entity (Sinfo.Subtype_Mark (Parent (Node)));

      while Ekind (Type_Entity) in Access_Kind loop
         Type_Entity := Directly_Designated_Type (Type_Entity);
      end loop;

      if Has_Discriminants (Type_Entity) then
         return A_Discriminant_Constraint;
      else
         return An_Index_Constraint;
      end if;

   end N_Index_Or_Discriminant_Constraint_Mapping;

   --------------------------------------
   -- N_Iterator_Specification_Mapping --
   --------------------------------------

   function N_Iterator_Specification_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result : Internal_Element_Kinds := A_Generalized_Iterator_Specification;
   begin
      if Of_Present (Node) then
         Result := An_Element_Iterator_Specification;
      end if;

      return Result;
   end N_Iterator_Specification_Mapping;

   ------------------------------
   -- N_Loop_Statement_Mapping --
   ------------------------------

   function N_Loop_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Iteration : Node_Id;
   begin
      --  Three Internal_Element_Kinds values may be possible:
      --    A_Loop_Statement,
      --    A_While_Loop_Statement,
      --    A_For_Loop_Statement,

      Iteration := Iteration_Scheme (Node);

      if Present (Iteration) then

         if Present (Condition (Iteration)) then
            return A_While_Loop_Statement;
         else
            return A_For_Loop_Statement;
         end if;

      else
         return A_Loop_Statement;
      end if;

   end N_Loop_Statement_Mapping;

   ----------------------------------
   -- N_Number_Declaration_Mapping --
   ----------------------------------

   function N_Number_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    An_Integer_Number_Declaration
      --    A_Real_Number_Declaration

         if Ekind (Defining_Identifier (Node)) = E_Named_Integer then
            return An_Integer_Number_Declaration;
         else
            return A_Real_Number_Declaration;
         end if;

   end N_Number_Declaration_Mapping;

   ----------------------------------
   -- N_Object_Declaration_Mapping --
   ----------------------------------

   function N_Object_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result : Internal_Element_Kinds;
   begin
      --  Three Internal_Element_Kinds values may be possible:
      --     A_Variable_Declaration,
      --     A_Constant_Declaration,
      --     A_Deferred_Constant_Declaration.
--  |A2005 start
      --     A_Return_Variable_Specification
      --     A_Return_Constant_Specification
--  |A2005 end

      if Nkind (Parent (Node)) = N_Extended_Return_Statement then
         if not Constant_Present (Node) then
            Result := A_Return_Variable_Specification;
         else
            Result := A_Return_Constant_Specification;
         end if;
      elsif not Constant_Present (Node) then
         Result := A_Variable_Declaration;
      elsif Present (Sinfo.Expression (Node)) then
         Result := A_Constant_Declaration;
      else
         Result := A_Deferred_Constant_Declaration;
      end if;

      return Result;

   end N_Object_Declaration_Mapping;

   -------------------------------
   -- N_Operator_Symbol_Mapping --
   -------------------------------

   function N_Operator_Symbol_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Tmp              : Node_Id;
      Operator_Chars   : constant Name_Id := Chars (Node);
      Parameter_Number : Nat range 1 .. 2;

      --  see the GNAT components Namet.ads and Snames.ads

      --  N_Defining_Operator_Symbol_Mapping uses just the same approach,
      --  so if there is any error in it, then both functions contain it

   begin
      --  The following Internal_Element_Kinds values may be possible:
      --
      --    A_And_Operator,                    -- and
      --    A_Or_Operator,                     -- or
      --    A_Xor_Operator,                    -- xor
      --    A_Equal_Operator,                  -- =
      --    A_Not_Equal_Operator,              -- /=
      --    A_Less_Than_Operator,              -- <
      --    A_Less_Than_Or_Equal_Operator,     -- <=
      --    A_Greater_Than_Operator,           -- >
      --    A_Greater_Than_Or_Equal_Operator,  -- >=
      --    A_Plus_Operator,                   -- +
      --    A_Minus_Operator,                  -- -
      --    A_Concatenate_Operator,            -- &
      --    A_Unary_Plus_Operator,             -- +
      --    A_Unary_Minus_Operator,            -- -
      --    A_Multiply_Operator,               -- *
      --    A_Divide_Operator,                 -- /
      --    A_Mod_Operator,                    -- mod
      --    A_Rem_Operator,                    -- rem
      --    A_Exponentiate_Operator,           -- **
      --    A_Abs_Operator,                    -- abs
      --    A_Not_Operator,                    -- not

         if Operator_Chars = Name_Op_And then
            return An_And_Operator;

         elsif Operator_Chars = Name_Op_Or then
            return An_Or_Operator;

         elsif Operator_Chars = Name_Op_Xor then
            return An_Xor_Operator;

         elsif Operator_Chars = Name_Op_Eq then
            return An_Equal_Operator;

         elsif Operator_Chars = Name_Op_Ne then
            return A_Not_Equal_Operator;

         elsif Operator_Chars = Name_Op_Lt then
            return A_Less_Than_Operator;

         elsif Operator_Chars = Name_Op_Le then
            return A_Less_Than_Or_Equal_Operator;

         elsif Operator_Chars = Name_Op_Gt then
            return A_Greater_Than_Operator;

         elsif Operator_Chars = Name_Op_Ge then
            return A_Greater_Than_Or_Equal_Operator;

         elsif Operator_Chars = Name_Op_Concat then
            return A_Concatenate_Operator;

         elsif Operator_Chars = Name_Op_Multiply then
            return A_Multiply_Operator;

         elsif Operator_Chars = Name_Op_Divide then
            return A_Divide_Operator;

         elsif Operator_Chars = Name_Op_Mod then
            return A_Mod_Operator;

         elsif Operator_Chars = Name_Op_Rem then
            return A_Rem_Operator;

         elsif Operator_Chars = Name_Op_Expon then
            return An_Exponentiate_Operator;

         elsif Operator_Chars = Name_Op_Abs then
            return An_Abs_Operator;

         elsif Operator_Chars = Name_Op_Not then
            return A_Not_Operator;

         else
            --  for + and - operator signs binary and unary cases
            --  should be distinguished

            --  A simple case - we have an entity:

            if Entity_Present (Node) then
               Tmp := Entity (Node);

               if First_Entity (Tmp) = Last_Entity (Tmp) then
                  Parameter_Number := 1;
               else
                  Parameter_Number := 2;
               end if;

            else

               Parent_Node := Parent (Node);

               if Nkind (Parent_Node) = N_Integer_Literal or else
                  Nkind (Parent_Node) = N_Real_Literal
               then
                  --  Static expression of the form "+"(1, 2) rewritten into
                  --  a literal value
                  Parent_Node := Original_Node (Parent_Node);
               end if;

               --  we have to do this assignment also here, because this
               --  function may be called outside Node_To_Element
               --  convertors

               --  because of the possible tree rewriting, the parent node
               --  may be either of N_Function_Call or of N_Op_Xxx type)

               --  it can be also of N_Formal_Subprogram_Declaration kind,
               --  or even of N_Expanded_Name kind,

               while Nkind (Parent_Node) = N_Expanded_Name loop
                  Parent_Node := Parent (Parent_Node);

                  if Nkind (Parent_Node) = N_Integer_Literal or else
                     Nkind (Parent_Node) = N_Real_Literal
                  then
                     --  Static expression of the form
                     --       Prefix_Name."+"(1, 2)
                     --  rewritten into a literal value
                     Parent_Node := Original_Node (Parent_Node);
                  end if;

               end loop;

               if   Nkind (Parent_Node) = N_Op_Plus or else
                    Nkind (Parent_Node) = N_Op_Minus
               then
                  Parameter_Number := 1;

               elsif Nkind (Parent_Node) = N_Op_Add or else
                     Nkind (Parent_Node) = N_Op_Subtract
               then
                  Parameter_Number := 2;

               elsif Nkind (Parent_Node) = N_Function_Call then
                  Parameter_Number :=
                    List_Length (Parameter_Associations (Parent_Node));

               elsif Nkind (Parent_Node) = N_Subprogram_Renaming_Declaration
                   or else
                     Nkind (Parent_Node) in N_Formal_Subprogram_Declaration
               then
                  Parameter_Number := List_Length
                    (Parameter_Specifications (Specification (Parent_Node)));

               elsif Nkind (Parent_Node) = N_Indexed_Component then
                  Parameter_Number := List_Length
                    (Sinfo.Expressions (Parent_Node));

               elsif Nkind (Parent_Node) = N_Pragma_Argument_Association then
                  --  this is for pragma inline ("+");
                  Parameter_Number := 2;
                  --  this choice is somewhat arbitrary :)
               elsif Nkind (Parent_Node) = N_Generic_Association then
                  Tmp := Defining_Gen_Parameter (Node);

                  Parameter_Number := List_Length
                     (Parameter_Specifications (Parent (Tmp)));
               else
                  --  Impossible case
                  raise Internal_Implementation_Error;
               end if;

            end if;

            if Operator_Chars = Name_Op_Add then
               if Parameter_Number = 1 then
                  return A_Unary_Plus_Operator;
               else
                  return A_Plus_Operator;
               end if;
            else -- Operator_Chars = "-"
               if Parameter_Number = 1 then
                  return A_Unary_Minus_Operator;
               else
                  return A_Minus_Operator;
               end if;
            end if;

         end if;

   end N_Operator_Symbol_Mapping;

   ----------------------
   -- N_Pragma_Mapping --
   ----------------------

   function N_Pragma_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Pragma_Chars : constant Name_Id := Pragma_Name (Node);

   begin

      -- Language-Defined Pragmas --

      if    Pragma_Chars = Name_All_Calls_Remote then
         return An_All_Calls_Remote_Pragma;      -- I.2.3(6)

      elsif Pragma_Chars = Name_Asynchronous then
         return An_Asynchronous_Pragma;          -- I.4.1(3)

      elsif Pragma_Chars = Name_Atomic then
         return An_Atomic_Pragma;                 -- G.5(3)

      elsif Pragma_Chars = Name_Atomic_Components then
         return An_Atomic_Components_Pragma;      -- G.5(3)

      elsif Pragma_Chars = Name_Attach_Handler then
         return An_Attach_Handler_Pragma;         -- G.3.1(3)

      elsif Pragma_Chars = Name_Controlled then
         return A_Controlled_Pragma;              -- 13.11.3(3), B(12)

      elsif Pragma_Chars = Name_Convention then
         return A_Convention_Pragma;              -- B(16), M.1(5)

      elsif Pragma_Chars = Name_Discard_Names then
         return A_Discard_Names_Pragma;           -- C.5(2)

      elsif Pragma_Chars = Name_Elaborate then
         return An_Elaborate_Pragma;              -- 10.2.1(20)

      elsif Pragma_Chars = Name_Elaborate_All then
         return An_Elaborate_All_Pragma;          -- 10.2.1(19), B(8)

      elsif Pragma_Chars = Name_Elaborate_Body then
         return An_Elaborate_Body_Pragma;         -- 10.2.1(21), B(9)

      elsif Pragma_Chars = Name_Export then
         return An_Export_Pragma;                 -- B(15), M.1(5)

      elsif Pragma_Chars = Name_Import then
         return An_Import_Pragma;                 -- B(14), M.1(5)

      elsif Pragma_Chars = Name_Inline then
         return An_Inline_Pragma;                 -- 6.3.2(4), B(5)

      elsif Pragma_Chars = Name_Inspection_Point then
         return An_Inspection_Point_Pragma;       -- L.2.2(2)

      elsif Pragma_Chars = Name_Interrupt_Handler then
         return An_Interrupt_Handler_Pragma;      -- G.3.1(2)

      elsif Pragma_Chars = Name_Interrupt_Priority then
         return An_Interrupt_Priority_Pragma;     -- H.1(4)

      elsif Pragma_Chars = Name_Linker_Options then
         return A_Linker_Options_Pragma;          -- B.1(8)

      elsif Pragma_Chars = Snames.Name_List then
         return A_List_Pragma;                    -- 2.8(18), B(2)

      elsif Pragma_Chars = Name_Locking_Policy then
         return A_Locking_Policy_Pragma;          -- H.3(3)

      elsif Pragma_Chars = Name_Normalize_Scalars then
         return A_Normalize_Scalars_Pragma;       -- L.1.1(2)

      elsif Pragma_Chars = Name_Optimize then
         return An_Optimize_Pragma;               -- 2.8(18), B(4)

      elsif Pragma_Chars = Name_Pack then
         return A_Pack_Pragma;                    -- 13.2(2), B(11)

      elsif Pragma_Chars = Name_Page then
         return A_Page_Pragma;                    -- 2.8(18), B(3)

      elsif Pragma_Chars = Name_Preelaborate then
         return A_Preelaborate_Pragma;            -- 10.2.1(3), B(6)

      elsif Pragma_Chars = Name_Priority then
         return A_Priority_Pragma;                -- H.1(3)

      elsif Pragma_Chars = Name_Pure then
         return A_Pure_Pragma;                    -- 10.2.1(13), B(7)

      elsif Pragma_Chars = Name_Queuing_Policy then
         return A_Queuing_Policy_Pragma;          -- H.4(3)

      elsif Pragma_Chars = Name_Remote_Call_Interface then
         return A_Remote_Call_Interface_Pragma;   -- I.2.3(4)

      elsif Pragma_Chars = Name_Remote_Types then
         return A_Remote_Types_Pragma;            -- I.2.2(4)

      elsif Pragma_Chars = Name_Restrictions then
         return A_Restrictions_Pragma;            -- 13.12(2), B(13)

      elsif Pragma_Chars = Name_Reviewable then
         return A_Reviewable_Pragma;              -- L.2.1(2)

      elsif Pragma_Chars = Name_Shared_Passive then
         return A_Shared_Passive_Pragma;          -- I.2.1(4)

      elsif Pragma_Chars = Name_Storage_Size then
         --  the same name entry as for 'Storage_Size attribute!
         return A_Storage_Size_Pragma;          -- 13.3(62)

      elsif Pragma_Chars = Name_Suppress then
         return A_Suppress_Pragma;                -- 11.5(4), B(10)

      elsif Pragma_Chars = Name_Task_Dispatching_Policy then
         return A_Task_Dispatching_Policy_Pragma; -- H.2.2(2)

      elsif Pragma_Chars = Name_Volatile then
         return A_Volatile_Pragma;                -- G.5(3)

      elsif Pragma_Chars = Name_Volatile_Components then
         return A_Volatile_Components_Pragma;     -- G.5(3)

--  --|A2005 start
--  New Ada 2005 pragmas. To be alphabetically ordered later
      elsif Pragma_Chars = Name_Assert           then
         return An_Assert_Pragma;
      elsif Pragma_Chars = Name_Assertion_Policy then
         return An_Assertion_Policy_Pragma;
      elsif Pragma_Chars = Name_Detect_Blocking  then
         return A_Detect_Blocking_Pragma;
      elsif Pragma_Chars = Name_No_Return  then
         return A_No_Return_Pragma;
--    A_Partition_Elaboration_Policy_Pragma  - not implemented yet!

      elsif Pragma_Chars = Name_Preelaborable_Initialization  then
         return A_Preelaborable_Initialization_Pragma;
      elsif Pragma_Chars = Name_Priority_Specific_Dispatching  then
         return A_Priority_Specific_Dispatching_Pragma;
      elsif Pragma_Chars = Name_Profile  then
         return A_Profile_Pragma;
      elsif Pragma_Chars = Name_Relative_Deadline  then
         return A_Relative_Deadline_Pragma;
      elsif Pragma_Chars = Name_Unchecked_Union  then
         return An_Unchecked_Union_Pragma;
      elsif Pragma_Chars = Name_Unsuppress  then
         return An_Unsuppress_Pragma;

--  --|A2005 end

--  --|A2012 start
      --  New Ada 2012 pragmas. To be alphabetically ordered later
      elsif Pragma_Chars = Name_Default_Storage_Pool then
         return A_Default_Storage_Pool_Pragma;
--        elsif Pragma_Chars = Name_Dispatching_Domain then  SCz
--           return A_Dispatching_Domain_Pragma;
      elsif Pragma_Chars = Name_CPU then
         return A_CPU_Pragma;
      elsif Pragma_Chars = Name_Independent then
         return An_Independent_Pragma;
      elsif Pragma_Chars = Name_Independent_Components then
         return A_Independent_Components_Pragma;
      --  To be continued...
--  --|A2012 end

      -- Implementation(GNAT)-Defined Pragmas --

      elsif Pragma_Chars in First_Pragma_Name .. Last_Pragma_Name then
         --  We have already checked for all the standard pragma names, so
         --  all the rest known as pragma name should be GNAT-specific pragmas.
         return An_Implementation_Defined_Pragma; -- Vendor Appendix M
      else
         return An_Unknown_Pragma;         -- Unknown to the compiler.
      end if;

   end N_Pragma_Mapping;

   ----------------------------------------
   -- N_Procedure_Call_Statement_Mapping --
   ----------------------------------------

   function N_Procedure_Call_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Parent_N : Node_Id                := Parent (Parent (Node));
      Result   : Internal_Element_Kinds := A_Procedure_Call_Statement;
   begin
      --  The only special case we have to process is a procedure call that is
      --  an argument of a pragma Debug. To satisfy the Ada syntax, we have to
      --  classify it as A_Function_Call (see G330-002).

      if Nkind (Parent_N) = N_Block_Statement
        and then
         not Comes_From_Source (Parent_N)
      then
         Parent_N := Parent (Parent_N);

         if Nkind (Parent_N) = N_If_Statement
           and then
            Nkind (Original_Node (Parent_N)) = N_Pragma
           and then
            Pragma_Name (Original_Node (Parent_N)) = Name_Debug
         then
            Result := A_Function_Call;
         end if;

      end if;

      return Result;
   end N_Procedure_Call_Statement_Mapping;

   -------------------------------------
   -- N_Quantified_Expression_Mapping --
   -------------------------------------

   function N_Quantified_Expression_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result : Internal_Element_Kinds := A_For_Some_Quantified_Expression;
   begin
      if All_Present (Node) then
         Result := A_For_All_Quantified_Expression;
      end if;

      return Result;
   end N_Quantified_Expression_Mapping;

   --------------------------------
   -- N_Range_Constraint_Mapping --
   --------------------------------

   function N_Range_Constraint_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Range_Attribute_Reference
      --   A_Simple_Expression_Range

         if Nkind (Original_Node (Range_Expression (Node))) = N_Range then
            return A_Simple_Expression_Range;
         else
            return A_Range_Attribute_Reference;
         end if;

   end N_Range_Constraint_Mapping;

   ---------------------
   -- N_Range_Mapping --
   ---------------------

   function N_Range_Mapping (Node : Node_Id) return Internal_Element_Kinds is
      Context_Kind : constant Node_Kind := Nkind (Original_Node (Parent_Node));
   begin
      --  The following Internal_Element_Kinds values may be possible:
      --     A_Discrete_Simple_Expression_Range_As_Subtype_Definition
      --     A_Discrete_Simple_Expression_Range
      --     ???
      --     Other values should be added during constructing the
      --     full implementation

         case Context_Kind is

         when   N_Constrained_Array_Definition
              | N_Entry_Declaration =>

            return A_Discrete_Simple_Expression_Range_As_Subtype_Definition;

         when   N_Index_Or_Discriminant_Constraint |
                N_Slice                            |
                N_Case_Statement_Alternative       |
                N_Component_Association
                =>

            return A_Discrete_Simple_Expression_Range;

         when N_In     |
              N_Not_In =>
            return A_Simple_Expression_Range;
         when others => -- not implemented cases (???)

            Not_Implemented_Mapping (Nkind (Node));

         end case;

   end N_Range_Mapping;

   ---------------------------------
   -- N_Record_Definition_Mapping --
   ---------------------------------

   function N_Record_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Result    : Internal_Element_Kinds := A_Record_Type_Definition;
      Is_Formal : constant Boolean       :=
        Nkind (Original_Node (Parent (Node))) = N_Formal_Type_Declaration;
   begin
      --  Two Internal_Element_Kinds values may be possible (Ada 95):
      --   A_Record_Type_Definition
      --   A_Tagged_Record_Type_Definition

--  --|A2005 start
      --  Plus the following values for Ada 2005:
      --
      --   An_Ordinary_Interface
      --   A_Limited_Interface
      --   A_Task_Interface
      --   A_Protected_Interface
      --   A_Synchronized_Interface
      --
      --   A_Formal_Ordinary_Interface
      --   A_Formal_Limited_Interface
      --   A_Formal_Task_Interface
      --   A_Formal_Protected_Interface
      --   A_Formal_Synchronized_Interface
--  --|A2005 end

      --  Implementation revised for Ada 2005

      if Interface_Present (Node) then

         if Limited_Present (Node) then

            if Is_Formal then
               Result := A_Formal_Limited_Interface;
            else
               Result := A_Limited_Interface;
            end if;

         elsif Task_Present (Node) then

            if Is_Formal then
               Result := A_Formal_Task_Interface;
            else
               Result := A_Task_Interface;
            end if;

         elsif Protected_Present (Node) then

            if Is_Formal then
               Result := A_Formal_Protected_Interface;
            else
               Result := A_Protected_Interface;
            end if;

         elsif Synchronized_Present (Node) then

            if Is_Formal then
               Result := A_Formal_Synchronized_Interface;
            else
               Result := A_Synchronized_Interface;
            end if;

         else

            if Is_Formal then
               Result := A_Formal_Ordinary_Interface;
            else
               Result := An_Ordinary_Interface;
            end if;

         end if;

      elsif Tagged_Present (Node) then
         Result := A_Tagged_Record_Type_Definition;
      end if;

      return Result;
--  --|A2005 end
   end N_Record_Definition_Mapping;

   ---------------------------------
   -- N_Requeue_Statement_Mapping --
   ---------------------------------

   function N_Requeue_Statement_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   A_Requeue_Statement
      --   A_Requeue_Statement_With_Abort

      if Abort_Present (Node) then
         return A_Requeue_Statement_With_Abort;
      else
         return A_Requeue_Statement;
      end if;

   end N_Requeue_Statement_Mapping;

   -------------------------------
   -- N_Subprogram_Body_Mapping --
   -------------------------------

   function N_Subprogram_Body_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --  A_Procedure_Body_Declaration and A_Function_Body_Declaration

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Function_Body_Declaration;
         else
            return A_Procedure_Body_Declaration;
         end if;

   end N_Subprogram_Body_Mapping;

   ------------------------------------
   -- N_Subprogram_Body_Stub_Mapping --
   ------------------------------------

   function N_Subprogram_Body_Stub_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --  A_Procedure_Body_Stub and A_Function_Body_Stub,

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Function_Body_Stub;
         else
            return A_Procedure_Body_Stub;
         end if;

   end N_Subprogram_Body_Stub_Mapping;

   --------------------------------------
   -- N_Subprogram_Declaration_Mapping --
   --------------------------------------

   function N_Subprogram_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --    A_Procedure_Declaration
      --    A_Function_Declaration

         if Nkind (Specification (Node)) = N_Function_Specification then
            return A_Function_Declaration;
--  --|A2005 start
         elsif Null_Present (Specification (Node)) then
            return A_Null_Procedure_Declaration;
--  --|A2005 end
         else
            return A_Procedure_Declaration;
         end if;

   end N_Subprogram_Declaration_Mapping;

   -----------------------------------------------
   -- N_Subprogram_Renaming_Declaration_Mapping --
   -----------------------------------------------

   function N_Subprogram_Renaming_Declaration_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --  A_Procedure_Renaming_Declaration and A_Function_Renaming_Declaration

      if Nkind (Specification (Node)) = N_Function_Specification then
         return A_Function_Renaming_Declaration;
      else
         return A_Procedure_Renaming_Declaration;
      end if;
   end N_Subprogram_Renaming_Declaration_Mapping;

   ----------------------------------
   -- N_Subtype_Indication_Mapping --
   ----------------------------------

   function N_Subtype_Indication_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  The following Internal_Element_Kinds values may be possible:
      --     A_Discrete_Subtype_Indication_As_Subtype_Definition
      --     A_Discrete_Subtype_Indication
      --     A_Subtype_Indication

      case Nkind (Parent_Node) is

         when   N_Constrained_Array_Definition =>

            if Is_List_Member (Node) then
               return A_Discrete_Subtype_Indication_As_Subtype_Definition;
            end if;

         when N_Entry_Declaration =>
            return A_Discrete_Subtype_Indication_As_Subtype_Definition;

         when   N_Index_Or_Discriminant_Constraint |
                N_Slice                        =>

            return A_Discrete_Subtype_Indication;
         when others =>
            null;
      end case;

      return A_Subtype_Indication;
   end N_Subtype_Indication_Mapping;

   -------------------------------
   -- N_Use_Type_Clause_Mapping --
   -------------------------------

   function N_Use_Type_Clause_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      if All_Present (Node) then
         return A_Use_All_Type_Clause;
      else
         return A_Use_Type_Clause;
      end if;
   end N_Use_Type_Clause_Mapping;

   ----------------------
   -- N_To_E_List_New  --
   ----------------------

   function N_To_E_List_New
     (List             : List_Id;
      Include_Pragmas  : Boolean                := False;
      Starting_Element : Asis.Element           := Asis.Nil_Element;
      Node_Knd         : Node_Kind              := N_Empty;
      Internal_Kind    : Internal_Element_Kinds := Not_An_Element;
      Special_Case     : Special_Cases          := Not_A_Special_Case;
      Norm_Case        : Normalization_Cases    := Is_Not_Normalized;
      In_Unit          : Asis.Compilation_Unit  := Asis.Nil_Compilation_Unit)
      return Asis.Element_List
   is
   begin

      Set_Element_List
        (List,
         Include_Pragmas,
         Starting_Element,
         Node_Knd,
         Internal_Kind,
         Special_Case,
         Norm_Case,
         In_Unit);

      return Asis.Association_List
               (Internal_Asis_Element_Table.Table
                  (1 .. Internal_Asis_Element_Table.Last));

   end N_To_E_List_New;

   ----------------------------------------------
   -- N_Unconstrained_Array_Definition_Mapping --
   ----------------------------------------------

   function N_Unconstrained_Array_Definition_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
   begin
      --  Two Internal_Element_Kinds values may be possible:
      --   An_Unconstrained_Array_Definition
      --   A_Formal_Unconstrained_Array_Definition

      if Nkind (Parent (Node)) = N_Formal_Type_Declaration then
         return A_Formal_Unconstrained_Array_Definition;
      else
         return An_Unconstrained_Array_Definition;
      end if;

   end N_Unconstrained_Array_Definition_Mapping;

   ----------------
   -- No_Mapping --
   ----------------

   procedure No_Mapping (Node : Node_Id) is
   begin
      --  This function should never be called!
      raise Internal_Implementation_Error;
   end No_Mapping;

   -------------------------
   -- Node_To_Element_New --
   -------------------------

   function Node_To_Element_New
     (Node                     : Node_Id;
      Node_Field_1             : Node_Id                := Empty;
      Node_Field_2             : Node_Id                := Empty;
      Starting_Element         : Asis.Element           := Asis.Nil_Element;
      Internal_Kind            : Internal_Element_Kinds := Not_An_Element;
      Spec_Case                : Special_Cases          := Not_A_Special_Case;
      Norm_Case                : Normalization_Cases    := Is_Not_Normalized;
      Considering_Parent_Count : Boolean                := True;
      Using_Original_Node      : Boolean                := True;
      Inherited                : Boolean                := False;
      In_Unit                  : Asis.Compilation_Unit  :=
         Asis.Nil_Compilation_Unit)
      return Asis.Element
   is
      R_Node                   : Node_Id;
      Res_Node                 : Node_Id;
      Res_Node_Field_1         : Node_Id                := Empty;
      Res_Node_Field_2         : Node_Id                := Empty;
      Res_Internal_Kind        : Internal_Element_Kinds := Internal_Kind;
      Res_Enclosing_Unit       : Asis.Compilation_Unit;
      Res_Is_Part_Of_Implicit  : Boolean                := False;
      Res_Is_Part_Of_Inherited : Boolean                := False;
      Res_Is_Part_Of_Instance  : Boolean                := False;
      Res_Spec_Case            : Special_Cases          := Spec_Case;
      Res_Char_Code            : Char_Code              := 0;
      Res_Parenth_Count        : Nat                    := 0;

   begin

      --  first, check if Node is not Empty and return Nil_Element otherwise:
      if No (Node) then
         return Nil_Element;
      end if;

      if Using_Original_Node
        and then
         Is_Rewrite_Substitution (Node)
        and then
         not Is_Rewritten_Function_Prefix_Notation (Node)
      then
         --  Note that for a function call in Object.Operation notation we do
         --  not use the original node at all, because the original tree
         --  structure is not properly decorated and analyzed, but the
         --  rewritten subtree contains all we need

         Res_Node := Original_Node (Node);

         if Is_Rewrite_Substitution (Res_Node) then
            Res_Node := Original_Node (Res_Node);
         end if;

      else
         Res_Node := Node;
      end if;

      --  setting the global Parent_Node  needed for computing the kind of
      --  the returned Element
      Parent_Node := Parent (Node);

      if not Is_Nil (Starting_Element) then
         --  We need this to define the enclosing unit for the new Element
         Res_Spec_Case := Special_Case (Starting_Element);
      end if;

      --  setting result's enclosing unit:
      if Exists (In_Unit) then
         --  if In_Unit is set, we take information about the result's
         --  enclosing unit from it, unless we have to create a configuration
         --  pragma or component thereof.

         if Spec_Case = Configuration_File_Pragma then
            Res_Enclosing_Unit := Get_Configuration_CU (In_Unit);
         else
            Res_Enclosing_Unit := In_Unit;
         end if;

      elsif not Is_Nil (Starting_Element) then
         --  if Starting_Element is set, but In_Unit is not, we take
         --  information about the result's enclosing unit from
         --  Starting_Element
         Res_Enclosing_Unit := Encl_Unit  (Starting_Element);
      else
         --  we can be here only if both Starting_Element and In_Unit are
         --  nor set. This is definitely an error.
         raise Internal_Implementation_Error;
      end if;

      --  if Starting_Element is set, we "transfer" everything what is
      --  possible from it to the result:
      if not Is_Nil (Starting_Element) then

         if Nkind (Res_Node) = N_Null_Statement
           and then
            not (Comes_From_Source (Res_Node))
         then
            --  Implicit NULL statement'floating' labels are attached to
            Res_Is_Part_Of_Implicit := True;

         elsif Nkind (Res_Node) = N_Label then
            --  Needed in case of 'floating' labels
            Res_Is_Part_Of_Implicit := False;
         else
            Res_Is_Part_Of_Implicit  := Is_From_Implicit  (Starting_Element);
         end if;

         Res_Is_Part_Of_Inherited := Is_From_Inherited (Starting_Element);
         Res_Is_Part_Of_Instance  := Is_From_Instance  (Starting_Element);
         --  Res_Spec_Case is already set!

         if Present (Node_Field_1_Value (Starting_Element)) then
            Res_Node_Field_1 := Node_Field_1_Value (Starting_Element);
            pragma Assert (Get_Current_Tree = Encl_Tree (Starting_Element));
            --  We can not use Asis.Set_Get.Node_Field_1 here, because it
            --  might reset the tree.
         end if;

         if Present (Node_Field_2_Value (Starting_Element)) then
            Res_Node_Field_2 := Node_Field_2_Value (Starting_Element);
            pragma Assert (Get_Current_Tree = Encl_Tree (Starting_Element));
            --  We can not use Asis.Set_Get.Node_Field_2 here, because it
            --  might reset the tree.
         end if;

         if Internal_Kind = A_Defining_Character_Literal or else
            Internal_Kind = An_Enumeration_Literal_Specification
         then
            --  We keep Character_Code unless the result is type definition
            --  (it that case keeping Character_Code would make Is_Equal test
            --  not work properly
            Res_Char_Code := Character_Code (Starting_Element);
         end if;

         if Res_Spec_Case in Expanded_Spec then
            --  We have to reset Res_Spec_Case from
            --  Expanded_Package_Instantiation or
            --  Expanded_Subprogram_Instantiation to Not_A_Special_Case;
            --  because only (the whole) expanded generic declarations can have
            --  the value of Special_Case from Expanded_Spec
            Res_Spec_Case := Not_A_Special_Case;
         end if;

      elsif Res_Spec_Case in Expanded_Spec then
         Res_Is_Part_Of_Implicit  := False;

      else

         if Is_From_Instance (Original_Node (Node))            or else
            Is_Name_Of_Expanded_Subprogram (Res_Node)          or else
            Part_Of_Pass_Generic_Actual (Original_Node (Node)) or else
            Is_Stub_To_Body_Instanse_Replacement (Node)        or else
            Spec_Case = Expanded_Package_Instantiation         or else
            Spec_Case = Expanded_Subprogram_Instantiation
         then
            Res_Is_Part_Of_Instance := True;
         end if;

         if Inherited then
            Res_Is_Part_Of_Inherited := True;
            Res_Is_Part_Of_Implicit  := True;
         end if;

         if not Comes_From_Source (Res_Node)
           and then
            (not Part_Of_Pass_Generic_Actual (Res_Node)
            or else
             Norm_Case = Is_Normalized_Defaulted_For_Box)
           and then
             not Is_Name_Of_Expanded_Subprogram (Res_Node)
         then
            Res_Is_Part_Of_Implicit  := True;
         end if;

      end if;

      --   This patch below is really terrible!!! requires revising!!!
      --  ???
      if Spec_Case = Expanded_Package_Instantiation or else
         Spec_Case = Expanded_Subprogram_Instantiation
      then
         Res_Is_Part_Of_Instance := True;
      end if;

      --  if Spec_Case is set explicitly, we should set (or reset)
      --  Res_Spec_Case from it

      if Spec_Case /= Not_A_Special_Case then
         Res_Spec_Case := Spec_Case;
      end if;

      --  computing the kind of the result and correcting Special_Case,
      --  if needed

      if Res_Internal_Kind = Not_An_Element then
         --  Res_Internal_Kind is initialized by the value of the
         --  Internal_Kind parameter. If this value differs from
         --  Not_An_Element, we simply does not change it

         if Considering_Parent_Count and then
            (Parenth_Count (Res_Node, Node) > 0
            or else
             Requires_Parentheses (Res_Node))
         then
            --  special processing for A_Parenthesized_Expression
            Res_Internal_Kind := A_Parenthesized_Expression;

            if Parenth_Count (Res_Node, Node) > 0 then
               Res_Parenth_Count := Parenth_Count (Res_Node, Node);

               if Requires_Parentheses (Res_Node) then
                  Res_Parenth_Count := Res_Parenth_Count + 1;
               end if;
            else
               --  (conditional expression)
               Res_Parenth_Count := 1;
            end if;
         else
            --  from Sinfo (spec, rev. 1.334):
      ---------------------------------
      -- 9.5.3  Entry Call Statement --
      ---------------------------------

      --  ENTRY_CALL_STATEMENT ::= entry_NAME [ACTUAL_PARAMETER_PART];

      --  The parser may generate a procedure call for this construct. The
      --  semantic pass must correct this misidentification where needed.

            if Res_Node /= Node and then
               ((Nkind (Res_Node) = N_Procedure_Call_Statement and then
                  Nkind (Node)     = N_Entry_Call_Statement    and then
                  not Is_Protected_Procedure_Call (Node))
                or else
                 (Nkind (Res_Node) = N_Explicit_Dereference and then
                  Nkind (Node)     = N_Function_Call))
            then
               Res_Node := Node;  --  ???
               --  There is no need to keep the original structure in this
               --  case, it is definitely wrong

               if Nkind (Node) = N_Entry_Call_Statement then
                  Res_Internal_Kind := An_Entry_Call_Statement;
               else
                  Res_Internal_Kind := A_Function_Call;
               end if;

            else

               if (Nkind (Node) = N_Integer_Literal or else
                   Nkind (Node) = N_Real_Literal)
                  and then
                  ((not Is_Rewrite_Substitution (Node))
                   or else
                    Nkind (Original_Node (Node)) = N_Identifier)
                  and then
                   Comes_From_Source (Node) = False
                  and then
                   Is_Static_Expression (Node)
                  and then
                   Present (Original_Entity (Node))
               then
                  --  See BB10-002: The special case of named numbers rewritten
                  --  into numeric literals.
                  Res_Spec_Case     := Rewritten_Named_Number;
                  Res_Internal_Kind := An_Identifier;
               else
                  Res_Internal_Kind := Asis_Internal_Element_Kind (Res_Node);
               end if;

            end if;

         end if;

      end if;

      --  and now we have to check if the Element to be returned is from the
      --  Standard package, and if it is, we have to correct Res_Spec_Case:

      if Res_Spec_Case = Not_A_Special_Case and then
         Sloc (Res_Node) <= Standard_Location
      then

         if Nkind (Node) = N_Defining_Character_Literal then
            Res_Spec_Case := Stand_Char_Literal;
         else
            Res_Spec_Case := Explicit_From_Standard;
         end if;

      end if;

      if Res_Spec_Case = Explicit_From_Standard or else
         Res_Spec_Case = Stand_Char_Literal
      then
         Res_Is_Part_Of_Implicit := False;
      end if;

      --  ??? This assignment is not necessary and has been
      --  introduced to workaround a problem with the sgi n32 compiler

      R_Node := Node;

      if Nkind (Node) = N_Function_Call and then
         (Res_Internal_Kind = A_Selected_Component or else
          Res_Internal_Kind = An_Enumeration_Literal)
      then
         --  a reference to an overloaded enumeration literal represented as a
         --  function call, we have to go one step down:
         R_Node := Sinfo.Name (Node);
         Res_Node := R_Node;
      else
         R_Node := Node;
      end if;

      if Present (Node_Field_1) then
         Res_Node_Field_1 := Node_Field_1;
      end if;

      if Present (Node_Field_2) then
         Res_Node_Field_2 := Node_Field_2;
      end if;

      if Res_Spec_Case = From_Limited_View then
         Res_Is_Part_Of_Implicit := True;
      end if;

      return Set_Element (
          Node           => Res_Node,
          R_Node         => R_Node,
          Node_Field_1   => Res_Node_Field_1,
          Node_Field_2   => Res_Node_Field_2,
          Encl_Unit      => Res_Enclosing_Unit,
          Int_Kind       => Res_Internal_Kind,
          Implicit       => Res_Is_Part_Of_Implicit,
          Inherited      => Res_Is_Part_Of_Inherited,
          Instance       => Res_Is_Part_Of_Instance,
          Spec_Case      => Res_Spec_Case,
          Norm_Case      => Norm_Case,
          Par_Count      => Res_Parenth_Count,
          Character_Code => Res_Char_Code);

   end Node_To_Element_New;

   --------------------
   -- Normalize_Name --
   --------------------

   procedure Normalize_Name (Capitalized : Boolean := False) is
   begin
      if Namet.Name_Len = 0 then
         return;
      end if;

      Namet.Name_Buffer (1) := To_Upper (Namet.Name_Buffer (1));

      for I in 1 .. Namet.Name_Len - 1 loop

         if Capitalized or else
            Namet.Name_Buffer (I) = '_'
         then
            Namet.Name_Buffer (I + 1) := To_Upper (Namet.Name_Buffer (I + 1));
         end if;

      end loop;
   end Normalize_Name;

   -----------------------------
   -- Normalized_Namet_String --
   -----------------------------

   function Normalized_Namet_String (Node : Node_Id) return String is
      Capitalize : Boolean := False;
   begin
      Namet.Get_Name_String (Chars (Node));

      if Node = Standard_ASCII              or else
         Node in SE (S_LC_A) .. SE (S_LC_Z) or else
         Node in SE (S_NUL) .. SE (S_US)    or else
         Node =  SE (S_DEL)
      then
         Capitalize := True;
      end if;

      Normalize_Name (Capitalize);

      return Namet.Name_Buffer (1 .. Namet.Name_Len);
   end Normalized_Namet_String;

   -----------------------------
   -- Not_Implemented_Mapping --
   -----------------------------

   function Not_Implemented_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds is
   begin
      Not_Implemented_Yet (Diagnosis =>
                        "AST Node -> Asis.Element mapping for the "
                      &   Node_Kind'Image (Nkind (Node))
                      & " Node Kind value has not been implemented yet");
      return Not_An_Element; -- to make the code syntactically correct;
   end Not_Implemented_Mapping;

   procedure Not_Implemented_Mapping (Source_Node_Kind : Node_Kind) is
   begin
      Not_Implemented_Yet (Diagnosis =>
                        "AST Node -> Asis.Element mapping for the "
                      &   Node_Kind'Image (Source_Node_Kind)
                      & " Node Kind value has not been implemented yet");
   end Not_Implemented_Mapping;

   ----------------------------------
   -- Ordinary_Inclusion_Condition --
   ----------------------------------

   function Ordinary_Inclusion_Condition (Node : Node_Id) return Boolean is
      O_Node   : constant Node_Id   := Original_Node (Node);
      Arg_Kind : constant Node_Kind := Nkind (Node);

      Result   : Boolean := True;
   begin

      if Is_Rewrite_Insertion (Node)                   or else
         May_Be_Included_Switch (Nkind (Node)) = False or else

        (Nkind (Node) = N_With_Clause
         and then
         Implicit_With (Node))                        or else

        (Nkind (Node) = N_Entry_Call_Statement
         and then
         Node = O_Node)
      then
         Result := False;

      elsif not Comes_From_Source (O_Node) then

         if Arg_Kind = N_Null_Statement then
            --  We have to include implicit null statements 'floating' labels
            --  are attached to
            if not (No (Next (Node))
                  and then
                    Present (Prev (Node))
                  and then
                    Nkind (Prev (Node)) = N_Label
                  and then
                    Comes_From_Source (Prev (Node)))
            then
               Result := False;
            end if;
         elsif Arg_Kind = N_Object_Renaming_Declaration then
            --  For FB02-008

            if (Nkind (Sinfo.Name (Node)) = N_Identifier
               and then
                Is_Internal_Name (Chars (Sinfo.Name (Node))))
             or else
               (Nkind (Sinfo.Subtype_Mark (Node)) = N_Identifier
               and then
                Is_Internal_Name (Chars (Sinfo.Subtype_Mark (Node))))
            then
               Result := False;
            end if;

         elsif not ((Arg_Kind = N_Object_Declaration
                 and then
                  not Is_Internal_Name (Chars (Defining_Identifier (O_Node))))
                or else
                 Arg_Kind = N_Component_Declaration
                or else
                 Arg_Kind = N_Discriminant_Specification
                or else
                 Arg_Kind = N_Parameter_Specification
                or else
                 Arg_Kind = N_Component_Association
                or else

                 ((Arg_Kind = N_Integer_Literal
                  or else
                   Arg_Kind = N_Real_Literal)
                     and then
                  Is_Static_Expression (Node))
                  --  This condition describes the special case of a named
                  --  number rewritten into a literal node, see BB10-002

                or else

                 Sloc (Node) <= Standard_Location
                or else
                 Is_Rewritten_Function_Prefix_Notation (Parent (Node))
                or else
                 Pass_Generic_Actual (Node))
         then
            Result := False;
         end if;

      end if;

      return Result;

   end Ordinary_Inclusion_Condition;

   -------------------
   -- Parenth_Count --
   -------------------

   function Parenth_Count
     (N          : Node_Id;
      Original_N : Node_Id)
      return       Nat
   is
      Result : Nat := Paren_Count (N);
   begin

      if Result > 0 and then
         Nkind (Original_Node (Parent (Original_N))) = N_Qualified_Expression
      then
         Result := Result - 1;
      end if;

      return Result;

   end Parenth_Count;

   --------------------------
   -- Requires_Parentheses --
   --------------------------

   function Requires_Parentheses
     (N    : Node_Id)
      return Boolean
   is
      Left_Par_Sloc : Source_Ptr;
      Result        : Boolean := False;
   begin
      if Nkind (N) = N_Conditional_Expression
        or else
         Nkind (N) = N_Case_Expression
        or else
         Nkind (N) = N_Quantified_Expression
      then
         Result := True;

         --  And now - cases when the expression does not require parentheses:

         if Nkind (Parent (N)) = N_Qualified_Expression
           or else
            Nkind (Parent (N)) = N_Type_Conversion
           or else
            (Is_List_Member (N)
             and then
             List_Length (List_Containing (N)) = 1)
         then
            --  There is a corner case here:
            --  P ((if A then B else C));
            Left_Par_Sloc := Sloc (N);
            Left_Par_Sloc := Search_Prev_Word (Left_Par_Sloc);

            pragma Assert (Get_Character (Left_Par_Sloc) = '(');
            Left_Par_Sloc := Search_Prev_Word (Left_Par_Sloc);

            if Get_Character (Left_Par_Sloc) /= '(' then
               Result := False;
            end if;
         end if;
      end if;

--      if Nkind (N) = N_Conditional_Expression
--        or else
--         Nkind (N) = N_Case_Expression
--        or else
--         Nkind (N) = N_Quantified_Expression
--      then
--         Left_Par_Sloc := Sloc (N);
--         Left_Par_Sloc := Search_Prev_Word (Left_Par_Sloc);
--         Result        := Get_Character (Left_Par_Sloc) = '(';
--      end if;

--      if Result then
--         --  We have to check if this '(' belongs to some other construct:
--         Left_Par_Sloc := Search_Prev_Word (Left_Par_Sloc);

--         if Get_Character (Left_Par_Sloc) /= '(' then
--            case Nkind (Parent (N)) is
--               when N_Qualified_Expression |
--                    N_Type_Conversion      =>
--                  Result := False;

--               when N_Function_Call             |
--                    N_Indexed_Component         |
--                    N_Procedure_Call_Statement  |
--                    N_Entry_Call_Statement      =>
--                  --  ??? Is this list complete???
--                  if Get_Character (Left_Par_Sloc) /= ',' then
--                     Result := False;
--                  end if;
--               when others  =>
--                  null;
--            end case;
--         end if;

--      end if;

      return Result;
   end Requires_Parentheses;

   -----------------------------------------
   -- Set_Concurrent_Inherited_Components --
   -----------------------------------------

   procedure Set_Concurrent_Inherited_Components
     (Type_Def      : Asis.Element;
      Include_Discs : Boolean := True)
   is
      Type_Entity    : Entity_Id;
      Next_Comp_Node : Node_Id;
   begin

      Asis_Element_Table.Init;

      Type_Entity := Defining_Identifier (Parent (R_Node (Type_Def)));

      if Present (First_Entity (Type_Entity)) then

         if Include_Discs then
            Next_Comp_Node := First_Entity (Type_Entity);

            while Ekind (Next_Comp_Node) = E_Discriminant loop

               Asis_Element_Table.Append
                 (Node_To_Element_New
                    (Node             => Parent (Next_Comp_Node),
                     Starting_Element => Type_Def));

               Set_Node_Field_2
                 (Asis_Element_Table.Table (Asis_Element_Table.Last),
                  Next_Comp_Node);
               --  This is needed to unify the processing of inherited
               --  discriminants in Asis.Declarations.Names

               Next_Comp_Node := Next_Entity (Next_Comp_Node);
            end loop;

         end if;

         --  To set all the non-discriminant components, we have to go to the
         --  definition of the root concurrent type and to traverse it to
         --  grab all this components, because the (non-discrimiinant)
         --  entities attached to the entity of the derived concurrent type
         --  are artificial entities created for further tree expansion

         while Type_Entity /= Etype (Type_Entity) loop
            Type_Entity := Etype (Type_Entity);
         end loop;

         Type_Entity := Parent (Type_Entity);

         if Nkind (Type_Entity) = N_Task_Type_Declaration then
            Type_Entity := Task_Definition (Type_Entity);
         else
            Type_Entity := Protected_Definition (Type_Entity);
         end if;

         Next_Comp_Node :=
            First_Non_Pragma (Visible_Declarations (Type_Entity));

         while Present (Next_Comp_Node) loop

            Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node             => Next_Comp_Node,
                  Starting_Element => Type_Def));

            Next_Comp_Node := Next_Non_Pragma (Next_Comp_Node);
         end loop;

         if Present (Private_Declarations (Type_Entity)) then
            Next_Comp_Node :=
               First_Non_Pragma (Private_Declarations (Type_Entity));

            while Present (Next_Comp_Node) loop

               Asis_Element_Table.Append
                 (Node_To_Element_New
                    (Node             => Next_Comp_Node,
                     Starting_Element => Type_Def));

               Next_Comp_Node := Next_Non_Pragma (Next_Comp_Node);
            end loop;

         end if;

      end if;

   end Set_Concurrent_Inherited_Components;

   ----------------------
   -- Set_Element_List --
   ----------------------

   --  At the moment, the implementation is just a copy from N_To_E_List_New
   --  with changes related to building the result list in Element Table

   procedure Set_Element_List
     (List             : List_Id;
      Include_Pragmas  : Boolean                := False;
      Starting_Element : Asis.Element           := Asis.Nil_Element;
      Node_Knd         : Node_Kind              := N_Empty;
      Internal_Kind    : Internal_Element_Kinds := Not_An_Element;
      Special_Case     : Special_Cases          := Not_A_Special_Case;
      Norm_Case        : Normalization_Cases    := Is_Not_Normalized;
      In_Unit          : Asis.Compilation_Unit  := Asis.Nil_Compilation_Unit;
      Append           : Boolean                := False)
   is
      List_El       : Node_Id;
      List_El_Kind  : Node_Kind;

      function First_List_Element (List : List_Id) return Node_Id;
      --  returns the first element of List, taking into account
      --  the value of Include_Pragmas
      function Next_List_Element (Node : Node_Id) return Node_Id;
      --  returns the first element of List, taking into account
      --  the value of Include_Pragmas

      procedure Skip_Pragmas (N : in out Node_Id);
      --  Supposes that Is_List_Member (N). If N is of N_Pragma kind and if
      --  Include_Pragmas is set OFF, moves N to the next element of the same
      --  list that is not of N_Pragma kinds or set it to Empty if there is no
      --  such node. We can not just use Nlists.First_Non_Pragma and
      --  Nlists.Next_Non_Pragma, because they also skip N_Null_Statement
      --  nodes.

      ------------------------
      -- First_List_Element --
      ------------------------

      function First_List_Element (List : List_Id) return Node_Id is
         Result : Node_Id;
      begin

         Result := First (List);
         Skip_Pragmas (Result);

         if Is_Rewrite_Substitution (Result)
           and then
            Nkind (Result) = N_Loop_Statement
           and then
            Nkind (Original_Node (Result)) = N_Goto_Statement
         then
            --  This is the case when infinite loop implemented as
            --
            --    <<Target>> ...
            --       ...
            --    goto Target;
            --
            --  Is rewritten into N_Loop_Statement

            if not Is_Empty_List (Statements (Result)) then
               Result := First (Statements (Result));
            end if;

         end if;

         return Result;

      end First_List_Element;

      -----------------------
      -- Next_List_Element --
      -----------------------

      function Next_List_Element (Node : Node_Id) return Node_Id is
         Tmp    : Node_Id;
         Result : Node_Id;
      begin

         Result := Next (Node);
         Skip_Pragmas (Result);

         if Is_Rewrite_Substitution (Result)
           and then
            Nkind (Result) = N_Loop_Statement
           and then
            Nkind (Original_Node (Result)) = N_Goto_Statement
         then
            --  This is the case when infinite loop implemented as
            --
            --    <<Target>> ...
            --       ...
            --    goto Target;
            --
            --  Is rewritten into N_Loop_Statement

            if not Is_Empty_List (Statements (Result)) then
               Result := First (Statements (Result));
            end if;

         end if;

         if No (Result) then
            Tmp := Parent (Node);

            if Is_Rewrite_Substitution (Tmp)
              and then
               Nkind (Tmp) = N_Loop_Statement
              and then
               Nkind (Original_Node (Tmp)) = N_Goto_Statement
            then
               --  We have just finished traversing of the artificial loop
               --  statement created for goto, so it's tome to
               --  return this goto itself. Note, that we are returning the
               --  rewritten N_Loop_Statement to keep list and parent
               --  references

               Result := Tmp;
            end if;

         end if;

         return Result;

      end Next_List_Element;

      ------------------
      -- Skip_Pragmas --
      ------------------

      procedure Skip_Pragmas (N : in out Node_Id) is
      begin

         if not Include_Pragmas then

            while Present (N)
               and then
                  Nkind (Original_Node (N)) = N_Pragma
            loop
               N := Next (N);
            end loop;

         end if;

      end Skip_Pragmas;

   begin

      if not Append then
         Internal_Asis_Element_Table.Init;
      end if;

      if No (List) or else Is_Empty_List (List) then
         return;
      end if;

      List_El := First_List_Element (List);

      while Present (List_El) loop

         if Debug_Flag_L then
            Write_Node (N      => List_El,
                        Prefix => "Set_Element_List debug info-> ");
            Write_Str ("May_Be_Included is ");
            Write_Str (Boolean'Image (May_Be_Included (List_El)));
            Write_Eol;
            Write_Eol;
         end if;

         List_El_Kind := Nkind (List_El);

         if May_Be_Included (List_El) and then
            not ((Node_Knd /= N_Empty) and then (List_El_Kind /= Node_Knd))
         then

            Internal_Asis_Element_Table.Append
              (Node_To_Element_New
                 (Starting_Element => Starting_Element,
                  Node             => List_El,
                  Internal_Kind    => Internal_Kind,
                  Spec_Case        => Special_Case,
                  Norm_Case        => Norm_Case,
                  In_Unit          => In_Unit));

            if List_El_Kind = N_Object_Declaration         or else
               List_El_Kind = N_Number_Declaration         or else
               List_El_Kind = N_Discriminant_Specification or else
               List_El_Kind = N_Component_Declaration      or else
               List_El_Kind = N_Parameter_Specification    or else
               List_El_Kind = N_Exception_Declaration      or else
               List_El_Kind = N_Formal_Object_Declaration  or else
               List_El_Kind = N_With_Clause
            then
               Skip_Normalized_Declarations (List_El);
            end if;

         end if;

         List_El := Next_List_Element (List_El);
      end loop;

   end Set_Element_List;

   ---------------------------------
   -- Set_Inherited_Discriminants --
   ---------------------------------

   procedure Set_Inherited_Discriminants (Type_Def : Asis.Element) is
      Next_Discr_Elmt : Elmt_Id;
      Next_Elem_Node  : Node_Id;
   begin
      Next_Elem_Node := Defining_Identifier (Parent (R_Node (Type_Def)));

      if Present (Stored_Constraint (Next_Elem_Node)) then
         Asis_Element_Table.Init;

         Next_Discr_Elmt := First_Elmt (Stored_Constraint (Next_Elem_Node));

         while Present (Next_Discr_Elmt) loop
            Next_Elem_Node  := Parent (Entity (Node (Next_Discr_Elmt)));

            Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node             => Next_Elem_Node,
                  Internal_Kind    => A_Discriminant_Specification,
                  Starting_Element => Type_Def));

            Next_Discr_Elmt := Next_Elmt (Next_Discr_Elmt);

         end loop;
      end if;

   end Set_Inherited_Discriminants;

   ------------------------------
   -- Set_Inherited_Components --
   ------------------------------

   procedure Set_Inherited_Components
     (Type_Def      : Asis.Element;
      Include_Discs : Boolean := True)
   is
      Next_Comp_Node : Node_Id;

      function Is_Implicit_Component (E : Entity_Id) return Boolean;
      --  Checks if E is an entity representing implicit inherited
      --  component

      function Is_Implicit_Component (E : Entity_Id) return Boolean is
         Result : Boolean := False;
      begin

         if Ekind (E) = E_Component then
            Result :=
              (E /= Original_Record_Component (E)) or else
              not Comes_From_Source (Parent (E));

         elsif Ekind (E) = E_Discriminant and then Include_Discs then
            Result := not Is_Completely_Hidden (E);
         end if;

         return Result;
      end Is_Implicit_Component;

   begin

      Asis_Element_Table.Init;

      Next_Comp_Node := Defining_Identifier (Parent (R_Node (Type_Def)));

      if Ekind (Next_Comp_Node) = E_Record_Subtype then
         --  For subtypes we may have components depending on discriminants
         --  skipped in case of static discriminant constraints
         Next_Comp_Node := Etype (Next_Comp_Node);
      end if;

      if Present (First_Entity (Next_Comp_Node)) then

         Next_Comp_Node := First_Entity (Next_Comp_Node);

         while Present (Next_Comp_Node) loop

            if Is_Implicit_Component (Next_Comp_Node) then

               Asis_Element_Table.Append
                 (Node_To_Element_New
                    (Node             => Parent (Next_Comp_Node),
                     Starting_Element => Type_Def));

               Set_Node_Field_2
                 (Asis_Element_Table.Table (Asis_Element_Table.Last),
                  Next_Comp_Node);
               Set_From_Implicit
                  (Asis_Element_Table.Table (Asis_Element_Table.Last), True);
               Set_From_Inherited
                  (Asis_Element_Table.Table (Asis_Element_Table.Last), True);
            end if;

            Next_Comp_Node := Next_Entity (Next_Comp_Node);

         end loop;

      end if;

   end Set_Inherited_Components;

   ----------------------------
   -- Set_Inherited_Literals --
   ----------------------------

   procedure Set_Inherited_Literals (Type_Def : Asis.Element) is
      Next_Literal_Node : Node_Id;
      Res_Etype         : Entity_Id;
      Encl_Type         : constant Node_Id := Parent (R_Node (Type_Def));
   begin

      Asis_Element_Table.Init;

      Next_Literal_Node := Defining_Identifier (Parent (R_Node (Type_Def)));

      if Present (First_Literal (Next_Literal_Node)) then

         Next_Literal_Node := First_Literal (Next_Literal_Node);
         Res_Etype         := Etype (Next_Literal_Node);

         while Present (Next_Literal_Node) and then
               Etype (Next_Literal_Node) = Res_Etype
         loop

            Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node             => Next_Literal_Node,
                  Internal_Kind    => An_Enumeration_Literal_Specification,
                  Starting_Element => Type_Def));

            Set_Node_Field_1
               (Asis_Element_Table.Table (Asis_Element_Table.Last),
                Encl_Type);

            Set_From_Implicit
               (Asis_Element_Table.Table (Asis_Element_Table.Last), True);
            Set_From_Inherited
               (Asis_Element_Table.Table (Asis_Element_Table.Last), True);

            Next_Literal_Node := Next_Entity (Next_Literal_Node);

         end loop;

      else
         Not_Implemented_Yet
           (Diagnosis =>
           "Asis.Definitions.Implicit_Inherited_Declarations "
           & "(derived from Standard character type)");

      end if;

   end Set_Inherited_Literals;

   ----------------------------------
   -- Skip_Normalized_Declarations --
   ----------------------------------

   procedure Skip_Normalized_Declarations (Node : in out Node_Id) is
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

            if More_Ids (Node) then
               Node := Next (Node);

               while Nkind (Node) /= Arg_Kind loop
                  --  some implicit subtype declarations may be inserted by
                  --  the compiler in between the normalized declarations, so:
                  Node := Next (Node);
               end loop;

            else
               return;
            end if;

         else
            --  Arg_Kind = N_With_Clause.
            --  Note that we should skip implicit clauses that can be added by
            --  front-end.
            if Comes_From_Source (Node) and then Last_Name (Node) then
               return;
            else
               Node := Next (Node);
            end if;

         end if;

      end loop;
   end Skip_Normalized_Declarations;

   -------------------------------
   -- Subprogram_Attribute_Kind --
   -------------------------------

   function Subprogram_Attribute_Kind
     (Node : Node_Id)
      return Internal_Element_Kinds
   is
      Attribute_Chars : Name_Id;
   begin

      Attribute_Chars := Attribute_Name (Node);

      --  language-defined attributes which are functions:

      if Attribute_Chars = Name_Adjacent             then
         return An_Adjacent_Attribute;
      elsif Attribute_Chars = Name_Ceiling           then
         return A_Ceiling_Attribute;
      elsif Attribute_Chars = Name_Compose           then
         return A_Compose_Attribute;
      elsif Attribute_Chars = Name_Copy_Sign         then
         return A_Copy_Sign_Attribute;
      elsif Attribute_Chars = Name_Exponent          then
         return An_Exponent_Attribute;
      elsif Attribute_Chars = Name_Floor             then
         return A_Floor_Attribute;
      elsif Attribute_Chars = Name_Fraction          then
         return A_Fraction_Attribute;
      elsif Attribute_Chars = Name_Image             then
         return An_Image_Attribute;
      elsif Attribute_Chars = Name_Input             then
         return An_Input_Attribute;
      elsif Attribute_Chars = Name_Leading_Part      then
         return A_Leading_Part_Attribute;
      elsif Attribute_Chars = Name_Machine           then
         return A_Machine_Attribute;
      elsif Attribute_Chars = Name_Max               then
         return A_Max_Attribute;
      elsif Attribute_Chars = Name_Min               then
         return A_Min_Attribute;
      elsif Attribute_Chars = Name_Model             then
         return A_Model_Attribute;
      elsif Attribute_Chars = Name_Pos               then
         return A_Pos_Attribute;
      elsif Attribute_Chars = Name_Pred              then
         return A_Pred_Attribute;
      elsif Attribute_Chars = Name_Remainder         then
         return A_Remainder_Attribute;
      elsif Attribute_Chars = Name_Round             then
         return A_Round_Attribute;
      elsif Attribute_Chars = Name_Rounding          then
         return A_Rounding_Attribute;
      elsif Attribute_Chars = Name_Scaling           then
         return A_Scaling_Attribute;
      elsif Attribute_Chars = Name_Succ              then
         return A_Succ_Attribute;
      elsif Attribute_Chars = Name_Truncation        then
         return A_Truncation_Attribute;
      elsif Attribute_Chars = Name_Unbiased_Rounding then
         return An_Unbiased_Rounding_Attribute;
      elsif Attribute_Chars = Name_Val               then
         return A_Val_Attribute;
      elsif Attribute_Chars = Name_Value             then
         return A_Value_Attribute;
      elsif Attribute_Chars = Name_Wide_Image        then
         return A_Wide_Image_Attribute;
      elsif Attribute_Chars = Name_Wide_Value        then
         return A_Wide_Value_Attribute;

--  |A2005 start

            --  Ada 2005 attributes that are functions:
      elsif Attribute_Chars = Name_Machine_Rounding  then
         return A_Machine_Rounding_Attribute;
      elsif Attribute_Chars = Name_Mod               then
         return A_Mod_Attribute;
      elsif Attribute_Chars = Name_Wide_Wide_Image   then
         return A_Wide_Wide_Image_Attribute;
      elsif Attribute_Chars = Name_Wide_Wide_Value   then
         return A_Wide_Wide_Value_Attribute;
--  |A2005 end

--  |A2012 start
            --  Ada 2012 attributes that are functions:
--        elsif Attribute_Chars = Name_Overlaps_Storage  then  (SCz)
--           return An_Overlaps_Storage_Attribute;
--  |A2012 end

      --  language-defined attributes which are procedures:

      elsif Attribute_Chars = Name_Output then
         return An_Output_Attribute;
      elsif Attribute_Chars = Name_Read then
         return A_Read_Attribute;
      elsif Attribute_Chars = Name_Write then
         return A_Write_Attribute;

               -- Implementation Dependent Attributes-Functions --

      elsif Attribute_Chars = Name_Asm_Input         or else
            Attribute_Chars = Name_Asm_Output        or else
            Attribute_Chars = Name_Enum_Rep          or else
            Attribute_Chars = Name_Enum_Val          or else
            Attribute_Chars = Name_Fixed_Value       or else
            Attribute_Chars = Name_Integer_Value     or else
            Attribute_Chars = Name_Ref
      then
         return An_Implementation_Defined_Attribute;
      else
         return An_Unknown_Attribute;
      end if;

   end Subprogram_Attribute_Kind;

   -----------------
   -- Ureal_Image --
   -----------------

   function Ureal_Image (N : Node_Id) return String is
      Result : String (1 .. 256);
      Res_Len : Natural range 0 .. 256 := 0;
      --  bad solution!!! ASIS needs a general-purpose String buffer
      --  somewhere!!! ???
      Real : constant Ureal  := Realval (N);
      Nom  : constant Uint   := Norm_Num (Real);
      Den  : constant Uint   := Norm_Den (Real);
      Dot_Outputed : Boolean := False;
   begin
      if UR_Is_Negative (Real) then
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '(';
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '-';
      end if;

      UI_Image (Nom, Decimal);
      for I in 1 .. UI_Image_Length loop
         if UI_Image_Buffer (I) = 'E' then
            Res_Len := Res_Len + 1;
            Result (Res_Len) := '.';
            Res_Len := Res_Len + 1;
            Result (Res_Len) := '0';
            Res_Len := Res_Len + 1;
            Result (Res_Len) := 'E';
            Dot_Outputed := True;
         else
            Res_Len := Res_Len + 1;
            Result (Res_Len) := UI_Image_Buffer (I);
         end if;
      end loop;
      if not Dot_Outputed then
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '.';
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '0';
      end if;
      Dot_Outputed := False;

      Res_Len := Res_Len + 1;
      Result (Res_Len) := '/';

      UI_Image (Den, Decimal);
      for I in 1 .. UI_Image_Length loop
         if UI_Image_Buffer (I) = 'E' then
            Res_Len := Res_Len + 1;
            Result (Res_Len) := '.';
            Res_Len := Res_Len + 1;
            Result (Res_Len) := '0';
            Res_Len := Res_Len + 1;
            Result (Res_Len) := 'E';
            Dot_Outputed := True;
         else
            Res_Len := Res_Len + 1;
            Result (Res_Len) := UI_Image_Buffer (I);
         end if;
      end loop;
      if not Dot_Outputed then
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '.';
         Res_Len := Res_Len + 1;
         Result (Res_Len) := '0';
      end if;

      if UR_Is_Negative (Real) then
         Res_Len := Res_Len + 1;
         Result (Res_Len) := ')';
      end if;

      return Result (1 .. Res_Len);
   end Ureal_Image;

end A4G.Mapping;
