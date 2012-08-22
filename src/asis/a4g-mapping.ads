------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . M A P P I N G                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
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

--  This package defines and implements the mapping from the nodes in the
--  tree created by the front-end nodes onto ASIS Elements.
--  The main part of this mapping are:
--
--  - the function constructing an ASIS Element from a tree
--    node;
--  - the function constructing an ASIS Element_List from a tree node
--    list;
--  - a set of functions defining the ASIS kind (or, more exactly, the
--    corresponding value of the Internal_Element_Kinds type) of an Element
--    which can be built on the base of a given node. Most of these functions
--    are hidden in the package body, but for some of them their
--    specifications are placed in the package spec in order to make them
--    visible for the implementation of some ASIS queries (for example,
--    sometimes we have to know the exact kind of an operator symbol or
--    of an attribute reference.

--  The functions defined in this package are of relatively low level. In
--  particular, as a rule, they do not check the correctness of their
--  arguments, and a caller is responsible for the correct usage of these
--  functions.

with Asis;

with A4G.A_Types;  use A4G.A_Types;
with A4G.Int_Knds; use A4G.Int_Knds;

with Types;        use Types;
with Sinfo;        use Sinfo;

package A4G.Mapping is

   Parent_Node : Node_Id;
   --  The determination of the ASIS Internal Element Kind is based on
   --  the original tree nodes. Sometimes it requires information about
   --  the "context" of a node, that is, about its the parent node.
   --  If the node is rewritten, its original node has Parent field
   --  set to N_Empty. The global variable Parent_Node is used to
   --  transfer the parent node from Node_To_Element function to
   --  Asis_Internal_Element_Kind function.
   --
   --  ???
   --  For now, it is defined in the package spec, because it is needed
   --  by the current implementation of Enclosing_Element. As soon as
   --  the implementation of Enclosing _Element is rewritten on top of
   --  Traverse_Element, the declaration of Parent_Node should be
   --  moved in the body of the package.

   function Asis_Internal_Element_Kind
     (Node : Node_Id)
      return Internal_Element_Kinds;
   --  This function is the kernel of the tree node -> Asis Element
   --  mapping. Taking a tree Node, it defines the Internal_Element_Kinds
   --  value for the Element which may be constructed on the base of this
   --  node. Node should be an original, but not the rewritten node in case
   --  if tree rewriting takes place for a given construct.
   --
   --  This function does not check, whether or not an Element could be
   --  really construct for a given Node, a caller is responsible for
   --  obtaining the proper node for Element construction, or for filtering
   --  out inappropriate nodes in case when this function is applied
   --  to construct an Element_List.
   --
   --  If no Asis_Element could be constructed on the base of the Node, then
   --  ASIS_Failed is raised.
   --
   --  This function is placed in the package spec, because it is needed
   --  by A4G.CU_Info2 routines

   function N_Operator_Symbol_Mapping
     (Node : Node_Id)
      return Internal_Element_Kinds;
   --  One of the functional components of the general
   --  Asis_Internal_Element_Kind function, it is placed in the package spec
   --  because it is needed by Asis.Expressions.Prefix.
   --  function.

   function Is_Not_Duplicated_Decl (Node : Node_Id) return Boolean;
   --  This function checks if Node does not correspond to the declaration
   --  which is included in the tree twice - first on its own and then
   --  as an original node for some rewritten structure. For instance, this
   --  is the case for a type derived from private type and having no
   --  explicit constrain.
   --
   --  This function return False for the second of the two duplicated
   --  declarations.
   --
   --  This function is placed in the package spec, because it is used by
   --  A4G.Decl_Sem.Serach_First_View

   function Subprogram_Attribute_Kind
     (Node : Node_Id)
      return Internal_Element_Kinds;
   --  This function defines the kind of an Element which can be built on
   --  N_Attribute_Reference node corresponding to a reference to
   --  attribute-subprogram, but opposite to the general attribute kind
   --  determination implemented by N_Attribute_Reference_Mapping, it does
   --  not classify an Element as a function or a procedure call, but it
   --  defines the corresponding Attribute_Kinds value.

   function Is_GNAT_Attribute_Routine (N : Node_Id) return Boolean;
   --  Checks if N represents a GNAT-specific attribute which is a function or
   --  a procedure

   function Is_Rewritten_Function_Prefix_Notation
     (N     : Node_Id)
      return Boolean;
   --  Checks if N represents a function call given in Object.Operation [(...)]
   --  form. In this case N is a rewritten node, and the original subtree
   --  is not decorated and the original node is not even classified as
   --  a function call

   function Is_Rewritten_Impl_Deref_Function_Prefix_Notation
     (N     : Node_Id)
      return Boolean;
   --  Checks if N represents a function call given in Object.Operation [(...)]
   --  for a special case when the call is used in implicit dereference (the
   --  caller is responsible to call this function only for nodes corresponding
   --  to implicit dereference!)

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
      In_Unit          : Asis.Compilation_Unit  := Asis.Nil_Compilation_Unit)
      return Asis.Element;
   --  This functions creates the ASIS Element which could be built on the
   --  basis of Node. Its other parameters have the following meaning (the
   --  wording "the parameter is set" means below that some non-nil value
   --  is explicitly passed in the call):
   --
   --  Starting_Element - if this parameter is set, the following fields
   --     of the result Element are set as being equal to the corresponding
   --     fields of Starting_Element:
   --
   --     -  Enclosing_Unit (unless the In_Unit parameter is set);
   --     -  Enclosing_Context (unless the In_Unit parameter is set);
   --     -  Is_Part_Of_Implicit;
   --     -  Is_Part_Of_Inherited;
   --     -  Is_Part_Of_Instance;
   --     -  Special_Case (unless the Special_Case parameter is set);
   --     -  Node_Field_1 (unless the Node_Field_1 parameter is set)
   --     -  Node_Field_2 (unless the Node_Field_2 parameter is set)
   --  The typical situation of using this function in the implementations of
   --  ASIS (structural) queries is to set Starting_Element as being equal to
   --  the argument Element of the ASIS query.
   --
   --     If Starting_Element is not set, the corresponding fields of the
   --     result are computed from Node.
   --
   --  Internal_Kind - if this parameter is set, its value is set as the
   --     value of the Internal_Kind field of the result Element. Otherwise
   --     the kind of the result Element is determined automatically
   --
   --  Spec_Case  - if this parameter is set, its value is set as the
   --     value of the Special_Case field of the result Element. Otherwise
   --     the value of the Special_Case field of the result Element is set
   --     from Starting_Element, if Starting_Element itself is set, or as
   --     being equal to Not_A_Special_Case, if Starting_Element is not set.
   --
   --  Norm_Case - This parameter represents if the Element to create is a
   --     normalized association. It is not processed by this routine and it
   --     is transferred as-is to the low-level Element construction routine.
   --
   --  Considering_Parent_Count - boolean flag indicating if the value of the
   --     Parent_Count field of the Node should be taken into account for the
   --     definition of the kind of the Asis Element to be returned by the
   --     function.
   --
   --  Using_Original_Node - boolean flag indicating if the original node
   --     should be used as the basis of the Element being constructed.
   --     Usually the default True is used, but sometimes we have to
   --     avoid using the original node, for example, when constructing the
   --     expanded spec for a generic instantiation in case of a library_item
   --
   --  Inherited - boolean flag indicating if the element being constructed
   --     represents a declaration of an implicit inherited subprogram or a
   --     subcomponent thereof.
   --     ??? why do we need it???
   --
   --  In_Unit - The Asis Compilation Unit, to which the Element being
   --     constructed belongs. It is an error not to set both Starting_Element
   --     and In_Unit parameters. If both of them are set, the value of
   --     the Enclosing_Unit and Enclosing_Context fields of the result
   --     Element are set from In_Unit
   --
   --  NOTE, that if Starting_Element is NOT set, the Is_Part_Of_Implicit and
   --  the Is_Part_Of_Inherited fields  of  the result are set off (equal to
   --  False), therefore the caller (that is, the implementation of the
   --  corresponding ASIS query) is responsible for correct setting of these
   --  fields. As for the Is_Part_Of_Instance field, it is set on the base of
   --  Sloc field of the corresponding Node. (???)
   --
   --  An Empty node is not an expected parameter for this function. But if it
   --  is passed as an actual for Node, the result of the function is
   --  Nil_Element, independently of all the other parameters.

   function N_To_E_List_New
     (List             : List_Id;
      Include_Pragmas  : Boolean                := False;
      Starting_Element : Asis.Element           := Asis.Nil_Element;
      Node_Knd         : Node_Kind              := N_Empty;
      Internal_Kind    : Internal_Element_Kinds := Not_An_Element;
      Special_Case     : Special_Cases          := Not_A_Special_Case;
      Norm_Case        : Normalization_Cases    := Is_Not_Normalized;
      In_Unit          : Asis.Compilation_Unit  := Asis.Nil_Compilation_Unit)
      return Asis.Element_List;
   --  This function converts tree Node list into the corresponding ASIS
   --  Element List.
   --
   --  This function is intended to be used to create lists of EXPLICIT
   --  ASIS Elements. It checks Comes_From_Source Flag of the tree nodes from
   --  its List argument to test if the node should be used to create an
   --  Element in the result Element list
   --
   --  The parameters have the following meaning (we say "if the parameter
   --  is set" with the meaning "if some actual different from the default
   --  nil value is passed for the parameter")
   --
   --    List - specifies the tree node list to be converted into an ASIS
   --           Element List;
   --
   --    Include_Pragmas - boolean flags signifying if pragmas should be
   --           included in the result list;
   --
   --    Node_Knd - if this parameter is set, only those nodes from List
   --           which have this node kind are used to construct Elements in
   --           the result Element List;
   --
   --    Internal_Kind - if this parameter is set, all the Elements of the
   --           result list are set having this kind, otherwise the kind
   --           of each element of the result list is determined
   --           automatically;
   --
   --    Special_Case  - if this parameter is set, all the Elements of the
   --           result list are set having this special case;
   --
   --    Norm_Case  - if this parameter is set, all the Elements of the
   --           result list are set having this normalization case;
   --
   --    Starting_Element - the following fields of all the Elements in
   --           the result Element List are set as being equal to the
   --           corresponding fields of Starting_Element:
   --             -  Enclosing_Unit (unless the In_Unit parameter is set);
   --             -  Enclosing_Context (unless the In_Unit parameter is set);
   --             -  Is_Part_Of_Implicit;
   --             -  Is_Part_Of_Inherited;
   --             -  Is_Part_Of_Instance;
   --             -  Special_Case (unless the Special_Case parameter is set);
   --           Setting Starting_Element as being equal to the argument
   --           Element of the ASIS query where N_To_E_List is called
   --           to form the result of the query is the common case for the
   --           ASIS structural queries
   --
   --    In_Unit - The Asis Compilation Unit, to which the Elements being
   --           constructed belong. It is an error not to set both
   --           Starting_Element and In_Unit parameters. If both of them
   --           are set, the value of the Enclosing_Unit and
   --           Enclosing_Context fields of the result Elements are set
   --           from In_Unit

   procedure Set_Element_List
     (List             : List_Id;
      Include_Pragmas  : Boolean                := False;
      Starting_Element : Asis.Element           := Asis.Nil_Element;
      Node_Knd         : Node_Kind              := N_Empty;
      Internal_Kind    : Internal_Element_Kinds := Not_An_Element;
      Special_Case     : Special_Cases          := Not_A_Special_Case;
      Norm_Case        : Normalization_Cases    := Is_Not_Normalized;
      In_Unit          : Asis.Compilation_Unit  := Asis.Nil_Compilation_Unit;
      Append           : Boolean                := False);
   --  Does exactly the same as the function N_To_E_List_New, but places
   --  its result into A4G.Asis_Tables.Internal_Asis_Element_Table.
   --
   --  If Append parameter is set ON, the list created as the result of the
   --  call to this procedure is appended to the current content of the
   --  Element Table
   --
   --  ???
   --  It seems that we will have to get rid of functions creating Element
   --  Lists and to build Element lists only with Element Table. Moreover, now
   --  N_To_E_List_New just calls  Set_Element_List with Append OFF and returns
   --  the content of Internal_Asis_Element_Table

   procedure Set_Inherited_Discriminants (Type_Def : Asis.Element);
   --  Provided that Type_Def represents the definition of a derived
   --  type which may inherit discriminants, this procedure creates in
   --  the Element Table the list of declarations representing inherited
   --  discriminants.
   --  Note, that this function may not set properly Node_Field_1,
   --  Is_Part_Of_Implicit and Is_Part_Of_Inherited fields of the result
   --  Elements.

   procedure Set_Inherited_Components
     (Type_Def      : Asis.Element;
      Include_Discs : Boolean := True);
   --  Provided that Type_Def represents the definition of a derived
   --  record type, this procedure creates in
   --  A4G.Asis_Tables.Asis_Element_Table the list of declarations representing
   --  inherited components. If Include_Discs parameter is set ON, this list
   --  also contain discriminants, otherwise it does not. That is, this
   --  procedure does not determine itself whether or not the discriminants are
   --  inherited by the type, it should be done at the caller side.
   --  Note, that this procedure may not set properly Node_Field_1,
   --  Is_Part_Of_Implicit and Is_Part_Of_Inherited fields of the result
   --  Elements.

   procedure Set_Concurrent_Inherited_Components
     (Type_Def      : Asis.Element;
      Include_Discs : Boolean := True);
   --  Similar to the previous Set_Inherited_Components procedure, but woks
   --  on defininitions of derived concurrent types.

   procedure Set_Inherited_Literals (Type_Def : Asis.Element);
   --  Provided that Type_Def represents the definition of a derived
   --  enumeration type, this procedure creates in
   --  A4G.Asis_Tables.Asis_Element_Table the list of enumeration literal
   --  specifications representing inherited literals

   function Discrete_Choice_Node_To_Element_List
     (Choice_List      : List_Id;
      Starting_Element : Asis.Element)
      return Asis.Element_List;
   --  This function is a special variant of the Node_To_Element_List
   --  function, it constructs the ASIS Element list for an Ada
   --  discrete_choice_list. We need a special Note-to-Element list
   --  constructor for discrete_choice_list, because the automatic
   --  determination of the ASIS kinds for the elements of the resulting
   --  ASIS list does not work in this case.
   --
   --  Starting_Element has the same meaning as in general Node-to-Element
   --  and Node-to-Element list constructors.

   function Defining_Id_List_From_Normalized
     (N : Node_Id;
      From_Declaration : Asis.Element)
      return Asis.Defining_Name_List;
   --  This function constructs an (ASIS) list of A_Defining_Identifier
   --  ASIS Elements from a normalized sequence of one-identified
   --  declarations or specifications. N should be of one of the
   --  following kinds:
   --    N_Object_Declaration
   --    N_Number_Declaration
   --    N_Discriminant_Specification
   --    N_Component_Declaration
   --    N_Parameter_Specification
   --    N_Exception_Declaration
   --    N_Formal_Object_Declaration
   --  This function is intended to be used in the implementation of
   --  Asis.Declarations.Names query

   function Normalized_Namet_String (Node : Node_Id) return String;
   --  Returns the string representation of the construct representing by
   --  Node (assuming that Node represents some terminal Ada construct, such
   --  as an identifier) on the base of the information contained in the
   --  front-end Name Table and obtained by the value of the Chars field of
   --  the Node. This representation is "normalized" by capitalizing the first
   --  letter and every letter after underscore (except for some defining names
   --  from Standard, such as ASCII, for theses names all the letters are
   --  converted to upper case)

   function Ureal_Image (N : Node_Id) return String;
   --  Returns the string representation of a value represented by an
   --  N_Real_Literal node.

   function Is_Statement (N : Node_Id) return Boolean;
   --  Checks if N is a statement node.

   function Parenth_Count
     (N          : Node_Id;
      Original_N : Node_Id)
      return       Nat;
   --  This function is the same as Atree.Paren_Count, except that in case of
   --  the argument of a qualified expression, it decreases the level of
   --  parentheses to return the result corresponding to the syntax defined
   --  in RM 4.7(2) (see 9114-002)

   function Get_Next_Configuration_Pragma (N : Node_Id) return Node_Id;
   --  Returns the next configuration pragma node in the list. (If the argument
   --  is a configuration pragma node, returns the argument). If there is no
   --  more configuration pragmas in the list or if N is an empty node, returns
   --  Empty. A caller is responsible for calling this function for list
   --  elements only.

end A4G.Mapping;
