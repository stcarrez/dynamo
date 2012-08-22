------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                      A S I S . E X P R E S S I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes that you have made are clearly indicated.                        --
--                                                                          --
-- This  specification  also  contains  suggestions  and  discussion  items --
-- related to revising the  ASIS Standard according to the changes proposed --
-- for  the  new  revision of the Ada standard. The copyright notice above, --
-- and the license provisions that follow apply solely to these suggestions --
-- and  discussion  items  that  are separated by the corresponding comment --
-- sentinels                                                                --
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

------------------------------------------------------------------------------
--  17 package Asis.Expressions

--  Suggestions related to changing this specification to accept new Ada
--  features as defined in incoming revision of the Ada Standard (ISO 8652)
--  are marked by following comment sentinels:
--
--  --|A2005/2012 start
--   ... the suggestion goes here ...
--  --|A2005/2012 end
--
--  and the discussion items are marked by the comment sentinels of the form:
--
--  --|D2005/2012 start
--   ... the discussion item goes here ...
--  --|D2005/2012 end
--
--  Suggestions related to changing this specification to accept new Ada
--  features as suggested by ARG for next revision of the Ada Standard
--  (ISO 8652) grouped in the end of the package specification.

------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Expressions is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Expressions encapsulates a set of queries that operate on
--  An_Expression and An_Association elements.
------------------------------------------------------------------------------

--  --|D2005 start
--  It looks there is a problem with defining the result of Enclosing_Element
--  for expression components of normalized associations. The problem exists
--  for all kinds of associations having the normalized form and, respectively,
--  for the definitions of the corresponding queries that obtain the expression
--  from an association:
--
--    Component_Expression
--    Actual_Parameter
--    Discriminant_Expression
--
--  The following problem exists for a normalized association in case if the
--  corresponding expression (parameter) is given explicitly. All these queries
--  say that in this case they return an Element representing the corresponding
--  expression as it is given in the program text, this Element is not
--  Is_Normalized. And they say that the Enclosing_Element for such expression
--  Element is the argument An_Association Element. And now the question^
--
--  Consider the following subprogram declaration:
--
--     procedure P (X : Integer);
--
--  and the corresponding call:
--
--     P (13);
--
--  Suppose Call_Of_P is an Element representing this call. Now consider the
--  following ASIS-based code (all the variables are of Asis.Element type:
--
--  A := Call_Statement_Parameters      --  A represents non-normalized
--         (Statement => Call_Of_P,     --  parameter association
--          Normalized = False) (1);
--
--  NA := Call_Statement_Parameters      --  NA represents normalized
--          (Statement => Call_Of_P,     --  parameter association
--           Normalized = True) (1);
--
--  A_Actual  := Actual_Parameter (A);   --  actual parameter from
--                                       --  non-normalized association
--
--  NA_Actual := Actual_Parameter (NA);  --  actual parameter from
--                                       --  normalized association
--
--  And now the first question: both A_Actual and NA_Actual represent the same
--  numeric literal from the source code. And what should be the value of
--  Is_Equal (A_Actual, NA_Actual)? I would say it should be True. But if it
--  is really so, consider the following:
--
--  A_Actual_Enclosing  := Enclosing_Element (A_Actual);
--  NA_Actual_Enclosing := Enclosing_Element (NA_Actual);
--
--  According to the wording of ASIS section 17.22 (function Actual_Parameter)
--  Is_Equal (A_Actual_Enclosing, A) and Is_Equal (NA_Actual_Enclosing, NA)!
--  And for sure Is_Equal (A, NA) is False! That is, Enclosing_Element should
--  return two DIFFERENT results when applied to THE SAME argument ("different"
--  and "the same" means not Is_Equal () and Is_Equal ()).
--
--  This definitely needs clarification!
--  --|D2005 end

------------------------------------------------------------------------------
--  17.1  function Corresponding_Expression_Type
------------------------------------------------------------------------------

   function Corresponding_Expression_Type
     (Expression : Asis.Expression)
      return       Asis.Declaration;

------------------------------------------------------------------------------
--  Expression  - Specifies the expression to query
--
--  Returns the type declaration for the type or subtype of the expression.
--  This query does not "unwind" subtypes or derived types to get to the
--  Corresponding_First_Subtype or Corresponding_Parent_Subtype declarations.
--  For example, for the following program text:
--
--      type Int is range -5_000 .. 5_000;
--      type My_Int is new Int;
--      type Good_Int is new My_Int;
--      Var: Good_Int;
--
--  The type declaration for Good_Int should be returned. The "unwinding"
--  should not occur. The type declaration for either My_Int or Int should not
--  be returned.
--
--  Returns a Nil_Element if the argument Expression does not represent an Ada
--  expression having an Ada type, including the following classes:
--
--   - Naming expressions that name packages, subprograms, tasks, etc.  These
--     expressions do have a Corresponding_Name_Definition and a
--     Corresponding_Name_Declaration. Although task objects do have
--     a type, this query is limited, on purpose.  Thus, when a naming
--     expression is given to this query (for packages, subprograms,
--     tasks, etc.), this query will return Nil_Element.  As the
--     Application Note below indicates, if any further information
--     is needed, the element should be queried by
--     Corresponding_Name_Definition or Corresponding_Name_Declaration,
--     which should eventually return an A_Task_Type_Declaration element.
--
--   - When An_Identifier Element representing an attribute designator is
--     passed as the actual to this query.
--
--   - The Actual_Parameter Expression from A_Pragma_Argument_Association for a
--     Pragma may or may not have a Corresponding_Expression_Type.
--
--   - An_Attribute_Reference Element also may or may not have a
--     Corresponding_Expression_Type;
--
--   - An enumeration_aggregate which is a part of
--     enumeration_representation_clause.
--
--  Returns a Nil_Element, if statically determinable type of Expression is a
--  class-wide type.
--
--  --|AN Application Note:
--  --|AN
--  --|AN If the returned declaration is Nil, an application should make its
--  --|AN own analysis based on Corresponding_Name_Definition or
--  --|AN Corresponding_Name_Declaration to get more information about the
--  --|AN argument, including the static type resolution for class-wide
--  --|AN expressions, if needed. Use Enclosing_Element to determine if
--  --|AN Expression is from pragma argument association. If for such an
--  --|AN expression, Corresponding_Name_Definition raises ASIS_Failed (with a
--  --|AN Status of Value_Error), this An_Expression element does not represent
--  --|AN a normal Ada expression at all and does not follow normal Ada
--  --|AN semantic rules.
--  --|AN For example, "pragma Private_Part (Open => Yes);", the "Yes"
--  --|AN expression may simply be a "keyword" that is specially recognized by
--  --|AN the implementor's compilation system and may not refer to any
--  --|AN declared object.
--
--  Appropriate Element_Kinds:
--       An_Expression
--
--  Returns Element_Kinds:
--       Not_An_Element
--       A_Declaration
--
--  --|A2005 start
--  --|D2005 start
--  I would propose to add to the ASIS interface the following query (the
--  wording below is for discussion, but to for defining the query):

------------------------------------------------------------------------------
--  17.#???  function Corresponding_Expression_Type_Definition
------------------------------------------------------------------------------

   function Corresponding_Expression_Type_Definition
     (Expression : Asis.Expression)
      return       Asis.Element;

------------------------------------------------------------------------------
--  Expression  - Specifies the expression to query
--
--  This query is similar to Corresponding_Expression_Type, but it returns not
--  the *declaration* of the argument expression type, but the *definition*
--  of the argument expression (sub)type.
--
--  This may be useful in the following cases:
--
--  (1) Consider:
--
--         type Rec is record
--             C : String (1 .. 10);
--         end record;
--
--         Var : Rec;
--
--      If we apply Corresponding_Expression_Type to the argument representing
--      Var.C, the result will be the declaration of String, but
--      Corresponding_Expression_Type_Definition should return the component
--      definition representing 'String (1 .. 10)', that may be of more
--      interest for the ASIS client
--
--  (2) In case of anonymous access types we do not have type*declarations*
--      to be returned by Corresponding_Expression_Type, but this query may
--      return the corresponding An_Access_Definition Element
--
--  Appropriate Element_Kinds:
--       An_Expression
--
--  Returns Element_Kinds:
--        Not_An_Element
--
--  Returns Definition_Kinds:
--        Not_A_Definition
--        A_Type_Definition
--            Returns Type_Kinds:
--                  A_Constrained_Array_Definition
--        A_Subtype_Indication
--        A_Task_Definition
--        A_Protected_Definition
--        A_Component_Definition
--        An_Access_Definition
--
--  Returns Expression_Kinds:
--       An_Identifier
--       A_Selected_Component
--       An_Attribute_Reference

------------------------------------------------------------------------------
--  |D2005 end
--  |A2005 end

--  --|ER An_Integer_Literal - 2.4 - No child elements
--  --|ER A_Real_Literal     - 2.4 - No child elements
--  --|ER A_String_Literal   - 2.6 - No child elements
--  --|ER
--  --|ER A string image returned by:
--  --|ER    function Value_Image
--
------------------------------------------------------------------------------
--  17.2  function Value_Image
------------------------------------------------------------------------------

   function Value_Image
     (Expression : Asis.Expression)
      return       Wide_String;

------------------------------------------------------------------------------
--  Expression  - Specifies the expression to query
--
--  Returns the string image of the value of the string, integer, or real
--  literal.
--
--  For string literals, Value will return the quotes around the string
--  literal, these quotes are doubled, just as any quote appearing embedded in
--  the string literal in the program text.
--
--  The form of numbers returned by this query may vary between implementors.
--  Implementors are encouraged, but not required, to return numeric literals
--  using the same based or exponent form used in the original compilation
--  text.
--
--  Appropriate Expression_Kinds:
--       An_Integer_Literal
--       A_Real_Literal
--       A_String_Literal
--
------------------------------------------------------------------------------
--  --|ER An_Identifier          - 4.1 - No child elements
--  --|ER An_Operator_Symbol     - 4.1 - No child elements
--  --|ER A_Character_Literal    - 4.1 - No child elements
--  --|ER An_Enumeration_Literal - 4.1 - No child elements
--  --|ER
--  --|ER A string image returned by:
--  --|ER    function Name_Image
--  --|ER
--  --|ER Semantic elements returned by:
--  --|ER    function Corresponding_Name_Definition
--  --|ER    function Corresponding_Name_Definition_List
--  --|ER    function Corresponding_Name_Declaration
--
------------------------------------------------------------------------------
--  17.3  function Name_Image
------------------------------------------------------------------------------

   function Name_Image
     (Expression : Asis.Expression)
      return       Program_Text;

------------------------------------------------------------------------------
--  Name  - Specifies the name to query
--
--  Returns the program text image of the name.
--
--  An_Operator_Symbol elements have names with embedded quotes """abs"""
--    (function "abs").
--
--  A_Character_Literal elements have names with embedded apostrophes "'x'"
--    (literal 'x').
--
--  An_Enumeration_Literal and An_Identifier elements have identifier names
--    "Blue" (literal Blue) "Abc" (identifier Abc).
--
--  Note: Implicit subtypes that can be encountered while traversing the
--  semantic information embedded in implicit inherited subprogram declarations
--  (Reference Manual 3.4 (17-22)) could have names that are unique in a
--  particular scope.  This is because these subtypes are Is_Part_Of_Implicit
--  declarations that do not form part of the physical text of the original
--  compilation units. Some applications may wish to carefully separate the
--  names of declarations from the names of Is_Part_Of_Implicit declaration
--  when creating symbol tables and other name-specific lookup mechanisms.
--
--  The case of names returned by this query may vary between implementors.
--  Implementors are encouraged, but not required, to return names in the
--  same case as was used in the original compilation text.
--
--  Appropriate Expression_Kinds:
--       An_Identifier
--       An_Operator_Symbol
--       A_Character_Literal
--       An_Enumeration_Literal
--
------------------------------------------------------------------------------
--  17.4  function References
------------------------------------------------------------------------------

   function References
     (Name           : Asis.Element;
      Within_Element : Asis.Element;
      Implicitly     : Boolean := False)
      return           Asis.Name_List;

------------------------------------------------------------------------------
--  Name    - Specifies the entity to query
--  Within_Element - Specifies the limits for the query which is limited
--                   to the Element and its children.
--
--  If the Implicitly argument is True:
--    Returns all usage references of the given entity made by both explicit
--    and implicit elements within the given limits.
--
--  If the Implicitly argument is False:
--    Returns all usage references of the given entity made only by explicit
--    elements within the given limits.
--
--  Returned references are in their order of appearance.
--
--  Appropriate Element_Kinds:
--       A_Defining_Name
--  Returns Element_Kinds:
--       An_Expression
--
--  May raise ASIS_Failed with a Status of Obsolete_Reference_Error if the
--  argument is part of an inconsistent compilation unit.
--
------------------------------------------------------------------------------
--  17.5  function Is_Referenced
------------------------------------------------------------------------------

   function Is_Referenced
     (Name           : Asis.Element;
      Within_Element : Asis.Element;
      Implicitly     : Boolean := False)
      return           Boolean;

------------------------------------------------------------------------------
--  Name    - Specifies the entity to query
--  Within_Element - Specifies the limits for the query which is limited
--                   to the Element and its children.
--

--  If the Implicitly argument is True:
--    Returns True if the Name is referenced by either implicit or explicit
--    elements within the given limits.
--
--  If the Implicitly argument is False:
--    Returns True only if the Name is referenced by explicit elements.
--
--  Returns False for any unexpected Element.
--
--  Expected Element_Kinds:
--       A_Defining_Name
--
--  May raise ASIS_Failed with a Status of Obsolete_Reference_Error if the
--  argument is part of an inconsistent compilation unit.
--
------------------------------------------------------------------------------
--  17.6  function Corresponding_Name_Definition
------------------------------------------------------------------------------

   function Corresponding_Name_Definition
     (Reference : Asis.Expression)
      return      Asis.Defining_Name;

------------------------------------------------------------------------------
--  Reference   - Specifies an expression to query
--
--  Returns the defining_identifier, defining_character_literal,
--  defining_operator_symbol, or defining_program_unit_name from the
--  declaration of the referenced entity.
--
--   - Record component references return the defining name of the
--     record discriminant or component_declaration.  For references to
--     inherited declarations of derived types, the
--     Corresponding_Name_Definition returns
--     the defining name of the implicit inherited declaration.
--
--   - References to implicit operators and inherited subprograms will return
--     an Is_Part_Of_Implicit defining name for the operation.  The
--     Enclosing_Element of the name is an implicit declaration for the
--     operation.  The Enclosing_Element of the declaration is the associated
--     derived_type_definition.
--
--   - References to formal parameters given in calls to inherited subprograms
--     will return an Is_Part_Of_Implicit defining name for the
--     Parameter_Specification from the inherited subprogram specification.
--
--   - References to visible components of instantiated generic packages will
--     return a name from the expanded generic specification instance.
--
--   - References, within expanded generic instances, that refer to other
--     components of the same, or an enclosing, expanded generic instance,
--     return a name from the appropriate expanded specification or body
--     instance.
--
--  In case of renaming, the function returns the new name for the entity.
--
--  Returns a Nil_Element if the reference is to an implicitly declared
--  element for which the implementation does not provide declarations and
--  defining name elements.
--
--  Returns a Nil_Element if the argument is a dispatching call.
--
--  --|A2005 start
--  Returns  Nil_Element if Reference denotes an artificial null procedure
--  that is a default actual for a formal procedure having null default.
--  --|A2005 end
--
--  --|D2005 start
--
--  The previous statement is, first of all, wrong - for this query, the
--  argument of this query in no case can be a call. Most probably, the idea is
--  to say that if argument is a reference to a called subprogram or to a
--  formal parameter from a dispatching call, then the result is Nil_Element.
--
--  Second, there is no real need to return Nil_Element in these cases.
--  According to the Ada Standard, for a dispatching call
--
--     Proc (X, Y, Z => Val);
--
--  both Proc and Z can and should be determined statically (see RM 2005
--  3.9.2 (2)): Proc should denote the reference to the dispatching subprogram
--  (that is, to the top-most procedure in the derivation class that can be
--  selected at run-time for this call), and Z - the corresponding formal
--  parameter from the declaration of Proc.
--
--  Third, ASIS has a serious gap related to processing of dispatching calls.
--  Corresponding_Called_Entity/Corresponding_Called_Function queries return
--  Nil_Element if their argument Is_Dispatching_Call, so there is no way to
--  get any information about the profile of the called subprogram.
--
--  If we remove this limitation (to return Nil_Element for references to
--  dispatching operations and their formal parameters) from this query, this
--  would (first) make this query with the Ada static semantic as it is
--  described in the Ada Standard, and (second) give to an application means
--  to get the complete information about the profile of the called subprogram,
--  and, moreover, about the top of the hierarchy of the subprograms that can
--  be  called at run time.
--
--  Starting from 2007.02.20, the GNAT ASIS implementation supports this
--  approach.
--
--  --|D2005 end
--
--  The Enclosing_Element of a non-Nil result is either a Declaration or a
--  Statement.
--
--  Appropriate Expression_Kinds:
--       An_Identifier
--       An_Operator_Symbol
--       A_Character_Literal
--       An_Enumeration_Literal
--
--  Returns Element_Kinds:
--       Not_An_Element
--       A_Defining_Name
--
--  --|IP Implementation Permissions:
--  --|IP
--  --|IP An implementation may choose to return any part of multi-part
--  --|IP declarations and definitions. Multi-part declaration/definitions
--  --|IP can occur for:
--  --|IP
--  --|IP    - Subprogram specification in package specification, package body,
--  --|IP      and subunits (is separate);
--  --|IP    - Entries in package specification, package body, and subunits
--  --|IP      (is separate);
--  --|IP    - Private type and full type declarations;
--  --|IP    - Incomplete type and full type declarations; and
--  --|IP    - Deferred constant and full constant declarations.
--  --|IP
--  --|IP No guarantee is made that the element will be the first part or
--  --|IP that the determination will be made due to any visibility rules.
--  --|IP An application should make its own analysis for each case based
--  --|IP on which part is returned.
--  --|IP
--  --|IP Some implementations do not represent all forms of implicit
--  --|IP declarations such that elements representing them can be easily
--  --|IP provided.  An implementation can choose whether or not to construct
--  --|IP and provide artificial declarations for implicitly declared elements.
--  --|IP
--  --|IR Implementation Requirements:
--  --|IR
--  Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if passed
--  a reference that does not have a declaration:
--
--  - a reference to an attribute_designator.  Attributes are defined, but
--    have no implicit or explicit declarations;
--
--  - an identifier which syntactically is placed before "=>" in a
--    pragma_argument_association which has the form of a named association;
--    such an identifier can never have a declaration;
--
--  - an identifier specific to a pragma (Reference Manual, 2.8(10));
--
--       pragma Should_I_Check ( Really => Yes );
--
--    In this example, both the names Really and Yes have no declaration.
--
--  Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if passed
--  a portion of a pragma that was "ignored" by the compiler and which does
--  not have (sufficient) semantic information for a proper return result
--  to be computed.  For example,
--
--       pragma I_Am_Ignored (Foof);
--
--  The "Foof" expression is An_Identifier but raises this exception
--  if passed to Corresponding_Name_Definition if the pragma was ignored
--  or unprocessed.
--
--  Raises ASIS_Inappropriate_Element, with a Status of Value_Error, if passed
--  a portion of a pragma that is an ambiguous reference to more than one
--  entity.  For example,
--
--       pragma Inline ("+");        -- Inlines all "+" operators
--
--  The "+" expression is An_Operator_Symbol but raises this
--  exception if it referenced more than one "+" operator.  In this
--  case, the Corresponding_Name_Definition_List query can be used to obtain a
--  list of referenced entities.
--
------------------------------------------------------------------------------
--  17.7  function Corresponding_Name_Definition_List
------------------------------------------------------------------------------

   function Corresponding_Name_Definition_List
     (Reference : Asis.Element)
      return      Asis.Defining_Name_List;

------------------------------------------------------------------------------
--  Reference   - Specifies an entity reference to query
--
--  Exactly like Corresponding_Name_Definition except it returns a list.
--  The list will almost always have a length of one.  The exception to this
--  is the case where an expression in a pragma is ambiguous and reference
--  more than one entity.  For example,
--
--       pragma Inline ("+");        -- Inlines all "+" operators
--
--  The "+" expression is An_Operator_Symbol but could reference more
--  than one "+" operator.  In this case, the resulting list includes all
--  referenced entities.
--
--  Appropriate Expression_Kinds:
--       An_Identifier
--       An_Operator_Symbol
--       A_Character_Literal
--       An_Enumeration_Literal
--
--  Returns Element_Kinds:
--       A_Defining_Name
--
------------------------------------------------------------------------------
--  17.8  function Corresponding_Name_Declaration
------------------------------------------------------------------------------

   function Corresponding_Name_Declaration
     (Reference : Asis.Expression)
      return      Asis.Element;

------------------------------------------------------------------------------
--  Reference   - Specifies the entity reference to query
--
--  Returns the declaration that declared the entity named by the given
--  reference.  The result is exactly the same as:
--
--       Result := Corresponding_Name_Definition (Reference);
--       if not Is_Nil (Result) then
--           Result := Enclosing_Element (Result);
--       end if;
--       return Result;
--
--  See the comments for Corresponding_Name_Definition for details.
--  The result is either a Declaration or a Statement.  Statements result
--  from references to statement labels, loop identifiers, and block
--  identifiers.
--
--  Appropriate Element_Kinds:
--       An_Expression
--
--  Appropriate Expression_Kinds:
--       An_Identifier
--       An_Operator_Symbol
--       A_Character_Literal
--       An_Enumeration_Literal
--
--  Returns Element_Kinds:
--       A_Declaration
--       A_Statement
--
--  Predefined types, exceptions, operators in package Standard can be
--  checked by testing that the enclosing Compilation_Unit is standard.
--  --|ER---------------------------------------------------------------------
--  --|ER An_Explicit_Dereference - 4.1
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Prefix
--  --|CR
--

------------------------------------------------------------------------------
--  17.9  function Prefix
------------------------------------------------------------------------------

   function Prefix (Expression : Asis.Expression) return Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the name expression to query
--
--  Returns the prefix (the construct to the left of: the rightmost unnested
--  left parenthesis in function_call elements and indexed_component elements
--  or slice elements, the rightmost 'dot' for selected_component elements, or
--  the rightmost tick for attribute_reference elements).
--
--  Returns the operator_symbol for infix operator function calls.  The infix
--  form A + B is equivalent to the prefix form "+"(A, B).
--
--  Appropriate Expression_Kinds:
--       An_Explicit_Dereference       P.ALL
--       An_Attribute_Reference        Priv'Base'First
--       A_Function_Call               Abc(...) or Integer'Image(...)
--       An_Indexed_Component          An_Array(3)
--       A_Selected_Component          A.B.C
--       A_Slice                       An_Array(3 .. 5)
--
--  Returns Expression_Kinds:
--       An_Expression
--
--  --|ER---------------------------------------------------------------------
--  --|ER An_Indexed_Component - 4.1.1
--  --|ER
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Prefix
--  --|CR    function Index_Expressions
--  --|CR
--
------------------------------------------------------------------------------
--  17.10 function Index_Expressions
------------------------------------------------------------------------------

   function Index_Expressions
     (Expression : Asis.Expression)
      return       Asis.Expression_List;

------------------------------------------------------------------------------
--  Expression  - Specifies an indexed_component to query
--
--  Returns the list of expressions (possibly only one) within the parenthesis,
--  in their order of appearance.
--
--  Appropriate Expression_Kinds:
--       An_Indexed_Component
--
--  Returns Element_Kinds:
--       An_Expression
--
--  --|ER---------------------------------------------------------------------
--  --|ER A_Slice - 4.1.2
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Prefix
--  --|CR    function Slice_Range
--  --|CR
--

------------------------------------------------------------------------------
--  17.11 function Slice_Range
------------------------------------------------------------------------------

   function Slice_Range
     (Expression : Asis.Expression)
      return       Asis.Discrete_Range;

------------------------------------------------------------------------------
--  Expression  - Specifies the slice to query
--
--  Returns the discrete range of the slice.
--
--  Appropriate Expression_Kinds:
--       A_Slice
--
--  Returns Definition_Kinds:
--       A_Discrete_Range
--
------------------------------------------------------------------------------
--  17.12 function Selector
------------------------------------------------------------------------------

   function Selector (Expression : Asis.Expression) return Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the selected_component to query
--
--  Returns the selector (the construct to the right of the rightmost 'dot' in
--  the selected_component).
--
--  Appropriate Expression_Kinds:
--       A_Selected_Component
--
--  Returns Expression_Kinds:
--       An_Identifier
--       An_Operator_Symbol
--       A_Character_Literal
--       An_Enumeration_Literal
--
------------------------------------------------------------------------------
--  17.13 function Attribute_Designator_Identifier
------------------------------------------------------------------------------

   function Attribute_Designator_Identifier
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies an attribute_reference expression to query
--
--  Returns the identifier of the attribute_designator (the construct to the
--  right of the rightmost tick of the attribute_reference).  The Prefix of
--  the attribute_reference can itself be an attribute_reference as in
--  T'BASE'FIRST where the prefix is T'BASE and the attribute_designator name
--  is FIRST.
--
--  Attribute_designator reserved words "access", "delta", and "digits" are
--  treated as An_Identifier.
--
--  Appropriate Expression_Kinds:
--       An_Attribute_Reference
--
--  Returns Expression_Kinds:
--       An_Identifier
--

------------------------------------------------------------------------------
--  17.14 function Attribute_Designator_Expressions
------------------------------------------------------------------------------

   function Attribute_Designator_Expressions
     (Expression : Asis.Expression)
      return       Asis.Expression_List;

------------------------------------------------------------------------------
--  Expression  - Specifies an attribute expression to query
--
--  Returns the static expressions associated with the optional argument of the
--  attribute_designator.  Expected predefined attributes are A'First(N),
--  A'Last(N), A'Length(N), and A'Range(N).
--
--  Returns a Nil_Element_List if there are no arguments.
--
--  Appropriate Expression_Kinds:
--       An_Attribute_Reference
--           Appropriate Attribute_Kinds:
--                A_First_Attribute
--                A_Last_Attribute
--                A_Length_Attribute
--                A_Range_Attribute
--                An_Implementation_Defined_Attribute
--                An_Unknown_Attribute
--
--  Returns Element_Kinds:
--       An_Expression
--
--  --|IP Implementation Permissions:
--  --|IP
--  --|IP This query returns a list to support implementation-defined
--  --|IP attributes that may have more than one static_expression.
--
------------------------------------------------------------------------------
--  17.15 function Record_Component_Associations
------------------------------------------------------------------------------

   function Record_Component_Associations
     (Expression : Asis.Expression;
      Normalized : Boolean := False)
      return       Asis.Association_List;

------------------------------------------------------------------------------
--  Expression  - Specifies an aggregate expression to query
--  Normalized  - Specifies whether the normalized form is desired
--
--  Returns a list of the record_component_association elements of a
--  record_aggregate or an extension_aggregate.
--
--  Returns a Nil_Element_List if the aggregate is of the form (null record).
--
--  An unnormalized list contains all needed associations ordered as they
--  appear in the program text.  Each unnormalized association has an optional
--  list of discriminant_selector_name elements, and an explicit expression.
--  |D2005 start
--  discriminant_selector_name should be replaced with component_selector_name
--  |D2005 end
--
--  A normalized list contains artificial associations representing all
--  needed components in an order matching the declaration order of the
--  needed components.
--
--  Each normalized association represents a one on one mapping of a
--  component to the explicit expression.  A normalized association has one
--  A_Defining_Name component that denotes the discriminant_specification or
--  component_declaration, and one An_Expression component that is the
--  expression.
--
--  |D2005 start
--  This description should be revised to explain what happens for a component
--  if the argument aggregate specifies a box for this components. The proposal
--  is to return the default component expression if the aggregate type
--  contains the default expression for this component, and Nil_Element
--  otherwise.
--  |D2005 end
--
--  Appropriate Expression_Kinds:
--       A_Record_Aggregate
--       An_Extension_Aggregate
--
--  Returns Association_Kinds:
--       A_Record_Component_Association
--
--  --|IR Implementation Requirements:
--  --|IR
--  --|IR Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
--  --|IR Normalized associations are never Is_Equal to unnormalized
--  --|IR associations.
--  --|IR
--  --|IP Implementation Permissions:
--  --|IP
--  --|IP An implementation may choose to normalize its internal representation
--  --|IP to use the defining_identifier element instead of the
--  --|IP component_selector_name  element.
--  --|IP
--  --|IP If so, this query will return Is_Normalized associations even if
--  --|IP Normalized is False, and the query
--  --|IP Record_Component_Associations_Normalized will return True.
--
------------------------------------------------------------------------------
--  17.16 function Extension_Aggregate_Expression
------------------------------------------------------------------------------

   function Extension_Aggregate_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies an extension_aggregate expression to query
--
--  Returns the ancestor_part expression preceding the reserved word with in
--  the extension_aggregate.
--
--  Appropriate Expression_Kinds:
--       An_Extension_Aggregate
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.17 function Array_Component_Associations
------------------------------------------------------------------------------

   function Array_Component_Associations
     (Expression : Asis.Expression)
      return       Asis.Association_List;

------------------------------------------------------------------------------
--  Expression  - Specifies an array aggregate expression to query
--
--  Returns a list of the Array_Component_Associations in an array aggregate.
--
--  Appropriate Expression_Kinds:
--       A_Positional_Array_Aggregate
--       A_Named_Array_Aggregate
--
--  Returns Association_Kinds:
--       An_Array_Component_Association
--
--  --|AN Application Note:
--  --|AN
--  --|AN While positional_array_aggregate elements do not have
--  --|AN array_component_association elements defined by Ada syntax, ASIS
--  --|AN treats A_Positional_Array_Aggregate as if it were
--  --|AN A_Named_Array_Aggregate. The An_Array_Component_Association elements
--  --|AN returned will have Array_Component_Choices that are a
--  --|AN Nil_Element_List for all positional expressions except an others
--  --|AN choice.
--

------------------------------------------------------------------------------
--  17.18 function Array_Component_Choices
------------------------------------------------------------------------------

   function Array_Component_Choices
     (Association : Asis.Association)
      return        Asis.Expression_List;

------------------------------------------------------------------------------
--  Association - Specifies the component association to query
--
--  If the Association is from a named_array_aggregate:
--
--    Returns the discrete_choice_list order of appearance.  The choices are
--    either An_Expression or A_Discrete_Range elements, or a single
--    An_Others_Choice element.
--
--  If the Association is from a positional_array_aggregate:
--
--    Returns a single An_Others_Choice if the association is an others
--    choice (others => expression).
--
--    Returns a Nil_Element_List otherwise.
--
--  Appropriate Association_Kinds:
--       An_Array_Component_Association
--
--  Returns Element_Kinds:
--       A_Definition
--       An_Expression
--
--  Returns Definition_Kinds:
--       A_Discrete_Range
--       An_Others_Choice
--
------------------------------------------------------------------------------
--  17.19 function Record_Component_Choices
------------------------------------------------------------------------------

   function Record_Component_Choices
     (Association : Asis.Association)
      return        Asis.Expression_List;

------------------------------------------------------------------------------
--  Association - Specifies the component association to query
--
--  If the Association argument is from an unnormalized list:
--
--  - If the Association is a named component association:
--
--    Returns the component_choice_list order of appearance.  The choices are
--    either An_Identifier elements representing component_selector_name
--    elements, or a single An_Others_Choice element.
--
--    The Enclosing_Element of the choices is the Association argument.
--
--  - If the Association is a positional component association:
--
--    Returns a Nil_Element_List.
--
--  If the Association argument is from a Normalized list:
--
--    Returns a list containing a single choice:
--
--    - A_Defining_Name element representing the defining_identifier of
--      the component_declaration.
--
--    The Enclosing_Element of the A_Defining_Name is the
--    component_declaration.
--
--   Normalized lists contain artificial ASIS An_Association elements that
--   provide one formal A_Defining_Name => An_Expression pair per
--   association.  These artificial associations are Is_Normalized.  Their
--   component A_Defining_Name is not Is_Normalized.
--
--  Appropriate Association_Kinds:
--       A_Record_Component_Association
--
--  Returns Element_Kinds:
--       A_Defining_Name             -- Is_Normalized(Association)
--       An_Expression               -- not Is_Normalized(Association)
--            Returns Expression_Kinds:
--                 An_Identifier
--       A_Definition
--            Returns Definition_Kinds:
--                 An_Others_Choice
--
------------------------------------------------------------------------------
--  17.20 function Component_Expression
------------------------------------------------------------------------------

   function Component_Expression
     (Association : Asis.Association)
      return        Asis.Expression;

------------------------------------------------------------------------------
--  Association - Specifies the component association to query
--
--  Returns the expression of the record_component_association or
--  array_component_association.

--  --|A2005 start
--  For non-normalized associations, returns Nil_Element if the argument
--  association contains a box.
--  --|A2005 end
--
--  The Enclosing_Element of the expression is the Association argument.
--
--  See SI99-0031-1/01:
--
--    In case if an actual parameter is from a Normalized association, but it
--    represents the name or the expression that is explicitely given in the
--    source, the Enclosing_Element of An_Expression is not the arhument
--    Association (that is normalized), but the corresponding non-normalized
--    An_Association Element
--
--  Normalized lists contain artificial ASIS An_Association elements that
--  provide one formal A_Defining_Name => An_Expression pair per
--  association.  These artificial associations are Is_Normalized.  Their
--  component An_Expression elements are not Is_Normalized.

--  --|A2005 start
--  If the enclosing aggregate specifies a box for the given component, the
--  result for An_Array_Component_Association is Nil_Element. For
--  A_Record_Component_Association, the result is either the corresponding
--  initialization expression from the record type definition, or Nil_Element
--  if the type of the aggregate does not provide a default expression for
--  the component
--  --|A2005 end
--
--  Appropriate Association_Kinds:
--       A_Record_Component_Association
--       An_Array_Component_Association
--
--  Returns Element_Kinds:
--  --|A2005 start
--       Not_An_Element
--  --|A2005 end
--       An_Expression
--
------------------------------------------------------------------------------
--  17.21 function Formal_Parameter
------------------------------------------------------------------------------

   function Formal_Parameter
     (Association : Asis.Association)
      return        Asis.Element;

------------------------------------------------------------------------------
--  Association - Specifies the association to query
--
--  |D2006 start
--  See the discussion for Asis.Declarations.Generic_Actual_Part (15.45).
--  The documentation of this query should be updated when it is finally
--  decided which way of representing formal_package_association to go.
--  |D2006 end
--
--  If the Association argument is from an unnormalized list:
--
--  - If the Association is given in named notation:
--
--    Returns An_Identifier representing the formal_parameter_selector_name,
--    generic_formal_parameter_selector_name, or pragma_argument_identifier.
--
--    The Enclosing_Element of the An_Identifier element is the Association
--    argument.
--
--  - If the Association is given in positional notation:
--
--    Returns a Nil_Element.
--
--  If the Association argument is from a Normalized list:
--
--   - Returns A_Defining_Name representing the defining_identifier of the
--     parameter_specification or generic_formal_parameter_declaration.
--     Pragma_argument_association elements are not available in normalized
--     form.
--
--   - The Enclosing_Element of the A_Defining_Name is the
--     parameter_specification or generic_formal_parameter_declaration element.
--
--  Normalized lists contain artificial ASIS An_Association elements that
--  provide one formal A_Defining_Name => An_Expression pair per
--  association.  These artificial associations are Is_Normalized.  Their
--  component A_Defining_Name elements are not Is_Normalized.
--
--  Appropriate Association_Kinds:
--       A_Parameter_Association
--       A_Generic_Association
--       A_Pragma_Argument_Association
--
--  Returns Element_Kinds:
--       Not_An_Element
--       An_Operator_Symbol
--       A_Defining_Name             -- Is_Normalized(Association)
--       An_Expression               -- not Is_Normalized(Association)
--            Returns Expression_Kinds:
--                  An_Identifier
--
--  --|A2006 start
--  Returns Definition_Kinds
--       An_Others_Choice            not Is_Normalized(Association)
--  --|A2006 end
--

------------------------------------------------------------------------------
--  17.22 function Actual_Parameter
------------------------------------------------------------------------------

   function Actual_Parameter
     (Association : Asis.Association)
      return        Asis.Expression;

------------------------------------------------------------------------------
--  Association   - Specifies the association to query
--
--  |D2006 start
--  See the discussion for Asis.Declarations.Generic_Actual_Part (15.45)
--  The documentation of this query should be updated when it is finally
--  decided which way of representing formal_package_association to go.
--  |D2006 end
--
--  If the Association argument is from an unnormalized list:
--
--    Returns An_Expression representing:
--
--    - the explicit_actual_parameter of a parameter_association.
--
--    - the explicit_generic_actual_parameter of a generic_association.
--
--    - the name or expression of a pragma_argument_association.
--
--    The Enclosing_Element of An_Expression is the Association argument.
--
--  If the Association argument is from a Normalized list:
--
--  - If the Association is given explicitly:
--
--    Returns An_Expression representing:
--
--    - the explicit_actual_parameter of a parameter_association.
--
--    - the explicit_generic_actual_parameter of a generic_association.
--
--    The Enclosing_Element of An_Expression is the Association argument.
--
--  See SI99-0031-1/01:
--
--    In case if an actual parameter is from a Normalized association, but it
--    represents the name or the expression that is explicitely given in the
--    source, the Enclosing_Element of An_Expression is not the arhument
--    Association (that is normalized), but the corresponding non-normalized
--    An_Association Element
--
--  - If the Association is given by default:
--
--    Returns An_Expression representing:
--
--    - the corresponding default_expression of the Is_Normalized
--      A_Parameter_Association.
--
--   - the corresponding default_expression or default_name of the
--     Is_Normalized A_Generic_Association.
--
--   - The Enclosing_Element of the An_Expression element is the
--     parameter_specification or generic_formal_parameter_declaration that
--     contains the default_expression or default_name, except for the case
--     when this An_Expression element is an implicit naming expression
--     representing the actual subprogram selected at the place of the
--     instantiation for A_Box_Default.  In the latter case, the
--     Enclosing_Element for such An_Expression is the instantiation.
--
--  Normalized lists contain artificial ASIS An_Association elements that
--  provide one formal A_Defining_Name => An_Expression pair per
--  association.  These artificial associations are Is_Normalized.
--  Artificial associations of default associations are
--  Is_Defaulted_Association.  Their component An_Expression elements are
--  not Is_Normalized and are not Is_Defaulted_Association.
--
--  If the argument is A_Pragma_Argument_Association, then this function may
--  return any expression to support implementation-defined pragmas.
--
--  Appropriate Association_Kinds:
--       A_Parameter_Association
--       A_Generic_Association
--       A_Pragma_Argument_Association
--
--  Returns Element_Kinds:
--       An_Expression
--  --|A2006 start
--       Not_An_Element
--  --|A2006 end
--
------------------------------------------------------------------------------
--  17.23 function Discriminant_Selector_Names
------------------------------------------------------------------------------

   function Discriminant_Selector_Names
     (Association : Asis.Discriminant_Association)
      return        Asis.Expression_List;

------------------------------------------------------------------------------
--  Association - Specifies the discriminant association to query
--
--  If the Association argument is from an unnormalized list:
--
--  - If the Association is a named discriminant_association:
--
--    Returns a list of the An_Identifier discriminant_selector_name elements
--    in order of appearance.
--
--    The Enclosing_Element of the names is the Association argument.
--
--  - If the Association is a positional discriminant_association:
--
--    Returns a Nil_Element_List.
--
--  If the Association argument is from a Normalized list:
--
--    Returns a list containing a single A_Defining_Name element representing
--    the defining_identifier of the discriminant_specification.
--
--    The Enclosing_Element of the A_Defining_Name is the
--    discriminant_specification.
--
--   Normalized lists contain artificial ASIS An_Association elements that
--   provide one formal A_Defining_Name => An_Expression pair per
--   association.  These artificial associations are Is_Normalized.  Their
--   component A_Defining_Name elements are not Is_Normalized.
--
--  Appropriate Association_Kinds:
--       A_Discriminant_Association
--
--  Returns Element_Kinds:
--       A_Defining_Name             -- Is_Normalized(Association)
--       An_Expression               -- not Is_Normalized(Association)
--            Returns Expression_Kinds:
--                 An_Identifier
--

------------------------------------------------------------------------------
--  17.24 function Discriminant_Expression
------------------------------------------------------------------------------

   function Discriminant_Expression
     (Association : Asis.Discriminant_Association)
      return        Asis.Expression;

------------------------------------------------------------------------------
--  Association - Specifies the discriminant_association to query
--
--  If the Association argument is from an unnormalized list:
--
--    Returns An_Expression representing the expression of the
--    discriminant_association.
--
--    The Enclosing_Element of An_Expression is the Association argument.
--
--  If the Association argument is from a Normalized list:
--
--  - If the Association is given explicitly:
--
--    Returns An_Expression representing the expression of the
--    discriminant_association.
--
--    The Enclosing_Element of An_Expression is the Association argument.
--
--  See SI99-0031-1/01:
--
--    In case if an actual parameter is from a Normalized association, but it
--    represents the name or the expression that is explicitely given in the
--    source, the Enclosing_Element of An_Expression is not the arhument
--    Association (that is normalized), but the corresponding non-normalized
--    An_Association Element
--
--  - If the Association is given by default:

--  --|D2005 start
--  But how this could be possible for a discriminant constraint??? The Ada
--  Standard explicitly says (3.7.1 (8)): "A discriminant_constraint shall
--  provide exactly one value for each discriminant of the subtype being
--  constrained."
--  --|D2005 end

--
--    Returns An_Expression representing:
--
--    - the corresponding default_expression of the Is_Normalized
--      A_Discriminant_Association.
--
--    - The Enclosing_Element of the An_Expression element is the
--      discriminant_specification that contains the default_expression.
--
--   Normalized lists contain artificial ASIS An_Association elements that
--   provide one formal A_Defining_Name => An_Expression pair per
--   association.  These artificial associations are Is_Normalized.
--   Artificial associations of default associations are
--   Is_Defaulted_Association.  Their component An_Expression elements are
--   not Is_Normalized and are not Is_Defaulted_Association.
--
--   Appropriate Association_Kinds:
--       A_Discriminant_Association
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.25 function Is_Normalized
------------------------------------------------------------------------------

   function Is_Normalized (Association : Asis.Association) return Boolean;

------------------------------------------------------------------------------
--  Association - Specifies the association to query
--
--  Returns True if the association is a normalized, artificially created
--  association returned by the queries Discriminant_Associations,
--  Generic_Actual_Part, Call_Statement_Parameters,
--  Record_Component_Associations, or Function_Call_Parameters where
--  Normalized => True (or the operation returns Is_Normalized associations
--  even if Normalized => False).  See the Implementation Permissions for
--  these queries.
--
--  Returns False for any unexpected Element.
--

--  Expected Association_Kinds:
--       A_Discriminant_Association
--       A_Record_Component_Association
--       A_Parameter_Association
--       A_Generic_Association
--
------------------------------------------------------------------------------
--  17.26 function Is_Defaulted_Association
------------------------------------------------------------------------------

   function Is_Defaulted_Association
     (Association : Asis.Association)
      return        Boolean;

------------------------------------------------------------------------------
--   Association - Specifies the association to query
--
--  Returns True if the association is a normalized, artificially created
--  association returned by the queries Discriminant_Associations,
--  Generic_Actual_Part, Record_Component_Associations,
--  Call_Statement_Parameters, or Function_Call_Parameters where
--  Normalized => True (or the operation returns default associations even if
--  Normalized => False) and the association contains a default expression.
--  A default expression is one that is implicitly supplied by the language
--  semantics and that was not explicitly supplied (typed) by the user.
--
--  Returns False for any unexpected Element.
--
--  Expected Association_Kinds:
--       A_Parameter_Association
--       A_Generic_Association
--  --|A2005 start
--       A_Record_Component_Association
--  --|A2005 end
--
--  --|AN Note: Always returns False for discriminant associations.  Defaulted
--  --|AN discriminant associations occur only when the discriminant constraint
--  --|AN is completely missing from a subtype indication.  Consequently, it is
--  --|AN not possible to obtain a (normalized) discriminant constraint list
--  --|AN for such subtype indications. Always returns False for component
--  --|AN associations. Aggregates cannot have defaulted components.
--  |D2005 start
--  Is it true in Ada 2005?
--  |D2005 end
--
------------------------------------------------------------------------------
--  17.27 function Expression_Parenthesized
------------------------------------------------------------------------------

   function Expression_Parenthesized
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the parenthesized expression to query
--
--  Returns the expression within the parenthesis.  This operation unwinds only
--  one set of parenthesis at a time, so the result may itself be
--  A_Parenthesized_Expression.
--
--  A_Parenthesized_Expression kind corresponds only to the (expression)
--  alternative in the syntax notion of primary in Reference Manual 4.4.  For
--  example, an expression of a type_conversion is A_Parenthesized_Expression
--  only if it is similar to the form subtype_mark((expression)) where it has
--  at least one set of its own parenthesis.
--
--  Appropriate Expression_Kinds:
--       A_Parenthesized_Expression
--
--  Returns Element_Kinds:
--       An_Expression
--

------------------------------------------------------------------------------
--  17.28 function Is_Prefix_Call
------------------------------------------------------------------------------

   function Is_Prefix_Call (Expression : Asis.Expression) return Boolean;

------------------------------------------------------------------------------
--  Expression  - Specifies the function call expression to query
--
--  Returns True if the function call is in prefix form.
--
--  Returns False for any unexpected Element.
--
--  For example, - Foo (A, B);   -- Returns TRUE
--                 "<" (A, B);   -- Returns TRUE
--                 ... A < B ... -- Returns FALSE
--
--  Expected Expression_Kinds:
--       A_Function_Call
--
------------------------------------------------------------------------------
--  17.29 function Corresponding_Called_Function
------------------------------------------------------------------------------

   function Corresponding_Called_Function
     (Expression : Asis.Expression)
      return       Asis.Declaration;

------------------------------------------------------------------------------
--  Expression  - Specifies the function_call to query
--
--  Returns the declaration of the called function.
--
--  Returns a Nil_Element if the:
--
--   - function_prefix denotes a predefined operator for which the
--     implementation does not provide an artificial function declaration,
--
--   - prefix of the call denotes an access to a function implicit or explicit
--     dereference,
--
--   - argument is a dispatching call.
--
--  If function_prefix denotes an attribute_reference, and if the corresponding
--  attribute is (re)defined by an attribute definition clause, an
--  implementation is encouraged, but not required, to return the definition
--  of the corresponding subprogram whose name is used after "use" in this
--  attribute definition clause. If an implementation cannot return such a
--  subprogram definition, a Nil_Element should be returned. For an attribute
--  reference which is not (re)defined by an attribute definition clause, a
--  Nil_Element should be returned.
--
--  Appropriate Expression_Kinds:
--       A_Function_Call
--
--  Returns Declaration_Kinds:
--       Not_A_Declaration
--       A_Function_Declaration
--       A_Function_Body_Declaration
--       A_Function_Body_Stub
--       A_Function_Renaming_Declaration
--       A_Function_Instantiation
--       A_Formal_Function_Declaration
--       A_Generic_Function_Declaration
--
--  |D2005 start
--
--  In case if an actual for a formal function is an enumeration literal,
--  this query should return An_Enumeration_Literal_Specification when applied
--  to the corresponding function call in generic expanded code. Consider:

--     generic
--        type T is private;
--        with function F return T;
--     procedure P;

--     procedure P is
--        Var : T;
--        ...
--     begin
--        Var := F;
--        ...
--     end P;

--     type Enum is (A, B, C);
--     procedure Q is new P (T => Enum, F => A);
--
--  If we traverse the expanded body for Q, what we have for "Var := F"?
--  Is the left part of this assignment statement (in the instantiation!) a
--  function call? If it is, then Corresponding_Called_Function may be applied
--  to it, and if it is applied, it has to return the definition of the
--  enumeration literal A!
--
--  Another problem is the need for Corresponding_Ancestor_Function query, see
--  the discussion for Asis.Statements.Corresponding_Called_Entity
--
--  |D2005 end
--
--  --|IP Implementation Permissions:
--  --|IP
--  --|IP An implementation may choose to return any part of multi-part
--  --|IP declarations and definitions. Multi-part declaration/definitions
--  --|IP can occur for:
--  --|IP
--  --|IP    - Subprogram specification in package specification, package body,
--  --|IP      and subunits (is separate);
--  --|IP    - Entries in package specification, package body, and subunits
--  --|IP      (is separate);
--  --|IP    - Private type and full type declarations;
--  --|IP    - Incomplete type and full type declarations; and
--  --|IP    - Deferred constant and full constant declarations.
--  --|IP
--  --|IP No guarantee is made that the element will be the first part or
--  --|IP that the determination will be made due to any visibility rules.
--  --|IP An application should make its own analysis for each case based
--  --|IP on which part is returned.
--  --|IP
--  --|IP An implementation can choose whether or not to construct and provide
--  --|IP artificial implicit declarations for predefined operators.
--
------------------------------------------------------------------------------
--  17.30 function Function_Call_Parameters
------------------------------------------------------------------------------

   function Function_Call_Parameters
     (Expression : Asis.Expression;
      Normalized : Boolean := False)
      return       Asis.Association_List;

------------------------------------------------------------------------------
--  Expression  - Specifies the function call expression to query
--  Normalized  - Specifies whether the normalized form is desired
--
--  Returns a list of parameter_association elements of the call.
--
--  Returns a Nil_Element_List if there are no parameter_association elements.
--
--  An unnormalized list contains only explicit associations ordered as they
--  appear in the program text.  Each unnormalized association has an optional
--  formal_parameter_selector_name and an explicit_actual_parameter component.
--
--  A normalized list contains artificial associations representing all
--  explicit and default associations.  It has a length equal to the number of
--  parameter_specification elements of the formal_part of the
--  parameter_and_result_profile.  The order of normalized associations matches
--  the order of parameter_specification elements.
--
--  Each normalized association represents a one on one mapping of a
--  parameter_specification elements to the explicit or default expression.
--  A normalized association has one A_Defining_Name component that denotes the
--  parameter_specification, and one An_Expression component that is either the
--  explicit_actual_parameter or a default_expression.
--
--  If the prefix of the call denotes an access to a function implicit or
--  explicit deference, normalized associations are constructed on the basis
--  of the formal_part of the parameter_and_result_profile from the
--  corresponding access_to_subprogram definition.

--  Returns Nil_Element for normalized associations in the case where
--  the called function can be determined only dynamically (dispatching
--  calls). ASIS cannot produce any meaningful result in this case.

--  --|D2005 start
--  The cases when the called functioncan be determined only dynamically also
--  include calls to the functions pointed by access values
--  --|D2005 end

--  The exception ASIS_Inappropriate_Element is raised when the function
--  call is an attribute reference and Is_Normalized is True.
--
--  Note that the ASIS implementation for GNAT does not support implicitly
--  declared predefined operators, and if the argument call is a call to such
--  an operator and Normalized is set to True, the result list is
--  Nil_Element_List.
--
--  Appropriate Expression_Kinds:
--       A_Function_Call
--
--  Returns Element_Kinds:
--       A_Parameter_Association
--
--  --|IR Implementation Requirements:
--  --|IR
--  --|IR Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
--  --|IR Normalized associations provided by default are
--  --|AN Is_Defaulted_Association. Normalized associations are never Is_Equal
--  --|AN to unnormalized associations.
--  --|IR
--  --|IP Implementation Permissions:
--  --|IP
--  --|IP An implementation may choose to always include default parameters in
--  --|IP its internal representation.
--  --|IP
--  --|IP An implementation may also choose to normalize its representation
--  --|IP to use defining_identifier elements rather than
--  --|IP formal_parameter_selector_name elements.
--  --|IP
--  --|IP In either case, this query will return Is_Normalized associations
--  --|IP even if Normalized is False, and the query
--  --|IP Function_Call_Parameters_Normalized will return True.
--
------------------------------------------------------------------------------
--  17.31 function Short_Circuit_Operation_Left_Expression
------------------------------------------------------------------------------

   function Short_Circuit_Operation_Left_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the short circuit operation to query
--
--  Returns the expression preceding the reserved words "and then" or "or else"
--  in the short circuit expression.
--
--  Appropriate Expression_Kinds:
--       An_And_Then_Short_Circuit
--       An_Or_Else_Short_Circuit
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.32 function Short_Circuit_Operation_Right_Expression
------------------------------------------------------------------------------

   function Short_Circuit_Operation_Right_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the short circuit operation to query
--
--  Returns the expression following the reserved words "or else" or "and then"
--  in the short circuit expression.
--
--  Appropriate Expression_Kinds:
--       An_And_Then_Short_Circuit
--       An_Or_Else_Short_Circuit
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.33 function Membership_Test_Expression
------------------------------------------------------------------------------

   function Membership_Test_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the membership test operation to query
--
--  Returns the expression on the left hand side of the membership test.
--
--  Appropriate Expression_Kinds:
--       An_In_Range_Membership_Test
--       A_Not_In_Range_Membership_Test
--       An_In_Type_Membership_Test
--       A_Not_In_Type_Membership_Test
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.34 function Membership_Test_Range
------------------------------------------------------------------------------

   function Membership_Test_Range
     (Expression : Asis.Expression)
      return       Asis.Range_Constraint;

   pragma Obsolescent
     (Membership_Test_Range,
      "use Membership_Test_Choices instead");

------------------------------------------------------------------------------
--  Expression  - Specifies the membership test operation to query
--
--  Returns the range following the reserved words "in" or "not in" from the
--  membership test.
--
--  This query is kept for applications that are supposed to process Ada code
--  that corresponds to the Ada Language Reference manual up to the version
--  known as Ada 2005. It can process arguments that represent membership tests
--  that:
--
--  * have exactly one choice in membership_choice_list
--
--  * this choice is of A_Range_Attribute_Reference or
--    A_Simple_Expression_Range Constraint_Kind
--
--  Any argument that does not satisfy these conditions raises
--  ASIS_Inappropriate_Element
--
--  Appropriate Expression_Kinds:
--      An_In_Membership_Test
--      A_Not_In_Membership_Test
--
--  Returns Constraint_Kinds:
--       A_Range_Attribute_Reference
--       A_Simple_Expression_Range
--
------------------------------------------------------------------------------
--  17.35 function Membership_Test_Subtype_Mark
------------------------------------------------------------------------------

   function Membership_Test_Subtype_Mark
     (Expression : Asis.Expression)
      return       Asis.Expression;

   pragma Obsolescent
     (Membership_Test_Subtype_Mark,
      "use Membership_Test_Choices instead");

------------------------------------------------------------------------------
--  Expression  - Specifies the membership test operation to query
--
--  Returns the subtype_mark expression following the reserved words "in" or
--  "not in" from the membership test.
--
--  This query is kept for applications that are supposed to process Ada code
--  that corresponds to the Ada Language Reference manual up to the version
--  known as Ada 2005. It can process arguments that represent membership tests
--  that:
--
--  * have exactly one choice in membership_choice_list
--
--  * this choice is of An_Identifier, A_Selected_Component or
--    An_Attribute_Reference Expression_Kind, and represents a subtype_mark
--
--  Any argument that does not satisfy these conditions raises
--  ASIS_Inappropriate_Element
--
--  Appropriate Expression_Kinds:
--      An_In_Membership_Test
--      A_Not_In_Membership_Test
--
--  Returns Expression_Kinds:
--       An_Identifier
--       A_Selected_Component
--       An_Attribute_Reference
--
--  --|A2012 start
------------------------------------------------------------------------------
--  17.??? function Membership_Test_Choices
------------------------------------------------------------------------------

   function Membership_Test_Choices
     (Expression : Asis.Expression)
      return Asis.Element_List;

--  Expression specifies a membership test to query.
--
--  Returns a list of choices from the membership test
--
--  Appropriate Expression_Kinds:
--      An_In_Membership_Test
--      A_Not_In_Membership_Test
--
--   Returns Element_Kinds:
--      An_Expression
--      A_Definition
--
--   Returns Expression_Kinds:
--      All except An_In_Membership_Test and A_Not_In_Membership_Test
--
--   Returns Definition_Kinds:
--      A_Constraint
--
--   Returns Constraint_Kinds:
--      A_Range_Attribute_Reference
--      A_Simple_Expression_Range

--  --|A2012 end
--
------------------------------------------------------------------------------
--  17.36 function Converted_Or_Qualified_Subtype_Mark
------------------------------------------------------------------------------

   function Converted_Or_Qualified_Subtype_Mark
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the type conversion or qualified expression to
--                query.
--
--  Returns the subtype_mark expression that converts or qualifies the
--  expression.
--
--  Appropriate Expression_Kinds:
--       A_Type_Conversion
--       A_Qualified_Expression
--
--  Returns Expression_Kinds:
--       An_Identifier
--       A_Selected_Component
--       An_Attribute_Reference
--
--
------------------------------------------------------------------------------
--  17.37 function Converted_Or_Qualified_Expression
------------------------------------------------------------------------------

   function Converted_Or_Qualified_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the type conversion or qualified expression to
--                query
--
--  Returns the expression being converted or qualified.
--
--  Appropriate Expression_Kinds:
--       A_Type_Conversion
--       A_Qualified_Expression
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  17.38 function Allocator_Subtype_Indication
------------------------------------------------------------------------------

   function Allocator_Subtype_Indication
     (Expression : Asis.Expression)
      return       Asis.Subtype_Indication;

------------------------------------------------------------------------------
--  Expression  - Specifies the allocator expression to query
--
--  Returns the subtype indication for the object being allocated.
--
--  Appropriate Expression_Kinds:
--       An_Allocation_From_Subtype
--
--  Returns Definition_Kinds:
--       A_Subtype_Indication
--
------------------------------------------------------------------------------
--  17.39 function Allocator_Qualified_Expression
------------------------------------------------------------------------------

   function Allocator_Qualified_Expression
     (Expression : Asis.Expression)
      return       Asis.Expression;

------------------------------------------------------------------------------
--  Expression  - Specifies the allocator expression to query
--
--  Returns the qualified expression for the object being allocated.
--
--  Appropriate Expression_Kinds:
--       An_Allocation_From_Qualified_Expression
--
--  Returns Expression_Kinds:
--       A_Qualified_Expression
--

------------------------------------------------------------------------------
--  Processing of the Ada extensions that most likely will be included in   --
--  Ada 2015 and that are already implemented in GNAT                       --
------------------------------------------------------------------------------

-----------------------------
-- Conditional Expressions --
-----------------------------

   function Expression_Paths
     (Expression : Asis.Expression)
      return       Asis.Element_List;
   --  Returns a list of the paths of the conditional expression, in their
   --  order of appearance.
   --
   --  Appropriate Expression_Kinds:
   --       A_Conditional_Expression
   --
   --  Returns Element_Kinds:
   --       An_Expression_Path

--   function Path_Condition_Expression
--     (Path : Asis.Element)
--      return Asis.Expression;
   --  ??? Name of the query is not perfect, but we already have
   --  ??? Asis.Statements.Condition_Expression for statements paths.
   --  Returns the condition expression for an IF path or an ELSIF path.
   --
   --  Appropriate Expression_Path_Kinds:
   --       An_If_Expression_Path
   --       An_Elsif_Expression_Path
   --
   --  Returns Element_Kinds:
   --       An_Expression

   function Dependent_Expression
     (Path : Asis.Path)
      return Asis.Expression;
   --  Returns the expression for conditional expression path.
   --
   --  Appropriate Element_Kinds:
   --       An_Expression_Path
   --
   --  Returns Element_Kinds:
   --       An_Expression

----------------------------
-- Quantified Expressions --
----------------------------

   function Iterator_Specification
     (Expression : Asis.Expression)
      return       Asis.Declaration;
   --  Returns a loop parameter specification or an iterator specification from
   --  the expression.
   --
   --  Appropriate Expression_Kinds:
   --     A_Quantified_Expression
   --
   --  Returns Declaration_Kinds:
   --     A_Loop_Parameter_Specification
   --     A_Generalized_Iterator_Specification
   --     An_Element_Iterator_Specification

   function Predicate
     (Expression : Asis.Expression)
      return      Asis.Expression;
   --  Returns a predicate expression from the argument expression.
   --
   --  Appropriate Expression_Kinds:
   --     A_Quantified_Expression
   --
   --  Returns Element_Kinds:
   --     An_Expression

---------------------------
--  Subpool in allocator --
---------------------------

   function Subpool_Name
     (Expression : Asis.Expression)
      return      Asis.Expression;
   --  Returns the subpool specification name from the argument allocator.
   --  Returns Nil_Element if the allocator does not contain a subpool
   --  specification.
   --
   --  Appropriate Expression_Kinds:
   --     An_Allocation_From_Subtype
   --     An_Allocation_From_Qualified_Expression
   --
   --  Returns Element_Kinds:
   --     An_Expression

end Asis.Expressions;
