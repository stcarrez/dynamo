------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ T Y P E S                           --
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
with Ada.Characters.Latin_1;  use  Ada.Characters.Latin_1;
with Ada.Characters.Handling; use  Ada.Characters.Handling;

with GNAT.OS_Lib;             use  GNAT.OS_Lib;

package A4G.A_Types is

   pragma Elaborate_Body (A4G.A_Types);

--  This package is the ASIS implementation's analog of the GNAT Types
--  package (except the part related to the ASIS_OS_Time type).
--  It contains host independent type and constant definitions
--  which is supposed to be used in more than one unit in the ASIS
--  implementation.

   ------------------
   -- ASIS_OS_Time --
   ------------------

   --  To check, that a given abstraction is valid in the sense defined by the
   --  ASIS standard (that is, that the enclosing Context of the given
   --  abstraction has not been closed after creating this abstraction), ASIS
   --  needs some kind of logical time (or logical time stamp). This logical
   --  time is increased each time when any ASIS Context is opened. It is not
   --  reset when ASIS is initialized, because it may lead to collisions in
   --  validity checks

   --  An ASIS abstraction is valid if its logical time stamp is equal or
   --  greater than the time stamp of its enclosing Context.

   type ASIS_OS_Time is private;

   Nil_ASIS_OS_Time  : constant ASIS_OS_Time;
   Last_ASIS_OS_Time : constant ASIS_OS_Time;

   procedure Increase_ASIS_OS_Time;
   --  Increases the ASIS logical "clock"

   function A_OS_Time return ASIS_OS_Time;
   --  Gets the current value of the ASIS logical "clock"

   function Later (L, R : ASIS_OS_Time) return Boolean;
   --  Compares time stamps.

   -----------------------------------------
   -- Types for Context and Context Table --
   -----------------------------------------

   Inconsistent_Incremental_Context : exception;
   --  raised when any inconsistency found for Incremental Tree processing
   --  mode

   Context_Low_Bound  : constant :=         0;
   Context_High_Bound : constant := 1_000_000;

   type Context_Id is range Context_Low_Bound .. Context_High_Bound;
   --  Type used to identify entries in ASIS Context table

   Non_Associated   : constant Context_Id := Context_Low_Bound;
   Nil_Context_Id   : constant Context_Id := Context_Low_Bound;
   First_Context_Id : constant Context_Id := Context_Low_Bound + 1;

   ---------------------------------------------
   -- Types for Container and Container Table --
   ---------------------------------------------

   Container_Low_Bound  : constant :=   0;
   Container_High_Bound : constant := 100;

   type Container_Id is range Container_Low_Bound .. Container_High_Bound;
   --  Type used to identify entries in ASIS Container table

   Nil_Container_Id   : constant Container_Id := Container_Low_Bound;
   First_Container_Id : constant Container_Id := Container_Low_Bound + 1;

   -----------------------------------------------
   -- Types for Compilation_Unit and Unit Table --
   -----------------------------------------------

   Unit_Low_Bound  : constant :=       0;
   Unit_High_Bound : constant := 100_000;

   type Unit_Id is range Unit_Low_Bound .. Unit_High_Bound;
   --  Type used to identify entries in the ASIS Unit table

   Nil_Unit   : constant Unit_Id := Unit_Low_Bound;
   No_Unit_Id :          Unit_Id renames Nil_Unit;

   First_Unit_Id : constant Unit_Id := Unit_Low_Bound + 1;
   Standard_Id   : constant Unit_Id := First_Unit_Id;
   --  The entry in the Unit table corresponding to the package Standard
   --  Standard goes first in any Unit table

   Config_Comp_Id : constant Unit_Id := Standard_Id + 1;
   --  The entry in the Unit table corresponding to the artificial
   --  A_Configuration_Compilation unit. We may have at most one such unit.
   --  If there is no configuration pragmas in the Context, there is no harm
   --  to allocate such a unit, because the only way for an ASIS client to get
   --  it is to get the enclosing unit for a configuration pragma.

   type Unit_Id_List is array (Natural range <>) of Unit_Id;
   Nil_Unit_Id_List : constant Unit_Id_List (1 .. 0) := (others => Nil_Unit);

   --------------------------
   -- Types for Tree Table --
   --------------------------

   Tree_Low_Bound  : constant :=       0;
   Tree_High_Bound : constant := 100_000;

   type Tree_Id is range Tree_Low_Bound .. Tree_High_Bound;
   --  Type used to identify entries in ASIS Tree table

   Nil_Tree      : constant Tree_Id := Tree_Low_Bound;
   No_Tree_Name  : Tree_Id renames Nil_Tree; --  ???
   First_Tree_Id : constant Tree_Id := Tree_Low_Bound + 1;

   -----------------------------------------------
   -- Types for Search Directories Paths Tables --
   -----------------------------------------------

   No_Dir       : constant :=     0;
   First_Dir_Id : constant :=     1;
   Last_Dir_Id  : constant := 1_000;

   type Dir_Id is range No_Dir .. Last_Dir_Id;

   type Search_Dir_Kinds is (
      Source, --  for source search path
      Object, --  for object search path
      Tree);  --  for tree search path
   --  this type may be further expanded

   --------------------------------------------
   --  Types for Internal Element Structure  --
   --------------------------------------------

   type Special_Cases is (
   --  this enumeration type is needed to distinguish some special
   --  cases in Element constructing and handling
      Not_A_Special_Case,

      A_Dummy_Block_Statement,
      --  the result of an obsolescent function
      --  Declarations.Body_Block_Statement

      Predefined_Operation,
      --  indicates the predefined operation for a user-defined type
      --  (or component thereof???). Note, that such an operation is
      --  defined not in the Standard package.

      Explicit_From_Standard,
      --  indicates the explicit Element obtained  from the package
      --  Standard. "Explicit" means here any construct which is
      --  contained in the "source" text of Standard included in RM95
      --  plus explicit constants substituting "implementation-defined"
      --  italic strings in this "source"

      Numeric_Error_Renaming,
      --  Indicates the artificial ASIS Element created to represent the
      --  obsolete renaming of Numeric_Error in the package Standard
      --  (see B712-005)

      Implicit_From_Standard,
      --  indicates the implicit Element obtained from the package
      --  Standard, that is, implicitly declared predefined operations
      --  and their components, and root and universal numeric type
      --  definitions and declarations

      Stand_Char_Literal,
      --  indicates the defining character literal declared in the
      --  definition of the predefined type Standard.Character
      --  or Standard.Wide_Character. An ASIS Element representing such
      --  a literal has no corresponding node in the tree, and it is
      --  based on the N_Defining_Identifier node for the corresponding
      --  type

      Expanded_Package_Instantiation,
      --  indicates A_Package_Declaration element which represents the
      --  package declaration which is the result of an instantiation
      --  of a generic package

      Expanded_Subprogram_Instantiation,
      --  indicates A_Procedure_Declaration or A_Function_Declaration
      --  element which represents the package declaration which is the
      --  result of an instantiation of a generic package

      Configuration_File_Pragma,
      --  Indicates a configuration pragma belonging not to the source of some
      --  Ada compilation unit, but to the configuration file (an components
      --  thereof)

      Rewritten_Named_Number,
      --  Indicates An_Identifier Element representing a named number in the
      --  situation when the corresponding tree structure is rewritten into
      --  N_Integer/Real_Literal node and no original tree structure is
      --  available (see BB10-002)

      Is_From_Gen_Association,
      --  See D722-012.
      --  The problem here is that in case of a formal object, the front-end
      --  creates the renaming declaration as a means to pass an actual
      --  parameter, and the parameter itself (the corresponding tree node)
      --  is used as a part of this renaming declaration. So we have a problem
      --  with Enclosing_Element. The Parent pointer from this actual points
      --  to the renaming declaration structure. In case if we are not in the
      --  expanded code, we may compare levels of instantiation and it helps,
      --  but in general case it is too complicated. So the solution is to
      --  mark the corresponding node if it comes from the generic association
      --  (and we can gen into this node only by means of a structural query!)
      --  and to use this mark in the Enclosing_Element processing.

      Is_From_Imp_Neq_Declaration,
      --  Indicates if the given element is an implicit declaration of the
      --  "/=" operation corresponding to the explicit redefinition of "=" or
      --  a subcomponent thereof

--      Implicit_Inherited_Subprogram
      --  indicates the declaration of an implicit inherited user-defined
      --  subprogram or a component thereof.
      --  may be continued...

      Dummy_Base_Attribute_Designator,
      Dummy_Class_Attribute_Designator,
      Dummy_Base_Attribute_Prefix,
      Dummy_Class_Attribute_Prefix,
      --  These four values are used to mark components of the artificial
      --  'Base and 'Class attribute reference that ASIS has to simulate when
      --  processing references to a formal type in the instantiation in case
      --  when a formal type is an unconstrained type, and the actual type is a
      --  'Class attribute, or when an actual is a 'Base attribute and the
      --  front-end creates too much of artificial data structures in the tree.

      From_Limited_View
      --  The corresponding Element is (a part of) a package or type limited
      --  view, see RM 05 10.1.1 (12.1/2 .. 12.5.2)

      --  may be continued...

   );

   type Normalization_Cases is (
   --  This enumeration type represents the different possible states of
   --  An_Association Elements in respect to normalization of associations
      Is_Not_Normalized,
      Is_Normalized,
      --  normalized association created for an actual parameter which itself
      --  is presented at the place of the call/instantiation
      Is_Normalized_Defaulted,
      --  normalized association created for an actual parameter which itself
      --  is NOT presented at the place of the call/instantiation, so the
      --  default value should be used
      Is_Normalized_Defaulted_For_Box);
      --  normalized association created for an actual parameter which itself
      --  is NOT presented at the place of the instantiation and the definition
      --  of the formal parameter includes box as the default value, so the
      --  actual parameter should be found at the place of the instantiation

   subtype Expanded_Spec is Special_Cases
     range Expanded_Package_Instantiation .. Expanded_Subprogram_Instantiation;

   subtype Normalized_Association is Normalization_Cases
     range Is_Normalized .. Is_Normalized_Defaulted_For_Box;

   subtype Defaulted_Association is Normalization_Cases
     range Is_Normalized_Defaulted .. Is_Normalized_Defaulted_For_Box;

   subtype Predefined is Special_Cases
     range Predefined_Operation .. Stand_Char_Literal;

   --  COMMENTS
   --
   --  *1* Handling the Parenthesized Expressions and
   --      One_Pair_Of_Parentheses_Away and Two_Pairs_Of_Parentheses_Away
   --      Special Cases.
   --
   --      An Asis Element of A_Parenthesized_Expression could be built
   --      on the base of any tree node which could be used for building the
   --      elements of all other An_Expresion subordinate kinds.
   --      A_Parenthesized_Expression kind is determined by comparing (during
   --      the automatic Internal_Element_Kinds determination only!!!) the
   --      Paren_Count field of the node with zero - see Sinfo.ads, the
   --      documentation item for "4.4  (Primary)" RM subsection, and
   --      Atree.ads the documentation item related to the Paren_Count field.
   --
   --      When a subexpression is to be selected from the element of
   --      A_Parenthesized_Expression kind by the
   --      Asis_Definition.Expression_Parenthesized function, the result will
   --      be built on the base of just the same node as the argument having,
   --      just the same value of the Paren_Count field. If the argument has
   --      more than one pair of parentheses, the result will also be of

   --     A_Parenthesized_Expression kind, and the Special_Cases values
   --     One_Pair_Of_Parentheses_Away and Two_Pairs_Of_Parentheses_Away
   --     are intended to be used to count the pairs of parentheses remained
   --     in the result element. All the corresponding element kind
   --     determination and element construction should be performed in
   --     "by-hand" mode, except the case when the argument parenthesized
   --     expression has only one pair of parentheses.
   --
   --     GNAT cannot distinguish more than three levels of the enclosing
   --     pairs of parentheses for a non-parenthesized enclosed expression.
   --     (Paren_Count = 3 stands for any number of the enclosing parentheses
   --     equal or greater than 3.) So ASIS-for-GNAT implementation cannot
   --     do more than GNAT itself (of course, we could do some search in the
   --     source buffer, but we prefer to agree with GNAT team that even
   --     Paren_Count = 3 already is a pathological case :).
   --
   --     See also Asis_Definition.Expression_Parenthesized (body) and
   --     A4G.Mapping.Node_To_Element (body)
   --
   --  *2* Root/Universal types definitions - we do not need any special
   --      value for representing elements of Root_Type_Kinds, because for
   --      each value there may be only one Element of the corresponding kind
   --      in a given opened Context.
   --

   -------------------------
   -- Nil String constants--
   -------------------------

   Nil_Asis_String      : constant String      := "";
   Nil_Asis_Wide_String : constant Wide_String := "";

   -------------------------------------------------
   --  Constants for the Diagnosis string buffer  --
   -------------------------------------------------

   ASIS_Line_Terminator : constant String := (1 => LF);
   --  what about DOS-like end-of-line?

   Diagnosis_String_Length : constant Positive :=
     76 + ASIS_Line_Terminator'Length;
   --  We are trying to set ASIS_Line_Terminator in the Diagnosis string to
   --  keep text strings at most 76 characters long

   Max_Diagnosis_Length : constant Positive := 32 * Diagnosis_String_Length;
   --  The length of the buffer in which the Diagnosis string is formed,
   --  now it is at most 32 lines 76 character each. Should be enough for
   --  any practically meaningful diagnosis

   Asis_Wide_Line_Terminator : constant Wide_String :=
      (1 => To_Wide_Character (LF));
   --
   --  the physical line terminator, is used in the Diagnosis string
   --  to separate the parts of the diagnosis message
   --  See also documentation of the Skip_Line_Terminators procedure
   --  in the (GNAT.)sinput.adb

   ASIS_Line_Terminator_Len : constant Positive
                            := ASIS_Line_Terminator'Length;

   Incorrect_Setting : constant String := "Attempt to set Not_An_Error "
                                & "status with non-nil diagnosis string";

   Incorrect_Setting_Len : constant Positive := Incorrect_Setting'Length;

   -------------------
   -- Miscellaneous --
   -------------------

   ASIS_Path_Separator : Character;
   --  Is initialized in the package body. Takes into account that in VMS
   --  ',' should be used instead of GNAT.OS_Lib.Path_Separator.

   ASIS_Current_Directory : String_Access;
   --  Is initialized in the package body. "[]" in VMS, "." otherwise

   function Asis_Normalize_Pathname
     (Name           : String;
      Directory      : String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return String;
   --  ASIS version of GNAT.OS_Lib.Normalize_Pathname. It applies
   --  To_Host_Dir_Spec to the result of GNAT.OS_Lib.Normalize_Pathname.
   --  Should be applied to directory names only! For file names
   --  GNAT.OS_Lib.Normalize_Pathname should be used.
   --  ??? Is this the right place for this subprogram???

   Internal_Implementation_Error : exception;
   --  Means exactly this. Is supposed to be raised in control statement
   --  paths which should never be reached. We need this exception mostly
   --  because some parts of old ASIS code (developed at the research stage of
   --  the ASIS project) sometimes are not structured properly.

   function Parameter_String_To_List
     (Par_String : String)
      return       Argument_List_Access;
   --  Take a string that is a converted to the String type Parameters string
   --  of the ASIS query Initialize, Associate or Finalize (??? Should we
   --  process the original Wide_String Parameters string without converting
   --  it to String?) and parse it into an Argument_List.
   --
   --  This function is similar to GNAT.OS_Int.Argument_String_To_List, but
   --  it does not treat '\' as a backquoting character.

private

   type ASIS_OS_Time is new Long_Integer range 0 .. Long_Integer'Last;

   ASIS_Clock : ASIS_OS_Time := 1;
   --  This is the ASIS logical "clock" used to ret ASIS logical time.

   Nil_ASIS_OS_Time  : constant ASIS_OS_Time := 0;
   Last_ASIS_OS_Time : constant ASIS_OS_Time := ASIS_OS_Time'Last;

end A4G.A_Types;
