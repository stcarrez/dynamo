------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A S I S . S E T _ G E T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
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

with Ada.Calendar;       use Ada.Calendar;

with Asis.Extensions;    use Asis.Extensions;

package Asis.Set_Get is

   pragma Elaborate_Body (Asis.Set_Get);

--  This package contains the interface routines for setting and getting
--  the values of the internal components of the main ASIS abstractions -
--  Context, Compilation_Unit and Element. All the operations for getting
--  components are defined as functions, but the operations for obtaining
--  the tree nodes from an Element value may change the tree being accessed,
--  which should be considered as the side effect.

--  THE DOCUMENTATION IS INCOMPLETE!!!!!

   ---------------------------------------
   -- String -> Program_Text conversion --
   ---------------------------------------

   function To_Program_Text (S : String) return Program_Text;
   --  This function takes the string which is a part of the source code
   --  representation and converts it into ASIS Program_Text type. It tries
   --  to recognize the wide character encoding and to convert it back into
   --  Wide_Character

   -------------
   -- CONTEXT --
   -------------

   -------------------------------------
   -- Id <-> ASIS Context conversions --
   -------------------------------------

   function Get_Cont_Id (C  : Context)    return Context_Id;
   function Get_Cont    (Id : Context_Id) return Context;

   procedure Set_Cont   (C  : out Context; Id : Context_Id);
   --  Assigns the value of Id to the Id fields of a given variable C
   --  of the Asis Context type

   pragma Inline (Get_Cont_Id);
   pragma Inline (Get_Cont);

   --------------------------------
   -- Getting Context Attributes --
   --------------------------------

   function Valid (C : Context) return Boolean;
   --  checks if its argument is an opened (=valid) Context
   --  DO WE REALLY NEED THIS FUNCTION??

----------------------
-- COMPILATION_UNIT --
----------------------

   ----------------------------------------------
   -- Id <-> ASIS Compilation Unit conversions --
   ----------------------------------------------

   function Get_Unit_Id   (C_U : Compilation_Unit) return Unit_Id;

   function Get_Comp_Unit
     (U  : Unit_Id;
      C  : Context_Id)
      return Compilation_Unit;
   --  this function creates the new value of the Compilation_Unit
   --  type; note, that it also sets the field Obtained as equal to
   --  the current OS time

   function Get_Comp_Unit_List
     (U_List : Unit_Id_List;
      C      : Context_Id)
      return Compilation_Unit_List;
   --  Creates the ASIS Compilation Unit List from the list of unit Ids

   pragma Inline (Get_Unit_Id);
   pragma Inline (Get_Comp_Unit);

   -----------------------------------------
   -- Getting Compilation Unit Attributes --
   -----------------------------------------

   function Not_Nil (C_U : Compilation_Unit) return Boolean;
   --  Check if C_U /= Nil_Compilation_Unit (the name "Exists" is already
   --  busy in ASIS

   function Nil (C_U : Compilation_Unit) return Boolean;
   --  Check if C_U = Nil_Compilation_Unit

   function Is_Standard (C_U : Compilation_Unit) return Boolean;
   --  Check if C_U represents the predefined package Standard

   function Kind             (C_U : Compilation_Unit) return Asis.Unit_Kinds;
   function Class            (C_U : Compilation_Unit) return Unit_Classes;
   function Origin           (C_U : Compilation_Unit) return Unit_Origins;
   function Is_Main_Unit     (C_U : Compilation_Unit) return Boolean;
   function Top              (C_U : Compilation_Unit) return Node_Id;
   function Is_Body_Required (C_U : Compilation_Unit) return Boolean;
   function Unit_Name        (C_U : Compilation_Unit) return String;
   function Encl_Cont        (C_U : Compilation_Unit) return Context;
   function Encl_Cont_Id     (C_U : Compilation_Unit) return Context_Id;
   function Source_File      (C_U : Compilation_Unit) return String;
   function Ref_File         (C_U : Compilation_Unit) return String;
   function Context_Info     (C_U : Compilation_Unit) return String;
   function Time_Stamp       (C_U : Compilation_Unit) return Time;
   function Source_Status    (C_U : Compilation_Unit)
      return Source_File_Statuses;
   function Main_Tree        (C_U : Compilation_Unit) return Tree_Id;

   -------------------
   -- Miscellaneous --
   -------------------

   function "=" (Left, Right : Compilation_Unit) return Boolean;
   --  This function "re-implements" the equivalent-to-predefined
   --  compare operation for Compilation_Unit. It should never be used in
   --  any ASIS application code.

   function Valid (C_U : Compilation_Unit) return Boolean;
   --  checks, if the argument is valid, that is, if its enclosing
   --  Context is opened

   procedure Reset_Main_Tree (C_U : Compilation_Unit);
   --  If C_U is a main unit in some tree, this procedure resets
   --  this tree, otherwise it does nothing. This procedure does not
   --  reset the context, it should be done by a caller.

   function Get_Configuration_CU
     (C_U :  Compilation_Unit)
      return Compilation_Unit;
   --  Returns the ASIS COmpilation unit which represents
   --  A_Configuration_Compilation in the enclosing context of C_U

   pragma Inline (Not_Nil);
   pragma Inline (Nil);
   pragma Inline (Is_Standard);
   pragma Inline (Kind);
   pragma Inline (Class);
   pragma Inline (Origin);
   pragma Inline (Is_Main_Unit);
   pragma Inline (Top);
   pragma Inline (Is_Body_Required);
   pragma Inline (Unit_Name);
   pragma Inline (Encl_Cont);
   pragma Inline (Encl_Cont_Id);
   pragma Inline (Valid);

   --  THIS "INLINE" LIST IS INCOMPLETE!!!

-------------
-- ELEMENT --
-------------

   function "=" (Left, Right : Element) return Boolean;
   --  This function "re-implements" the equivalent-to-predefined
   --  compare operation for Elements. It should never be used in
   --  any ASIS application code.

   ---------
   -- Get --
   ---------

   function Node               (E : Element) return Node_Id;
   function R_Node             (E : Element) return Node_Id;
   function Node_Field_1       (E : Element) return Node_Id;
   function Node_Field_2       (E : Element) return Node_Id;
   function Node_Value         (E : Element) return Node_Id;
   function R_Node_Value       (E : Element) return Node_Id;
   function Node_Field_1_Value (E : Element) return Node_Id;
   function Node_Field_2_Value (E : Element) return Node_Id;
   --  Node, R_Node and Node_Field_1 reset the tree when returning
   --  the node value in a way that the returned node will be the
   --  proper node value for the tree being accessed by ASIS,
   --  whereas Node_Value, R_Node_Value and Node_Field_1_Value
   --  just return the node value without changing the currently
   --  accessed tree

   function Encl_Unit          (E : Element) return Compilation_Unit;
   function Encl_Unit_Id       (E : Element) return Unit_Id;
   function Encl_Cont          (E : Element) return Context;
   function Encl_Cont_Id       (E : Element) return Context_Id;
   function Kind               (E : Element) return Asis.Element_Kinds;
   function Int_Kind           (E : Element) return Internal_Element_Kinds;
   function Is_From_Implicit   (E : Element) return Boolean;
   function Is_From_Inherited  (E : Element) return Boolean;
   function Is_From_Instance   (E : Element) return Boolean;
   function Special_Case       (E : Element) return Special_Cases;
   function Normalization_Case (E : Element) return Normalization_Cases;
   function Parenth_Count      (E : Element) return Nat;
   function Encl_Tree          (E : Element) return Tree_Id;
   function Rel_Sloc           (E : Element) return Source_Ptr;
   function Character_Code     (E : Element) return Char_Code;
   function Obtained           (E : Element) return ASIS_OS_Time;

   function Location      (E : Asis.Element) return Source_Ptr;
   --  this function returns not relative (as Rel_Sloc does), but
   --  "absolute" location of the source position corresponding
   --  to the Node on which E is based. This function is
   --  "tree-swapping-safe"

   function Valid             (E : Element) return Boolean;
   --  checks, if the argument is valid, that is, if the enclosing
   --  Context of its enclosing Unit is opened

   pragma Inline (Node);
   pragma Inline (R_Node);
   pragma Inline (Encl_Unit);
   pragma Inline (Encl_Unit_Id);
   pragma Inline (Encl_Cont);
   pragma Inline (Encl_Cont_Id);
   pragma Inline (Kind);
   pragma Inline (Int_Kind);
   pragma Inline (Is_From_Implicit);
   pragma Inline (Is_From_Inherited);
   pragma Inline (Is_From_Instance);
   pragma Inline (Special_Case);
   pragma Inline (Encl_Tree);
   pragma Inline (Rel_Sloc);
   pragma Inline (Valid);

   ---------
   -- Set --
   ---------

   procedure Set_Node               (E : in out Element; N : Node_Id);
   procedure Set_R_Node             (E : in out Element; N : Node_Id);
   procedure Set_Node_Field_1       (E : in out Element; N : Node_Id);
   procedure Set_Node_Field_2       (E : in out Element; N : Node_Id);
   procedure Set_Encl_Unit_Id       (E : in out Element; U : Unit_Id);
   procedure Set_Enclosing_Context  (E : in out Element; C : Context_Id);
   procedure Set_Obtained           (E : in out Element; T : ASIS_OS_Time);
   procedure Set_Int_Kind           (E : in out Element;
                                                 K :  Internal_Element_Kinds);
   procedure Set_From_Implicit      (E : in out Element; I : Boolean := True);
   procedure Set_From_Inherited     (E : in out Element; I : Boolean := True);
   procedure Set_From_Instance      (E : in out Element; I : Boolean := True);
   procedure Set_Special_Case       (E : in out Element; S : Special_Cases);
   procedure Set_Rel_Sloc           (E : in out Element; S : Source_Ptr);
   procedure Set_Character_Code     (E : in out Element; C : Char_Code);
   procedure Set_Encl_Tree          (E : in out Element; T : Tree_Id);
   procedure Set_Normalization_Case (E : in out Element;
                                                 N : Normalization_Cases);
   procedure Set_Parenth_Count      (E : in out Element; Val : Nat);

   function Set_Element
     (Node           : Node_Id;
      R_Node         : Node_Id;
      Node_Field_1   : Node_Id;
      Node_Field_2   : Node_Id;
      Encl_Unit      : Compilation_Unit;
      --  contains Ids for both Enclosing Compilation Unit and Enclosing
      --  Context
      Int_Kind       : Internal_Element_Kinds;
      Implicit       : Boolean;
      Inherited      : Boolean;
      Instance       : Boolean;
      Spec_Case      : Special_Cases;
      Norm_Case      : Normalization_Cases;
      Par_Count      : Nat;
      Character_Code : Char_Code)
      return Element;
   --  Constructs and returns the ASIS Element value on the base of
   --  Element attributes
   --  Note, that it should not be any parameter passed for the
   --  Enclosing_Tree field, because this field should be set equal
   --  to the Id of the tree being currently accessed!
   --  Note also, that it should not be any parameter passed for the
   --  Rel_Scr field, because this field should be computed as the
   --  difference between the source location of the node upon
   --  the given element is to be built (that is, passed as the
   --  actual for the Node parameter, and the top node of the
   --  Element's enclosing Unit.
   --
   --  It is supposed, that this function is called as the part of the
   --  constructing of the new element during processing some ASIS
   --  query, so the actuals for Node, R_Node and the current setting of
   --  the top node for the Unit pointed by Encl_Unit are consistent.
   --  See also A4G.Mapping (body).

   function Set_In_List
     (EL           : Element_List;
      Node_Field_1 : Node_Id := Empty;
      Implicit     : Boolean := False;
      Inherited    : Boolean := False)
      return         Element_List;
   --  For all the Elements in EL sets their fields Node_Field_1,
   --  Is_Part_Of_Implicit, Is_Part_Of_Inherited to the values passed as
   --  actuals accordingly for Node_Field_1, Implicit and Inherited.

   procedure Convert_To_Limited_View (El : in out Asis.Element);
   --  Provided that A4G.A_Sem.Belongs_To_Limited_View (El), changes its
   --  internal representation in such a way that El represents the limited
   --  view of the corresponding type or package. This includes changing of the
   --  Element kind for type elements.

   Char_Literal_Spec_Template : Element;
   --  Used as a template for creating lists of
   --  An_Enumeration_Literal_Specification representing defining
   --  character literals of types Standard.Character and
   --  Standard.Wide_Character. This variable is initialized in package body

   Numeric_Error_Template : Element;
   --  Used as a template for the artificial elements representing the obsolete
   --  renaming of Numeric_Error exception in package Standard and components
   --  thereof.

   -----------------------------------------------------------
   -- Special processing for Elements representing root and --
   -- universal numeric types in ASIS                       --
   -----------------------------------------------------------

   function Set_Root_Type_Declaration
     (Int_Kind : Internal_Element_Kinds;
      Cont     : Context_Id)
      return Element;
   --  Constructs and returns the ASIS Element representing the declaration
   --  of a root or universal numeric type. If an actual for Int_Kind does
   --  not belong to Internal_Root_Type_Kinds, Nil_Element is returned.
   --  Otherwise the child element of the result returned by the
   --  Type_Declaration_View function should be of Int_Kind kind.
   --  Every opened Context contains exactly one Element representing
   --  the declaration of a given root or universal numeric type.
   --  These elements (as well as their child elements) have no Node to
   --  be based upon (they simply do not need such a Node), they are
   --  implicit declarations located in the predefined Standard package.

   function Is_Root_Num_Type (Declaration : Asis.Declaration) return Boolean;
   --  Checks if Declaration is A_Type_Declaration Element representing
   --   the declaration of a root or universal numeric type.

   function Root_Type_Definition
     (Declaration : Asis.Declaration)
      return Asis.Definition;
   --  Transforms A_Type_Declaration Element representing the declaration
   --  of a root or universal numeric type into the corresponding type
   --  definition (being of Root_Type_Kinds). This function does not
   --  check if its argument really represents the declaration of a root
   --  or universal numeric type

end Asis.Set_Get;
