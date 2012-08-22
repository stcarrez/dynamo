------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A 4 G . S P A N _ E N D                           --
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

with Asis.Declarations; use  Asis.Declarations;
with Asis.Definitions;  use  Asis.Definitions;
with Asis.Elements;     use Asis.Elements;
with Asis.Expressions;  use Asis.Expressions;
with Asis.Extensions;   use Asis.Extensions;

with Asis.Set_Get;      use Asis.Set_Get;

with A4G.A_Debug;       use A4G.A_Debug;
with A4G.A_Sinput;      use A4G.A_Sinput;
with A4G.Span_Beginning;
with A4G.A_Types;       use A4G.A_Types;
with A4G.Int_Knds;      use A4G.Int_Knds;
with A4G.Skip_TB;       use A4G.Skip_TB;

with Atree;             use Atree;
with Nlists;            use Nlists;
with Output;            use Output;
with Sinfo;             use Sinfo;

package body A4G.Span_End is

   --  !!!??? This file is '-gnatg-compilable', but both its content and its
   --  !!!???  documentation need revising

   ------------------------
   -- Main Look-Up Table --
   ------------------------

   --  Set_Image_End function is implemented as look-up table indexed by
   --  Internal_Element_Kinds and switching onto the specific functions
   --  which find the end of an Element of a given kind. This look-up table
   --  is implemented as one-dimension array of references to specific search
   --  functions. To save SLOCs, we define these specific functions without
   --  giving their separate specifications before (the only exception is
   --  made for Nonterminal_Component implementing the recursive search in
   --  case of composite element), and we initialize the array
   --  implementing the look-up table in its declaration

   -------------------------------
   -- Specific Search Functions --
   -------------------------------

   function Nonterminal_Component (E : Asis.Element)  return Source_Ptr;
   --  This function is used when Element to process has components.
   --  It goes down to the right-most *terminal* component, and it unwinds
   --  all the "syntax sugar" (such as '... end;' ')' etc) from all the
   --  right-most component being traversed. In the very end it returns
   --  the pointer in the Source Buffer set on the very last component of
   --  the Element it was originally called. This function may cal
   --  Set_Image_End which in turn call it again, and this makes the
   --  recursive processing of an Element if its right-most component
   --  itself has components.

   function Word_Or_Character_End (E : Asis.Element) return Source_Ptr;
   --  In some specific cases finding an end of identifier require special
   --  processing. It happens with identifiers representing attribute
   --  designators or labels. This function is used to find the end of
   --  such an identifier

   function Plus4 (E : Asis.Element) return Source_Ptr;
   function Plus3 (E : Asis.Element) return Source_Ptr;
   function Plus2 (E : Asis.Element) return Source_Ptr;
   --  Jumping N position to the right

   function No_Search (E : Asis.Element) return Source_Ptr;
   --  Skips the syntax sugar after the end of its argument

   function Access_To_Procedure_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of access_to_procedure

   function Access_To_Protected_Procedure_End
     (E : Asis.Element)
      return Source_Ptr;
   --  Looks for the end of a protected procedure

   function Second_Word_After_Last_Component_End
     (E : Asis.Element)
      return Source_Ptr;
   --  Used to skip syntax sugar like "end record"

   function Second_Word_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of a second word after E

   function Formal_Numeric_Type_Definition_End
     (E : Asis.Element)
      return Source_Ptr;
   --  Looks for the end of a formal numeric type declaration other than a
   --  formal decimal fixed point type definition

   function Formal_Decimal_Fixed_Point_Definition_End
     (E : Asis.Element)
      return Source_Ptr;
   --  Looks for the end of a formal decimal fixed point type definition

   function Numeric_Literal_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of a numeric literal

   function Character_Literal_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of a (defining) character literal, We can not just use
   --  Plus2 for a character literal because of encodings

   function Private_Type_Definition_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of a private type definition

   function String_Literal_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of a string literal

   function Operator_Symbol_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end of an operator symbol

   function Unknown_Discriminant_Part_End
     (E    : Asis.Element)
      return Source_Ptr;
   --  Looks for the end of an unknown discriminant part

   function A_Bug (E : Asis.Element) return Source_Ptr;
   --  Placeholder for "others" choice, should never be called. Raises
   --  Internal_Implementation_Error

--  --|A2005 start
   --  Search functions added for new Ada 2005 features

   function Interface_Definition_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end for an interface definition

   function Component_Association_End (E : Asis.Element) return Source_Ptr;
   --  Looks for the end for an component association, the problem here is that
   --  the association can contain a box

   function Formal_Discrete_Type_Definition_End
     (E    : Asis.Element)
      return Source_Ptr;
   --  Computes the end of the (<>) construct, taking into account possible
   --  spaces between <> and brackets

   function Function_Call_End (E : Asis.Element) return Source_Ptr;
   --  Computes the end of the function call, making the difference between
   --  traditional and prefixed notation of the function name

--  --|A2005 end

--  --|A2005 start

   ---------------------------
   -- Character_Literal_End --
   ---------------------------

   function Character_Literal_End (E : Asis.Element) return Source_Ptr is
      Result : Source_Ptr := Get_Location (E) + 2;
      --  '+ 2' is needed to process correctly ''' !
   begin

      while Get_Character (Result) /= ''' loop
         Result := Result + 1;
      end loop;

      return Result;
   end Character_Literal_End;

   -------------------------------
   -- Component_Association_End --
   -------------------------------

   function Component_Association_End (E : Asis.Element) return Source_Ptr is
      S : Source_Ptr := Get_Location (E);
   begin

      if not Is_Nil (Component_Expression (E)) then
         return Nonterminal_Component (E);
      else
         --  Here we have a component association of the form
         --  something => <>
         --  S points to "=>"

         S := S + 2;
         S := Search_Next_Word (S);
         S := S + 1;

         return S;
      end if;

   end Component_Association_End;

   -----------------------------------------
   -- Formal_Discrete_Type_Definition_End --
   -----------------------------------------

   function Formal_Discrete_Type_Definition_End
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return Search_Rightmost_Symbol (S, ')');
   end Formal_Discrete_Type_Definition_End;

   -----------------------
   -- Function_Call_End --
   -----------------------

   function Function_Call_End (E : Asis.Element) return Source_Ptr is
      S        : Source_Ptr := Get_Location (E);
   begin

      if Is_Prefix_Notation (E) then

         if Function_Call_Parameters (E)'Length > 1 then
            S := Nonterminal_Component (E);
         else
            S := Set_Image_End (Prefix (E));
            --  S := Skip_Trailing_Brackets (E, S);
         end if;

      else
         S := Nonterminal_Component (E);
      end if;

      return S;
   end Function_Call_End;

--  --|A2005 end

   ---------------------------
   -- Word_Or_Character_End --
   ---------------------------

   function Word_Or_Character_End (E : Asis.Element) return Source_Ptr is
      S : Source_Ptr := Get_Location (E);
   begin
      if Nkind (Node (E)) = N_Attribute_Definition_Clause then
         --  this is a case of the attribute designator in a
         --  pseudo-attribute-reference from an attribute definition clause
         --  Note, that we can have more than one attribute here (see FA30-016)
         --  e.g.
         --        for Root'Class'Output use Class_Output;

         S := A4G.Span_Beginning.Search_Identifier_Beginning (E);
      end if;

      if Special_Case (E) = Dummy_Class_Attribute_Designator then
         S := Search_Rightmost_Symbol (S, ''');
         S := Next_Identifier (S);
      end if;

      if Get_Character (S) = ''' then
         --  Two cases are possible: character literal or An_Identifier Element
         --  representing an attribute designator and based on
         --  N_Attribute_Reference node

         if Nkind (Node (E)) = N_Defining_Character_Literal
           or else
            Nkind (Node (E)) = N_Character_Literal
         then
            S := S + 2;

            while Get_Character (S) /= ''' loop
               --  Wide_ or Wide_Wide_Character literal
               S := S + 1;
            end loop;

            return S;
         end if;

      elsif Get_Character (S) = '<' and then Get_Character (S + 1) = '<' then
         S := S + 1;
         --  this is a label, and now S points to the second character in "<<"
      end if;

      --  here we should find the end of an identifier

      --  but in case of a label or an attribute designator S points
      --  to ''' or to the second '<' in "<<", and we have to take into
      --  account such crazy cases as spaces  or/end comments between
      --  ''' or "<<" and the identifier itself
      if Get_Character (S) = '<' or else Get_Character (S) = ''' then
         S := Next_Identifier (S);
      end if;
      --  and now it's safe to do this:
      return Search_End_Of_Word (S);
   end Word_Or_Character_End;

   -----------
   -- PlusN --
   -----------

   --  Only jumping N position to the right:
   function Plus4 (E : Asis.Element) return Source_Ptr is
   begin
      return Get_Location (E) + 4;
   end Plus4;

   function Plus3 (E : Asis.Element) return Source_Ptr is
   begin
      return Get_Location (E) + 3;
   end Plus3;

   function Plus2 (E : Asis.Element) return Source_Ptr is
   begin
      return Get_Location (E) + 2;
   end Plus2;

   ---------------
   -- No_Search --
   ---------------

   function No_Search (E : Asis.Element) return Source_Ptr is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return Skip_Trailing_Brackets (E, S);
   end No_Search;

   -----------------------------
   -- Access_To_Procedure_End --
   -----------------------------

   function Access_To_Procedure_End (E : Asis.Element) return Source_Ptr is
   --  We have to consider an access to procedure as a special case, because
   --  if there is no formal parameter, we have to skip two words
   --  (access procedure) and no syntax sugar
         Last_Comp : Asis.Element;
         S         : Source_Ptr;
   begin
      Last_Comp := Get_Last_Component (E);
      if Is_Nil (Last_Comp) then
         S := Get_Location (E);
         S := Search_End_Of_Word (S);
         S := Search_Next_Word (S);
         S := Search_End_Of_Word (S);
      else
         S := Set_Image_End (Last_Comp);
         S := Skip_Trailing_Brackets (E, S);
      end if;
      return S;
   end Access_To_Procedure_End;

   ---------------------------------------
   -- Access_To_Protected_Procedure_End --
   ---------------------------------------

   function Access_To_Protected_Procedure_End
     (E : Asis.Element)
      return Source_Ptr
   is
   --  Similar to Access_To_Procedure_End, but here we have to skip three
   --  words ('access protected procedure') in case if no parameters
      Last_Comp : Asis.Element;
      S         : Source_Ptr;
   begin
      Last_Comp := Get_Last_Component (E);
      if Is_Nil (Last_Comp) then
         S := Get_Location (E);
         S := Search_End_Of_Word (S);
         S := Search_Next_Word (S);
         S := Search_End_Of_Word (S);
         S := Search_Next_Word (S);
         S := Search_End_Of_Word (S);
      else
         S := Set_Image_End (Last_Comp);
         S := Skip_Trailing_Brackets (E, S);
      end if;
      return S;
   end Access_To_Protected_Procedure_End;

   ------------------------------------------
   -- Second_Word_After_Last_Component_End --
   ------------------------------------------

   function Second_Word_After_Last_Component_End
     (E : Asis.Element)
      return Source_Ptr
   is
   --  We need this special case to skip "end record"
      Last_Comp : Asis.Element;
      S         : Source_Ptr;
   begin
      Last_Comp := Get_Last_Component (E);
      S         := Set_Image_End (Last_Comp);

      S := Search_Next_Word (S);
      S := Search_End_Of_Word (S);
      S := Search_Next_Word (S);
      S := Search_End_Of_Word (S);

      return S;

   end Second_Word_After_Last_Component_End;

   ---------------------
   -- Second_Word_End --
   ---------------------

   function Second_Word_End (E : Asis.Element) return Source_Ptr is
      S : Source_Ptr := Get_Location (E);
   begin
      S := Search_End_Of_Word (S);
      S := Search_Next_Word (S);
      S := Search_End_Of_Word (S);
      return S;
   end Second_Word_End;

   ----------------------------------------
   -- Formal_Numeric_Type_Definition_End --
   ----------------------------------------

   function Formal_Numeric_Type_Definition_End
     (E : Asis.Element)
      return Source_Ptr
   is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return Search_Rightmost_Symbol (S, '>');
   end Formal_Numeric_Type_Definition_End;

   -----------------------------------------------
   -- Formal_Decimal_Fixed_Point_Definition_End --
   -----------------------------------------------

   function Formal_Decimal_Fixed_Point_Definition_End
     (E : Asis.Element)
      return Source_Ptr
   is
      S : Source_Ptr := Get_Location (E);
   begin
      --  delta <>
      S := Search_Rightmost_Symbol (S, '>');
      --  digits <>
      S := S + 1;
      S := Search_Rightmost_Symbol (S, '>');
      return Search_Rightmost_Symbol (S, '>');
   end Formal_Decimal_Fixed_Point_Definition_End;

--  --|A2005 start
   ------------------------------
   -- Interface_Definition_End --
   ------------------------------

   function Interface_Definition_End (E : Asis.Element) return Source_Ptr is
      S        : Source_Ptr;
      Last_Int : Asis.Element := Get_Last_Component (E);
   begin

      if Is_Nil (Last_Int) then
         S := Get_Location (E);
         S := Search_Prev_Word (S);
      else

         if Int_Kind (Last_Int) = A_Selected_Component then
            Last_Int := Get_Last_Component (Last_Int);
         end if;

         S        := Get_Location (Last_Int);
         S        := Search_End_Of_Word (S);
      end if;

      return S;
   end Interface_Definition_End;
--  --|A2005 end

   -------------------------
   -- Numeric_Literal_End --
   -------------------------

   function Numeric_Literal_End (E : Asis.Element) return Source_Ptr is
   begin
      return Get_Num_Literal_End (Get_Location (E));
   end Numeric_Literal_End;

   ---------------------------------
   -- Private_Type_Definition_End --
   ---------------------------------

   function Private_Type_Definition_End (E : Asis.Element) return Source_Ptr is
      N : constant Node_Id := Node (E);
      S : Source_Ptr := Get_Location (E);
   begin
      --  if the enclosing type declaration contains a discriminant part, we
      --  should skip it first

      if Nkind (N) = N_Private_Type_Declaration and then
         Present (Discriminant_Specifications (N))
      then
         declare
            Discr_Part : constant Asis.Element_List :=
              Discriminants (Discriminant_Part (Enclosing_Element (E)));
            Tmp : Asis.Element := Discr_Part (Discr_Part'Last);
         begin
            Tmp := Get_Last_Component (Tmp);
            S   := A4G.Span_End.Set_Image_End (Tmp);
         end;

         S := Search_Rightmost_Symbol (S, ')');
      end if;

      S := Search_Rightmost_Symbol (S, ';');
      S := Search_Prev_Word (S);
      return S;
   end Private_Type_Definition_End;

   ------------------------
   -- String_Literal_End --
   ------------------------

   function String_Literal_End (E : Asis.Element) return Source_Ptr is
   begin
      return Get_String_End (Get_Location (E));
   end String_Literal_End;

   -------------------------
   -- Operator_Symbol_End --
   -------------------------

   function Operator_Symbol_End (E : Asis.Element) return Source_Ptr is
   --  the problem is that an operator symbol may (in a prefix call) or
   --  may not (in an infix call) contain string quoters
      function Operator_Len
        (Op_Kind : Internal_Operator_Symbol_Kinds)
         return Source_Ptr;
      --  just returns the length of the string representing a given
      --  operator symbol

      function Operator_Len
        (Op_Kind : Internal_Operator_Symbol_Kinds)
         return Source_Ptr
      is
      begin
         case Op_Kind is
            when An_And_Operator |
                 An_Xor_Operator |
                 A_Mod_Operator  |
                 A_Rem_Operator  |
                 An_Abs_Operator |
                 A_Not_Operator   =>
               return 3;
            when An_Or_Operator                   |
                 A_Not_Equal_Operator             |
                 A_Less_Than_Or_Equal_Operator    |
                 A_Greater_Than_Or_Equal_Operator |
                 An_Exponentiate_Operator          =>
               return 2;
            when An_Equal_Operator       |
                 A_Less_Than_Operator    |
                 A_Greater_Than_Operator |
                 A_Plus_Operator         |
                 A_Minus_Operator        |
                 A_Concatenate_Operator  |
                 A_Unary_Plus_Operator   |
                 A_Unary_Minus_Operator  |
                 A_Multiply_Operator     |
                 A_Divide_Operator         =>
               return 1;
         end case;
      end Operator_Len;

      S       : Source_Ptr                      := Get_Location (E);
      Op_Kind : constant Internal_Element_Kinds := Int_Kind (E);
   begin
      if Get_Character (S) = '"' or else
         Get_Character (S) = '%'
      then
         S := S + 2;
      end if;
      return  (S + Operator_Len (Op_Kind) - 1);
   end Operator_Symbol_End;

   -----------
   -- A_Bug --
   -----------

   function A_Bug (E : Asis.Element) return Source_Ptr is
   --  This function should never be called. We need it for "others"
   --  choice in the aggregate initializing the look-up table for those
   --  values of Internal_Element_Kinds which should never be processed
   --  (they correspond to implicit root and universal types). This
   --  function raises Internal_Implementation_Error;
   begin
      pragma Unreferenced (E);

      raise Internal_Implementation_Error;
      return No_Location;
   end A_Bug;

   -----------------------------------
   -- Unknown_Discriminant_Part_End --
   -----------------------------------

   function Unknown_Discriminant_Part_End
     (E    : Asis.Element)
      return Source_Ptr
   is
      S : constant Source_Ptr := Get_Location (E);
   begin
      return Search_Rightmost_Symbol (S, ')');
   end Unknown_Discriminant_Part_End;

   ------------------
   -- Search Array --
   ------------------

   type Set_Source_Location_Type is access
      function (E : Asis.Element) return Source_Ptr;

   Search_Switch : constant array (Internal_Element_Kinds)
                                       of Set_Source_Location_Type :=
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
      An_Unknown_Pragma                      => Nonterminal_Component'Access,

      A_Defining_Identifier                  => Word_Or_Character_End'Access,
      A_Defining_Character_Literal           => Character_Literal_End'Access,
      A_Defining_Enumeration_Literal         => Word_Or_Character_End'Access,
      A_Defining_And_Operator                => Plus4'Access,
      A_Defining_Or_Operator                 => Plus3'Access,
      A_Defining_Xor_Operator                => Plus4'Access,
      A_Defining_Equal_Operator              => Plus2'Access,
      A_Defining_Not_Equal_Operator          => Plus3'Access,
      A_Defining_Less_Than_Operator          => Plus2'Access,
      A_Defining_Less_Than_Or_Equal_Operator => Plus3'Access,
      A_Defining_Greater_Than_Operator       => Plus2'Access,
      A_Defining_Greater_Than_Or_Equal_Operator => Plus3'Access,
      A_Defining_Plus_Operator               => Plus2'Access,
      A_Defining_Minus_Operator              => Plus2'Access,
      A_Defining_Concatenate_Operator        => Plus2'Access,
      A_Defining_Unary_Plus_Operator         => Plus2'Access,
      A_Defining_Unary_Minus_Operator        => Plus2'Access,
      A_Defining_Multiply_Operator           => Plus2'Access,
      A_Defining_Divide_Operator             => Plus2'Access,
      A_Defining_Mod_Operator                => Plus4'Access,
      A_Defining_Rem_Operator                => Plus4'Access,
      A_Defining_Exponentiate_Operator       => Plus3'Access,
      A_Defining_Abs_Operator                => Plus4'Access,
      A_Defining_Not_Operator                => Plus4'Access,

      A_Defining_Expanded_Name ..
      --  An_Ordinary_Type_Declaration
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
      A_Real_Number_Declaration            => Nonterminal_Component'Access,

      An_Enumeration_Literal_Specification => Word_Or_Character_End'Access,

      A_Discriminant_Specification ..
      --  A_Component_Declaration
      --  A_Loop_Parameter_Specification
      --  A_Generalized_Iterator_Specification
      --  An_Element_Iterator_Specification
      --  A_Procedure_Declaration
      --  A_Function_Declaration
      --  A_Parameter_Specification
      --  A_Procedure_Body_Declaration
      --  A_Function_Body_Declaration
      --  A_Package_Declaration
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
      --  An_Entry_Body_Declaration
      --  An_Entry_Index_Specification
      --  A_Procedure_Body_Stub
      A_Function_Body_Stub             => Nonterminal_Component'Access,

      A_Package_Body_Stub ..
      --  A_Task_Body_Stub
      --  A_Protected_Body_Stub
      An_Exception_Declaration         => No_Search'Access,

      A_Choice_Parameter_Specification => Word_Or_Character_End'Access,

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
      --  A_Formal_Package_Declaration_With_Box
      --  A_Derived_Type_Definition
      --  A_Derived_Record_Extension_Definition
      --  An_Enumeration_Type_Definition
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
      --  A_Constrained_Array_Definition
      --  A_Record_Type_Definition
      A_Tagged_Record_Type_Definition => Nonterminal_Component'Access,

--  --|A2005 start

      An_Ordinary_Interface ..
      --  A_Limited_Interface,
      --  A_Task_Interface,
      --  A_Protected_Interface,
      A_Synchronized_Interface => Interface_Definition_End'Access,
--  --|A2005 end

      A_Pool_Specific_Access_To_Variable ..
      --  An_Access_To_Variable
      An_Access_To_Constant            => Nonterminal_Component'Access,

      An_Access_To_Procedure           => Access_To_Procedure_End'Access,
      An_Access_To_Protected_Procedure =>
         Access_To_Protected_Procedure_End'Access,

      An_Access_To_Function ..
      --  An_Access_To_Protected_Function
      --  A_Subtype_Indication
      --  A_Range_Attribute_Reference
      --  A_Simple_Expression_Range
      --  A_Digits_Constraint
      --  A_Delta_Constraint
      --  An_Index_Constraint
      --  A_Discriminant_Constraint
      --  A_Component_Definition
      --  A_Discrete_Subtype_Indication_As_Subtype_Definition
      --  A_Discrete_Range_Attribute_Reference_As_Subtype_Definition
      --  A_Discrete_Simple_Expression_Range_As_Subtype_Definition
      --  A_Discrete_Subtype_Indication
      --  A_Discrete_Range_Attribute_Reference
      A_Discrete_Simple_Expression_Range      => Nonterminal_Component'Access,

      An_Unknown_Discriminant_Part    => Unknown_Discriminant_Part_End'Access,
      A_Known_Discriminant_Part               => Nonterminal_Component'Access,

      A_Record_Definition                     =>
         Second_Word_After_Last_Component_End'Access,

      A_Null_Record_Definition                => Second_Word_End'Access,
      A_Null_Component                        => No_Search'Access,
      A_Variant_Part                          => Nonterminal_Component'Access,
      A_Variant                               => Nonterminal_Component'Access,
      An_Others_Choice                        => Word_Or_Character_End'Access,

--  --|A2005 start
      An_Anonymous_Access_To_Variable ..
      An_Anonymous_Access_To_Constant => Nonterminal_Component'Access,

      An_Anonymous_Access_To_Procedure ..
      An_Anonymous_Access_To_Protected_Procedure =>
        Access_To_Procedure_End'Access,

      An_Anonymous_Access_To_Function ..
      An_Anonymous_Access_To_Protected_Function =>
         Nonterminal_Component'Access,
--  --|A2005 end

      A_Private_Type_Definition               =>
         Private_Type_Definition_End'Access,

      A_Tagged_Private_Type_Definition        =>
         Private_Type_Definition_End'Access,

      A_Private_Extension_Definition          =>
         Second_Word_After_Last_Component_End'Access,

      A_Task_Definition                       => Nonterminal_Component'Access,
      A_Protected_Definition                  => Nonterminal_Component'Access,
      A_Formal_Private_Type_Definition        => Word_Or_Character_End'Access,
      A_Formal_Tagged_Private_Type_Definition => Word_Or_Character_End'Access,
      A_Formal_Derived_Type_Definition        => Nonterminal_Component'Access,
      A_Formal_Discrete_Type_Definition       =>
         Formal_Discrete_Type_Definition_End'Access,

      A_Formal_Signed_Integer_Type_Definition ..
      --  A_Formal_Modular_Type_Definition
      --  A_Formal_Floating_Point_Definition
      A_Formal_Ordinary_Fixed_Point_Definition =>
         Formal_Numeric_Type_Definition_End'Access,

      A_Formal_Decimal_Fixed_Point_Definition  =>
         Formal_Decimal_Fixed_Point_Definition_End'Access,

--  --|A2005 start
      A_Formal_Ordinary_Interface ..
      --  A_Formal_Limited_Interface
      --  A_Formal_Task_Interface
      --  A_Formal_Protected_Interface
      A_Formal_Synchronized_Interface => Interface_Definition_End'Access,
--  --|A2005 end

      A_Formal_Unconstrained_Array_Definition ..
      --  A_Formal_Constrained_Array_Definition
      --  A_Formal_Pool_Specific_Access_To_Variable
      --  A_Formal_Access_To_Variable
      A_Formal_Access_To_Constant            => Nonterminal_Component'Access,

      A_Formal_Access_To_Procedure           => Access_To_Procedure_End'Access,

      A_Formal_Access_To_Protected_Procedure =>
         Access_To_Protected_Procedure_End'Access,

      A_Formal_Access_To_Function            => Nonterminal_Component'Access,
      A_Formal_Access_To_Protected_Function  => Nonterminal_Component'Access,
      An_Aspect_Specification                => Nonterminal_Component'Access,
      An_Integer_Literal                     => Numeric_Literal_End'Access,
      A_Real_Literal                         => Numeric_Literal_End'Access,
      A_String_Literal                       => String_Literal_End'Access,
      An_Identifier                          => Word_Or_Character_End'Access,

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
      A_Not_Operator          => Operator_Symbol_End'Access,

      A_Character_Literal     => Character_Literal_End'Access,
      An_Enumeration_Literal  => Word_Or_Character_End'Access,
      An_Explicit_Dereference => Plus2'Access,
      A_Function_Call         => Function_Call_End'Access,

      An_Indexed_Component ..
      --  A_Slice
      A_Selected_Component    => Nonterminal_Component'Access,

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
      An_Unknown_Attribute           => Nonterminal_Component'Access,

      A_Record_Aggregate ..
      --  An_Extension_Aggregate
      --  A_Positional_Array_Aggregate
      --  A_Named_Array_Aggregate
      --  An_And_Then_Short_Circuit
      --  An_Or_Else_Short_Circuit
      --  An_In_Range_Membership_Test
      --  A_Not_In_Range_Membership_Test
      --  An_In_Type_Membership_Test
      --  A_Not_In_Type_Membership_Test
      --  An_In_Membership_Test
      A_Not_In_Membership_Test  => Nonterminal_Component'Access,

      A_Null_Literal                 => Plus3'Access,

      A_Parenthesized_Expression ..
      --  A_Type_Conversion
      --  A_Qualified_Expression
      --  An_Allocation_From_Subtype
      --  An_Allocation_From_Qualified_Expression
      --  A_Conditional_Expression  --  Ada 2015
      --  A_Pragma_Argument_Association
      A_Discriminant_Association => Nonterminal_Component'Access,

--  --|A2005 start
      A_Record_Component_Association ..
      An_Array_Component_Association => Component_Association_End'Access,
--  --|A2005 end

      A_Parameter_Association ..
      --  A_Generic_Association
      --  A_Null_Statement
      --      We can treat this statement as a Nonterminal_Component.
      --     Get_Last_Comp function will return Nil_Element.
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
      --      We can treat this statement as a Nonterminal_Component.
      --      Get_Last_Comp function will return Nil_Element.
      --  A_Requeue_Statement_With_Abort
      --  A_Delay_Until_Statement
      --  A_Delay_Relative_Statement
      --  A_Terminate_Alternative_Statement
      --      We can treat this statement as a Nonterminal_Component.
      --      Get_Last_Comp function will return Nil_Element.
      --  A_Selective_Accept_Statement
      --  A_Timed_Entry_Call_Statement
      --  A_Conditional_Entry_Call_Statement
      --  An_Asynchronous_Select_Statement
      --  An_Abort_Statement
      --  A_Raise_Statement
      --  A_Code_Statement
      --  An_If_Path
      --  An_Elsif_Path
      --  An_Else_Path
      --  A_Case_Path
      --  A_Select_Path
      --  An_Or_Path
      --  A_Then_Abort_Path
      --  A_Case_Expression_Path    --  Ada 2012
      --  An_If_Expression_Path     --  Ada 2012
      --  An_Elsif_Expression_Path  --  Ada 2012
      --  An_Else_Expression_Path   --  Ada 2012
      --  A_Use_Package_Clause
      --  A_Use_Type_Clause
      --  A_Use_All_Type_Clause     --  Ada 2012
      --  A_With_Clause
      --  An_Attribute_Definition_Clause
      --  An_Enumeration_Representation_Clause
      --  A_Record_Representation_Clause
      --  An_At_Clause
      --  A_Component_Clause
      An_Exception_Handler => Nonterminal_Component'Access,

      others => A_Bug'Access);

   ---------------------------
   -- Nonterminal_Component --
   ---------------------------

   function Nonterminal_Component (E : Asis.Element)  return Source_Ptr is
      Last_Comp : Asis.Element;
      Image_End : Source_Ptr;
   begin
      if Debug_Flag_X then
         Write_Str ("Nonterminal_Component - called for ");
         Write_Str (Internal_Element_Kinds'Image (Int_Kind (E)));
         Write_Eol;
      end if;

      Last_Comp := Get_Last_Component (E);

      if not Is_Nil (Last_Comp) then
         Image_End := Set_Image_End (Last_Comp);
      else
         Image_End := Get_Location (E);
      end if;

      Image_End := Skip_Trailing_Brackets (E, Image_End);

      return Image_End;
   end Nonterminal_Component;

   -------------------
   -- Set_Image_End --
   -------------------

   function Set_Image_End (E : Asis.Element) return Source_Ptr is
   begin
      --  all that this function does is switching to the function
      --  implementing the specific processing for the given element
      --  kind

      if Debug_Flag_X then
         Write_Str ("  Set_Image_End - called for ");
         Write_Str (Internal_Element_Kinds'Image (Int_Kind (E)));
         Write_Eol;
         Write_Eol;
      end if;

      return Search_Switch (Int_Kind (E)) (E);
   end Set_Image_End;

end A4G.Span_End;
