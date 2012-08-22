------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . Q U E R I E S                            --
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
-- The   original   version  of  this  component  has  been  developed   by --
-- Jean-Charles Marteau  (Jean-Charles.Marteau@ensimag.imag.fr)  and  Serge --
-- Reboul  (Serge.Reboul@ensimag.imag.fr),  ENSIMAG  High  School Graduates --
-- (Computer sciences) Grenoble, France in Sema Group Grenoble, France. Now --
-- this component is maintained by the ASIS team at AdaCore                 --
--                                                                          --
------------------------------------------------------------------------------

--------------------------------------------------
-- The structure of this package is very basic, --
-- it consists in massive imbricated cases      --
-- determining what element we're considering   --
-- and returning an array containing its        --
-- corresponding queries.                       --
--------------------------------------------------

with Asis.Clauses;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Extensions;
with Asis.Statements;

with A4G.A_Types; use A4G.A_Types;

package body A4G.Queries is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Subprogram_Call_Needs_Reordering
     (El   : Asis.Element)
      return Boolean;
   --  Checks if for a subprogram call element the sequence of its components
   --  obtained as
   --
   --     'Name_Of_Called_Subprogram' -> 'Parameter_Associations'
   --
   --  should be reordered because of one of the following reasons:
   --
   --  1. For 'A + B' the right order of traversing is
   --
   --        'A' -> '+' -> 'B'
   --
   --  2. For Obj.Operation (Par1, Par2) where Obj is the dispatching
   --     operand for subprogram Operation the right order of traversing is
   --
   --        'Obj' -> 'Fun' -> 'Par1' ->'Par2'

   function First_Parameter_Association
     (Call : Asis.Element)
      return Asis.Element;
   --  Returns the first parameter association from the Call that is supposed
   --  to be either A_Function_Call or A_Procedure_Call_Statement Element, and
   --  the corresponding call has at least one parameter association.

   function All_But_First_Associations
     (Call : Asis.Element)
      return Asis.Element_List;
   --  Returns the parameter association list from the Call that contains all
   --  but first associations. The Call that is supposed to be either
   --  A_Function_Call or A_Procedure_Call_Statement Element, and the
   --  corresponding call has at least one parameter association. If Call
   --  contains exactly one parameter association, the result is
   --  Nil_Element_List

   --  Subprograms declared below implement first-depth-level parsing of
   --  Elements of specific kinds - they return a list of queries needed to
   --  get all the first-depth-level components of their argument in
   --  from-left-to-right order

   function PARSE_Defining_Name
     (Ada_Defining_Name : Asis.Element)
      return              Query_Array;

   function PARSE_Association
     (Ada_Association : Asis.Element)
      return            Query_Array;

   function PARSE_Clause
     (Ada_Clause : Asis.Element)
      return       Query_Array;

   function PARSE_Expression
     (Ada_Expression : Asis.Element)
      return           Query_Array;

   function PARSE_Path (Ada_Path : Asis.Element) return Query_Array;

   function PARSE_Definition
     (Ada_Definition : Asis.Element)
      return           Query_Array;

   function PARSE_Declaration
     (Ada_Declaration : Asis.Element)
      return            Query_Array;

   function PARSE_Statement
     (Ada_Statement : Asis.Element)
      return          Query_Array;

   --------------------------------
   -- All_But_First_Associations --
   --------------------------------

   function All_But_First_Associations
     (Call : Asis.Element)
      return Asis.Element_List
   is
      Result : constant Asis.Element_List :=
        Asis.Extensions.Get_Call_Parameters (Call);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end All_But_First_Associations;

   -------------------------
   -- Appropriate_Queries --
   -------------------------

   function Appropriate_Queries (Element : Asis.Element) return Query_Array is
   begin
      case Asis.Elements.Element_Kind (Element) is
         when Not_An_Element =>
            return No_Query;

         when A_Pragma =>

            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Elements.Pragma_Argument_Associations'Access));

         when A_Defining_Name =>

            return PARSE_Defining_Name (Element);

         when A_Declaration =>

            return PARSE_Declaration (Element);

         when A_Definition =>

            return PARSE_Definition (Element);

         when An_Expression =>

            return PARSE_Expression (Element);

         when An_Association =>

            return PARSE_Association (Element);

         when A_Statement =>

            return PARSE_Statement (Element);

         when A_Path =>

            return PARSE_Path (Element);

         when A_Clause =>

            return PARSE_Clause (Element);

         when An_Exception_Handler =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Statements.Choice_Parameter_Specification'Access),

               2 => (Element_List_Query,
                     Asis.Statements.Exception_Choices'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Handler_Statements'Access, True));
      end case;
   end Appropriate_Queries;

   ---------------------------------
   -- First_Parameter_Association --
   ---------------------------------

   function First_Parameter_Association
     (Call : Asis.Element)
      return Asis.Element
   is
      Result : constant Asis.Element_List :=
        Asis.Extensions.Get_Call_Parameters (Call);
   begin
      return Result (Result'First);
   end First_Parameter_Association;

   -----------------------
   -- PARSE_Association --
   -----------------------

   function PARSE_Association
     (Ada_Association : Asis.Element)
      return            Query_Array
   is
   begin
      case Asis.Elements.Association_Kind (Ada_Association) is
         when Not_An_Association =>
            raise Internal_Implementation_Error;

         when A_Discriminant_Association =>
            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Expressions.Discriminant_Selector_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Discriminant_Expression'Access));

         when A_Record_Component_Association =>
            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Expressions.Record_Component_Choices'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Component_Expression'Access));

         when An_Array_Component_Association =>
            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Expressions.Array_Component_Choices'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Component_Expression'Access));

         when A_Parameter_Association       |
              A_Pragma_Argument_Association |
              A_Generic_Association =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Expressions.Formal_Parameter'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Actual_Parameter'Access));

      end case;
   end PARSE_Association;

   ------------------
   -- PARSE_Clause --
   ------------------

   function PARSE_Clause
     (Ada_Clause : Asis.Element)
      return       Query_Array
   is
   begin
      case Asis.Elements.Clause_Kind (Ada_Clause) is
         when Not_A_Clause =>
            raise Internal_Implementation_Error;

         when A_Use_Package_Clause  |
              A_Use_Type_Clause     |
              A_Use_All_Type_Clause | --  Ada 2012
              A_With_Clause         =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Clauses.Clause_Names'Access));

         when A_Representation_Clause =>
            case Asis.Elements.Representation_Clause_Kind (Ada_Clause) is
               when Not_A_Representation_Clause =>
                  raise Internal_Implementation_Error;

               when An_Attribute_Definition_Clause |
                    An_Enumeration_Representation_Clause |
                    An_At_Clause =>
                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Clauses.Representation_Clause_Name'Access),

                     2 => (Single_Element_Query,
                        Asis.Clauses.Representation_Clause_Expression'Access));

               when A_Record_Representation_Clause =>
                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Clauses.Representation_Clause_Name'Access),

                     2 => (Single_Element_Query,
                           Asis.Clauses.Mod_Clause_Expression'Access),

                     3 => (Element_List_Query_With_Boolean,
                           Asis.Clauses.Component_Clauses'Access, True));

            end case;

         when A_Component_Clause =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Clauses.Representation_Clause_Name'Access),

               2 => (Single_Element_Query,
                     Asis.Clauses.Component_Clause_Position'Access),

               3 => (Single_Element_Query,
                     Asis.Clauses.Component_Clause_Range'Access));

      end case;
   end PARSE_Clause;

   -----------------------
   -- PARSE_Declaration --
   -----------------------

   ----------------------------------------------------------------------------
   --  function PARSE_Declaration
   --
   --  This function parse every declarations
   --  The Declaration_Kinds are :
   --
   --      Declaration_Kind            LRM P.
   ----------------------------------------------------------
   --
   --     An_Ordinary_Type_Declaration,              -- 3.2.1
   --     A_Task_Type_Declaration,                   -- 3.2.1
   --     A_Protected_Type_Declaration,              -- 3.2.1
   --     An_Incomplete_Type_Declaration,            -- 3.2.1
   --     A_Private_Type_Declaration,                -- 3.2.1
   --     A_Private_Extension_Declaration,           -- 3.2.1
   --
   --     A_Subtype_Declaration,                     -- 3.2.2
   --
   --     A_Variable_Declaration,                    -- 3.3.1
   --     A_Constant_Declaration,                    -- 3.3.1
   --     A_Deferred_Constant_Declaration,           -- 3.3.1
   --     A_Single_Task_Declaration,                 -- 3.3.1
   --     A_Single_Protected_Declaration,            -- 3.3.1
   --
   --     An_Integer_Number_Declaration,             -- 3.3.2
   --     A_Real_Number_Declaration,                 -- 3.3.2
   --
   --     An_Enumeration_Literal_Specification,      -- 3.5.1
   --
   --     A_Discriminant_Specification,              -- 3.7
   --
   --     A_Component_Declaration,                   -- 3.8
   --
   --     A_Loop_Parameter_Specification,            -- 5.5
   --     A_Generalized_Iterator_Specification,      -- 5.5.2
   --     An_Element_Iterator_Specification,         -- 5.5.2
   --
   --     A_Procedure_Declaration,                   -- 6.1
   --     A_Function_Declaration,                    -- 6.1
   --
   --     A_Parameter_Specification,                 -- 6.1
   --
   --     A_Procedure_Body_Declaration,              -- 6.3
   --     A_Function_Body_Declaration,               -- 6.3
   --
   --     A_Package_Declaration,                     -- 7.1
   --     A_Package_Body_Declaration,                -- 7.2
   --
   --     An_Object_Renaming_Declaration,            -- 8.5.1
   --     An_Exception_Renaming_Declaration,         -- 8.5.2
   --     A_Package_Renaming_Declaration,            -- 8.5.3
   --     A_Procedure_Renaming_Declaration,          -- 8.5.4
   --     A_Function_Renaming_Declaration,           -- 8.5.4
   --     A_Generic_Package_Renaming_Declaration,    -- 8.5.5
   --     A_Generic_Procedure_Renaming_Declaration,  -- 8.5.5
   --     A_Generic_Function_Renaming_Declaration,   -- 8.5.5
   --
   --     A_Task_Body_Declaration,                   -- 9.1
   --     A_Protected_Body_Declaration,              -- 9.4
   --
   --     An_Entry_Declaration,                      -- 9.5.2
   --     An_Entry_Body_Declaration,                 -- 9.5.2
   --     An_Entry_Index_Specification,              -- 9.5.2
   --
   --     A_Procedure_Body_Stub,                     -- 10.1.3
   --     A_Function_Body_Stub,                      -- 10.1.3
   --     A_Package_Body_Stub,                       -- 10.1.3
   --     A_Task_Body_Stub,                          -- 10.1.3
   --     A_Protected_Body_Stub,                     -- 10.1.3
   --
   --     An_Exception_Declaration,                  -- 11.1
   --     A_Choice_Parameter_Specification,          -- 11.2
   --
   --     A_Generic_Procedure_Declaration,           -- 12.1
   --     A_Generic_Function_Declaration,            -- 12.1
   --     A_Generic_Package_Declaration,             -- 12.1
   --
   --     A_Package_Instantiation,                   -- 12.3
   --     A_Procedure_Instantiation,                 -- 12.3
   --     A_Function_Instantiation,                  -- 12.3
   --
   --     A_Formal_Object_Declaration,               -- 12.4
   --     A_Formal_Type_Declaration,                 -- 12.5
   --     A_Formal_Procedure_Declaration,            -- 12.6
   --     A_Formal_Function_Declaration,             -- 12.6
   --     A_Formal_Package_Declaration,              -- 12.7
   --     A_Formal_Package_Declaration_With_Box,     -- 12.7
   --
   --     Not_A_Declaration.                         -- An unexpected element
   ----------------------------------------------------------------------------

   function PARSE_Declaration
     (Ada_Declaration : Asis.Element)
      return            Query_Array
   is
   begin
      case Asis.Elements.Declaration_Kind (Ada_Declaration) is

         --  An_Ordinary_Type_Declaration,              -- 3.2.1
         --  A_Protected_Type_Declaration,              -- 3.2.1
         --  A_Formal_Type_Declaration,                 -- 12.5
         when An_Ordinary_Type_Declaration |
              A_Formal_Type_Declaration     =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Discriminant_Part'Access),
               3 => (Single_Element_Query,
                     Asis.Declarations.Type_Declaration_View'Access));

         --  A_Task_Type_Declaration,                   -- 3.2.1
         --  A_Private_Type_Declaration,                -- 3.2.1
         --  A_Private_Extension_Declaration,           -- 3.2.1

         when A_Private_Type_Declaration      |
              A_Private_Extension_Declaration =>

            return Query_Array'

              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Discriminant_Part'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Type_Declaration_View'Access));

--  |A2005 start
         when A_Task_Type_Declaration        |
              A_Protected_Type_Declaration   =>

            return Query_Array'

              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Discriminant_Part'Access),

               3 => (Element_List_Query,
                     Asis.Declarations.Declaration_Interface_List'Access),

               4 => (Single_Element_Query,
                     Asis.Declarations.Type_Declaration_View'Access));
--  |A2005 end
         --  An_Incomplete_Type_Declaration,            -- 3.2.1
--  |A2005 start
         --  A_Tagged_Incomplete_Type_Declaration,     --  3.10.1(2)
--  |A2005 end
         when An_Incomplete_Type_Declaration       |
--  |A2005 start
              A_Tagged_Incomplete_Type_Declaration |
--  |A2005 end
--  |A2012 start
             A_Formal_Incomplete_Type_Declaration  =>
--  |A2012 end

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Discriminant_Part'Access));

         --  A_Variable_Declaration,                    -- 3.3.1
         --  A_Constant_Declaration,                    -- 3.3.1
         when A_Variable_Declaration |
              A_Constant_Declaration  =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));

         --  A_Deferred_Constant_Declaration,           -- 3.3.1
         --  A_Single_Task_Declaration,                 -- 3.3.1
         --  A_Single_Protected_Declaration.            -- 3.3.1
         when A_Deferred_Constant_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access));

--  |A2005 start
         when A_Single_Task_Declaration      |
              A_Single_Protected_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Declaration_Interface_List'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access));

--  |A2005 end

         --  An_Integer_Number_Declaration,             -- 3.3.2
         --  A_Real_Number_Declaration,                 -- 3.3.2
         when An_Integer_Number_Declaration |
              A_Real_Number_Declaration       =>

            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));

         --  A_Procedure_Declaration,                   -- 6.1
         --  A_Null_Procedure_Declaration,               -- 6.7
         when A_Procedure_Declaration      |
--  |A2005 start
              A_Null_Procedure_Declaration =>
--  |A2005 end
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),
               3 => (Element_List_Query,
                     Asis.Declarations.Aspect_Specifications'Access));

         --  A_Procedure_Body_Stub,                     -- 10.1.3
         when A_Procedure_Body_Stub =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access));

         --  A_Function_Declaration,                    -- 6.1
--  |A2005 start
         when A_Function_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access),

               4 => (Element_List_Query,
                     Asis.Declarations.Aspect_Specifications'Access));
--  |A2005 end

         --  A_Function_Body_Stub,                      -- 10.1.3
         when A_Function_Body_Stub =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access));

--  |A2005 start
         --  A_Return_Variable_Specification,            -- 6.5
         --  A_Return_Constant_Specification,            -- 6.5

         when A_Return_Variable_Specification |
              A_Return_Constant_Specification =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));
--  |A2005 end

         --  A_Package_Declaration,                     -- 7.1

--  |A2012 start
         when An_Expression_Function_Declaration =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access),

               4 => (Single_Element_Query,
                     Asis.Declarations.Result_Expression'Access),

               5 => (Element_List_Query,
                     Asis.Declarations.Aspect_Specifications'Access));
--  |A2012 end

         when A_Package_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query_With_Boolean,
                Asis.Declarations.Visible_Part_Declarative_Items'Access, True),

               3 => (Element_List_Query_With_Boolean,
               Asis.Declarations.Private_Part_Declarative_Items'Access, True));

         --  An_Entry_Declaration,                      -- 9.5.2
         when An_Entry_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Entry_Family_Definition'Access),

               3 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access));

         --  A_Package_Body_Stub,                       -- 10.1.3
         --  A_Task_Body_Stub,                          -- 10.1.3
         --  A_Protected_Body_Stub,                     -- 10.1.3
         --  An_Exception_Declaration,                  -- 11.1
         when A_Package_Body_Stub     |
              A_Task_Body_Stub        |
              A_Protected_Body_Stub   |
              An_Exception_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access));

         --   A_Generic_Procedure_Declaration,           -- 12.1
         when A_Generic_Procedure_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Generic_Formal_Part'Access, True),

               2 => (Element_List_Query, Asis.Declarations.Names'Access),

               3 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access));

         --  A_Generic_Function_Declaration,            -- 12.1
         when A_Generic_Function_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Generic_Formal_Part'Access, True),

               2 => (Element_List_Query,
                     Asis.Declarations.Names'Access),

               3 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               4 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access));

         --  A_Generic_Package_Declaration.             -- 12.1
         when A_Generic_Package_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Generic_Formal_Part'Access, True),

               2 => (Element_List_Query,
                     Asis.Declarations.Names'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Visible_Part_Declarative_Items'Access,
                     True),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Private_Part_Declarative_Items'Access,
                     True));

         --  A_Procedure_Body_Declaration,              -- 6.3
         when A_Procedure_Body_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Declarative_Items'Access,
                     True),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Statements'Access,
                     True),

               5 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Exception_Handlers'Access,
                     True));

         --  A_Function_Body_Declaration,               -- 6.3
         when A_Function_Body_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Declarative_Items'Access,
                     True),

               5 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Statements'Access,
                     True),

               6 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Exception_Handlers'Access,
                     True));

         --  A_Package_Body_Declaration,                -- 7.2
         when A_Package_Body_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Declarative_Items'Access,
                     True),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Statements'Access,
                     True),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Exception_Handlers'Access,
                     True));

         --  A_Task_Body_Declaration,                   -- 9.1
         when A_Task_Body_Declaration =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Declarative_Items'Access,
                     True),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Statements'Access, True),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Exception_Handlers'Access,
                     True));

         --  An_Entry_Body_Declaration,                 -- 9.5.2
         when An_Entry_Body_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Entry_Index_Specification'Access),

               3 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               4 => (Single_Element_Query,
                     Asis.Declarations.Entry_Barrier'Access),

               5 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Declarative_Items'Access,
                     True),

               6 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Statements'Access,
                     True),

               7 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Body_Exception_Handlers'Access,
                     True));

         --  A_Protected_Body_Declaration,              -- 9.4
         when A_Protected_Body_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),
               2 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Protected_Operation_Items'Access,
                     True));

         --  An_Object_Renaming_Declaration,            -- 8.5.1
         when An_Object_Renaming_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Renamed_Entity'Access));

         --  An_Exception_Renaming_Declaration,         -- 8.5.2
         --  A_Package_Renaming_Declaration,            -- 8.5.3
         --  A_Generic_Package_Renaming_Declaration,    -- 8.5.5
         --  A_Generic_Procedure_Renaming_Declaration,  -- 8.5.5
         --  A_Generic_Function_Renaming_Declaration.   -- 8.5.5
         when An_Exception_Renaming_Declaration        |
              A_Package_Renaming_Declaration           |
              A_Generic_Package_Renaming_Declaration   |
              A_Generic_Procedure_Renaming_Declaration |
              A_Generic_Function_Renaming_Declaration   =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Renamed_Entity'Access));

         --  A_Procedure_Renaming_Declaration,          -- 8.5.4
         when A_Procedure_Renaming_Declaration =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Renamed_Entity'Access));

         --  A_Function_Renaming_Declaration,           -- 8.5.4
         when A_Function_Renaming_Declaration =>
            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access),

               4 => (Single_Element_Query,
                     Asis.Declarations.Renamed_Entity'Access));

         --  A_Package_Instantiation,                   -- 12.3
         --  A_Procedure_Instantiation,                 -- 12.3
         --  A_Function_Instantiation,                  -- 12.3
         --  A_Formal_Package_Declaration.              -- 12.3
         when A_Package_Instantiation     |
              A_Procedure_Instantiation   |
              A_Function_Instantiation    |
              A_Formal_Package_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Generic_Unit_Name'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Declarations.Generic_Actual_Part'Access,
                     False)
                     --  Will Parse the Actual_Part in the UN-Normalized Form.
                     --  (as noticed in the Traverse_Element specification
                     --  (see asisi_elements.ads))
               );

         --  A_Subtype_Declaration,                     -- 3.2.2
         when A_Subtype_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),
               2 => (Single_Element_Query,
                     Asis.Declarations.Type_Declaration_View'Access));

         --  An_Enumeration_Literal_Specification,      -- 3.5.1
         when An_Enumeration_Literal_Specification =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access));

         --  A_Discriminant_Specification,            -- 3.7   -> Trait_Kinds
         --  A_Parameter_Specification,               -- 6.1   -> Trait_Kinds
         when A_Discriminant_Specification |
              A_Parameter_Specification     =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));

         --  A_Component_Declaration,                   -- 3.8
         when A_Component_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));

         --  A_Loop_Parameter_Specification,        -- 5.5   -> Trait_Kinds
         --  An_Entry_Index_Specification,          -- 9.5.2
         when A_Loop_Parameter_Specification |
              An_Entry_Index_Specification    =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                   Asis.Declarations.Specification_Subtype_Definition'Access));

         when A_Generalized_Iterator_Specification =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                   Asis.Declarations.Iteration_Scheme_Name'Access));

         when An_Element_Iterator_Specification    =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                   Asis.Declarations.Subtype_Indication'Access),

               3 => (Single_Element_Query,
                   Asis.Declarations.Iteration_Scheme_Name'Access));

         --  A_Choice_Parameter_Specification,          -- 11.2
         when A_Choice_Parameter_Specification =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access));

         --  A_Formal_Object_Declaration,               -- 12.4  -> Mode_Kinds
--  |A2005 start
         when A_Formal_Object_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Object_Declaration_View'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Initialization_Expression'Access));
--  |A2005 end

         --  A_Formal_Procedure_Declaration,        -- 12.6  -> Default_Kinds
         when A_Formal_Procedure_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Extensions.Formal_Subprogram_Default'Access)
                     --  Asis.Declarations.Formal_Subprogram_Default
                     --  cannot be used here!
               );

         --  A_Formal_Function_Declaration,        -- 12.6  -> Default_Kinds
         when A_Formal_Function_Declaration =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Element_List_Query,
                     Asis.Declarations.Parameter_Profile'Access),

               3 => (Single_Element_Query,
                     Asis.Declarations.Result_Profile'Access),

               4 => (Single_Element_Query,
                     Asis.Extensions.Formal_Subprogram_Default'Access)
                     --  Asis.Declarations.Formal_Subprogram_Default
                     --  cannot be used here!
               );

         --  A_Formal_Package_Declaration_With_Box      -- 12.7
         when A_Formal_Package_Declaration_With_Box =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Declarations.Names'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Generic_Unit_Name'Access));

         --  Not_A_Declaration,              -- An unexpected element
         when Not_A_Declaration =>
            raise Internal_Implementation_Error;
      end case;

   end PARSE_Declaration;

   -------------------------
   -- PARSE_Defining_Name --
   -------------------------

   function PARSE_Defining_Name
     (Ada_Defining_Name : Asis.Element)
      return              Query_Array
   is
   begin
   --  PARSE_Defining_Name deals with every Defining_Name 's Element_Kinds.
   --  That is to say, it deals with Defining_Name_Kinds :
   --     Not_A_Defining_Name,                     -- An unexpected element
   --     A_Defining_Identifier,                   -- 3.1
   --     A_Defining_Character_Literal,            -- 3.5.1
   --     A_Defining_Enumeration_Literal,          -- 3.5.1
   --     A_Defining_Operator_Symbol,              -- 6.1
   --     A_Defining_Expanded_Name.
   --                        6.1 program_unit_name.defining_identifier
      case Asis.Elements.Defining_Name_Kind (Ada_Defining_Name) is

         --  Terminal Elements : DO NOTHING !!!!
         when A_Defining_Identifier |
           A_Defining_Character_Literal |
           A_Defining_Enumeration_Literal |
           A_Defining_Operator_Symbol =>
            --  Terminal Elements : DO NOTHING !!!!
            return No_Query;

         when A_Defining_Expanded_Name =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Declarations.Defining_Prefix'Access),

               2 => (Single_Element_Query,
                     Asis.Declarations.Defining_Selector'Access));

         when Not_A_Defining_Name =>
            raise Internal_Implementation_Error;
      end case;
   end PARSE_Defining_Name;

   ----------------------
   -- PARSE_Definition --
   ----------------------

   -----------------------------------------------------------------------
   --  function PARSE_Definition
   --
   --  This function parse every definitions
   --  The Definition_Kinds are :
   --
   --     Definition_Kinds                     LRM P.
   --          SubType_Kind
   -----------------------------------------------------------------------
   --     Not_A_Definition,                 -- An unexpected element
   --
   --     A_Type_Definition,                -- 3.2.1
   --       Not_A_Type_Definition,                 -- An unexpected element
   --       A_Derived_Type_Definition,             -- 3.4
   --       A_Derived_Record_Extension_Definition, -- 3.4
   --       An_Enumeration_Type_Definition,        -- 3.5.1
   --       A_Signed_Integer_Type_Definition,      -- 3.5.4
   --       A_Modular_Type_Definition,             -- 3.5.4
   --       A_Root_Type_Definition,                -- 3.5.4(10), 3.5.6(4)
   --       A_Floating_Point_Definition,           -- 3.5.7
   --       An_Ordinary_Fixed_Point_Definition,    -- 3.5.9
   --       A_Decimal_Fixed_Point_Definition,      -- 3.5.9
   --       An_Unconstrained_Array_Definition,     -- 3.6
   --       A_Constrained_Array_Definition,        -- 3.6
   --       A_Record_Type_Definition,              -- 3.8
   --       A_Tagged_Record_Type_Definition,       -- 3.8
   --       An_Access_Type_Definition.             -- 3.10
   --        Not_An_Access_Type_Definition,       -- An unexpected element
   --        A_Pool_Specific_Access_To_Variable,  -- access subtype_indication
   --        An_Access_To_Variable,            -- access all subtype_indication
   --        An_Access_To_Constant,       -- access constant subtype_indication
   --        An_Access_To_Procedure,              -- access procedure
   --        An_Access_To_Protected_Procedure,    -- access protected procedure
   --        An_Access_To_Function,               -- access function
   --        An_Access_To_Protected_Function.     -- access protected function
   --
   --    A_Subtype_Indication,             -- 3.2.2
   --
   --    A_Constraint,                     -- 3.2.2
   --       Not_A_Constraint,                      -- An unexpected element
   --       A_Range_Attribute_Reference,           -- 3.2.2, 3.5
   --       A_Simple_Expression_Range,             -- 3.2.2, 3.5
   --       A_Digits_Constraint,                   -- 3.2.2, 3.5.9
   --       A_Delta_Constraint,                    -- 3.2.2, J.3
   --       An_Index_Constraint,                   -- 3.2.2, 3.6.1
   --       A_Discriminant_Constraint.             -- 3.2.2
   --
   --    A_Component_Definition,           -- 3.6
   --
   --    A_Discrete_Subtype_Definition,    -- 3.6
   --    A_Discrete_Range,                 -- 3.6.1
   --       Not_A_Discrete_Range,                  -- An unexpected element
   --       A_Discrete_Subtype_Indication,         -- 3.6.1, 3.2.2
   --       A_Discrete_Range_Attribute_Reference,  -- 3.6.1, 3.5
   --       A_Discrete_Simple_Expression_Range.    -- 3.6.1, 3.5
   --
   --
   --    An_Unknown_Discriminant_Part,     -- 3.7
   --    A_Known_Discriminant_Part,        -- 3.7
   --
   --    A_Record_Definition,              -- 3.8
   --    A_Null_Record_Definition,         -- 3.8
   --
   --    A_Null_Component,                 -- 3.8
   --    A_Variant_Part,                   -- 3.8
   --    A_Variant,                        -- 3.8
   --
   --    An_Others_Choice,                 -- 3.8.1, 4.3.1, 4.3.3, 11.2
   --
   --    A_Private_Type_Definition,        -- 7.3
   --    A_Tagged_Private_Type_Definition, -- 7.3
   --    A_Private_Extension_Definition,   -- 7.3
   --
   --    A_Task_Definition,                -- 9.1
   --    A_Protected_Definition,           -- 9.4
   --
   --    A_Formal_Type_Definition.         -- 12.5
   --       Not_A_Formal_Type_Definition,             -- An unexpected element
   --       A_Formal_Private_Type_Definition,         -- 12.5.1
   --       A_Formal_Tagged_Private_Type_Definition,  -- 12.5.1
   --       A_Formal_Derived_Type_Definition,         -- 12.5.1
   --       A_Formal_Discrete_Type_Definition,        -- 12.5.2
   --       A_Formal_Signed_Integer_Type_Definition,  -- 12.5.2
   --       A_Formal_Modular_Type_Definition,         -- 12.5.2
   --       A_Formal_Floating_Point_Definition,       -- 12.5.2
   --       A_Formal_Ordinary_Fixed_Point_Definition, -- 12.5.2
   --       A_Formal_Decimal_Fixed_Point_Definition,  -- 12.5.2
   --       A_Formal_Unconstrained_Array_Definition,  -- 12.5.3
   --       A_Formal_Constrained_Array_Definition,    -- 12.5.3
   --       A_Formal_Access_Type_Definition.          -- 12.5.4
   --
   -----------------------------------------------------------------------

   function PARSE_Definition
     (Ada_Definition : Asis.Element)
      return           Query_Array
   is
   begin
      case Asis.Elements.Definition_Kind (Ada_Definition) is
         when Not_A_Definition =>
            raise Internal_Implementation_Error;

            --   A_Type_Definition.                -- 3.2.1
         when A_Type_Definition =>
            case Asis.Elements.Type_Kind (Ada_Definition) is
               --   Not_A_Type_Definition,            -- An unexpected element
               when Not_A_Type_Definition =>
                  raise Internal_Implementation_Error;

               --   A_Derived_Type_Definition,             -- 3.4
               when A_Derived_Type_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Parent_Subtype_Indication'Access));

               --   A_Derived_Record_Extension_Definition, -- 3.4
               when A_Derived_Record_Extension_Definition =>

--  |A2005 start
                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.
                             Parent_Subtype_Indication'Access),

                     2 => (Element_List_Query,
                           Asis.Definitions.
                             Definition_Interface_List'Access),

                     3 => (Single_Element_Query,
                           Asis.Definitions.Record_Definition'Access));
--  |A2005 end
               --   An_Enumeration_Type_Definition,        -- 3.5.1
               when An_Enumeration_Type_Definition =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                    Asis.Definitions.Enumeration_Literal_Declarations'Access));

               --   A_Signed_Integer_Type_Definition,      -- 3.5.4
               when A_Signed_Integer_Type_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Integer_Constraint'Access));

               --   A_Modular_Type_Definition,             -- 3.5.4
               when A_Modular_Type_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Mod_Static_Expression'Access));

               --   A_Root_Type_Definition,             -- 3.5.4(10), 3.5.6(4)
               when A_Root_Type_Definition =>
                  return No_Query;

               --   A_Floating_Point_Definition,           -- 3.5.7
               when A_Floating_Point_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Digits_Expression'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Real_Range_Constraint'Access));

               --   An_Ordinary_Fixed_Point_Definition,    -- 3.5.9
               when An_Ordinary_Fixed_Point_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Delta_Expression'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Real_Range_Constraint'Access));

               --   A_Decimal_Fixed_Point_Definition,      -- 3.5.9
               when A_Decimal_Fixed_Point_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Delta_Expression'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Digits_Expression'Access),

                     3 => (Single_Element_Query,
                           Asis.Definitions.Real_Range_Constraint'Access));

               --   An_Unconstrained_Array_Definition,     -- 3.6
               when An_Unconstrained_Array_Definition =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.Index_Subtype_Definitions'Access),

                     2 => (Single_Element_Query,
                          Asis.Definitions.Array_Component_Definition'Access));

               --   A_Constrained_Array_Definition,        -- 3.6
               when A_Constrained_Array_Definition =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                         Asis.Definitions.Discrete_Subtype_Definitions'Access),

                     2 => (Single_Element_Query,
                          Asis.Definitions.Array_Component_Definition'Access));

               --   A_Record_Type_Definition,              -- 3.8
               --   A_Tagged_Record_Type_Definition,       -- 3.8
               when A_Record_Type_Definition |
                    A_Tagged_Record_Type_Definition =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Record_Definition'Access));

--  |A2005 start
               --  An_Interface_Type_Definition,          -- 3.9.4

               when An_Interface_Type_Definition =>
                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.Definition_Interface_List'Access));
--  |A2005 end

               --   An_Access_Type_Definition.             -- 3.10
               when An_Access_Type_Definition =>

                  case Asis.Elements.Access_Type_Kind (Ada_Definition) is
                     --   Not_An_Access_Type_Definition,
                     when Not_An_Access_Type_Definition =>
                        raise Internal_Implementation_Error;

                     --  A_Pool_Specific_Access_To_Variable,
                     --  An_Access_To_Variable,
                     --  An_Access_To_Constant,
                     when A_Pool_Specific_Access_To_Variable |
                          An_Access_To_Variable              |
                          An_Access_To_Constant =>

                        return Query_Array'
                          (1 => (Single_Element_Query,
                         Asis.Definitions.Access_To_Object_Definition'Access));

                     --  An_Access_To_Procedure,
                     --  An_Access_To_Protected_Procedure,
                     when An_Access_To_Procedure |
                          An_Access_To_Protected_Procedure =>

                        return Query_Array'
                          (1 => (Element_List_Query,
              Asis.Definitions.Access_To_Subprogram_Parameter_Profile'Access));

                     --  An_Access_To_Function,
                     --  An_Access_To_Protected_Function
                     when An_Access_To_Function |
                          An_Access_To_Protected_Function =>

                        return Query_Array'
                          (1 => (Element_List_Query,
               Asis.Definitions.Access_To_Subprogram_Parameter_Profile'Access),

                           2 => (Single_Element_Query,
               Asis.Definitions.Access_To_Function_Result_Profile'Access));

                  end case;
            end case;

            --   A_Subtype_Indication,             -- 3.2.2
         when A_Subtype_Indication =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Definitions.Subtype_Mark'Access),

               2 => (Single_Element_Query,
                     Asis.Definitions.Subtype_Constraint'Access));

            --  A_Constraint,                     -- 3.2.2
         when A_Constraint =>
            case Asis.Elements.Constraint_Kind (Ada_Definition) is
               --   Not_A_Constraint,             -- An unexpected element
               when Not_A_Constraint =>
                  raise Internal_Implementation_Error;

               --  A_Range_Attribute_Reference,           -- 3.2.2, 3.5
               when A_Range_Attribute_Reference =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Range_Attribute'Access));

               --  A_Simple_Expression_Range,             -- 3.2.2, 3.5
               when A_Simple_Expression_Range =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Lower_Bound'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Upper_Bound'Access));

               --  A_Digits_Constraint,                   -- 3.2.2, 3.5.9
               when A_Digits_Constraint =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Digits_Expression'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Real_Range_Constraint'Access));

               --  A_Delta_Constraint,                    -- 3.2.2, J.3
               when A_Delta_Constraint =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Delta_Expression'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Real_Range_Constraint'Access));

               --  An_Index_Constraint,                   -- 3.2.2, 3.6.1
               when An_Index_Constraint =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.Discrete_Ranges'Access));

               --  A_Discriminant_Constraint.             -- 3.2.2
               when A_Discriminant_Constraint =>

                  return Query_Array'
                    (1 => (Element_List_Query_With_Boolean,
                 Asis.Definitions.Discriminant_Associations'Access, False));
            end case;

         --  A_Component_Definition,           -- 3.6
         when A_Component_Definition =>

--  |A2005 start
            return Query_Array'
              (1 => (Single_Element_Query,
                    Asis.Definitions.Component_Definition_View'Access));
--  |A2005 end

         --   A_Discrete_Subtype_Definition,    -- 3.6
         --   A_Discrete_Range,                 -- 3.6.1
         when A_Discrete_Subtype_Definition |
              A_Discrete_Range               =>
            case Asis.Elements.Discrete_Range_Kind (Ada_Definition) is
               --  Not_A_Discrete_Range,        -- An unexpected element
               when Not_A_Discrete_Range =>
                  raise Internal_Implementation_Error;

               --  A_Discrete_Subtype_Indication,         -- 3.6.1, 3.2.2
               when A_Discrete_Subtype_Indication =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Subtype_Mark'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Subtype_Constraint'Access));

               --  A_Discrete_Range_Attribute_Reference,  -- 3.6.1, 3.5
               when A_Discrete_Range_Attribute_Reference =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Range_Attribute'Access));

               --  A_Discrete_Simple_Expression_Range.    -- 3.6.1, 3.5
               when A_Discrete_Simple_Expression_Range =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Lower_Bound'Access),

                     2 => (Single_Element_Query,
                           Asis.Definitions.Upper_Bound'Access));

            end case;

         --  An_Unknown_Discriminant_Part,     -- 3.7
         when An_Unknown_Discriminant_Part =>
            return No_Query;

         --  A_Known_Discriminant_Part,        -- 3.7
         when A_Known_Discriminant_Part =>

            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Definitions.Discriminants'Access));

         --  A_Record_Definition,              -- 3.8
         when A_Record_Definition =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Definitions.Record_Components'Access, True));

         --  A_Null_Record_Definition,         -- 3.8
         --  A_Null_Component,                 -- 3.8
         when A_Null_Record_Definition |
              A_Null_Component           =>

            return No_Query;

         --  A_Variant_Part,                   -- 3.8
         when A_Variant_Part =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Definitions.Discriminant_Direct_Name'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Definitions.Variants'Access, True));

         --  A_Variant,                        -- 3.8
         when A_Variant =>

            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Definitions.Variant_Choices'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Definitions.Record_Components'Access, True));

         --  An_Others_Choice,                 -- 3.8.1, 4.3.1, 4.3.3, 11.2
         when An_Others_Choice =>

            return No_Query;

--  |A2005 start
         --  An_Access_Definition,  -- 3.10(6/2)   -> Access_Definition_Kinds

         when An_Access_Definition =>

            case Asis.Elements.Access_Definition_Kind (Ada_Definition) is

               when Not_An_Access_Definition =>
                  raise Internal_Implementation_Error;

               when An_Anonymous_Access_To_Variable |
                    An_Anonymous_Access_To_Constant =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.
                              Anonymous_Access_To_Object_Subtype_Mark'
                                Access));

               when An_Anonymous_Access_To_Procedure            |
                    An_Anonymous_Access_To_Protected_Procedure  =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.
                              Access_To_Subprogram_Parameter_Profile'
                                Access));

               when An_Anonymous_Access_To_Function           |
                    An_Anonymous_Access_To_Protected_Function =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.
                             Access_To_Subprogram_Parameter_Profile'Access),
                     2 => (Single_Element_Query,
                           Asis.Definitions.
                             Access_To_Function_Result_Profile'Access));

            end case;

--  |A2005 end

         --  A_Private_Type_Definition,        -- 7.3
         --  A_Tagged_Private_Type_Definition, -- 7.3
         when A_Private_Type_Definition |
              A_Tagged_Private_Type_Definition =>

            return No_Query;

         --  A_Private_Extension_Definition,   -- 7.3
         when A_Private_Extension_Definition =>

--  |A2005 start

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Definitions.Ancestor_Subtype_Indication'Access),
               2 => (Element_List_Query,
                     Asis.Definitions.Definition_Interface_List'Access));

--  |A2005 end

         --  A_Task_Definition,                -- 9.1
         --  A_Protected_Definition,           -- 9.4
         when A_Task_Definition |
              A_Protected_Definition =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Definitions.Visible_Part_Items'Access, True),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Definitions.Private_Part_Items'Access, True));

         --  A_Formal_Type_Definition.         -- 12.5
         when A_Formal_Type_Definition =>
            case Asis.Elements.Formal_Type_Kind (Ada_Definition) is
               --  Not_A_Formal_Type_Definition,       -- An unexpected element
               when Not_A_Formal_Type_Definition =>
                  raise Internal_Implementation_Error;

               --  A_Formal_Private_Type_Definition,         -- 12.5.1
               --  A_Formal_Tagged_Private_Type_Definition,  -- 12.5.1
               --  A_Formal_Discrete_Type_Definition,        -- 12.5.2
               --  A_Formal_Signed_Integer_Type_Definition,  -- 12.5.2
               --  A_Formal_Modular_Type_Definition,         -- 12.5.2
               --  A_Formal_Floating_Point_Definition,       -- 12.5.2
               --  A_Formal_Ordinary_Fixed_Point_Definition, -- 12.5.2
               --  A_Formal_Decimal_Fixed_Point_Definition,  -- 12.5.2
               when A_Formal_Private_Type_Definition         |
                    A_Formal_Tagged_Private_Type_Definition  |
                    A_Formal_Discrete_Type_Definition        |
                    A_Formal_Signed_Integer_Type_Definition  |
                    A_Formal_Modular_Type_Definition         |
                    A_Formal_Floating_Point_Definition       |
                    A_Formal_Ordinary_Fixed_Point_Definition |
                    A_Formal_Decimal_Fixed_Point_Definition   =>

                  return No_Query;

               --  A_Formal_Derived_Type_Definition,         -- 12.5.1
               when A_Formal_Derived_Type_Definition =>

--  |A2005 start
                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Definitions.Subtype_Mark'Access),

                     2 => (Element_List_Query,
                           Asis.Definitions.
                             Definition_Interface_List'Access));
--  |A2005 end

--  |A2005 start
               --  A_Formal_Interface_Type_Definition, -- 12.5.5(2)

               when A_Formal_Interface_Type_Definition =>
                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.Definition_Interface_List'Access));
--  |A2005 end

               --  A_Formal_Unconstrained_Array_Definition,  -- 12.5.3
               when A_Formal_Unconstrained_Array_Definition =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                           Asis.Definitions.Index_Subtype_Definitions'Access),

                     2 => (Single_Element_Query,
                          Asis.Definitions.Array_Component_Definition'Access));

               --  A_Formal_Constrained_Array_Definition,    -- 12.5.3
               when A_Formal_Constrained_Array_Definition =>

                  return Query_Array'
                    (1 => (Element_List_Query,
                         Asis.Definitions.Discrete_Subtype_Definitions'Access),

                     2 => (Single_Element_Query,
                          Asis.Definitions.Array_Component_Definition'Access));

               --  A_Formal_Access_Type_Definition.          -- 12.5.4
               when A_Formal_Access_Type_Definition =>

                  case Asis.Elements.Access_Type_Kind (Ada_Definition) is
                     --  Not_An_Access_Type_Definition,
                     when Not_An_Access_Type_Definition =>
                        raise Internal_Implementation_Error;

                     --  A_Pool_Specific_Access_To_Variable,
                     --  An_Access_To_Variable,
                     --  An_Access_To_Constant,
                     when A_Pool_Specific_Access_To_Variable |
                          An_Access_To_Variable |
                          An_Access_To_Constant =>

                        return Query_Array'
                          (1 => (Single_Element_Query,
                         Asis.Definitions.Access_To_Object_Definition'Access));

                     --  An_Access_To_Procedure,
                     --  An_Access_To_Protected_Procedure,
                     when An_Access_To_Procedure |
                          An_Access_To_Protected_Procedure =>

                        return Query_Array'
                          (1 => (Element_List_Query,
              Asis.Definitions.Access_To_Subprogram_Parameter_Profile'Access));

                     --  An_Access_To_Function,
                     --  An_Access_To_Protected_Function
                     when An_Access_To_Function |
                          An_Access_To_Protected_Function =>

                        return Query_Array'
                          (1 => (Element_List_Query,
               Asis.Definitions.Access_To_Subprogram_Parameter_Profile'Access),

                           2 => (Single_Element_Query,
                   Asis.Definitions.Access_To_Function_Result_Profile'Access));
                  end case;
            end case;

--  |A2012 start
         when An_Aspect_Specification =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Definitions.Aspect_Mark'Access),
               2 => (Single_Element_Query,
                    Asis.Definitions.Aspect_Definition'Access));
--  |A2012 end
      end case;
   end PARSE_Definition;

   ----------------------
   -- PARSE_Expression --
   ----------------------

   ----------------------------------------------------------------------------
   --  function PARSE_Expression
   --
   --  This function parse every expressions
   --  The Expression_Kind are :
   --
   --  An_Integer_Literal,                        -- 2.4
   --  A_Real_Literal,                            -- 2.4.1
   --  A_String_Literal,                          -- 2.6
   --
   --  An_Identifier,                             -- 4.1
   --  An_Operator_Symbol,                        -- 4.1
   --  A_Character_Literal,                       -- 4.1
   --  An_Enumeration_Literal,                    -- 4.1
   --  An_Explicit_Dereference,                   -- 4.1
   --  A_Function_Call,                           -- 4.1
   --
   --  An_Indexed_Component,                      -- 4.1.1
   --  A_Slice,                                   -- 4.1.2
   --  A_Selected_Component,                      -- 4.1.3
   --  An_Attribute_Reference,                    -- 4.1.4
   --
   --  A_Record_Aggregate,                        -- 4.3
   --  An_Extension_Aggregate,                    -- 4.3
   --  A_Positional_Array_Aggregate,              -- 4.3
   --  A_Named_Array_Aggregate,                   -- 4.3
   --
   --  An_And_Then_Short_Circuit,                 -- 4.4
   --  An_Or_Else_Short_Circuit,                  -- 4.4
   --
   --  An_In_Membership_Test,                     -- 4.4  Ada 2012
   --  A_Not_In_Membership_Test,                  -- 4.4  Ada 2012

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
   --  Not_An_Expression.                         -- An unexpected element
   --
   function PARSE_Expression
     (Ada_Expression : Asis.Element)
      return           Query_Array
   is
   begin
      --  maybe there could be a factorization of all Prefix processing here
      case Asis.Elements.Expression_Kind (Ada_Expression) is
         when Not_An_Expression =>
            raise Internal_Implementation_Error;

         when An_Integer_Literal     |
              A_Real_Literal         |
              A_String_Literal       |
              An_Identifier          |
              An_Operator_Symbol     |
              A_Character_Literal    |
              An_Enumeration_Literal |
              A_Null_Literal           =>
            return No_Query;

         when An_Explicit_Dereference =>
            --  P.ALL
            return Query_Array'
              (1 => (Single_Element_Query, Asis.Expressions.Prefix'Access));

         when A_Function_Call =>
            --  Abc(...) or Integer'Image(...)

            if Subprogram_Call_Needs_Reordering (Ada_Expression) then
               return Query_Array'
                 (1 => (Single_Element_Query,
                        First_Parameter_Association'Access),
                  2 => (Single_Element_Query, Asis.Expressions.Prefix'Access),
                  3 => (Element_List_Query,
                        All_But_First_Associations'Access));
            else
               return Query_Array'
                 (1 => (Single_Element_Query, Asis.Expressions.Prefix'Access),
                  2 => (Element_List_Query_With_Boolean,
                        Asis.Expressions.Function_Call_Parameters'Access,
                        False));
            end if;

         when An_Indexed_Component =>
            --  An_Array(3)
            return Query_Array'
              (1 => (Single_Element_Query, Asis.Expressions.Prefix'Access),
               2 => (Element_List_Query,
                     Asis.Expressions.Index_Expressions'Access));

         when A_Slice =>  -- An_Array(3 .. 5)
            return Query_Array'
              (1 => (Single_Element_Query, Asis.Expressions.Prefix'Access),
           2 => (Single_Element_Query, Asis.Expressions.Slice_Range'Access));

         when A_Selected_Component =>  -- A.B.C
            return Query_Array'
              (1 => (Single_Element_Query, Asis.Expressions.Prefix'Access),
           2 => (Single_Element_Query, Asis.Expressions.Selector'Access));

         when An_Attribute_Reference =>  -- Priv'Base'First

            --  Attribute_Designator_Expressions
            case Asis.Elements.Attribute_Kind (Ada_Expression) is
               when Not_An_Attribute =>
                  raise Internal_Implementation_Error;

               when A_First_Attribute                   |
                    A_Last_Attribute                    |
                    A_Length_Attribute                  |
                    A_Range_Attribute                   |
                    An_Implementation_Defined_Attribute |
                    An_Unknown_Attribute                  =>

                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Expressions.Prefix'Access),

                     2 => (Single_Element_Query,
                      Asis.Expressions.Attribute_Designator_Identifier'Access),
                     3 => (Element_List_Query,
                    Asis.Expressions.Attribute_Designator_Expressions'Access));

               when others =>
                  return Query_Array'
                    (1 => (Single_Element_Query,
                           Asis.Expressions.Prefix'Access),
                     2 => (Single_Element_Query,
                     Asis.Expressions.Attribute_Designator_Identifier'Access));
            end case;

         when A_Record_Aggregate =>
            --  (Field1 => value1, Field2 => value2)

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                Asis.Expressions.Record_Component_Associations'Access, False));

         when An_Extension_Aggregate =>
            --  (Ewpr with Field1 => value1, Field2 => value2)

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Expressions.Extension_Aggregate_Expression'Access),

               2 => (Element_List_Query_With_Boolean,
                Asis.Expressions.Record_Component_Associations'Access, False));

         when A_Positional_Array_Aggregate |
              A_Named_Array_Aggregate       =>

            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Expressions.Array_Component_Associations'Access));

         when An_And_Then_Short_Circuit |
              An_Or_Else_Short_Circuit    =>

            return Query_Array'
              (1 => (Single_Element_Query,
              Asis.Expressions.Short_Circuit_Operation_Left_Expression'Access),

               2 => (Single_Element_Query,
            Asis.Expressions.Short_Circuit_Operation_Right_Expression'Access));

         when An_In_Membership_Test    |
              A_Not_In_Membership_Test =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Expressions.Membership_Test_Expression'Access),

               2 => (Element_List_Query,
                     Asis.Expressions.Membership_Test_Choices'Access));

         when A_Parenthesized_Expression =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Expressions.Expression_Parenthesized'Access));

         when A_Type_Conversion |
              A_Qualified_Expression =>

            return Query_Array'
              (1 => (Single_Element_Query,
                  Asis.Expressions.Converted_Or_Qualified_Subtype_Mark'Access),
               2 => (Single_Element_Query,
                   Asis.Expressions.Converted_Or_Qualified_Expression'Access));

         when An_Allocation_From_Subtype =>

            return Query_Array'
              (1 => (Single_Element_Query,                   -- Ada 2012
                     Asis.Expressions.Subpool_Name'Access),
               2 => (Single_Element_Query,
                     Asis.Expressions.Allocator_Subtype_Indication'Access));

         when An_Allocation_From_Qualified_Expression =>

            return Query_Array'
              (1 => (Single_Element_Query,                   -- Ada 2012
                     Asis.Expressions.Subpool_Name'Access),
               2 => (Single_Element_Query,
                    Asis.Expressions.Allocator_Qualified_Expression'Access));

         when A_Case_Expression => -- Ada 2012

            return Query_Array'
              (1 => (Single_Element_Query,
                    Asis.Statements.Case_Expression'Access),
               2 => (Element_List_Query,
                    Asis.Expressions.Expression_Paths'Access));

         when An_If_Expression  => -- Ada 2012

            return Query_Array'
              (1 => (Element_List_Query,
                    Asis.Expressions.Expression_Paths'Access));
         when A_For_All_Quantified_Expression |   -- Ada 2012
              A_For_Some_Quantified_Expression => -- Ada 2012

            return Query_Array'
              (1 => (Single_Element_Query,
                    Asis.Expressions.Iterator_Specification'Access),
               2 => (Single_Element_Query,
                    Asis.Expressions.Predicate'Access));

      end case;
   end PARSE_Expression;

   ----------------
   -- PARSE_Path --
   ----------------

   --  PARSE_Path deals with every Path 's Element_Kind.
   --  That is to say, it deals with Path_Kinds :
   --    Not_A_Path,                    -- An unexpected element
   --    An_If_Path,                    -- 5.3:
   --                -- if condition then
   --                --   sequence_of_statements
   --    An_Elsif_Path,                 -- 5.3:
   --                -- elsif condition then
   --                --   sequence_of_statements
   --    An_Else_Path,                  -- 5.3, 9.7.1, 9.7.3:
   --                -- else sequence_of_statements
   --    A_Case_Path,                   -- 5.4:
   --                -- when discrete_choice_list =>
   --                --   sequence_of_statements
   --    A_Select_Path,                 -- 9.7.1:
   --                -- select [guard] select_alternative
   --                -- 9.7.2, 9.7.3:
   --                -- select entry_call_alternative
   --                -- 9.7.4:
   --                -- select triggering_alternative
   --    An_Or_Path,                    -- 9.7.1:
   --                -- or [guard] select_alternative
   --                -- 9.7.2:
   --                -- or delay_alternative
   --    A_Then_Abort_Path.             -- 9.7.4
   --                -- then abort sequence_of_statements
   --
   --  (See asis_element_kind.ads for more details)
   --
   function PARSE_Path (Ada_Path : Asis.Element) return Query_Array is
   begin
      case Asis.Elements.Path_Kind (Ada_Path) is
         when An_If_Path |
              An_Elsif_Path =>

            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Statements.Condition_Expression'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Sequence_Of_Statements'Access, True));

         when An_Else_Path |
              A_Then_Abort_Path =>

            return Query_Array'
              (1 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Sequence_Of_Statements'Access, True));

         when A_Case_Path =>

            return Query_Array'
              (1 => (Element_List_Query,
                    Asis.Statements.Case_Statement_Alternative_Choices'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Sequence_Of_Statements'Access, True));

         when A_Select_Path |
              An_Or_Path      =>

            return Query_Array'
              (1 => (Single_Element_Query, Asis.Statements.Guard'Access),
               2 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Sequence_Of_Statements'Access, True));

--  |A2012 start
         when A_Case_Expression_Path =>
            return Query_Array'
              (1 => (Element_List_Query,
                    Asis.Statements.Case_Statement_Alternative_Choices'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Dependent_Expression'Access));
         when An_If_Expression_Path |
              An_Elsif_Expression_Path =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Statements.Condition_Expression'Access),

               2 => (Single_Element_Query,
                     Asis.Expressions.Dependent_Expression'Access));

         when An_Else_Expression_Path =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Expressions.Dependent_Expression'Access));

         when Not_A_Path =>
            raise Internal_Implementation_Error;

      end case;

   end PARSE_Path;

   ---------------------
   -- PARSE_Statement --
   ---------------------

   function PARSE_Statement
     (Ada_Statement : Asis.Element)
      return          Query_Array
   is
   begin
      --  all statements can have one or several Labels
      case Asis.Elements.Statement_Kind (Ada_Statement) is
         when Not_A_Statement =>
            raise Internal_Implementation_Error;

         when A_Null_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access));

         when An_Assignment_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Assignment_Variable_Name'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.Assignment_Expression'Access));

         when An_If_Statement                    |
              A_Selective_Accept_Statement       |
              A_Timed_Entry_Call_Statement       |
              A_Conditional_Entry_Call_Statement |
              An_Asynchronous_Select_Statement    =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),
               2 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Statement_Paths'Access,
                     True));

         when A_Case_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Case_Expression'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Statement_Paths'Access,
                     True));

         when A_Loop_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Statement_Identifier'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Loop_Statements'Access,
                     True));

         when A_While_Loop_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Statement_Identifier'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.While_Condition'Access),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Loop_Statements'Access,
                     True));

         when A_For_Loop_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Statement_Identifier'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.For_Loop_Parameter_Specification'Access),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Loop_Statements'Access,
                     True));

         when A_Block_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Statement_Identifier'Access),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Block_Declarative_Items'Access,
                     True),

               4 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Block_Statements'Access,
                     True),

               5 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Block_Exception_Handlers'Access,
                     True));

         when An_Exit_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Exit_Loop_Name'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.Exit_Condition'Access));

         when A_Goto_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),
               2 => (Single_Element_Query, Asis.Statements.Goto_Label'Access));

         when A_Procedure_Call_Statement |
              An_Entry_Call_Statement     =>

            if Subprogram_Call_Needs_Reordering (Ada_Statement) then
               return Query_Array'
                 (1 => (Element_List_Query,
                        Asis.Statements.Label_Names'Access),
                  2 => (Single_Element_Query,
                        First_Parameter_Association'Access),
                  3 => (Single_Element_Query,
                        Asis.Statements.Called_Name'Access),
                  4 => (Element_List_Query,
                        All_But_First_Associations'Access));
            else
               return Query_Array'
                 (1 => (Element_List_Query,
                        Asis.Statements.Label_Names'Access),
                  2 => (Single_Element_Query,
                        Asis.Statements.Called_Name'Access),
                  3 => (Element_List_Query_With_Boolean,
                        Asis.Statements.Call_Statement_Parameters'Access,
                        False));
            end if;

         when A_Return_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Return_Expression'Access));

--  |A2005 start
         --  An_Extended_Return_Statement,        -- 6.5

         when An_Extended_Return_Statement =>
            return Query_Array'
              (1 => (Single_Element_Query,
                     Asis.Statements.Return_Object_Declaration'Access),

               2 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Extended_Return_Statements'Access,
                     True),

               3 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Extended_Return_Exception_Handlers'Access,
                     True));
--  |A2005 end

         when An_Accept_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Accept_Entry_Direct_Name'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.Accept_Entry_Index'Access),

               4 => (Element_List_Query,
                     Asis.Statements.Accept_Parameters'Access),

               5 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Accept_Body_Statements'Access, True),

               6 => (Element_List_Query_With_Boolean,
                     Asis.Statements.Accept_Body_Exception_Handlers'Access,
                     True));

         when A_Requeue_Statement |
              A_Requeue_Statement_With_Abort =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Requeue_Entry_Name'Access));

         when A_Delay_Until_Statement |
              A_Delay_Relative_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Delay_Expression'Access));

         when A_Terminate_Alternative_Statement =>

            return No_Query;

         when An_Abort_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Element_List_Query,
                     Asis.Statements.Aborted_Tasks'Access));

         when A_Raise_Statement =>

--  |A2005 start
            return Query_Array'
              (1 => (Element_List_Query,
                     Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Raised_Exception'Access),

               3 => (Single_Element_Query,
                     Asis.Statements.Associated_Message'Access));
--  |A2005 end

         when A_Code_Statement =>

            return Query_Array'
              (1 => (Element_List_Query, Asis.Statements.Label_Names'Access),

               2 => (Single_Element_Query,
                     Asis.Statements.Qualified_Expression'Access));

      end case;

   end PARSE_Statement;

   --  We've separated the functions to make the program clearer,
   --  but it is better to expand them inline ...
   pragma Inline (PARSE_Defining_Name);
   pragma Inline (PARSE_Declaration);
   pragma Inline (PARSE_Definition);
   pragma Inline (PARSE_Expression);
   pragma Inline (PARSE_Association);
   pragma Inline (PARSE_Statement);
   pragma Inline (PARSE_Path);
   pragma Inline (PARSE_Clause);

   --------------------------------------
   -- Subprogram_Call_Needs_Reordering --
   --------------------------------------

   function Subprogram_Call_Needs_Reordering
     (El   : Asis.Element)
      return Boolean
   is
      Result : Boolean := False;
   begin
      if Asis.Elements.Is_Prefix_Notation (El)
        or else
        (Asis.Elements.Expression_Kind (El) = A_Function_Call
         and then
          not Asis.Expressions.Is_Prefix_Call (El)
         and then
          Asis.Expressions.Function_Call_Parameters (El)'Length = 2)
      then
         Result := True;
      end if;

      return Result;
   end Subprogram_Call_Needs_Reordering;

end A4G.Queries;
