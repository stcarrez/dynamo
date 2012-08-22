------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A S I S . S T A T E M E N T S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
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

with Asis.Declarations; use Asis.Declarations;
with Asis.Elements;     use Asis.Elements;
with Asis.Errors;       use Asis.Errors;
with Asis.Exceptions;   use Asis.Exceptions;
with Asis.Expressions;  use Asis.Expressions;
with Asis.Extensions;   use Asis.Extensions;

with Asis.Set_Get;      use  Asis.Set_Get;

with A4G.A_Sem;         use A4G.A_Sem;
with A4G.A_Sinput;      use A4G.A_Sinput;
with A4G.Contt.UT;      use A4G.Contt.UT;
with A4G.Mapping;       use A4G.Mapping;
with A4G.Norm;          use A4G.Norm;
with A4G.Vcheck;        use A4G.Vcheck;
with A4G.Span_End;      use A4G.Span_End;

with Atree;             use Atree;
with Nlists;            use Nlists;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;

package body Asis.Statements is

   Package_Name : constant String := "Asis.Statements.";

   ---------------------------
   -- ASIS 2005 Draft stuff --
   ---------------------------

   ------------------------
   -- Associated_Message --
   ------------------------

   function Associated_Message
      (Statement : Asis.Statement)
       return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
   begin
      Check_Validity (Statement, Package_Name & "Associated_Message");

      if not (Arg_Kind = A_Raise_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Associated_Message",
            Wrong_Kind => Arg_Kind);
      end if;

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Node (Statement)),
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Associated_Message");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Associated_Message",
            Ex          => Ex,
            Arg_Element => Statement);
   end Associated_Message;

   ----------------------------------------
   -- Extended_Return_Exception_Handlers --
   ----------------------------------------

   function Extended_Return_Exception_Handlers
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Exception_Handler_List
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Resilt_List : List_Id                         := No_List;
   begin
      Check_Validity
        (Statement, Package_Name & "Extended_Return_Exception_Handlers");

      if not (Arg_Kind = An_Extended_Return_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Extended_Return_Exception_Handlers",
            Wrong_Kind => Arg_Kind);
      end if;

      if Present (Handled_Statement_Sequence (Node (Statement))) then
         Resilt_List :=
           Exception_Handlers (Handled_Statement_Sequence (Node (Statement)));
      end if;

      return N_To_E_List_New
               (List             => Resilt_List,
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name &
                             "Extended_Return_Exception_Handlers");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Extended_Return_Exception_Handlers",
            Ex          => Ex,
            Arg_Element => Statement);
   end Extended_Return_Exception_Handlers;

   --------------------------------
   -- Extended_Return_Statements --
   --------------------------------

   function Extended_Return_Statements
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Result_List :          List_Id                := No_List;
   begin
      Check_Validity (Statement, Package_Name & "Extended_Return_Statements");

      if not (Arg_Kind = An_Extended_Return_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Extended_Return_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      if Present (Handled_Statement_Sequence (Node (Statement))) then
         Result_List :=
           Sinfo.Statements (Handled_Statement_Sequence (Node (Statement)));
      end if;

      return N_To_E_List_New (List             => Result_List,
                              Include_Pragmas  => Include_Pragmas,
                              Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Extended_Return_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Extended_Return_Statements",
            Ex          => Ex,
            Arg_Element => Statement);
   end Extended_Return_Statements;

   -------------------------------
   -- Return_Object_Declaration --
   -------------------------------

   function Return_Object_Declaration
     (Statement : Asis.Statement)
      return      Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Result_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Return_Object_Declaration");

      if not (Arg_Kind = An_Extended_Return_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Return_Object_Declaration",
            Wrong_Kind => Arg_Kind);
      end if;

      Result_Node := First (Return_Object_Declarations (Node (Statement)));

      while Nkind (Result_Node) /= N_Object_Declaration loop
         --  It may be some internal subtypes here
         Result_Node := Next (Result_Node);
      end loop;

      return Node_To_Element_New (Node             => Result_Node,
                                  Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Return_Object_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Return_Object_Declaration",
            Ex          => Ex,
            Arg_Element => Statement);
   end Return_Object_Declaration;

------------------------------------------------------------------------------

   -------------------
   -- Aborted_Tasks --
   -------------------

   function Aborted_Tasks
     (Statement : Asis.Statement)
      return      Asis.Expression_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Aborted_Tasks");

      if not (Arg_Kind = An_Abort_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Aborted_Tasks",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return N_To_E_List_New
               (List             => Names (Arg_Node),
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Aborted_Tasks");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Aborted_Tasks",
            Ex          => Ex,
            Arg_Element => Statement);
   end Aborted_Tasks;

   ------------------------------------
   -- Accept_Body_Exception_Handlers --
   ------------------------------------

   function Accept_Body_Exception_Handlers
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Exception_Handler_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;

      Resilt_List : List_Id := No_List;
   begin

      Check_Validity (Statement,
                     Package_Name & "Accept_Body_Exception_Handlers");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Accept_Body_Exception_Handlers",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      if Present (Handled_Statement_Sequence (Arg_Node)) then
         Resilt_List :=
            Exception_Handlers (Handled_Statement_Sequence (Arg_Node));
      end if;

      return N_To_E_List_New (List             => Resilt_List,
                              Include_Pragmas  => Include_Pragmas,
                              Starting_Element => Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Accept_Body_Exception_Handlers");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Accept_Body_Exception_Handlers",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Accept_Body_Exception_Handlers;

   ----------------------------
   -- Accept_Body_Statements --
   ----------------------------

   function Accept_Body_Statements
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;

      Resilt_List : List_Id := No_List;
   begin

      Check_Validity (Statement, Package_Name & "Accept_Body_Statements");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Accept_Body_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      if Present (Handled_Statement_Sequence (Arg_Node)) then
         Resilt_List :=
            Sinfo.Statements (Handled_Statement_Sequence (Arg_Node));
      end if;

      return N_To_E_List_New
               (List             => Resilt_List,
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Accept_Body_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Accept_Body_Statements",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Accept_Body_Statements;

   ------------------------------
   -- Accept_Entry_Direct_Name --
   ------------------------------

   function Accept_Entry_Direct_Name
     (Statement : Asis.Statement)
      return      Asis.Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Accept_Entry_Direct_Name");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Accept_Entry_Direct_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (
               Node             => Entry_Direct_Name (Arg_Node),
               Internal_Kind    => An_Identifier,
               Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Accept_Entry_Direct_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Accept_Entry_Direct_Name",
            Ex          => Ex,
            Arg_Element => Statement);
   end Accept_Entry_Direct_Name;

   ------------------------
   -- Accept_Entry_Index --
   ------------------------

   function Accept_Entry_Index
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Accept_Entry_Index");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Accept_Entry_Index",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);
      Result_Node := Entry_Index (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Statement);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Accept_Entry_Index");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Accept_Entry_Index",
            Ex          => Ex,
            Arg_Element => Statement);
   end Accept_Entry_Index;

   -----------------------
   -- Accept_Parameters --
   -----------------------

   function Accept_Parameters
     (Statement : Asis.Statement)
      return      Asis.Parameter_Specification_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Accept_Parameters");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Accept_Parameters",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return N_To_E_List_New
               (List             => Parameter_Specifications (Arg_Node),
                Starting_Element => Statement,
                Internal_Kind    => A_Parameter_Specification);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Accept_Parameters");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Accept_Parameters",
            Ex          => Ex,
            Arg_Element => Statement);
   end Accept_Parameters;

   ---------------------------
   -- Assignment_Expression --
   ---------------------------

   function Assignment_Expression
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Assignment_Expression");

      if not (Arg_Kind = An_Assignment_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Assignment_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (
                       Node             => Sinfo.Expression (Arg_Node),
                       Starting_Element => Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Assignment_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Assignment_Expression",
            Ex          => Ex,
            Arg_Element => Statement);
   end Assignment_Expression;

   ------------------------------
   -- Assignment_Variable_Name --
   ------------------------------

   function Assignment_Variable_Name
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Assignment_Variable_Name");

      if not (Arg_Kind = An_Assignment_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Assignment_Variable_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (
                     Node             => Sinfo.Name (Arg_Node),
                     Starting_Element => Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Assignment_Variable_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Assignment_Variable_Name",
            Ex          => Ex,
            Arg_Element => Statement);
   end Assignment_Variable_Name;

   -----------------------------
   -- Block_Declarative_Items --
   -----------------------------

   function Block_Declarative_Items
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Declarative_Item_List
   is
      Arg_El   : Asis.Element;
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Block_Declarative_Items");

      if not (Arg_Kind = A_Block_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Block_Declarative_Items",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      Arg_El := Statement;

      if Special_Case (Arg_El) = A_Dummy_Block_Statement then
         Set_Special_Case (Arg_El, Not_A_Special_Case);
      end if;

      return N_To_E_List_New
               (List             => Sinfo.Declarations (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Arg_El);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Block_Declarative_Items");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Block_Declarative_Items",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Block_Declarative_Items;

   ------------------------------
   -- Block_Exception_Handlers --
   ------------------------------

   function Block_Exception_Handlers
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Exception_Handler_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
      Arg_El   : Asis.Element;
   begin
      Check_Validity (Statement, Package_Name & "Block_Exception_Handlers");

      if not (Arg_Kind = A_Block_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Block_Exception_Handlers",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      if Special_Case (Statement) = A_Dummy_Block_Statement and then
         No (Handled_Statement_Sequence (Arg_Node))
      then
         --  for the dummy block originated from the package_body_declaration
         --  having no handled_sequence_of_statements on its own.
         return Nil_Element_List;
      end if;

      Arg_El := Statement;

      if Special_Case (Arg_El) = A_Dummy_Block_Statement then
         Set_Special_Case (Arg_El, Not_A_Special_Case);
      end if;

      return N_To_E_List_New
               (List             =>
                   Exception_Handlers (Handled_Statement_Sequence (Arg_Node)),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Arg_El);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Block_Exception_Handlers");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Block_Exception_Handlers",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Block_Exception_Handlers;

   ----------------------
   -- Block_Statements --
   ----------------------

   function Block_Statements
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_El   : Asis.Element;
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Block_Statements");

      if not (Arg_Kind = A_Block_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Block_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      Arg_El := Statement;

      if Special_Case (Arg_El) = A_Dummy_Block_Statement then
         Set_Special_Case (Arg_El, Not_A_Special_Case);
      end if;

      return N_To_E_List_New
               (List             =>
                   Sinfo.Statements (Handled_Statement_Sequence (Arg_Node)),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Arg_El);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Block_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Block_Statements",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Block_Statements;

   -------------------------------
   -- Call_Statement_Parameters --
   -------------------------------

   function Call_Statement_Parameters
     (Statement  : Asis.Statement;
      Normalized : Boolean := False)
      return       Asis.Association_List
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node  : Node_Id;

      Res_Norm_Case : Normalization_Cases := Is_Not_Normalized;
      Res_Node_List : List_Id;
   begin

      Check_Validity (Statement, Package_Name & "Call_Statement_Parameters");

      Arg_Node := Node (Statement);

      if   (not (Arg_Kind = An_Entry_Call_Statement  or else
                 Arg_Kind = A_Procedure_Call_Statement))
         or else
           (Normalized and then Nkind (Arg_Node) = N_Attribute_Reference)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Call_Statement_Parameters",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_Prefix_Notation (Statement) then
         Arg_Node := R_Node (Statement);
      end if;

      if Normalized then
         Res_Norm_Case := Is_Normalized;
      end if;

      if Normalized and then
         Nkind (Arg_Node) /= N_Attribute_Reference
      then

         if No (Parameter_Associations (Arg_Node))
          or else
            Is_Nil (Corresponding_Called_Entity (Statement))
         then
            return Nil_Element_List;
         else
            return Normalized_Param_Associations (Call_Elem => Statement);
         end if;

      else

         if Nkind (Arg_Node) = N_Attribute_Reference then
            --  call to 'Output, 'Read or 'Write
            Res_Node_List := Sinfo.Expressions (Arg_Node);
         else
            Res_Node_List := Parameter_Associations (Arg_Node);
         end if;

         return N_To_E_List_New (List             => Res_Node_List,
                                 Internal_Kind    => A_Parameter_Association,
                                 Norm_Case        => Res_Norm_Case,
                                 Starting_Element => Statement);

      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Normalized,
               Outer_Call => Package_Name & "Call_Statement_Parameters");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Call_Statement_Parameters",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Normalized);
   end Call_Statement_Parameters;

   -----------------
   -- Called_Name --
   -----------------

   function Called_Name
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;

      --  local variables needed for processing calls to 'Output, 'Read
      --  and 'Write:
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
      Result_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Called_Name");

      if not (Arg_Kind = An_Entry_Call_Statement   or else
              Arg_Kind = A_Procedure_Call_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Called_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      if Is_Prefix_Notation (Statement) then
         Arg_Node := R_Node (Statement);
      else
         Arg_Node := Node (Statement);
      end if;

      if Nkind (Arg_Node) =  N_Attribute_Reference then
         --  calls like T'Output (...); T'Read (...) and T'Write (...)
         --  should be processed separately, and the result should
         --  be built on the same node as argument
         Result_Kind := Subprogram_Attribute_Kind (Arg_Node);
         Result_Node := Arg_Node;
      else
         Result_Node := Sinfo.Name (Arg_Node);
      end if;

      if Is_Rewrite_Substitution (Result_Node)
        and then
         Nkind (Result_Node) = N_Explicit_Dereference
        and then
         Nkind (Prefix (Result_Node)) = N_Function_Call
      then
         --  Needed to process cases like F (1), where F - parameterless
         --  function that returns access-to-subprogram result.
         Result_Node := Prefix (Result_Node);
      end if;

      return Node_To_Element_New (
                Starting_Element => Statement,
                Node             => Result_Node,
                Internal_Kind    => Result_Kind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Called_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Called_Name",
            Ex          => Ex,
            Arg_Element => Statement);
   end Called_Name;

   ---------------------
   -- Case_Expression --
   ---------------------

   function Case_Expression
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Case_Expression");

      if not (Arg_Kind = A_Case_Statement or else
              Arg_Kind = A_Case_Expression)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Case_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Case_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Case_Expression",
            Ex          => Ex,
            Arg_Element => Statement);
   end Case_Expression;

   ----------------------------------------
   -- Case_Statement_Alternative_Choices --
   ----------------------------------------

   function Case_Statement_Alternative_Choices
     (Path : Asis.Path)
      return Asis.Element_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Path);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Path, Package_Name & "Case_Statement_Alternative_Choices");

      if not (Arg_Kind = A_Case_Path) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Case_Statement_Alternative_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Path);

      return Discrete_Choice_Node_To_Element_List (
               Choice_List      => Discrete_Choices (Arg_Node),
               Starting_Element => Path);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Path,
               Outer_Call => Package_Name &
                             "Case_Statement_Alternative_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Case_Statement_Alternative_Choices",
            Ex          => Ex,
            Arg_Element => Path);
   end Case_Statement_Alternative_Choices;

   -----------------------------------
   -- Choice_Parameter_Specification --
   -------------------------------------

   function Choice_Parameter_Specification
     (Handler : Asis.Exception_Handler)
      return    Asis.Declaration
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Handler);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity
        (Handler, Package_Name & "Choice_Parameter_Specification");

      if not (Arg_Kind = An_Exception_Handler) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Choice_Parameter_Specification",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Handler);
      Result_Node := Choice_Parameter (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (
                  Node             => Result_Node,
                  Internal_Kind    => A_Choice_Parameter_Specification,
                  Starting_Element => Handler);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Handler,
               Outer_Call => Package_Name & "Choice_Parameter_Specification");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Choice_Parameter_Specification",
            Ex          => Ex,
            Arg_Element => Handler);
   end Choice_Parameter_Specification;

   --------------------------
   -- Condition_Expression --
   --------------------------

   function Condition_Expression (Path : Asis.Path) return Asis.Expression is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Path);
      Res_Node : Node_Id                         := Empty;
      Arg_Node : Node_Id;
   begin
      Check_Validity (Path, Package_Name & "Condition_Expression");

      if not (Arg_Kind = An_If_Path               or else
              Arg_Kind = An_Elsif_Path            or else
              Arg_Kind = An_If_Expression_Path    or else
              Arg_Kind = An_Elsif_Expression_Path)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Condition_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := R_Node (Path);

      case Arg_Kind is
         when An_If_Path    |
              An_Elsif_Path =>
            Res_Node := Condition (Arg_Node);
         when An_If_Expression_Path    |
              An_Elsif_Expression_Path =>
            Res_Node := Prev (Arg_Node);
         when others =>
            null;
      end case;

      return Node_To_Element_New (Node             => Res_Node,
                                  Starting_Element => Path);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Path,
               Outer_Call => Package_Name & "Condition_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Condition_Expression",
            Ex          => Ex,
            Arg_Element => Path);
   end Condition_Expression;

   ---------------------------------
   -- Corresponding_Called_Entity --
   ---------------------------------

   function Corresponding_Called_Entity
     (Statement : Asis.Statement)
      return      Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
   begin
      Check_Validity (Statement, Package_Name & "Corresponding_Called_Entity");

      if not (Arg_Kind = An_Entry_Call_Statement or else
              Arg_Kind = A_Procedure_Call_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Called_Entity",
            Wrong_Kind => Arg_Kind);
      end if;

      return Get_Corr_Called_Entity (Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Corresponding_Called_Entity");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Called_Entity",
            Ex          => Ex,
            Arg_Element => Statement);
   end Corresponding_Called_Entity;

   -----------------------------------------
   -- Corresponding_Destination_Statement --
   -----------------------------------------

   function Corresponding_Destination_Statement
     (Statement : Asis.Statement)
      return      Asis.Statement
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node  : Node_Id;
      Res_Label : Node_Id;
      Res_Stmt  : Node_Id;
   begin
      Check_Validity
        (Statement, Package_Name & "Corresponding_Destination_Statement");

      if not (Arg_Kind = A_Goto_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Destination_Statement",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := R_Node (Statement);

      if Is_Rewrite_Substitution (Arg_Node)
        and then
         Nkind (Arg_Node) = N_Loop_Statement
        and then
         Nkind (Original_Node (Arg_Node)) = N_Goto_Statement
      then
         --  goto statement is rewritten into infinite loop
         if not Is_Empty_List (Sinfo.Statements (Arg_Node)) then
            Res_Stmt := First (Sinfo.Statements (Arg_Node));
         else
            --  Pathological case:
            --
            --   <<Junk>> goto Junk;

            Res_Stmt := Arg_Node;
         end if;
      else
         Arg_Node := Node (Statement);

         Res_Label := Parent (Entity (Sinfo.Name (Arg_Node)));
         --  this is N_Implicit_Label_Declaration node representing the
         --  implicit declaration of the destination label
         Res_Stmt := Label_Construct (Res_Label);

         while not Is_Statement (Res_Stmt) loop
            Res_Stmt := Next (Res_Stmt);
         end loop;
         --  if we are in the tree corresponding to a successful compiler
         --  run, we shall for sure find a statement after any label!
      end if;

      return Node_To_Element_New (Node             => Res_Stmt,
                                  Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name &
                             "Corresponding_Destination_Statement");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Corresponding_Destination_Statement",
            Ex          => Ex,
            Arg_Element => Statement);
   end Corresponding_Destination_Statement;

   -------------------------
   -- Corresponding_Entry --
   -------------------------

   function Corresponding_Entry
     (Statement : Asis.Statement)
      return      Asis.Declaration
   is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node      : Node_Id;
      Res_Entry_Dcl : Node_Id;
      Result_Unit   : Compilation_Unit;
   begin
      Check_Validity (Statement, Package_Name & "Corresponding_Entry");

      if not (Arg_Kind = An_Accept_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Entry",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node      := Node (Statement);
      Res_Entry_Dcl := Parent (Entity (Entry_Direct_Name (Arg_Node)));
      Result_Unit   := Enclosing_Unit
                          (Encl_Cont_Id (Statement), Res_Entry_Dcl);

      return Node_To_Element_New (Node    => Res_Entry_Dcl,
                                  In_Unit => Result_Unit);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Corresponding_Entry");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Entry",
            Ex          => Ex,
            Arg_Element => Statement);
   end Corresponding_Entry;

   -------------------------------
   -- Corresponding_Loop_Exited --
   -------------------------------

   function Corresponding_Loop_Exited
     (Statement : Asis.Statement)
      return      Asis.Statement
   is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node  : Node_Id;
      Res_Loop  : Node_Id;
      Loop_Name : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Corresponding_Loop_Exited");

      if not (Arg_Kind = An_Exit_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Corresponding_Loop_Exited",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node  := Node (Statement);
      Loop_Name := Sinfo.Name (Arg_Node);

      if Present (Loop_Name) then
         --  we simply jump to the result loop:
         Loop_Name := Parent (Entity (Loop_Name));
         --  here we are in the implicit declaration of the loop name
         Res_Loop := Label_Construct (Loop_Name);
      else
         --  here we have to traverse the tree up to the first enclosing
         --  loop statement
         Res_Loop := Parent (Arg_Node);

         while Nkind (Res_Loop) /= N_Loop_Statement loop
            Res_Loop := Parent (Res_Loop);
         end loop;

      end if;

      return Node_To_Element_New (Node             => Res_Loop,
                                  Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Corresponding_Loop_Exited");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Loop_Exited",
            Ex          => Ex,
            Arg_Element => Statement);
   end Corresponding_Loop_Exited;

   ----------------------
   -- Delay_Expression --
   ----------------------

   function Delay_Expression
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Delay_Expression");

      if not (Arg_Kind = A_Delay_Until_Statement
              or else
                Arg_Kind = A_Delay_Relative_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Delay_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New
               (Node             => Sinfo.Expression (Arg_Node),
                Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Delay_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Delay_Expression",
            Ex          => Ex,
            Arg_Element => Statement);
   end Delay_Expression;

   -----------------------
   -- Exception_Choices --
   -----------------------

   function Exception_Choices
     (Handler : Asis.Exception_Handler)
      return    Asis.Element_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Handler);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Handler, Package_Name & "Exception_Choices");

      if not (Arg_Kind = An_Exception_Handler) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Exception_Choices",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Handler);

      return N_To_E_List_New
               (List             => Exception_Choices (Arg_Node),
                Starting_Element =>  Handler);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Handler,
               Outer_Call => Package_Name & "Exception_Choices");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Exception_Choices",
            Ex          => Ex,
            Arg_Element => Handler);
   end Exception_Choices;

   --------------------
   -- Exit_Condition --
   --------------------

   function Exit_Condition
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Exit_Condition");

      if not (Arg_Kind = An_Exit_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Exit_Loop_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);
      Result_Node := Condition (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Statement);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Exit_Condition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Exit_Condition",
            Ex          => Ex,
            Arg_Element => Statement);
   end Exit_Condition;

   --------------------
   -- Exit_Loop_Name --
   --------------------

   function Exit_Loop_Name
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Exit_Loop_Name");

      if not (Arg_Kind = An_Exit_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Exit_Loop_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);
      Result_Node := Sinfo.Name (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Statement);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Exit_Loop_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Exit_Loop_Name",
            Ex          => Ex,
            Arg_Element => Statement);
   end Exit_Loop_Name;

   --------------------------------------
   -- For_Loop_Parameter_Specification --
   --------------------------------------

   function For_Loop_Parameter_Specification
     (Statement : Asis.Statement)
      return      Asis.Declaration
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Res_Node : Node_Id;
      Res_Kind : Internal_Element_Kinds := Not_An_Element;
   begin
      Check_Validity
        (Statement, Package_Name & "For_Loop_Parameter_Specification");

      if not (Arg_Kind = A_For_Loop_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "For_Loop_Parameter_Specification",
            Wrong_Kind => Arg_Kind);
      end if;

      Res_Node := Iteration_Scheme (Node (Statement));

      if Present (Iterator_Specification (Res_Node)) then
         Res_Node := Iterator_Specification (Res_Node);

         if Of_Present (Res_Node) then
            Res_Kind := An_Element_Iterator_Specification;
         else
            Res_Kind := A_Generalized_Iterator_Specification;
         end if;

      elsif Present (Loop_Parameter_Specification (Res_Node)) then
         Res_Node := Loop_Parameter_Specification (Res_Node);
         Res_Kind := A_Loop_Parameter_Specification;

      else
         null;
         pragma Assert (False);
      end if;

      return Node_To_Element_New (
             Node             => Res_Node,
             Internal_Kind    => Res_Kind,
             Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name &
                             "For_Loop_Parameter_Specification");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "For_Loop_Parameter_Specification",
            Ex          => Ex,
            Arg_Element => Statement);
   end For_Loop_Parameter_Specification;

   ----------------
   -- Goto_Label --
   ----------------

   function Goto_Label
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Goto_Label");

      if not (Arg_Kind = A_Goto_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Goto_Label",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (Node             => Sinfo.Name (Arg_Node),
                                  Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Goto_Label");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Goto_Label",
            Ex          => Ex,
            Arg_Element => Statement);
   end Goto_Label;

   -----------
   -- Guard --
   -----------

   function Guard (Path : Asis.Path) return Asis.Expression is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Path);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Path, Package_Name & "Guard");

      if not (Arg_Kind = A_Select_Path or else
              Arg_Kind = An_Or_Path)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Guard",
            Wrong_Kind => Arg_Kind);
      end if;

      if not (Nkind (Parent (R_Node (Path))) = N_Selective_Accept) then
         return Nil_Element;
      end if;

      Arg_Node := Node (Path);
      Result_Node := Condition (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else

         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Path);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Path,
               Outer_Call => Package_Name & "Guard");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Guard",
            Ex          => Ex,
            Arg_Element => Path);
   end Guard;

   ------------------------
   -- Handler_Statements --
   ------------------------

   function Handler_Statements
     (Handler         : Asis.Exception_Handler;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Handler);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Handler, Package_Name & "Handler_Statements");

      if not (Arg_Kind = An_Exception_Handler) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Handler_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Handler);

      return N_To_E_List_New
               (List             => Sinfo.Statements (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Handler);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Handler,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Handler_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Handler_Statements",
            Ex          => Ex,
            Arg_Element => Handler,
            Bool_Par_ON => Include_Pragmas);
   end Handler_Statements;

   --------------------------------------
   -- Is_Call_On_Dispatching_Operation --
   --------------------------------------

   function Is_Call_On_Dispatching_Operation
     (Call : Asis.Element)
      return Boolean
   is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Call);
      Called_Entity : Asis.Element;
   begin
      --  Just the first version, should be tested more carefully!
      --  Is currently implemented as a secondary query based on
      --  some queries from Asis.Extensions.
      --  ??? !!!
      --  Still depends on partially implemented queries from
      --  Asis.Extensions
      Check_Validity (Call, Package_Name & "Is_Call_On_Dispatching_Operation");

      if not (Arg_Kind = A_Function_Call or else
              Arg_Kind = A_Procedure_Call_Statement)
      then
         return False;
      end if;

      if Arg_Kind = A_Function_Call then
         Called_Entity := Corresponding_Called_Function (Call);
      else
         Called_Entity := Corresponding_Called_Entity (Call);
      end if;

      if Is_Nil (Called_Entity) or else
         (not Is_Dispatching_Operation (Called_Entity))
      then
         return False;
      else
         return True;
      end if;

      --  Owning_Type      := Primary_Owner (Called_Entity);
      --  Owning_Type      := Type_Declaration_View (Owning_Type);

      --  Owning_Type_Kind := Int_Kind (Owning_Type);

      --  return
      --    (Owning_Type_Kind = A_Tagged_Private_Type_Definition      or else
      --     Owning_Type_Kind = A_Private_Extension_Definition        or else
      --     Owning_Type_Kind = A_Derived_Record_Extension_Definition or else
      --     Owning_Type_Kind = A_Tagged_Record_Type_Definition);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Call,
               Outer_Call => Package_Name &
                            "Is_Call_On_Dispatching_Operation");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Call_On_Dispatching_Operation",
            Ex          => Ex,
            Arg_Element => Call);
   end Is_Call_On_Dispatching_Operation;

   ----------------------
   -- Is_Declare_Block --
   ----------------------

   function Is_Declare_Block
     (Statement : Asis.Statement)
      return      Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;

      First_Letter : Character;
      --  the first character of the statement, should be either
      --  B[egin] or D[eclare]
   begin

      --  If the argument represents the dummy block statement created by
      --  the Asis_Declarations.Body_Block_Statement (obsolescent!)
      --  function, the result will be True if and only if the
      --  corresponding body has any declarative item on its own.

      Check_Validity (Statement, Package_Name & "Is_Declare_Block");

      if not (Arg_Kind = A_Block_Statement) then

         return False;
      end if;

      Arg_Node := Node (Statement);

      if Special_Case (Statement) = A_Dummy_Block_Statement then

         if Present (Sinfo.Declarations (Arg_Node)) then
            return True;
         else
            return False;
         end if;

      else
         --  a "normal" block statement: here we should be more accurate, and
         --  we cannot rely on "Present (Declarations (Arg_Node))" approach
         --  because of the implicit label declarations

         First_Letter := Source_Text (Get_Source_File_Index (
                  Sloc (Arg_Node)))       -- the unit's text buffer
                   (Sloc (Arg_Node));

         case First_Letter is
            when 'b' | 'B'   =>
               return False;
            when 'd' | 'D'   =>
               return True;
            when others      =>
               --  Unexpected beginning of the block statement
               raise Internal_Implementation_Error;
         end case;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Is_Declare_Block");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Declare_Block",
            Ex          => Ex,
            Arg_Element => Statement);
   end Is_Declare_Block;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call (Call : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Call);
      Arg_Node : Node_Id := R_Node (Call);
   begin

      Check_Validity (Call, Package_Name & "Is_Dispatching_Call");

      if not (Arg_Kind = A_Function_Call or else
              Arg_Kind = A_Procedure_Call_Statement)
      then
         return False;
      end if;

      if Is_Prefix_Notation (Call)
        and then
         Nkind (Arg_Node) = N_Explicit_Dereference
        and then
         Is_Rewrite_Substitution (Arg_Node)
        and then
         Nkind (Original_Node (Arg_Node)) = N_Function_Call
      then
         Arg_Node := Prefix (Arg_Node);
      end if;

      if not (Nkind (Arg_Node) = N_Function_Call or else
              Nkind (Arg_Node) = N_Procedure_Call_Statement)
      then
         --  this may be possible as a result of tree rewriting, but if we
         --  have rewriting, we do not have a dispatching call, so:
         return False;
      else
         return Present (Controlling_Argument (Arg_Node));
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Call,
               Outer_Call => Package_Name & "Is_Dispatching_Call");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Dispatching_Call",
            Ex          => Ex,
            Arg_Element => Call);
   end Is_Dispatching_Call;

   ----------------------
   -- Is_Name_Repeated --
   ----------------------

   function Is_Name_Repeated (Statement : Asis.Statement) return Boolean is
      Arg_Kind  : constant Internal_Element_Kinds := Int_Kind (Statement);
      Last_Comp : Asis.Element;
      S         : Source_Ptr;
      Result    : Boolean;
   begin
      Check_Validity (Statement, Package_Name & "Is_Name_Repeated");

      if not (Arg_Kind = A_Loop_Statement       or else
              Arg_Kind = A_While_Loop_Statement or else
              Arg_Kind = A_For_Loop_Statement   or else
              Arg_Kind = A_Block_Statement      or else
              Arg_Kind = An_Accept_Statement)
      then
         Result := False;
      end if;

      if Arg_Kind = A_Loop_Statement       or else
         Arg_Kind = A_While_Loop_Statement or else
         Arg_Kind = A_For_Loop_Statement   or else
         Arg_Kind = A_Block_Statement
      then
         Result := not Asis.Elements.Is_Nil (Statement_Identifier (Statement));
      elsif Arg_Kind = An_Accept_Statement then

         if Is_Nil (Accept_Body_Statements (Statement, True)) then
            --  no statements - no "do .. end;" part - no "end"
            --  to repeat the name after
            Result := False;
         else
            Last_Comp := Get_Last_Component (Statement);

            S := Set_Image_End (Last_Comp);
            --  now S points to the last character (it for sure is ';')
            --  of the last component (a statement, an exception
            --  handler or pragma) in the accept statement.
            --  First, we reset S to point onto the first character
            --  after the final end of the accept statement:
            --  the final "end" lexically is an identifier, so:

            S := Next_Identifier (S);
            S := S + 3;
            --  the first character after "end"
            S := Rightmost_Non_Blank (S);
            --  and the final check - what follows the final "end"

            if Get_Character (S) = ';' then
               Result := False;
            else
               Result := True;
            end if;

         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Is_Name_Repeated");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Name_Repeated",
            Ex          => Ex,
            Arg_Element => Statement);
   end Is_Name_Repeated;

   -----------------
   -- Label_Names --
   -----------------

   function Label_Names
     (Statement : Asis.Statement)
      return      Asis.Defining_Name_List
   is
      Arg_Kind      : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node      : Node_Id;
      Parent_Node   : Node_Id;
      Labels_Number : Nat := 0;  -- how many labels the statement has
      Label_Node    : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Label_Names");

      if not (Arg_Kind in Internal_Statement_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Label_Names",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      --  First, process a special case when an infinite loop is programmed as
      --
      --  <<Target>> Stmt;
      --  ...
      --  goto Target;
      --
      --  If Stmt has exactly one label attached to it, the front-end rewrites
      --  this construct as a subtree headed by N_Loop_Statement node

      Parent_Node := Parent (R_Node (Statement));

      if Nkind (Parent_Node) = N_Loop_Statement
        and then
         Is_Rewrite_Substitution (Parent_Node)
        and then
         Nkind (Original_Node (Parent_Node)) = N_Goto_Statement
        and then
         Arg_Node = First (Sinfo.Statements (Parent_Node))
      then
         return
           (1 => Node_To_Element_New
                   (Node             => Sinfo.Identifier (Parent_Node),
                    Internal_Kind    => A_Defining_Identifier,
                    Starting_Element => Statement));

      elsif Nkind (Arg_Node) = N_Goto_Statement
         and then
            Nkind (R_Node (Statement)) = N_Loop_Statement
         and then
            Is_Empty_List (Sinfo.Statements (R_Node (Statement)))
      then
         --  This is a pathological case of
         --
         --   <<Target>> goto Target;
         return
           (1 => Node_To_Element_New
                   (Node             => Sinfo.Identifier (R_Node (Statement)),
                    Internal_Kind    => A_Defining_Identifier,
                    Starting_Element => Statement));
      end if;

      if not Is_List_Member (Arg_Node) then
      --  the accept statement in accept alternative, it cannot
      --  have labels at all
         return Nil_Element_List;
      end if;

      Label_Node := Prev (Arg_Node);

      while Nkind (Label_Node) in N_Raise_xxx_Error loop
         --  See B920-A06
         Label_Node := Prev (Label_Node);
      end loop;

      while Nkind (Label_Node) = N_Label loop
         Labels_Number := Labels_Number + 1;
         Label_Node    := Prev (Label_Node);
      end loop;

      --  Label_Node is not the Node of N_Label kind now

      if Labels_Number = 0 then
         return Nil_Element_List;
      else
         declare

            Result_List : Asis.Element_List
                          (1 .. ASIS_Integer (Labels_Number));

         begin

            if Label_Node = Empty then
               --  special case: the first statement in the statement
               --  sequence is labeled
               Label_Node := First (List_Containing (Arg_Node));
            else
               Label_Node := Next (Label_Node);
            end if;

            --  the first label attached to the statement and the number of
            --  attached labels are obtained

            for I in 1 .. ASIS_Integer (Labels_Number) loop
            --  the order of labels is important !
               Result_List (I) := Node_To_Element_New (
                     Node             => Label_Node,
                     Internal_Kind    => A_Defining_Identifier,
                     Starting_Element => Statement);

               Label_Node    := Next (Label_Node);

            end loop;

            return Result_List;

         end;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Label_Names");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Label_Names",
            Ex          => Ex,
            Arg_Element => Statement);
   end Label_Names;

   ---------------------
   -- Loop_Statements --
   ---------------------

   function Loop_Statements
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Loop_Statements");

      if not (Arg_Kind = A_Loop_Statement       or else
              Arg_Kind = A_While_Loop_Statement or else
              Arg_Kind = A_For_Loop_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           ("Asis_Statement.Loop_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return N_To_E_List_New
               (List             => Sinfo.Statements (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Loop_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Loop_Statements",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Loop_Statements;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   function Qualified_Expression
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Qualified_Expression");

      if not (Arg_Kind = A_Code_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Qualified_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (
               Node             => Sinfo.Expression (Arg_Node),
               Internal_Kind    => A_Qualified_Expression,
               Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Qualified_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Qualified_Expression",
            Ex          => Ex,
            Arg_Element => Statement);
   end Qualified_Expression;

   ----------------------
   -- Raised_Exception --
   ----------------------

   function Raised_Exception
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Raised_Exception");

      if not (Arg_Kind = A_Raise_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Raised_Exception",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);
      Result_Node := Sinfo.Name (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (
                  Node             => Result_Node,
                  Starting_Element => Statement);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Raised_Exception");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Raised_Exception",
            Ex          => Ex,
            Arg_Element => Statement);
   end Raised_Exception;

   ------------------------
   -- Requeue_Entry_Name --
   ------------------------

   function Requeue_Entry_Name
     (Statement : Asis.Statement)
      return      Asis.Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Requeue_Entry_Name");

      if not (Arg_Kind = A_Requeue_Statement
              or else
                Arg_Kind = A_Requeue_Statement_With_Abort)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Requeue_Entry_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (Node             => Sinfo.Name (Arg_Node),
                                  Starting_Element => Statement);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Requeue_Entry_Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Requeue_Entry_Name",
            Ex          => Ex,
            Arg_Element => Statement);
   end Requeue_Entry_Name;

   -----------------------
   -- Return_Expression --
   -----------------------

   function Return_Expression
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
   begin

      Check_Validity (Statement, Package_Name & "Return_Expression");

      if not (Arg_Kind = A_Return_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Return_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);
      Result_Node := Sinfo.Expression (Arg_Node);

      if No (Result_Node) then
         return Nil_Element;
      else
         return Node_To_Element_New (Node             => Result_Node,
                                     Starting_Element => Statement);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Return_Expression");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Return_Expression",
            Ex          => Ex,
            Arg_Element => Statement);
   end Return_Expression;

   ----------------------------
   -- Sequence_Of_Statements --
   ----------------------------

   function Sequence_Of_Statements
     (Path            : Asis.Path;
      Include_Pragmas : Boolean := False)
      return            Asis.Statement_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Path);
      Arg_Node : Node_Id;

      Result_List    : List_Id;
      --  for processing An_If_Path, An_Elsif_Path, An_Else_Path, A_Case_Path
      --  and A_Then_Abort_Path arguments; the node of such argument has
      --  regular structure

      --  local variables for processing A_Select_Path and An_Or_Path
      --  arguments; the node of such arguments has irregular structure
      Statement_List        : List_Id;
      First_Element         : Asis.Element := Nil_Element;
      Alternative_Node_Kind : Node_Kind;

   begin

      Check_Validity (Path, Package_Name & "Sequence_Of_Statements");

      if not (Arg_Kind in Internal_Statement_Path_Kinds) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Sequence_Of_Statements",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Path);

      case Arg_Kind is
         when An_If_Path | An_Elsif_Path =>

            Result_List := Then_Statements (Arg_Node);
         when An_Else_Path  =>

            Result_List := Else_Statements (Arg_Node);
         when A_Case_Path   =>

            Result_List := Sinfo.Statements (Arg_Node);
         when A_Then_Abort_Path =>

            Result_List := Sinfo.Statements (Arg_Node);

         when A_Select_Path | An_Or_Path =>

            Alternative_Node_Kind := Nkind (Arg_Node);

            if Alternative_Node_Kind = N_Terminate_Alternative then

            --  special case: result list contains only one dummy terminate
            --  statement; no tree traversing needed: the result is based
            --  on the same node as the argument

               return Asis.Statement_List'(
                1 => Node_To_Element_New (
                         Node             => Arg_Node,
                         Internal_Kind    => A_Terminate_Alternative_Statement,
                         Starting_Element => Path));

            else
               --  this alternative corresponds to the situation of
               --  N_Accept_Alternative, N_Delay_Alternative,
               --  N_Entry_Call_Alternative or N_Triggering_Alternative

               --  forming the first element of the element list to be
               --  returned:

               if Alternative_Node_Kind =  N_Accept_Alternative then
                  First_Element :=
                     Node_To_Element_New (
                        Node             => Accept_Statement (Arg_Node),
                        Internal_Kind    => An_Accept_Statement,
                        Starting_Element => Path);

               elsif Alternative_Node_Kind = N_Delay_Alternative then
                  First_Element :=
                     Node_To_Element_New (
                        Node             => Delay_Statement (Arg_Node),
                        Starting_Element => Path);

               elsif Alternative_Node_Kind = N_Entry_Call_Alternative then
                  First_Element :=
                     Node_To_Element_New (
                        Node             => Entry_Call_Statement (Arg_Node),
                        Starting_Element => Path);

               elsif Alternative_Node_Kind = N_Triggering_Alternative then
                  First_Element :=
                     Node_To_Element_New (
                        Node             => Triggering_Statement (Arg_Node),
                        Starting_Element => Path);
               end if;

               --  the rest of the returned list:

               Statement_List := Sinfo.Statements (Arg_Node);

               return Asis.Statement_List'(1      => First_Element) &
                      N_To_E_List_New
                        (List             => Statement_List,
                         Include_Pragmas  => Include_Pragmas,
                         Starting_Element => Path);

            end if;

         when others =>
               null;
      end case;

      return N_To_E_List_New (List             => Result_List,
                              Include_Pragmas  => Include_Pragmas,
                              Starting_Element => Path);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Path,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Sequence_Of_Statements");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Sequence_Of_Statements",
            Ex          => Ex,
            Arg_Element => Path,
            Bool_Par_ON => Include_Pragmas);
   end Sequence_Of_Statements;

   --------------------------
   -- Statement_Identifier --
   --------------------------

   function Statement_Identifier
     (Statement : Asis.Statement)
      return      Asis.Defining_Name
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "Statement_Identifier");

      if not (Arg_Kind = A_Loop_Statement       or else
              Arg_Kind = A_While_Loop_Statement or else
              Arg_Kind = A_For_Loop_Statement   or else
              Arg_Kind = A_Block_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           ("Asis_Statement.Statement_Identifier",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      if Special_Case (Statement) = A_Dummy_Block_Statement or else
         Has_Created_Identifier (Arg_Node)
      then

         return Nil_Element;
      else

         return Node_To_Element_New (
                        Node             => Sinfo.Identifier (Arg_Node),
                        Internal_Kind    => A_Defining_Identifier,
                        Starting_Element => Statement);
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "Statement_Identifier");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Statement_Identifier",
            Ex          => Ex,
            Arg_Element => Statement);
   end Statement_Identifier;

   ---------------------
   -- Statement_Paths --
   ---------------------

   function Statement_Paths
     (Statement       : Asis.Statement;
      Include_Pragmas : Boolean := False)
      return            Asis.Path_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;

      Path_List_Length : ASIS_Integer;  -- Length of returned list
      Elsif_Or_Length  : ASIS_Integer;  -- Number of Elsif or Or paths
      Else_Present     : Boolean;

   begin

      Check_Validity (Statement, Package_Name & "Statement_Paths");

      if not (Arg_Kind = An_If_Statement                    or else
              Arg_Kind = A_Case_Statement                   or else
              Arg_Kind = A_Selective_Accept_Statement       or else
              Arg_Kind = A_Timed_Entry_Call_Statement       or else
              Arg_Kind = A_Conditional_Entry_Call_Statement or else
              Arg_Kind = An_Asynchronous_Select_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Statement_Paths",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      case  Arg_Kind is
         when An_If_Statement =>

            Path_List_Length := 1; -- An_If_Path
            Else_Present := Present (Else_Statements (Arg_Node));

            if Else_Present then
               Path_List_Length := Path_List_Length + 1;
            end if;

            if Present (Elsif_Parts (Arg_Node)) then
               Elsif_Or_Length := ASIS_Integer
                                    (List_Length (Elsif_Parts (Arg_Node)));
            else
               Elsif_Or_Length := 0;
            end if;

            Path_List_Length := Path_List_Length + Elsif_Or_Length;

            declare
               Path_List : Asis.Element_List (1 .. Path_List_Length);
               --  Element List to be returned by the function
            begin

               Path_List (1) := Node_To_Element_New
                       (Node             => Arg_Node,
                        Internal_Kind    => An_If_Path,
                        Starting_Element => Statement);

               Path_List (2 .. Elsif_Or_Length + 1) :=
                  N_To_E_List_New
                    (List             => Elsif_Parts (Arg_Node),
                     Internal_Kind    => An_Elsif_Path,
                     Starting_Element => Statement);

               if Else_Present then
                  Path_List (Path_List_Length) := Node_To_Element_New
                       (Node             => Arg_Node,
                        Internal_Kind    => An_Else_Path,
                        Starting_Element => Statement);

               end if;

               return Path_List;
            end;

         when A_Case_Statement =>
            --  only here the value of Include_Pragmas is important

            return N_To_E_List_New
                     (List             => Alternatives (Arg_Node),
                      Include_Pragmas  => Include_Pragmas,
                      Starting_Element => Statement);

         when A_Selective_Accept_Statement =>

            Elsif_Or_Length := ASIS_Integer
                (List_Length (Select_Alternatives (Arg_Node)));

            Path_List_Length := Elsif_Or_Length;

            Else_Present := Present (Else_Statements (Arg_Node));

            if Else_Present then
               Path_List_Length := Path_List_Length + 1;
            end if;

            declare
               Path_List : Asis.Element_List (1 .. Path_List_Length);
               --  Element List to be returned by the function
            begin

               Path_List (1 .. Elsif_Or_Length) :=
                  N_To_E_List_New
                    (List             => Select_Alternatives (Arg_Node),
                     Starting_Element => Statement);

               if Else_Present then
                  Path_List (Path_List_Length) := Node_To_Element_New
                       (Node             => Arg_Node,
                        Internal_Kind    => An_Else_Path,
                        Starting_Element => Statement);

               end if;

               return Path_List;
            end;

         when A_Timed_Entry_Call_Statement =>

            return Asis.Path_List'(
             1 =>  Node_To_Element_New (
                     Node             => Entry_Call_Alternative (Arg_Node),
                     Internal_Kind    => A_Select_Path,
                     Starting_Element => Statement),

             2 =>  Node_To_Element_New (
                     Node             => Delay_Alternative (Arg_Node),
                     Internal_Kind    => An_Or_Path,
                     Starting_Element => Statement));

         when A_Conditional_Entry_Call_Statement =>

            return Asis.Path_List'(
             1 =>  Node_To_Element_New (
                     Node             => Entry_Call_Alternative (Arg_Node),
                     Internal_Kind    => A_Select_Path,
                     Starting_Element => Statement),

             2 =>  Node_To_Element_New (
                     Node             => Arg_Node,
                     Internal_Kind    => An_Else_Path,
                     Starting_Element => Statement));

         when An_Asynchronous_Select_Statement =>
            return Asis.Path_List'(
             1 =>  Node_To_Element_New (
                     Node             => Triggering_Alternative (Arg_Node),
                     Internal_Kind    => A_Select_Path,
                     Starting_Element => Statement),

             2 =>  Node_To_Element_New (
                     Node             => Abortable_Part (Arg_Node),
                     Internal_Kind    => A_Then_Abort_Path,
                     Starting_Element => Statement));

         when others =>
            raise Internal_Implementation_Error;
            --  this choice can never be reached, see the condition
            --  for defining the appropriate element
      end case;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Bool_Par   => Include_Pragmas,
               Outer_Call => Package_Name & "Statement_Paths");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Statement_Paths",
            Ex          => Ex,
            Arg_Element => Statement,
            Bool_Par_ON => Include_Pragmas);
   end Statement_Paths;

   ---------------------
   -- While_Condition --
   ---------------------

   function While_Condition
     (Statement : Asis.Statement)
      return      Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Statement);
      Arg_Node : Node_Id;
   begin
      Check_Validity (Statement, Package_Name & "While_Condition");

      if not (Arg_Kind = A_While_Loop_Statement) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "While_Condition",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Statement);

      return Node_To_Element_New (
                Node             => Condition (Iteration_Scheme (Arg_Node)),
                Starting_Element => Statement);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Statement,
               Outer_Call => Package_Name & "While_Condition");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "While_Condition",
            Ex          => Ex,
            Arg_Element => Statement);
   end While_Condition;

end Asis.Statements;
