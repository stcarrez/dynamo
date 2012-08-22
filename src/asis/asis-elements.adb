------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A S I S . E L E M E N T S                         --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with GNAT.HTable;

with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Declarations;       use Asis.Declarations;
with Asis.Definitions;
with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Extensions;
with Asis.Limited_Views;      use Asis.Limited_Views;
with Asis.Statements;

with Asis.Set_Get;            use  Asis.Set_Get;

with A4G.A_Output;            use A4G.A_Output;
with A4G.A_Sem;               use A4G.A_Sem;
with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.A_Types;
with A4G.Asis_Tables;         use A4G.Asis_Tables;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Encl_El;             use A4G.Encl_El;
with A4G.Knd_Conv;            use A4G.Knd_Conv;
with A4G.Mapping;             use A4G.Mapping;
with A4G.Vcheck;              use A4G.Vcheck;

with Atree;                   use Atree;
with Einfo;                   use Einfo;
with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Sinfo;                   use Sinfo;
with Sinput;                  use Sinput;
with Snames;                  use Snames;
with Stand;                   use Stand;

package body Asis.Elements is

   function "=" (Left, Right : Element) return Boolean
      renames Asis.Set_Get."=";

   Package_Name : constant String := "Asis.Elements.";

------------------------------------------------------------------------------

   ---------------------------
   -- ASIS 2005 Draft stuff --
   ---------------------------

   ----------------------------
   -- Access_Definition_Kind --
   ----------------------------

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return       Asis.Access_Definition_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Access_Definition_Kind");

      return Access_Definition_Kind_From_Internal (Int_Kind (Definition));
   end Access_Definition_Kind;

   --------------------
   -- Interface_Kind --
   --------------------

   function Interface_Kind
     (Definition : Asis.Definition)
      return       Asis.Interface_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Interface_Kind");

      return Interface_Kind_From_Internal (Int_Kind (Definition));
   end Interface_Kind;

   ------------------------
   -- Is_Not_Null_Return --
   ------------------------

   function Is_Not_Null_Return
     (Element : Asis.Element)
      return    Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node : Node_Id                         := Node (Element);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Is_Not_Null_Return");

      case Arg_Kind is
         when A_Function_Declaration          |
              A_Function_Body_Declaration     |
              A_Function_Renaming_Declaration |
              A_Function_Body_Stub            |
              A_Generic_Function_Declaration  |
              A_Formal_Function_Declaration   =>

            Arg_Node := Specification (Arg_Node);
            Result   := Null_Exclusion_Present (Arg_Node);

         when An_Access_To_Function                     |
              An_Access_To_Protected_Function           |
              An_Anonymous_Access_To_Function           |
              An_Anonymous_Access_To_Protected_Function =>

            Result   := Null_Exclusion_Present (Arg_Node);

         when others =>
            null;
      end case;

      return Result;
   end Is_Not_Null_Return;

   ------------------------
   -- Is_Prefix_Notation --
   ------------------------

   function Is_Prefix_Notation (Call : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Call);
      Arg_Node : constant Node_Id                := R_Node (Call);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Call, Package_Name & "Is_Prefix_Notation");

      if Arg_Kind = A_Procedure_Call_Statement then

         if Is_Rewrite_Substitution (Arg_Node)
           and then
            Nkind (Original_Node (Arg_Node)) = Nkind (Arg_Node)
           and then
            Nkind (Sinfo.Name (Arg_Node)) = N_Identifier
           and then
             Nkind (Sinfo.Name (Original_Node (Arg_Node))) =
               N_Selected_Component
         then
            Result := True;
         end if;

      elsif Arg_Kind = A_Function_Call
           and then
            Is_Rewrite_Substitution (Arg_Node)
      then
         --  If prefix notation is used for a function call, the corresponding
         --  A_Function_Call element is based on the rewritten node and the
         --  original node is not used at all
         if Node (Call) = Arg_Node then
            Result := Is_Rewritten_Function_Prefix_Notation (Arg_Node);
         elsif Nkind (Arg_Node) = N_Explicit_Dereference then
            --  This is a case of *implicit* dereference :(
            Result :=
              Is_Rewritten_Impl_Deref_Function_Prefix_Notation (Node (Call));
         end if;

      end if;

      return Result;
   end Is_Prefix_Notation;

   -----------------------------------
   -- From ARG ASIS 2005 definition --
   -----------------------------------

   ------------------
   -- Has_Abstract --
   ------------------

   function Has_Abstract (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node :          Node_Id;
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Abstract");

      case Arg_Kind is
         when An_Ordinary_Type_Declaration =>
            Arg_Node := Sinfo.Type_Definition (Node (Element));
         when A_Formal_Procedure_Declaration          |
              A_Formal_Function_Declaration           |
              A_Function_Declaration                  |
              A_Private_Type_Declaration              |
              A_Private_Extension_Declaration         |
              A_Procedure_Declaration                 |
              A_Private_Extension_Definition          |
              A_Tagged_Private_Type_Definition        |
              Internal_Type_Kinds                     |
              A_Formal_Tagged_Private_Type_Definition |
              A_Formal_Derived_Type_Definition        =>
            Arg_Node := Node (Element);
         when others =>
            return False;
      end case;

      Result := Nkind (Arg_Node) = N_Abstract_Subprogram_Declaration
              or else
                Nkind (Arg_Node) = N_Formal_Abstract_Subprogram_Declaration
              or else
                Abstract_Present (Arg_Node);

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Abstract",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Abstract;

   -----------------
   -- Has_Aliased --
   -----------------

   function Has_Aliased (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Aliased");

      case Arg_Kind is
         when A_Constant_Declaration          |
              A_Deferred_Constant_Declaration |
              A_Return_Variable_Specification |
              A_Return_Constant_Specification |
              A_Variable_Declaration          |
              A_Component_Definition          |
              A_Parameter_Specification       =>
            Result := Aliased_Present (Node (Element));
         when others =>
            return False;
      end case;

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Aliased",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Aliased;

   -----------------
   -- Has_Limited --
   -----------------

   function Has_Limited (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node :          Node_Id;
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Limited");

      case Arg_Kind is
         when A_With_Clause                           |
              A_Private_Type_Declaration              |
              A_Private_Extension_Declaration         |
              Internal_Type_Kinds                     |
              A_Private_Type_Definition               |
              A_Tagged_Private_Type_Definition        |
              A_Private_Extension_Definition          |
              A_Formal_Private_Type_Definition        |
              A_Formal_Tagged_Private_Type_Definition |
              A_Formal_Derived_Type_Definition        =>
            Arg_Node := Node (Element);

         when An_Ordinary_Type_Declaration =>
            Arg_Node := Sinfo.Type_Definition (Node (Element));

         when others => return False;
      end case;

      Result := Limited_Present (Arg_Node);

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Limited",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Limited;

   -----------------
   -- Has_Private --
   -----------------

   function Has_Private (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
   begin
      Check_Validity (Element, Package_Name & "Has_Private");

      --  There is no case when a type definition element may contain PRIVATE
      case Arg_Kind is
         when A_With_Clause =>
            return Private_Present (Node (Element));
         when A_Private_Extension_Declaration         |
              A_Private_Type_Declaration              |
              A_Private_Type_Definition               |
              A_Tagged_Private_Type_Definition        |
              A_Private_Extension_Definition          |
              A_Formal_Private_Type_Definition        |
              A_Formal_Tagged_Private_Type_Definition =>
            return True;
         when others => return False;
      end case;

   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Private",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Private;

   -------------------
   -- Has_Protected --
   -------------------

   function Has_Protected (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
   begin
      Check_Validity (Element, Package_Name & "Has_Protected");

      --  If our interpretation is correct, there is nothing to compute here,
      --  and the result is completely defined by the argument kind

      case Arg_Kind is
         when A_Protected_Definition         |
              A_Protected_Body_Declaration   |
              A_Protected_Type_Declaration   |
              A_Single_Protected_Declaration |
              A_Protected_Body_Stub          |
              A_Protected_Interface          |
              A_Formal_Protected_Interface   =>
            return True;
         when others =>
            return False;
      end case;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Protected",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Protected;

   -----------------
   -- Has_Reverse --
   -----------------

   function Has_Reverse (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node :          Node_Id;
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Reverse");

      case Arg_Kind is
         when A_Loop_Parameter_Specification       |
              A_Generalized_Iterator_Specification |
              An_Element_Iterator_Specification    =>
            Arg_Node := Node (Element);

            Result := Reverse_Present (Arg_Node);
         when others =>
            null;
      end case;

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Reverse",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Reverse;

   ----------------------
   -- Has_Synchronized --
   ----------------------

   function Has_Synchronized (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Synchronized");

      case Arg_Kind is
         when A_Synchronized_Interface        |
              A_Formal_Synchronized_Interface =>
            Result := True;
         when A_Private_Extension_Definition   |
              A_Formal_Derived_Type_Definition =>
            Result := Synchronized_Present (R_Node (Element));
         when others =>
            null;
      end case;

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Synchronized",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Synchronized;

   ----------------
   -- Has_Tagged --
   ----------------

   function Has_Tagged (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Tagged");

      case Arg_Kind is
         when A_Tagged_Incomplete_Type_Declaration    |
              A_Tagged_Private_Type_Definition        |
              A_Tagged_Record_Type_Definition         |
              A_Formal_Tagged_Private_Type_Definition =>
            Result := True;
         when A_Formal_Incomplete_Type_Declaration =>
            Result :=
              Tagged_Present (Sinfo.Formal_Type_Definition (Node (Element)));
         when others =>
            null;
      end case;

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Tagged",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Tagged;

   ---------------
   -- Has_Task --
   ---------------

   function Has_Task (Element : Asis.Element) return Boolean is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Result   :          Boolean                := False;
   begin
      Check_Validity (Element, Package_Name & "Has_Task");

      case Arg_Kind is
         when A_Task_Definition         |
              A_Task_Type_Declaration   |
              A_Single_Task_Declaration |
              A_Task_Body_Declaration   |
              A_Task_Body_Stub          |
              A_Task_Interface          |
              A_Formal_Task_Interface   =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Task",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Task;

   ------------------------
   -- Has_Null_Exclusion --
   ------------------------

   function Has_Null_Exclusion
     (Element : Asis.Element)
      return    Boolean
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Node :          Node_Id;
   begin
      Check_Validity (Element, Package_Name & "Has_Null_Exclusion");

      case Arg_Kind is
         when Internal_Access_Definition_Kinds |
              Internal_Access_Type_Kinds       |
              A_Parameter_Specification         =>
            Arg_Node := Node (Element);
         when A_Discriminant_Specification =>
            Arg_Node := Node (Element);

            if not Null_Exclusion_Present (Arg_Node) then
               Arg_Node := Discriminant_Type (Arg_Node);

               if Nkind (Arg_Node) /= N_Access_Definition then
                  return False;
               end if;
            end if;
         when An_Object_Renaming_Declaration |
              A_Formal_Object_Declaration    =>
            Arg_Node := Node (Element);

            if not Null_Exclusion_Present (Arg_Node)
              and then
               Present (Access_Definition (Arg_Node))
            then
               Arg_Node := Access_Definition (Arg_Node);
            end if;
         when A_Subtype_Indication =>
            Arg_Node := Parent (Node (Element));
         when others =>
            return False;
      end case;

      return Null_Exclusion_Present (Arg_Node);
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Has_Null_Exclusion",
            Ex          => Ex,
            Arg_Element => Element);
   end Has_Null_Exclusion;

------------------------------------------------------------------------------

   function Unit_Declaration
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Declaration
   is
      Unit_Kind             : Asis.Unit_Kinds;
      Unit_Declaration_Node : Node_Id;
      Special_Case          : Special_Cases := Not_A_Special_Case;
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Unit_Declaration");

      Reset_Context (Encl_Cont_Id (Compilation_Unit));
      Unit_Kind := Kind (Compilation_Unit);

      if Unit_Kind = Not_A_Unit then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Unit_Declaration");
      end if;

      if Unit_Kind = A_Nonexistent_Declaration or else
         Unit_Kind = A_Nonexistent_Body        or else
         Unit_Kind = An_Unknown_Unit           or else
         Unit_Kind = A_Configuration_Compilation
      then
         return Nil_Element;
      end if;

      if Is_Standard (Compilation_Unit) then
         Special_Case          := Explicit_From_Standard;
         Unit_Declaration_Node := Standard_Package_Node;
      else
         Unit_Declaration_Node := Unit (Top (Compilation_Unit));
      end if;

      if Has_Limited_View_Only (Compilation_Unit) then
         Special_Case := From_Limited_View;
      end if;

      if  Unit_Kind = A_Procedure_Body_Subunit or else
          Unit_Kind = A_Function_Body_Subunit  or else
          Unit_Kind = A_Package_Body_Subunit   or else
          Unit_Kind = A_Task_Body_Subunit      or else
          Unit_Kind = A_Protected_Body_Subunit
      then
         --  one step down the tree is required. No Asis Element can correspond
         --  to the N_Subunit Node
         Unit_Declaration_Node := Proper_Body (Unit_Declaration_Node);
      end if;

      return Node_To_Element_New (Node      => Unit_Declaration_Node,
                                  Spec_Case => Special_Case,
                                  In_Unit   => Compilation_Unit);
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Unit_Declaration");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Unit_Declaration",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Unit_Declaration;
-----------------------------------------------------------------------------

   function Enclosing_Compilation_Unit
     (Element : Asis.Element)
      return    Asis.Compilation_Unit
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
   begin

      Check_Validity (Element, Package_Name & "Enclosing_Compilation_Unit");

      if Arg_Kind = Not_An_Element then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Enclosing_Compilation_Unit",
            Wrong_Kind => Arg_Kind);
      end if;

      return Encl_Unit (Element);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Enclosing_Compilation_Unit");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Enclosing_Compilation_Unit",
            Ex          => Ex,
            Arg_Element => Element);
   end Enclosing_Compilation_Unit;
-----------------------------------------------------------------------------

   function Context_Clause_Elements
     (Compilation_Unit : Asis.Compilation_Unit;
      Include_Pragmas  : Boolean := False)
      return             Asis.Context_Clause_List
   is
      Unit_Kind   : Asis.Unit_Kinds; -- Compilation_Unit kind
      List_Before : List_Id;
   begin
      Check_Validity
        (Compilation_Unit, Package_Name & "Context_Clause_Elements");

      Unit_Kind := Kind (Compilation_Unit);

      if Unit_Kind = Not_A_Unit then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Context_Clause_Elements");
      end if;

      if Is_Standard (Compilation_Unit)          or else
         Unit_Kind = A_Nonexistent_Declaration   or else
         Unit_Kind = A_Nonexistent_Body          or else
         Unit_Kind = An_Unknown_Unit             or else
         Unit_Kind = A_Configuration_Compilation or else
         Has_Limited_View_Only (Compilation_Unit)
      then
         --  The last part of the condition comes from the GNAT compilation
         --  model. But it seems that it should be in the definition of
         --  this query in the ASIS Standard
         return Nil_Element_List;
      end if;

      List_Before := Context_Items (Top (Compilation_Unit));

      return N_To_E_List_New
        (List             => List_Before,
         Include_Pragmas  => Include_Pragmas,
         In_Unit          => Compilation_Unit);
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Context_Clause_Elements",
              Bool_Par    => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Context_Clause_Elements",
            Ex          => Ex,
            Arg_CU      => Compilation_Unit,
            Bool_Par_ON => Include_Pragmas);
   end Context_Clause_Elements;
------------------------------------------------------------------------------

   function Configuration_Pragmas
     (The_Context : Asis.Context)
      return        Asis.Pragma_Element_List
   is
   begin
      Check_Validity (The_Context, Package_Name & "Configuration_Pragmas");

      --  In the GNAT environment, "a list of pragmas that apply to all future
      --  compilation_unit elements compiled into The_Context" is defined by
      --  the -gnatA and -gnatec options used when calling the compiler and
      --  by the content of configuration file(s) at the moment of the compiler
      --  call. These things cannot be detected from the set of tree files
      --  making up the Context, so the only thing we can do is to return
      --  Nil_Element_List

      return Nil_Element_List;

   exception

      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Configuration_Pragmas");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Configuration_Pragmas",
            Ex         => Ex);
   end Configuration_Pragmas;
-----------------------------------------------------------------------------

   function Compilation_Pragmas
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Pragma_Element_List
   is
      Unit_Kind      : Asis.Unit_Kinds;

      Config_Prgms : List_Id := No_List;
      List_Before  : List_Id;
      Next_Pragma  : Node_Id;
      List_After   : List_Id;
   begin

      Check_Validity (Compilation_Unit,
                      Package_Name & "Compilation_Pragmas");

      Unit_Kind := Kind (Compilation_Unit);

      if Unit_Kind = Not_A_Unit then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Package_Name & "Compilation_Pragmas");
      end if;

      if Is_Standard (Compilation_Unit)          or else
         Unit_Kind = A_Nonexistent_Declaration   or else
         Unit_Kind = A_Nonexistent_Body          or else
         Unit_Kind = An_Unknown_Unit             or else
         Unit_Kind = A_Configuration_Compilation or else
         Has_Limited_View_Only (Compilation_Unit)
      then
         --  The last part of the condition is GNAT-specific
         return Nil_Element_List;
      end if;

      Reset_Context (Encl_Cont_Id (Compilation_Unit));

      --  For the GNAT compilation model, we consider that configuration
      --  pragmas from the configuration file(s) are applied to the main unit
      --  of the compilation only. So if some unit belonging to the Context
      --  is compiled only as a supporter of some other units, but not on
      --  their own, the result of Compilation_Pragmas applied to this unit
      --  does not include any configuration pragmas from the configuration
      --  file(s).

      if Asis.Extensions.Is_Main_Unit_In_Tree (Compilation_Unit) then
         Reset_Main_Tree (Compilation_Unit);

         Config_Prgms :=
           Config_Pragmas (Aux_Decls_Node (Top (Compilation_Unit)));
      end if;

      List_Before := Context_Items (Top (Compilation_Unit));

      List_After  := Pragmas_After (Aux_Decls_Node (Top (Compilation_Unit)));

      Set_Element_List
        (List             => Config_Prgms,
         Include_Pragmas  => True,
         Node_Knd         => N_Pragma,
         Special_Case     => Configuration_File_Pragma,
         In_Unit          => Compilation_Unit,
         Append           => False);

      --  The middle part - pragmas from the context clause - we have to
      --  compose by hands, because we can add to the result only the
      --  configuration pragmas

      if Present (List_Before) then

         Next_Pragma := First (List_Before);

         Next_Pragma := Get_Next_Configuration_Pragma (Next_Pragma);

         while Present (Next_Pragma) loop

            Internal_Asis_Element_Table.Append
              (Node_To_Element_New
                 (Node    => Next_Pragma,
                  In_Unit => Compilation_Unit));

            Next_Pragma := Get_Next_Configuration_Pragma (Next (Next_Pragma));

         end loop;

      end if;

      Set_Element_List
        (List             => List_After,
         Include_Pragmas  => True,
         Node_Knd         => N_Pragma,
         In_Unit          => Compilation_Unit,
         Append           => True);

      return Asis.Pragma_Element_List
               (Internal_Asis_Element_Table.Table
                  (1 .. Internal_Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Pragmas");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Compilation_Pragmas",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Compilation_Pragmas;
------------------------------------------------------------------------------

   function Element_Kind
     (Element : Asis.Element)
      return    Asis.Element_Kinds
   is
   begin
      Check_Validity (Element, Package_Name & "Element_Kind");

      return Kind (Element);
   end Element_Kind;
-----------------------------------------------------------------------------

   function Pragma_Kind
     (Pragma_Element : Asis.Pragma_Element)
      return           Asis.Pragma_Kinds
   is
   begin
      Check_Validity (Pragma_Element, Package_Name & "Pragma_Kind");

      return Pragma_Kind_From_Internal (Int_Kind (Pragma_Element));
   end Pragma_Kind;
-----------------------------------------------------------------------------

   function Defining_Name_Kind
     (Defining_Name : Asis.Defining_Name)
      return          Asis.Defining_Name_Kinds
   is
   begin
      Check_Validity (Defining_Name, Package_Name & "Defining_Name_Kind");

      return Defining_Name_Kind_From_Internal (Int_Kind (Defining_Name));
   end Defining_Name_Kind;
-----------------------------------------------------------------------------

   function Declaration_Kind
     (Declaration : Asis.Declaration)
      return        Asis.Declaration_Kinds
   is
   begin
      Check_Validity (Declaration, Package_Name & "Declaration_Kind");

      return Declaration_Kind_From_Internal (Int_Kind (Declaration));
   end Declaration_Kind;

   ----------------
   -- Trait_Kind --
   ----------------

   function Trait_Kind (Element : Asis.Element) return Asis.Trait_Kinds is

   --  Trait-related flag values:

      Is_Abstract : Boolean;
      Is_Limited  : Boolean;
      Is_Aliased  : Boolean;
      Is_Private  : Boolean;

      Arg_Node : Node_Id;
      Result   : Asis.Trait_Kinds := An_Ordinary_Trait;

   begin

      Check_Validity (Element, Package_Name & "Trait_Kind");

--  ASIS_Element_Kinds.Trait_Kinds literals and GNAT tree flags mapping:
--
--  (This nice piece of documentation is for ASIS/Ada 95 only, we have not
--  extended it for Ada 2005)
--
--  type Trait_Kinds is (
--
--    Not_A_Trait,     --> Unexpected element, its node always has no
--                     --  corresponding flags and its kind does not belong
--                     --  to the Node Kinds for which A_Private_Trait could
--                     --  be determined
--
--    An_Ordinary_Trait, -->  all flags are set off, and the node kind
--                            does not belong to the Node Kinds for which
--                            A_Private_Trait could be determined
--
--    An_Aliased_Trait,  -->  Aliased_Present set ON
--
--    An_Access_Definition_Trait, --> no special flag, could be defined
--                                --  on the base of the presence of
--                                --  the N_Access_Definition node as the
--                                --  child node of the argument node
--
--    A_Reverse_Trait,  --> Reverse_Present set ON
--
--    A_Private_Trait,  --> except the case of
--                          A_Formal_Derived_Type_Definition,
--                      --  no special flag is presented in the corresponding
--                      --  node, the A_Private_Trait could be defined
--                      --  on the base of Node Kinds and setting other
--                      --  flags OFF;
--                      --  for A_Formal_Derived_Type_Definition -
--                      --  Private_Present set ON
--
--    A_Limited_Trait,  --> Limited_Present set ON and corresponding node
--                      --  does not belong to the Node Kinds for which
--                      --  A_Private_Trait could be defined
--
--    A_Limited_Private_Trait, --> Limited_Present set ON and corresponding
--                             -- node belongs to the Node Kinds for which
--                             -- A_Private_Trait could be defined
--
--    An_Abstract_Trait, --> For types: Abstract_Present set ON and
--                       --  corresponding node does not belong to the
--                       --  Node Kinds for which A_Private_Trait could be
--                       --  defined;
--                       --  For subprograms: no special flag, could be
--                       --  defined on the base of the Node Kind of the
--                       --  argument node
--
--    An_Abstract_Private_Trait, --> except the case of
--                               --  A_Formal_Derived_Type_Definition,
--                               --  Abstract_Present set ON and corresponding
--                               --  node belongs to the Node Kinds for which
--                               --  A_Private_Trait could be defined;
--                               --  for A_Formal_Derived_Type_Definition -
--                               --  Abstract_Present set ON and
--                               --  Private_Present set ON
--
--    An_Abstract_Limited_Trait, --> Abstract_Present set ON,
--                               --  Limited_Present set ON
--                               --  and corresponding node does not belong
--                               --  to the Node Kinds for which
--                               --  A_Private_Trait could be defined
--
--    An_Abstract_Limited_Private_Trait); --> Abstract_Present set ON,
--                                        --  Limited_Present set ON and
--                                        --  corresponding node belongs
--                                        --  to Node Kinds for which
--                                        --  A_Private_Trait could be defined
--
----------------------------------------------------------------------------
--  Expected Argument_Kinds:     ->       Corresponding tree Nodes:
--   Possible Trait values:     -->         Provided trait-related flags and
--                                          combination of their values
--                                          corresponding to the Trait value
----------------------------------------------------------------------------
--
--  Expected Declaration_Kinds:
--  ==========================
--
--     A_Private_Type_Declaration      -> N_Private_Type_Declaration (*1*)
--      A_Private_Trait                   --> Abstract_Present = OFF
--                                            Limited_Present  = OFF
--
--      A_Limited_Private_Trait           --> Abstract_Present = OFF
--                                            Limited_Present  = ON
--
--      An_Abstract_Private_Trait         --> Abstract_Present = ON
--                                            Limited_Present  = OFF
--
--      An_Abstract_Limited_Private_Trait --> Abstract_Present = ON
--                                            Limited_Present  = ON
-----------------------------------------------
--     A_Private_Extension_Declaration -> N_Private_Extension_Declaration (*2*)
--      A_Private_Trait                   --> Abstract_Present = OFF
--
--      An_Abstract_Private_Trait         --> Abstract_Present = ON
-----------------------------------------------
--     A_Variable_Declaration          -> N_Object_Declaration      (*3*)
--      An_Ordinary_Trait                 --> Aliased_Present = OFF
--
--      An_Aliased_Trait                  --> Aliased_Present = ON
-----------------------------------------------
--     A_Constant_Declaration          -> N_Object_Declaration      (*3*)
--      An_Ordinary_Trait                 --> Aliased_Present = OFF
--
--      An_Aliased_Trait                  --> Aliased_Present = ON
-----------------------------------------------
--     A_Deferred_Constant_Declaration -> N_Object_Declaration      (*3*)
--      An_Ordinary_Trait                 --> Aliased_Present = OFF
--
--      An_Aliased_Trait                  --> Aliased_Present = ON
-----------------------------------------------
--     A_Discriminant_Specification    -> N_Discriminant_Specification  (*4*)
--                                            Has no trait-related flags
--
--      An_Ordinary_Trait --> Nkind(Discriminant_Type(Definition.Node))
--                                   /=  N_Access_Definition
--      An_Access_Definition_Trait--> Nkind(Discriminant_Type(Definition.Node))
--                                   =   N_Access_Definition
-----------------------------------------------
--     A_Loop_Parameter_Specification  -> N_Loop_Parameter_Specification (*5*)
--     A_Generalized_Iterator_Specification -> N_Iterator_Specification  (*5*)
--     An_Element_Iterator_Specification -> N_Iterator_Specification     (*5*)
--
--      An_Ordinary_Trait                 --> Reverse_Present = OFF
--
--      A_Reverse_Trait                   --> Reverse_Present = ON
-----------------------------------------------
--     A_Procedure_Declaration         -> N_Subprogram_Declaration (*6*)
--      An_Ordinary_Trait  --> No flag needed to determine the trait
--                                     -> N_Abstract_Subprogram_Declaration
--      An_Abstract_Trait  --> No flag needed to determine the trait
-----------------------------------------------
--     A_Function_Declaration          -> N_Subprogram_Declaration  (*6*)
--      An_Ordinary_Trait  --> No flag needed to determine the trait
--                                     -> N_Abstract_Subprogram_Declaration
--      An_Abstract_Trait  --> No flag needed to determine the trait
-----------------------------------------------
--     A_Parameter_Specification       -> N_Parameter_Specification  (*4*)
--                                            Has no trait-related flags
--
--      An_Ordinary_Trait --> Nkind(Parameter_Type(Definition.Node))
--                                   /=  N_Access_Definition
--      An_Access_Definition_Trait --> Nkind(Parameter_Type(Definition.Node))
--                                   =   N_Access_Definition
-----------------------------------------------
--
--  Expected Definition_Kinds:
--  =========================
--
--     A_Component_Definition          -> N_Subtype_Indication    (*10*)
--                                        N_Identifier
--                                        N_Expanded_Name
--      An_Ordinary_Trait --> Aliased_Present set OFF in the PARENT node
--      An_Aliased_Trait  --> Aliased_Present set  ON in the PARENT nod
--
--     A_Private_Type_Definition       -> N_Private_Type_Declaration  (*1*)
--      The situation is just the same as for A_Private_Type_Declaration
-----------------------------------------------
--     A_Tagged_Private_Type_Definition-> N_Private_Type_Declaration  (*1*)
--      The situation is just the same as for A_Private_Type_Declaration
-----------------------------------------------
--     A_Private_Extension_Definition  -> N_Private_Extension_Declaration (*2*)
--      The situation is just the same as for N_Private_Extension_Declaration
-----------------------------------------------
--
--  Expected Type_Kinds:
--  ===================
--
-----------------------------------------------
--     A_Derived_Type_Definition             -> N_Derived_Type_Definition (*7*)
--      An_Ordinary_Trait                       --> Abstract_Present = OFF
--
--      An_Abstract_Trait                       --> Abstract_Present = ON
-----------------------------------------------
--     A_Derived_Record_Extension_Definition -> N_Derived_Type_Definition (*7*)
--      An_Ordinary_Trait                       --> Abstract_Present = OFF
--
--      An_Abstract_Trait                       --> Abstract_Present = ON
-----------------------------------------------
--     A_Record_Type_Definition              -> N_Record_Definition      (*8*)
--      An_Ordinary_Trait                       --> Abstract_Present = OFF
--                                                  Limited_Present  = OFF
--
--      An_Abstract_Trait                       --> Abstract_Present = ON
--                                                  Limited_Present  = OFF
--
--      A_Limited_Trait                         --> Abstract_Present = OFF
--                                                  Limited_Present  = ON
--
--      An_Abstract_Limited_Trait               --> Abstract_Present = ON
--                                                  Limited_Present  = ON
-----------------------------------------------
--     A_Tagged_Record_Type_Definition       -> N_Record_Definition      (*8*)
--      An_Ordinary_Trait                       --> Abstract_Present = OFF
--                                                  Limited_Present  = OFF
--
--      An_Abstract_Trait                       --> Abstract_Present = ON
--                                                  Limited_Present  = OFF
--
--      A_Limited_Trait                         --> Abstract_Present = OFF
--                                                  Limited_Present  = ON
--
--      An_Abstract_Limited_Trait               --> Abstract_Present = ON
--                                                  Limited_Present  = ON
-----------------------------------------------
--
--  Expected Formal_Type_Kinds:
--  ==========================
--
--     A_Formal_Private_Type_Definition -> N_Formal_Private_Type_Definition
--        (*1*)
--      The situation is just the same as for A_Private_Type_Declaration
-----------------------------------------------
--     A_Formal_Tagged_Private_Type_Definition ->
--        N_Formal_Private_Type_Definition (*1*)

--
--      The situation is just the same as for A_Private_Type_Declaration
-----------------------------------------------
--    A_Formal_Derived_Type_Definition -> N_Formal_Derived_Type_Definition(*9*)
--     An_Ordinary_Trait                       --> Abstract_Present = OFF
--                                                 Private_Present  = OFF
--
--     An_Abstract_Trait                       --> Abstract_Present = ON
--                                                 Private_Present  = OFF
--
--     A_Private_Trait                         --> Abstract_Present = OFF
--                                                 Private_Present  = ON
--
--     An_Abstract_Private_Trait               --> Abstract_Present = ON
--                                                 Private_Present  = ON
------------------------------------------------------------------------------

      Arg_Node := Node (Element);

      case Int_Kind (Element) is

      --  expected argument:

      when -- (*1*)
            A_Private_Type_Declaration
          | A_Private_Type_Definition
          | A_Tagged_Private_Type_Definition
          | A_Formal_Private_Type_Definition
          | A_Formal_Tagged_Private_Type_Definition =>

            Is_Abstract := Abstract_Present (Arg_Node);
            Is_Limited  := Limited_Present  (Arg_Node);

            if Is_Abstract and Is_Limited then
               Result := An_Abstract_Limited_Private_Trait;
            elsif Is_Abstract then
               Result := An_Abstract_Private_Trait;
            elsif Is_Limited then
               Result := A_Limited_Private_Trait;
            else
               Result := A_Private_Trait;
            end if;

      when -- (*2*)
            A_Private_Extension_Declaration
          | A_Private_Extension_Definition =>

            Is_Abstract := Abstract_Present (Arg_Node);

            if Is_Abstract then
               Result := An_Abstract_Private_Trait;
            else
               Result := A_Private_Trait;
            end if;

      when -- (*3*)
            A_Variable_Declaration
          | A_Constant_Declaration
          | A_Deferred_Constant_Declaration  =>

            Is_Aliased := Aliased_Present (Arg_Node);

            if Is_Aliased then
               Result := An_Aliased_Trait;
            end if;

      when -- (*4*)
            A_Discriminant_Specification
          | A_Parameter_Specification =>

--  --|A2005 start
            if Null_Exclusion_Present (Arg_Node) then
               Result := A_Null_Exclusion_Trait;
            elsif Int_Kind (Element) = A_Parameter_Specification
                and then
                  Aliased_Present (Arg_Node)
            then
               Result := An_Aliased_Trait;
            end if;
--  --|A2005 end

      when -- (*5*)
            A_Loop_Parameter_Specification       |
            A_Generalized_Iterator_Specification |
            An_Element_Iterator_Specification    =>

            if Reverse_Present (Arg_Node) then
               Result := A_Reverse_Trait;
            end if;

      when -- (*6*)
            A_Procedure_Declaration
          | A_Function_Declaration =>

            if Nkind (Arg_Node) = N_Abstract_Subprogram_Declaration then
               Result := An_Abstract_Trait;
            end if;
--  --|A2005 start
      when
            A_Formal_Procedure_Declaration
          | A_Formal_Function_Declaration =>

            if Nkind (Arg_Node) = N_Formal_Abstract_Subprogram_Declaration then
               Result := An_Abstract_Trait;
            end if;
--  --|A2005 end

      when -- (*7*)
            A_Derived_Type_Definition
          | A_Derived_Record_Extension_Definition =>

            if Abstract_Present (Arg_Node) then
               Result := An_Abstract_Trait;
            end if;

      when -- (*8*)
            A_Record_Type_Definition
          | A_Tagged_Record_Type_Definition =>

            Is_Abstract := Abstract_Present (Arg_Node);
            Is_Limited  := Limited_Present  (Arg_Node);

            if Is_Abstract and Is_Limited then
               Result := An_Abstract_Limited_Trait;
            elsif Is_Abstract then
               Result := An_Abstract_Trait;
            elsif Is_Limited then
               Result := A_Limited_Trait;
            end if;

      when -- (*9*)
            A_Formal_Derived_Type_Definition =>

            Is_Abstract := Abstract_Present (Arg_Node);
            Is_Limited  := Limited_Present  (Arg_Node);
            Is_Private  := Private_Present  (Arg_Node);

            if Is_Abstract and Is_Limited and Is_Private then
               Result := An_Abstract_Limited_Private_Trait;
            elsif Is_Abstract and Is_Limited then
               Result := An_Abstract_Limited_Trait;
            elsif Is_Abstract and Is_Private then
               Result := An_Abstract_Private_Trait;
            elsif Is_Limited and Is_Private then
               Result := A_Limited_Private_Trait;
            elsif Is_Abstract then
               Result := An_Abstract_Trait;
            elsif Is_Limited then
               Result := A_Limited_Trait;
            elsif Is_Private then
               Result := A_Private_Trait;
            end if;

      when -- (*10*)
            A_Component_Definition =>

            if Aliased_Present (R_Node (Element)) then
               Result := An_Aliased_Trait;
            end if;

--  --|A2005 start
      when A_With_Clause =>
            Is_Limited := Limited_Present  (Arg_Node);
            Is_Private := Private_Present (Arg_Node);

            if Is_Limited then
               if Is_Private then
                  Result := A_Limited_Private_Trait;
               else
                  Result := A_Limited_Trait;
               end if;
            elsif Is_Private then
               Result := A_Private_Trait;
            end if;
      when Internal_Access_Type_Kinds =>

            if Null_Exclusion_Present (Arg_Node) then
               Result := A_Null_Exclusion_Trait;
            end if;

      when Internal_Access_Definition_Kinds =>

            if Present (Sinfo.Access_To_Subprogram_Definition (Arg_Node)) then
               Arg_Node := Sinfo.Access_To_Subprogram_Definition (Arg_Node);
            end if;

            if Null_Exclusion_Present (Arg_Node) then
               Result := A_Null_Exclusion_Trait;
            end if;

      when A_Subtype_Indication =>
            Arg_Node := Parent (Arg_Node);

            if Null_Exclusion_Present (Arg_Node) then
               Result := A_Null_Exclusion_Trait;
            end if;

--  --|A2005 end

      when others =>
         --  unexpected argument:
         Result := Not_A_Trait;
      end case;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Trait_Kind");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Trait_Kind",
            Ex          => Ex,
            Arg_Element => Element);
   end Trait_Kind;
------------------------------------------------------------------------------

   function Declaration_Origin
     (Declaration : Asis.Declaration)
      return        Asis.Declaration_Origins
   is
   begin
      --  The implementation may require revising when the semantic queries
      --  and implicit elements are implemented.
      Check_Validity (Declaration, Package_Name & "Declaration_Origin");

      if Int_Kind (Declaration) not in Internal_Declaration_Kinds then
         return Not_A_Declaration_Origin;
      elsif not Is_From_Implicit (Declaration) then
         return An_Explicit_Declaration;
      elsif Is_From_Inherited (Declaration) then
         return An_Implicit_Inherited_Declaration;
      else
         return An_Implicit_Predefined_Declaration;
      end if;

   end Declaration_Origin;
-----------------------------------------------------------------------------

   function Mode_Kind
     (Declaration : Asis.Declaration)
      return        Asis.Mode_Kinds
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Declaration, Package_Name & "Mode_Kind");

      if not (Arg_Kind = A_Parameter_Specification or else
              Arg_Kind = A_Formal_Object_Declaration)
      then
         return Not_A_Mode;
      end if;

      Arg_Node := Node (Declaration);

      if In_Present (Arg_Node) and then Out_Present (Arg_Node) then
         return An_In_Out_Mode;
      elsif In_Present (Arg_Node) then
         return An_In_Mode;
      elsif Out_Present (Arg_Node) then
         return An_Out_Mode;
      else
         return A_Default_In_Mode;
      end if;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Mode_Kind");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Mode_Kind",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Mode_Kind;
-----------------------------------------------------------------------------

   function Default_Kind
     (Declaration : Asis.Generic_Formal_Parameter)
      return        Asis.Subprogram_Default_Kinds
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Declaration);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Declaration, Package_Name & "Default_Kind");

      Arg_Node := Node (Declaration);

      if not (Arg_Kind = A_Formal_Procedure_Declaration or else
              Arg_Kind = A_Formal_Function_Declaration)
      then
         return Not_A_Default;
      elsif Box_Present (Arg_Node) then
         return A_Box_Default;
      elsif Present (Default_Name (Arg_Node)) then
         return A_Name_Default;
      elsif Nkind (Specification (Arg_Node)) = N_Procedure_Specification
          and then
            Null_Present (Specification (Arg_Node))
      then
         return A_Null_Default;
      else
         return A_Nil_Default;
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Declaration,
               Outer_Call => Package_Name & "Default_Kind");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Default_Kind",
            Ex          => Ex,
            Arg_Element => Declaration);
   end Default_Kind;
-----------------------------------------------------------------------------

   function Definition_Kind
     (Definition : Asis.Definition)
      return       Asis.Definition_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Definition_Kind");

      return Definition_Kind_From_Internal (Int_Kind (Definition));
   end Definition_Kind;
-----------------------------------------------------------------------------

   function Type_Kind
     (Definition : Asis.Type_Definition)
      return       Asis.Type_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Type_Kind");

      return Type_Kind_From_Internal (Int_Kind (Definition));
   end Type_Kind;
-----------------------------------------------------------------------------

   function Formal_Type_Kind
     (Definition : Asis.Type_Definition)
      return       Asis.Formal_Type_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Formal_Type_Kind");

      return Formal_Type_Kind_From_Internal (Int_Kind (Definition));
   end Formal_Type_Kind;
-----------------------------------------------------------------------------

   function Access_Type_Kind
     (Definition : Asis.Type_Definition)
      return       Asis.Access_Type_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Access_Type_Kind");

      return Access_Type_Kind_From_Internal (Int_Kind (Definition));
   end Access_Type_Kind;
-----------------------------------------------------------------------------

   function Root_Type_Kind
     (Definition : Asis.Type_Definition)
      return       Asis.Root_Type_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Root_Type_Kind");

      return Root_Type_Kind_From_Internal (Int_Kind (Definition));
   end Root_Type_Kind;
-----------------------------------------------------------------------------

   function Constraint_Kind
     (Definition : Asis.Definition)
      return       Asis.Constraint_Kinds
   is
   begin
      Check_Validity (Definition, Package_Name & "Constraint_Kind");

      return Constraint_Kind_From_Internal (Int_Kind (Definition));
   end Constraint_Kind;
-----------------------------------------------------------------------------

   function Discrete_Range_Kind
     (Definition : Asis.Definition)
      return       Asis.Discrete_Range_Kinds
   is
   begin
      Check_Validity (Definition, "Discrete_Range_Kind.Expression_Kind");

      return Discrete_Range_Kind_From_Internal (Int_Kind (Definition));
   end Discrete_Range_Kind;

-----------------------------------------------------------------------------

   function Expression_Kind
     (Expression : Asis.Expression)
      return       Asis.Expression_Kinds
   is
   begin
      Check_Validity (Expression, Package_Name & "Expression_Kind");

      return Expression_Kind_From_Internal (Int_Kind (Expression));
   end Expression_Kind;
-----------------------------------------------------------------------------

   function Operator_Kind
     (Element : Asis.Element)
      return    Asis.Operator_Kinds
   is
   begin
      Check_Validity (Element, Package_Name & "Operator_Kind");

      return Operator_Kind_From_Internal (Int_Kind (Element));
   end Operator_Kind;
-----------------------------------------------------------------------------

   function Attribute_Kind
     (Expression : Asis.Expression)
      return       Asis.Attribute_Kinds
   is
   begin
      Check_Validity (Expression, Package_Name & "Attribute_Kind");

      return Attribute_Kind_From_Internal (Int_Kind (Expression));
   end Attribute_Kind;
-----------------------------------------------------------------------------

   function Association_Kind
     (Association : Asis.Association)
      return        Asis.Association_Kinds
   is
   begin
      Check_Validity (Association, Package_Name & "Association_Kind");

      return Association_Kind_From_Internal (Int_Kind (Association));
   end Association_Kind;
-----------------------------------------------------------------------------

   function Statement_Kind
     (Statement : Asis.Statement)
      return      Asis.Statement_Kinds
   is
   begin
      Check_Validity (Statement, Package_Name & "Statement_Kind");

      return Statement_Kind_From_Internal (Int_Kind (Statement));
   end Statement_Kind;
-----------------------------------------------------------------------------

   function Path_Kind (Path : Asis.Path) return Asis.Path_Kinds is
   begin
      Check_Validity (Path, Package_Name & "Clause_Kind");

      return Path_Kind_From_Internal (Int_Kind (Path));
   end Path_Kind;
-----------------------------------------------------------------------------

   function Clause_Kind (Clause : Asis.Clause) return Asis.Clause_Kinds is
   begin
      Check_Validity (Clause, Package_Name & "Clause_Kind");

      return Clause_Kind_From_Internal (Int_Kind (Clause));
   end Clause_Kind;
-----------------------------------------------------------------------------

   function Representation_Clause_Kind
     (Clause : Asis.Clause)
      return   Asis.Representation_Clause_Kinds
   is
   begin
      Check_Validity (Clause, Package_Name & "Representation_Clause_Kind");

      return Representation_Clause_Kind_From_Internal (Int_Kind (Clause));
   end Representation_Clause_Kind;
-----------------------------------------------------------------------------

   function Is_Nil (Right : Asis.Element) return Boolean is
   begin
      return Right = Asis.Nil_Element;
   end Is_Nil;
-----------------------------------------------------------------------------

   function Is_Nil (Right : Asis.Element_List) return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;
-----------------------------------------------------------------------------

   function Is_Equal
     (Left  : Asis.Element;
      Right : Asis.Element)
      return  Boolean
   is
      C_Left   : Context_Id;
      C_Right  : Context_Id;

      U_Left   : Unit_Id;
      U_Right  : Unit_Id;

      CU_Left  : Compilation_Unit;
      CU_Right : Compilation_Unit;

      N_Left   : Node_Id;
      N_Right  : Node_Id;

      Result   : Boolean := False;

   begin
      Check_Validity (Left,  Package_Name & "Is_Equal");
      Check_Validity (Right, Package_Name & "Is_Equal");

      --  To minimize the performance penalties, we are trying to filter
      --  out simple cases first. These are (more or less) simple cases
      --  when the function should return False

      --  First, checking the case when one of the arguments is Nil_Element

      if Int_Kind (Left)  = Not_An_Element or else
         Int_Kind (Right) = Not_An_Element
      then
         return (Int_Kind (Left) = Int_Kind (Right));
      end if;

      --  Then, we are checking if the basic properties of the argument are
      --  the same

      if not (Special_Case       (Left) = Special_Case       (Right) and then
              Int_Kind           (Left) = Int_Kind           (Right) and then
              Character_Code     (Left) = Character_Code     (Right) and then
              Is_From_Implicit   (Left) = Is_From_Implicit   (Right) and then
              Is_From_Inherited  (Left) = Is_From_Inherited  (Right) and then
              Is_From_Instance   (Left) = Is_From_Instance   (Right) and then
              Normalization_Case (Left) = Normalization_Case (Right) and then
              Parenth_Count (Left)      = Parenth_Count      (Right))
      then
         return False;
      end if;

      --  Now, checking that arguments are from the same Ada unit

      C_Left  := Encl_Cont_Id (Left);
      U_Left  := Encl_Unit_Id (Left);

      C_Right := Encl_Cont_Id (Right);
      U_Right := Encl_Unit_Id (Right);

      if C_Left = C_Right then

         if U_Left /= U_Right then
            return False;
         end if;

      else
         --  This case is a bit more complicated: we have to compare names
         --  and time stamps of enclosed units
         if U_Left = Standard_Id or else U_Right = Standard_Id then

            if U_Left /= U_Right then
               return False;
            end if;

         else

            if Time_Stamp (C_Left, U_Left) /=
               Time_Stamp (C_Right, U_Right)
            then
               return False;
            end if;

            --  Here we have to compare unit names. Let's check unit kind
            --  and class first

            CU_Left  := Encl_Unit (Left);
            CU_Right := Encl_Unit (Right);

            if not (Kind  (CU_Left) = Kind  (CU_Right) and then
                    Class (CU_Left) = Class (CU_Right))
            then
               return False;
            end if;

            --  And now - unit names. This case does not seem to be
            --  encountered very often, so we simply use Unit_Full_Name
            --  query to avoid manual Context switching:

            if Asis.Compilation_Units.Unit_Full_Name (CU_Left) /=
               Asis.Compilation_Units.Unit_Full_Name (CU_Right)
            then
               return False;
            end if;

         end if;

      end if;

      --  And if we are here, we are in the following situation: both Left
      --  and Right are non-nil Elements, they have all their properties
      --  the same and they are from the same Compilation_Unit.
      --  And now we have to check if they represents the same construct.

      if U_Left = Standard_Id or else
         (C_Left = C_Right
         and then
          Encl_Tree (Left) = Encl_Tree (Right))
      then
         --  In Standard, we may just compare the node values.
         --  In the same tree we may do the same
         return R_Node (Left) = R_Node (Right);
      end if;

      --  In case of configuration pragmas and components thereof we are very
      --  conservative - two elements can be equal only if there are from
      --  the same tree. The reason for this is that in ASIS we have no
      --  means to control that the content of the configuration files
      --  is the same in different trees.

      if Special_Case (Left) = Configuration_File_Pragma then

         return Encl_Tree (Left) = Encl_Tree (Right) and then
                R_Node (Left) = R_Node (Right);

      end if;

      --  And if we are here, we have to compare Elements obtained from
      --  different trees

      if not Is_From_Instance (Left) then
         --  May be, we have to use source-trace-based approach for
         --  all cases....????
         return Rel_Sloc (Left) = Rel_Sloc (Right);
      end if;

      --  If we are here, we have to compare node traces.

      Reset_Context (C_Left);

      N_Left := R_Node (Left);
      Create_Node_Trace (N_Left);

      Reset_Context (C_Right);

      N_Right := R_Node (Right);
      Result  := True;

      for J in Node_Trace.First .. Node_Trace.Last loop

         if No (N_Right) or else
            not Is_Equal (N_Right, Node_Trace.Table (J))
         then
            Result := False;
            exit;
         end if;

         N_Right := A4G.Asis_Tables.Enclosing_Scope (N_Right);

      end loop;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Is_Equal");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Equal",
            Ex            => Ex,
            Arg_Element   => Left,
            Arg_Element_2 => Right);
   end Is_Equal;

-----------------------------------------------------------------------------

   function Is_Identical
     (Left  : Asis.Element;
      Right : Asis.Element)
      return  Boolean
   is
      C_Left  : Context_Id;
      C_Right : Context_Id;
   begin
      Check_Validity (Left,  Package_Name & "Is_Identical");
      Check_Validity (Right, Package_Name & "Is_Identical");

      C_Left  := Encl_Cont_Id (Left);
      C_Right := Encl_Cont_Id (Right);

      if C_Left /= C_Right then
         return False;
      else
         return Is_Equal (Left, Right);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Is_Identical");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Is_Identical",
            Ex            => Ex,
            Arg_Element   => Left,
            Arg_Element_2 => Right);
   end Is_Identical;
------------------------------------------------------------------------------
--          The general principle of the implementation
--          of the Is_Part_Of_... functions:
--
--  These functions simply returns the corresponding flag value from the
--  Element passed as their argument. All necessary work should be done
--  during the creation of the Element when these flags are set
--
--  All of them (as well as the function Declaration_Origin above) will
--  require revisiting during semantic queries implementation
------------------------------------------------------------------------------

   function Is_Part_Of_Implicit (Element : Asis.Element) return Boolean is
   begin
      Check_Validity (Element, Package_Name & "Is_Part_Of_Implicit");

      return Is_From_Implicit (Element) or else
             Normalization_Case (Element) in Normalized_Association;
      --  for normalized associations Is_Part_Of_Implicit is not set ON   ???
      --  unless the association is from some enclosing implicit construct. ???
   end Is_Part_Of_Implicit;
-----------------------------------------------------------------------------

   function Is_Part_Of_Inherited (Element : Asis.Element) return Boolean is
   begin
      Check_Validity (Element, Package_Name & "Is_Part_Of_Inherited");

      return Is_From_Inherited (Element);
   end Is_Part_Of_Inherited;
-----------------------------------------------------------------------------

   function Is_Part_Of_Instance (Element : Asis.Element) return Boolean is
   begin
      Check_Validity (Element, Package_Name & "Is_Part_Of_Instance");

      return Is_From_Instance (Element);
   end Is_Part_Of_Instance;
-----------------------------------------------------------------------------
   function Enclosing_Element
     (Element : Asis.Element)
      return    Asis.Element
   is
      Argument_Kind : constant Internal_Element_Kinds := Int_Kind (Element);
      Arg_Spec_Case : constant Special_Cases := Special_Case (Element);
   begin
      Check_Validity (Element, Package_Name & "Enclosing_Element");

      if Argument_Kind = Not_An_Element then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Enclosing_Element",
            Wrong_Kind => Argument_Kind);
      end if;

      --  if the argument is an expanded generic declaration we have
      --  to return the corresponding instantiation:
      if Arg_Spec_Case in Expanded_Spec then
         return Corresponding_Instantiation (Element);
      end if;

      --  if the argument is from an expanded generic declaration,
      --  we have to be careful when coming from some top-level component
      --  of the expanded declaration to the declaration itself - we
      --  need to set the Special_Case field properly

      if Is_From_Instance (Element) and then
         not Is_From_Implicit (Element)
      then

         if Arg_Spec_Case in
            Dummy_Base_Attribute_Designator .. Dummy_Class_Attribute_Prefix
         then

            declare
               Result : Asis.Element := Element;
            begin
               Set_Special_Case (Result, Not_A_Special_Case);

               if Arg_Spec_Case = Dummy_Class_Attribute_Designator
                 or else
                  Arg_Spec_Case = Dummy_Class_Attribute_Prefix
               then
                  Set_Int_Kind (Result, A_Class_Attribute);
               elsif Arg_Spec_Case = Dummy_Base_Attribute_Designator
                    or else
                     Arg_Spec_Case = Dummy_Base_Attribute_Prefix
               then
                  Set_Int_Kind (Result, A_Base_Attribute);
               end if;

               return Result;
            end;
         else
            return Enclosing_For_Explicit_Instance_Component (Element);
         end if;

      end if;

      if not (Is_From_Implicit (Element) or else
              Is_From_Inherited (Element))
        or else
         --  'floating' labels in Ada 2012
         Statement_Kind (Element) = A_Null_Statement
      then
         return Enclosing_Element_For_Explicit (Element);
      elsif Is_From_Limited_View (Element) then
         return Enclosing_Element_For_Limited_View (Element);
      else
         return Enclosing_Element_For_Implicit (Element);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Enclosing_Element");

         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Enclosing_Element",
            Ex          => Ex,
            Arg_Element => Element);
   end Enclosing_Element;
------------------------------------------------------------------------------

   function Enclosing_Element
     (Element                    : Asis.Element;
      Expected_Enclosing_Element : Asis.Element)
      return                       Asis.Element
   is
   begin
      Check_Validity
        (Element,
         Package_Name & "Enclosing_Element (the Element parameter)");

      Check_Validity
        (Expected_Enclosing_Element,
         Package_Name &
         "Enclosing_Element (the Expected_Enclosing_Element parameter)");

      return Enclosing_Element (Element);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Enclosing_Element");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name    => Package_Name & "Enclosing_Element",
            Ex            => Ex,
            Arg_Element   => Element,
            Arg_Element_2 => Expected_Enclosing_Element);
   end Enclosing_Element;
-----------------------------------------------------------------------------
   function Pragmas
     (The_Element : Asis.Element)
      return        Asis.Pragma_Element_List
   is

--  This implementation is based on the following statement in the function
--  documentation:
--
--  This interface returns exactly those pragmas that would be returned by the
--  various interfaces, that accept these same argument kinds, and that
--  return Declaration_Lists and Statement_Lists, where the inclusion of
--  Pragmas is controlled by an Include_Pragmas parameter.
--
--  The general idea of the implementation is straightforward - to get
--  the "full" Element_List by the call of the corresponding interface
--  with Include_Pragmas => True, and then select only A_Pragma elements
--  from this intermediate result.
--
--  Some loss of effectiveness could be considered as the disadvantage of
--  this approach, but its advantages are:
--
--   - it saves implementation efforts;
--   - it allows to check whether the documentation fragment cited above
--     is really correct;
--   - it saves the debugging efforts on the first prototyping stage
--     (there is no need for the special debugging of this function
--     if other ASIS interfaces used for its implementation work correctly);
--   - it is more convenient for incremental development
--   - it yields the vendor-independent implementation of this function

      Context_Internal_Kind : Internal_Element_Kinds;

      function Extract_Pragmas
        (List : Asis.Element_List)
         return Asis.Pragma_Element_List;
      --  function extracts Elements of A_Pragma kind from its
      --  List parameter and returns the new List constructed from these
      --  Pragma Elements (in their order of appearance) as its result

      function Extract_Pragmas
        (List : Asis.Element_List)
         return Asis.Pragma_Element_List
      is
         Pragma_List               : Asis.Pragma_Element_List (List'Range);
         Pragma_List_Actual_Lenght : Asis.ASIS_Integer := 0;

      begin

         for I in List'Range loop

            if Element_Kind (List (I)) = A_Pragma then
               Pragma_List_Actual_Lenght := Pragma_List_Actual_Lenght + 1;
               Pragma_List (Pragma_List_Actual_Lenght) := List (I);
            end if;

         end loop;

         return Pragma_List (1 .. Pragma_List_Actual_Lenght);

      end Extract_Pragmas;

   begin -- Pragmas

      Check_Validity (The_Element, Package_Name & "Pragmas");

      Context_Internal_Kind := Int_Kind (The_Element);

      if not -- Appropriate Element_Kinds:

             (Context_Internal_Kind in Internal_Statement_Path_Kinds
            or else Context_Internal_Kind =  An_Exception_Handler

            --  Appropriate Declaration_Kinds:

            or else Context_Internal_Kind =  A_Procedure_Body_Declaration
            or else Context_Internal_Kind =  A_Function_Body_Declaration
            or else Context_Internal_Kind =  A_Package_Declaration
            or else Context_Internal_Kind =  A_Package_Body_Declaration
            or else Context_Internal_Kind =  A_Task_Body_Declaration
            or else Context_Internal_Kind =  A_Protected_Body_Declaration
            or else Context_Internal_Kind =  An_Entry_Body_Declaration
            or else Context_Internal_Kind =  A_Generic_Procedure_Declaration
            or else Context_Internal_Kind =  A_Generic_Function_Declaration
            or else Context_Internal_Kind =  A_Generic_Package_Declaration

            --  Appropriate Definition_Kinds:

            or else Context_Internal_Kind =  A_Record_Definition
            or else Context_Internal_Kind =  A_Variant_Part
            or else Context_Internal_Kind =  A_Variant
            or else Context_Internal_Kind =  A_Task_Definition
            or else Context_Internal_Kind =  A_Protected_Definition

            --  Appropriate Statement_Kinds:

            or else Context_Internal_Kind =  A_Loop_Statement
            or else Context_Internal_Kind =  A_While_Loop_Statement
            or else Context_Internal_Kind =  A_For_Loop_Statement
            or else Context_Internal_Kind =  A_Block_Statement
            or else Context_Internal_Kind =  An_Accept_Statement
            --  Representation_Clause_Kinds:
            or else Context_Internal_Kind = A_Record_Representation_Clause)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Pragmas",
            Wrong_Kind => Context_Internal_Kind);
      end if;

      case Context_Internal_Kind is

         --  Appropriate Element_Kinds:

         when Internal_Path_Kinds =>
            --  A_Path: (pragmas from the statement list)

            return Extract_Pragmas (
                   Asis.Statements.Sequence_Of_Statements (
                       Path            => The_Element,
                       Include_Pragmas => True));

         when An_Exception_Handler =>
            --  (pragmas from the statement list)

            return Extract_Pragmas (
                   Asis.Statements.Handler_Statements (
                       Handler         => The_Element,
                       Include_Pragmas => True));

--  Appropriate Declaration_Kinds:

         when A_Procedure_Body_Declaration  -- (pragmas from decl region
            | A_Function_Body_Declaration   --  + statements)
            | A_Package_Body_Declaration    -- !! SEE OPEN_PROBLEMS.1 BELOW
            | A_Task_Body_Declaration
            | An_Entry_Body_Declaration =>

            return (Extract_Pragmas (
                        Asis.Declarations.Body_Declarative_Items (
                            Declaration     => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                         Asis.Declarations.Body_Statements (
                            Declaration     => The_Element,
                            Include_Pragmas => True)));

         when A_Package_Declaration =>
            --  (pragmas from visible + private decl regions)
            return (Extract_Pragmas (
                        Asis.Declarations.Visible_Part_Declarative_Items (
                            Declaration     => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                        Asis.Declarations.Private_Part_Declarative_Items (
                            Declaration     => The_Element,
                            Include_Pragmas => True)));

         when A_Protected_Body_Declaration =>
            --  (pragmas from decl region)

            return Extract_Pragmas (
                      Asis.Declarations.Protected_Operation_Items (
                          Declaration     => The_Element,
                          Include_Pragmas => True));

         when A_Generic_Procedure_Declaration
            | A_Generic_Function_Declaration =>

            --  (pragmas from formal decl region
            return Extract_Pragmas (
                      Asis.Declarations.Generic_Formal_Part (
                          Declaration     => The_Element,
                          Include_Pragmas => True));

         when A_Generic_Package_Declaration =>
            --  (pragmas from formal + visible + private decl regions)
            return (Extract_Pragmas (
                        Asis.Declarations.Generic_Formal_Part (
                            Declaration     => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                        Asis.Declarations.Visible_Part_Declarative_Items (
                            Declaration     => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                        Asis.Declarations.Private_Part_Declarative_Items (
                            Declaration     => The_Element,
                            Include_Pragmas => True)));

--  Appropriate Definition_Kinds:

         when   A_Record_Definition
              | A_Variant           =>

            --  (pragmas from the component list)

            return Extract_Pragmas (
                      Asis.Definitions.Record_Components (
                          Definition        => The_Element,
                          Include_Pragmas   => True));

         when A_Variant_Part =>
            --  (pragmas from between variants)

            return Extract_Pragmas (
                      Asis.Definitions.Variants (
                          Variant_Part    => The_Element,
                          Include_Pragmas => True));

         when A_Task_Definition
            | A_Protected_Definition =>

            --  (pragmas from visible + private decl regions)
            return (Extract_Pragmas (
                        Asis.Definitions.Visible_Part_Items (
                            Definition      => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                        Asis.Definitions.Private_Part_Items (
                            Definition      => The_Element,
                            Include_Pragmas => True)));

--  Appropriate Statement_Kinds:

         when A_Loop_Statement
            | A_While_Loop_Statement
            | A_For_Loop_Statement =>

            --  (pragmas from statement list)

            return Extract_Pragmas (
                     Asis.Statements.Loop_Statements (
                         Statement       => The_Element,
                         Include_Pragmas => True));

         when A_Block_Statement =>
         --  (pragmas from decl region + statements)

            return (Extract_Pragmas (
                        Asis.Statements.Block_Declarative_Items (
                            Statement       => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                         Asis.Statements.Block_Statements (
                            Statement       => The_Element,
                            Include_Pragmas => True)));

         when An_Accept_Statement =>
            --  (pragmas from statement list+ pragma immediately preceding
            --  the first  exception handler, if any)
            --  !! SEE OPEN_PROBLEMS.2 BELOW
            return (Extract_Pragmas (
                        Asis.Statements.Accept_Body_Statements (
                            Statement       => The_Element,
                            Include_Pragmas => True))
                   &
                      Extract_Pragmas (
                         Asis.Statements.Accept_Body_Exception_Handlers (
                            Statement       => The_Element,
                            Include_Pragmas => True)));

--  Appropriate Representation_Clause_Kinds:

         when A_Record_Representation_Clause =>
            --  (pragmas from component specifications)

            return Extract_Pragmas (
                     Asis.Clauses.Component_Clauses (
                         Clause          => The_Element,
                         Include_Pragmas => True));

         when others =>
            --  Should never been reached !!!
            raise Internal_Implementation_Error;
      end case;

   exception
      when ASIS_Inappropriate_Element =>
         Add_Call_Information (Outer_Call => Package_Name & "Pragmas");
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => The_Element,
               Outer_Call => Package_Name & "Pragmas");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Pragmas",
            Ex          => Ex,
            Arg_Element => The_Element);
   end Pragmas;
------------------------------------------------------------------------------
--  PARTIALLY IMPLEMENTED

   ---------------------------
   -- Corresponding_Pragmas --
   ---------------------------

   function Corresponding_Pragmas
     (Element : Asis.Element)
      return    Asis.Pragma_Element_List
   is
      Next_Rep_Node    : Node_Id;
      Next_Pragma_Node : Node_Id := Empty;
      Arg_Node         : Node_Id;
   begin
      Check_Validity (Element, Package_Name & "Corresponding_Pragmas");

      if not (Element_Kind (Element) = A_Declaration
           or else
              Element_Kind (Element) = A_Statement)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Corresponding_Pragmas",
            Wrong_Kind => Int_Kind (Element));
      end if;

      --  At the moment, this is a partial implementation:
      --  - for A_Statement argument  Nil_Element_List is always returned;
      --  - for A_Declaration argument that represents
      --  - for A_Declaration argument that corresponds to a unit declaration
      --    from the compilation unit or to the proper body of subunit, nothing
      --    is returned.
      --  - implicit inherited declarations are not properly processed
      --  - the result list contains representation pragmas only

      if Element_Kind (Element) = A_Statement then
         return Nil_Element_List;
      else
         Asis_Element_Table.Init;

         case Declaration_Kind (Element) is
            when A_Procedure_Declaration          |
                 A_Function_Declaration           |
                 A_Procedure_Body_Declaration     |
                 A_Function_Body_Declaration      |
                 A_Procedure_Renaming_Declaration |
                 A_Function_Renaming_Declaration  |
                 An_Entry_Declaration             |
                 A_Procedure_Body_Stub            |
                 A_Function_Body_Stub             |
                 A_Procedure_Instantiation        |
                 A_Function_Instantiation         =>

            --  Overloadable entities, pragmas are not chained.
            --  At the moment we can process only explicit stuff.

            --  First, collect Pre- and Postcondition pragmas, if any.
            Arg_Node := R_Node (Element);

            case Declaration_Kind (Element) is
               when A_Procedure_Declaration |
                    A_Function_Declaration  =>

                  if Is_List_Member (Arg_Node) then
                     Next_Pragma_Node := Next (Arg_Node);
                  else
                     --  Spec of a library-level subprogram
                     Next_Pragma_Node := Aux_Decls_Node (Parent (Arg_Node));

                     if Present (Pragmas_After (Next_Pragma_Node)) then
                        Next_Pragma_Node :=
                          First (Pragmas_After (Next_Pragma_Node));
                     else
                        Next_Pragma_Node := Empty;
                     end if;

                  end if;

               when A_Procedure_Body_Declaration |
                    A_Function_Body_Declaration  =>
                  Next_Pragma_Node := First (Sinfo.Declarations (Arg_Node));

                  while Present (Next_Pragma_Node)
                     and then
                        not Comes_From_Source
                             (Original_Node (Next_Pragma_Node))
                  loop
                     Next_Pragma_Node := Next (Next_Pragma_Node);
                  end loop;

               when others => null;
            end case;

            while Present (Next_Pragma_Node)
                and then
                 Nkind (Next_Pragma_Node) = N_Pragma
            loop

               if Comes_From_Source (Original_Node (Next_Pragma_Node))
                 and then
                   (Pragma_Name (Original_Node (Next_Pragma_Node)) in
                      Name_Postcondition .. Name_Precondition)
                      --  SCz
--                      or else
--                       Pragma_Name (Original_Node (Next_Pragma_Node)) =
--                       Name_Test_Case
--                      or else
--                       Pragma_Name (Original_Node (Next_Pragma_Node)) =
--                       Name_Contract_Case)
               then
                  Asis_Element_Table.Append
                    (Node_To_Element_New
                      (Starting_Element => Element,
                       Node             => Next_Pragma_Node));
               end if;

               Next_Pragma_Node := Next (Next_Pragma_Node);

               while Present (Next_Pragma_Node)
                  and then
                     not Comes_From_Source (Original_Node (Next_Pragma_Node))
               loop
                  Next_Pragma_Node := Next (Next_Pragma_Node);
               end loop;

            end loop;

            --  Now - general processing of all the other pragmas that can be
            --  semantically associated with the argument
            if not Is_Part_Of_Implicit (Element)
              and then
               Enclosing_Element (Element) /= Nil_Element
            then

               case Nkind (Arg_Node) is
                  when N_Subprogram_Declaration          |
                       N_Abstract_Subprogram_Declaration |
                       N_Subprogram_Body                 |
                       N_Subprogram_Renaming_Declaration |
                       N_Subprogram_Body_Stub            =>
                     Arg_Node := Defining_Unit_Name (Specification (Arg_Node));
                  when N_Entry_Declaration =>
                     Arg_Node := Defining_Identifier (Arg_Node);

                  when N_Procedure_Instantiation |
                       N_Function_Instantiation  =>
                     Arg_Node := Defining_Unit_Name (Arg_Node);

                  when others =>
                     pragma Assert (False);
                     null;
               end case;

               Next_Rep_Node := R_Node (Element);
               Next_Rep_Node := Next (Next_Rep_Node);

               while Present (Next_Rep_Node) loop

                  if Nkind (Next_Rep_Node) = N_Pragma
                   and then
                     Is_Applied_To (Next_Rep_Node, Arg_Node)
                  then
                     Asis_Element_Table.Append
                       (Node_To_Element_New
                         (Starting_Element => Element,
                          Node             => Next_Rep_Node));
                  end if;

                  Next_Rep_Node := Next (Next_Rep_Node);

               end loop;

               --  In case if the argument declaration is in the visible part
               --  of the package spec, traverse the private part:
               Next_Rep_Node := Parent (R_Node (Element));

               if Nkind (Next_Rep_Node) = N_Package_Specification
                 and then
                  List_Containing (R_Node (Element)) =
                  Visible_Declarations (Next_Rep_Node)
               then
                  Next_Rep_Node :=
                    First (Private_Declarations (Next_Rep_Node));

                  while Present (Next_Rep_Node) loop

                     if Nkind (Next_Rep_Node) = N_Pragma
                      and then
                        Is_Applied_To (Next_Rep_Node, Arg_Node)
                     then
                        Asis_Element_Table.Append
                          (Node_To_Element_New
                            (Starting_Element => Element,
                             Node             => Next_Rep_Node));
                     end if;

                     Next_Rep_Node := Next (Next_Rep_Node);

                  end loop;

               end if;

            end if;

         when others =>

            --  Non-overloadable entity. This implementation is not good,
            --  but we have to deal with an error in query definition -
            --  the query should actually be applied to an entity, but not
            --  to a declaration that can define more than one entity.
            declare
               Decl_Names    : constant Element_List := Names (Element);
               Next_Name     : Asis.Element;
            begin

               for J in Decl_Names'Range loop
                  Next_Name := Decl_Names (J);

                  if Defining_Name_Kind (Next_Name) =
                     A_Defining_Expanded_Name
                  then
                     Next_Name := Defining_Selector (Next_Name);
                  end if;

                  Next_Rep_Node := First_Rep_Item (R_Node (Next_Name));

                  while Present (Next_Rep_Node) loop

                     if Nkind (Next_Rep_Node) = N_Pragma then
                        Asis_Element_Table.Append
                          (Node_To_Element_New
                            (Starting_Element => Element,
                             Node             => Next_Rep_Node));
                     end if;

                     Next_Rep_Node := Next_Rep_Item (Next_Rep_Node);

                  end loop;

               end loop;

            end;
         end case;

         return Asis.Pragma_Element_List
           (Asis_Element_Table.Table (1 .. Asis_Element_Table.Last));

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Corresponding_Pragmas");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Pragmas",
            Ex          => Ex,
            Arg_Element => Element);
   end Corresponding_Pragmas;
-----------------------------------------------------------------------------

   function Pragma_Name_Image
     (Pragma_Element : Asis.Pragma_Element)
      return           Wide_String
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Pragma_Element);
      Arg_Node : Node_Id;

      Image_Start : Source_Ptr;
      Image_End   : Source_Ptr;
   begin
      Check_Validity (Pragma_Element, Package_Name & "Pragma_Name_Image");

      if Arg_Kind not in Internal_Pragma_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Pragma_Name_Image",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Pragma_Element);
      Image_Start := Next_Identifier (Sloc (Arg_Node) + 5);
      Image_End   := Get_Word_End (P       => Image_Start,
                                   In_Word => In_Identifier'Access);

      return Get_Wide_Word (Image_Start, Image_End);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Pragma_Element,
               Outer_Call => Package_Name & "Pragma_Name_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Pragma_Name_Image",
            Ex          => Ex,
            Arg_Element => Pragma_Element);
   end Pragma_Name_Image;
-----------------------------------------------------------------------------

   function Pragma_Argument_Associations
     (Pragma_Element : Asis.Pragma_Element)
      return           Asis.Association_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Pragma_Element);
      Arg_Node : Node_Id;
   begin

      Check_Validity
        (Pragma_Element, Package_Name & "Pragma_Argument_Associations");

      if Arg_Kind not in Internal_Pragma_Kinds then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Pragma_Argument_Associations",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node     := Node (Pragma_Element);

      return N_To_E_List_New
               (List             => Pragma_Argument_Associations (Arg_Node),
                Internal_Kind    => A_Pragma_Argument_Association,
                Starting_Element => Pragma_Element);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Pragma_Element,
               Outer_Call => Package_Name & "Pragma_Argument_Associations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Pragma_Argument_Associations",
            Ex          => Ex,
            Arg_Element => Pragma_Element);
   end Pragma_Argument_Associations;
-----------------------------------------------------------------------------

   function Debug_Image (Element : Asis.Element) return Wide_String is
      LT : String renames A4G.A_Types.ASIS_Line_Terminator;
   begin
      Check_Validity (Element, Package_Name & "Debug_Image");

      Debug_String (Element);

      return To_Wide_String (
         LT & "Element Debug_Image:" & LT &
         Debug_Buffer (1 .. Debug_Buffer_Len));
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Debug_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Debug_Image",
            Ex          => Ex,
            Arg_Element => Element);
   end Debug_Image;
-----------------------------------------------------------------------------

   --  The following constants are used in the computation of hash values for
   --  Elements which are not from Standard:

   Line_Pos  : constant Natural := 6;
   Bit_Pos   : constant Natural := 1;
   Impl_Pos  : Natural renames Bit_Pos;
   Inh_Pos   : Natural renames Bit_Pos;
   Inst_Pos  : Natural renames Bit_Pos;
   Kind_Pos  : constant Natural := 8;
   Col_Pos   : constant Natural := 4;
   Name_Pos  : constant Natural := 9;
   Spec_Pos  : constant Natural := 2;

   Max_Names : constant Unsigned_32 := 2 ** Name_Pos;
   Max_Cols  : constant Unsigned_32 := 2 ** Col_Pos;
   Max_Kinds : constant Unsigned_32 := 2 ** Kind_Pos;
   Max_Lines : constant Unsigned_32 := 2 ** Line_Pos;
   Max_Specs : constant Unsigned_32 := 2 ** Spec_Pos;

   subtype Unit_Name_Hash_Range is Integer range 0 .. Integer (Max_Names) - 1;

   function Ada_Name_Hash is new GNAT.HTable.Hash (Unit_Name_Hash_Range);

   function Hash (Element : Asis.Element) return Asis.ASIS_Integer is
      N : Node_Id;
      S : Source_Ptr;

      L : Physical_Line_Number;
      C : Column_Number;

      Result : Unsigned_32 := 0;

      function Get_Ada_Name return String;
      --  Returns Ada name of the Element's enclosing unit appended with 'S' if
      --  the unit is a spec unit and with 'B' if it is a body unit. Returns
      --  null string for Nil_Element

      function Get_Ada_Name return String is
         CU           : Asis.Compilation_Unit;
         Spec_Or_Body : Character := 'B';
      begin

         if Is_Nil (Element) then
            return "";
         else
            CU := Enclosing_Compilation_Unit (Element);

            if Asis.Compilation_Units.Unit_Kind (CU) in
              A_Procedure .. A_Generic_Package_Renaming
            then
               Spec_Or_Body := 'S';
            end if;

            return To_String
              (Asis.Compilation_Units.Unit_Full_Name (CU)) & Spec_Or_Body;
         end if;

      end Get_Ada_Name;

      function To_ASIS_Integer is new
         Ada.Unchecked_Conversion
           (Source => Unsigned_32,
            Target => Asis.ASIS_Integer);

   begin
      Check_Validity (Element, Package_Name & "Hash");

      --  The hash value for Elements is first created as 32-bit unsigned
      --  integer and then converted into ASIS_Integer
      --
      --  Different approaches are used to create this 32-bit unsigned
      --  integer value for Elements which are and which are not from the
      --  predefined Standard package.
      --
      --  For Elements from Standard:
      --  -  If Element represents the An_Enumeration_Literal_Specification
      --     or A_Defining_Character_Literal from types Character or
      --     Standard_Character, the corresponding character code is used
      --     as hash value
      --  -  otherwise the Node Id of the Element is used as hash value;
      --
      --  For Elements which are not from Standard the 32 bits are first
      --  filled in by the following information:
      --
      --   0 ..  8 - the hash value computed from the Ada name of enclosing
      --             unit
      --         9 - Is_Part_Of_Implicit
      --  10 .. 13 - column in the source file computed from the Sloc of
      --             Element's Node reference
      --        14 - Is_Part_Of_Inherited
      --  15 .. 22 - Internal kind (converted to 'Pos value)
      --        23 - Is_Part_Of_Instance
      --  24 .. 29 - line in the source file computed from the Sloc of
      --             Element's Node reference
      --  30 .. 31 - Special_Case (converted to 'Pos value)
      --
      --  All the values are reduced modulo the corresponding values to fit
      --  the corresponding range. In case of extended generic code, line
      --  and column are computed as the sum of all the lines and columns
      --  in the chain of the source references corresponding to the
      --  instantiation
      --
      --  After creating such a value, it is rotated right by the number of
      --  the lines computed from Sloc of Element's Node reference

      if Encl_Unit_Id (Element) = Standard_Id then

         if Character_Code (Element) /= 0 then
            Result := Result + (Unsigned_32 (Character_Code (Element)));
         else
            N := Node_Value (Element);
            Result := Unsigned_32 (N);
         end if;

      elsif not Is_Nil (Element) then

         N := Node (Element);
         S := Sloc (N);

         L := Get_Physical_Line_Number (Sloc (N));
         C := Get_Column_Number        (Sloc (N));
         S := Instantiation_Location   (S);

         while S /= No_Location loop
            L := L + Get_Physical_Line_Number (Sloc (N));
            C := C + Get_Column_Number        (Sloc (N));
            S := Instantiation_Location       (S);
         end loop;

         --  Special Case:
         Result := Result +
            (Unsigned_32 (
               Special_Cases'Pos (Special_Case (Element))) mod Max_Specs);

         Result := Shift_Left (Result, Line_Pos);

         --  Line:
         Result := Result + (Unsigned_32 (L) mod Max_Lines);
         Result := Shift_Left (Result, Inst_Pos);

         --  Is_Part_Of_Instance
         if Is_From_Instance (Element) then
            Result := Result + 1;
         end if;
         Result := Shift_Left (Result, Kind_Pos);

         --  Internal kind:
         Result := Result +
           (Internal_Element_Kinds'Pos (Int_Kind (Element)) mod Max_Kinds);
         Result := Shift_Left (Result, Inh_Pos);

         --  Is_Part_Of_Inherited
         if Is_From_Inherited (Element) then
            Result := Result + 1;
         end if;
         Result := Shift_Left (Result, Col_Pos);

         --  Column:
         Result := Result + (Unsigned_32 (C) mod Max_Cols);
         Result := Shift_Left (Result, Impl_Pos);

         --  Is_Part_Of_Implicit:
         if Is_From_Implicit (Element) then
            Result := Result + 1;
         end if;
         Result := Shift_Left (Result, Name_Pos);

         --  Hash value computed from the name of enclosed unit:
         Result := Result + Unsigned_32 (Ada_Name_Hash (Get_Ada_Name));

         --  And now, rotating Result

         Result := Rotate_Right (Result, Natural (L));
      end if;

      return To_ASIS_Integer (Result);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Element,
               Outer_Call => Package_Name & "Hash");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Hash",
            Ex          => Ex,
            Arg_Element => Element);
   end Hash;
-----------------------------------------------------------------------------

------------------------------------------------------------------------------
--  Processing of the Ada extensions that most likely will be included in   --
--  Ada 2015 and that are already implemented in GNAT                       --
------------------------------------------------------------------------------

end Asis.Elements;
