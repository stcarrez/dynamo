------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A S I S . C L A U S E S                         --
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
------------------------------------------------------------------------------

with Asis.Errors;     use Asis.Errors;
with Asis.Exceptions; use Asis.Exceptions;

with Asis.Set_Get;    use  Asis.Set_Get;

with A4G.Mapping;     use A4G.Mapping;
with A4G.Vcheck;      use A4G.Vcheck;

with Atree;           use Atree;
with Namet;           use Namet;
with Nlists;          use Nlists;
with Sinfo;           use Sinfo;
with Snames;          use Snames;

package body Asis.Clauses is

   Package_Name : constant String := "Asis.Clauses.";

   ------------------
   -- Clause_Names --
   ------------------

   function Clause_Names (Clause : Asis.Element) return Asis.Element_List is
      Arg_Kind     : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node     : Node_Id;
      Result_List  : List_Id;
      Result_Len   : Natural := 1;
      Withed_Uname : Node_Id;
   begin
      Check_Validity (Clause, Package_Name & "Clause_Names");

      if not (Arg_Kind = A_Use_Package_Clause  or else
              Arg_Kind = A_Use_Type_Clause     or else
              Arg_Kind = A_Use_All_Type_Clause or else  --  Ada 2012
              Arg_Kind = A_With_Clause)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Clause_Names",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      if Arg_Kind = A_With_Clause then
         --  first, computing the number of names listed in the argument
         --  with clause
         --  Note that we should skip implicit with cleause that may be added
         --  by front-end
         while not (Comes_From_Source (Arg_Node)
                  and then
                    Last_Name (Arg_Node))
         loop
            if Comes_From_Source (Arg_Node) then
               Result_Len := Result_Len + 1;
            end if;

            Arg_Node := Next (Arg_Node);
         end loop;

         declare
            Result_List : Asis.Element_List (1 .. Result_Len);
         begin
            Arg_Node := Node (Clause);

            for I in 1 .. Result_Len loop

               Withed_Uname := Sinfo.Name (Arg_Node);

               Result_List (I) := Node_To_Element_New
                  (Starting_Element => Clause,
                   Node             => Withed_Uname);

               Arg_Node     := Next (Arg_Node);

               while Present (Arg_Node)
                  and then
                     not Comes_From_Source (Arg_Node)
               loop
                  Arg_Node := Next (Arg_Node);
               end loop;

            end loop;

            return Result_List;
         end;
      else

         if Nkind (Arg_Node) = N_Use_Package_Clause then
            Result_List := Names (Arg_Node);
         else
            Result_List := Subtype_Marks (Arg_Node);
         end if;

         return  N_To_E_List_New (List             => Result_List,
                                  Starting_Element => Clause);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Clause_Names",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Clause_Names",
           Ex          => Ex,
           Arg_Element => Clause);
   end Clause_Names;

   -------------------------------
   -- Component_Clause_Position --
   -------------------------------

   function Component_Clause_Position
     (Clause : Asis.Component_Clause)
      return   Asis.Expression
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Clause, Package_Name & "Component_Clause_Position");

      if not (Arg_Kind = A_Component_Clause) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Component_Clause_Position",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      return Node_To_Element_New (Node             => Position (Arg_Node),
                                  Starting_Element => Clause);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Clause_Position",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Component_Clause_Position",
           Ex          => Ex,
           Arg_Element => Clause);
   end Component_Clause_Position;

   ----------------------------
   -- Component_Clause_Range --
   ----------------------------

   function Component_Clause_Range
     (Clause : Asis.Component_Clause)
      return   Asis.Discrete_Range
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Clause, Package_Name & "Component_Clause_Range");

      if not (Arg_Kind = A_Component_Clause) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Component_Clause_Range",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      return Node_To_Element_New
               (Node             => Arg_Node,
                Internal_Kind    => A_Discrete_Simple_Expression_Range,
                Starting_Element => Clause);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Clause_Range",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Component_Clause_Range",
           Ex          => Ex,
           Arg_Element => Clause);
   end Component_Clause_Range;

   -----------------------
   -- Component_Clauses --
   -----------------------

   function Component_Clauses
     (Clause          : Asis.Representation_Clause;
      Include_Pragmas : Boolean := False)
      return            Asis.Component_Clause_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node : Node_Id;
   begin

      Check_Validity (Clause, Package_Name & "Component_Clauses");

      if not (Arg_Kind = A_Record_Representation_Clause) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Component_Clauses",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      return N_To_E_List_New
               (List             => Component_Clauses (Arg_Node),
                Include_Pragmas  => Include_Pragmas,
                Starting_Element => Clause);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Clauses",
               Argument   => Clause,
               Bool_Par   => Include_Pragmas);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Component_Clauses",
           Ex          => Ex,
           Arg_Element => Clause,
           Bool_Par_ON => Include_Pragmas);
   end Component_Clauses;

   ---------------------------
   -- Mod_Clause_Expression --
   ---------------------------

   function Mod_Clause_Expression
     (Clause : Asis.Representation_Clause)
      return   Asis.Expression
   is
      Arg_Kind        : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node        : Node_Id;
      Mod_Clause_Node : Node_Id;
   begin

      Check_Validity (Clause, Package_Name & "Mod_Clause_Expression");

      if not (Arg_Kind = A_Record_Representation_Clause) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Mod_Clause_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      Mod_Clause_Node := Next (Arg_Node);

      if Nkind (Mod_Clause_Node) = N_Attribute_Definition_Clause and then
         From_At_Mod (Mod_Clause_Node)
      then
         Mod_Clause_Node := Sinfo.Expression (Mod_Clause_Node);
      else
         Mod_Clause_Node := Empty;
      end if;

      if No (Mod_Clause_Node) then
         return Asis.Nil_Element;
      else
         return Node_To_Element_New
                  (Node             => Mod_Clause_Node,
                   Starting_Element => Clause);
      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Mod_Clause_Expression",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Mod_Clause_Expression",
           Ex          => Ex,
           Arg_Element => Clause);
   end Mod_Clause_Expression;

   --------------------------------------
   -- Representation_Clause_Expression --
   --------------------------------------

   function Representation_Clause_Expression
     (Clause : Asis.Representation_Clause)
      return   Asis.Expression
   is
      Arg_Kind    : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node    : Node_Id;
      Result_Node : Node_Id;
      Result_Kind : Internal_Element_Kinds := Not_An_Element;
   begin

      Check_Validity (Clause,
                     Package_Name & "Representation_Clause_Expression");

      if not (Arg_Kind = An_Attribute_Definition_Clause       or else
              Arg_Kind = An_Enumeration_Representation_Clause or else
              Arg_Kind = An_At_Clause)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Representation_Clause_Expression",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      if Nkind (Arg_Node) = N_Enumeration_Representation_Clause then
         Result_Node := Array_Aggregate (Arg_Node);

         if Present (Expressions (Result_Node)) then
            Result_Kind := A_Positional_Array_Aggregate;
         else
            Result_Kind := A_Named_Array_Aggregate;
         end if;

      else
         Result_Node := Sinfo.Expression (Arg_Node);
      end if;

      return Node_To_Element_New (Node             => Result_Node,
                                  Internal_Kind    => Result_Kind,
                                  Starting_Element => Clause);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Representation_Clause_Expression",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Representation_Clause_Expression",
           Ex          => Ex,
           Arg_Element => Clause);
   end Representation_Clause_Expression;

   --------------------------------
   -- Representation_Clause_Name --
   --------------------------------

   function Representation_Clause_Name
     (Clause : Asis.Clause)
      return   Asis.Name
   is
      Arg_Kind       : constant Internal_Element_Kinds := Int_Kind (Clause);
      Arg_Node       : Node_Id;
      Result_Node    : Node_Id;

      Result_Element : Element;
      Result_Kind    : Internal_Element_Kinds := Not_An_Element;
      Attr_Des       : Name_Id;
      --  needed for special processing of attribute definition clause
   begin

      Check_Validity (Clause, Package_Name & "Representation_Clause_Name");

      if not (Arg_Kind = An_Attribute_Definition_Clause       or else
              Arg_Kind = An_Enumeration_Representation_Clause or else
              Arg_Kind = A_Record_Representation_Clause       or else
              Arg_Kind = An_At_Clause                         or else
              Arg_Kind = A_Component_Clause)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Representation_Clause_Name",
            Wrong_Kind => Arg_Kind);
      end if;

      Arg_Node := Node (Clause);

      if Nkind (Arg_Node) = N_Attribute_Definition_Clause then
         --  for An_Attribute_Definition_Clause argument we have to return
         --  as the result the Element of An_Attribute_Reference kind.
         --  The tree does not contain the structures for attribute reference
         --  in this case (and it should not, because, according to RM 95,
         --  there is no attribute reference in the syntax structure of
         --  an attribute definition clause, so we have to "emulate"
         --  the result Elemet of An_Attribute_Reference kind on the base
         --  of the same node

         --  first, we have to define the exact kind of the "artificial"
         --  attribute reference to be returned
         Attr_Des := Chars (Arg_Node);

         case Attr_Des is
            when Name_Address =>
               Result_Kind := An_Address_Attribute;
            when Name_Alignment =>
               Result_Kind := An_Alignment_Attribute;
            when Name_Bit_Order =>
               Result_Kind := A_Bit_Order_Attribute;
            when Name_Component_Size =>
               Result_Kind := A_Component_Size_Attribute;
            when Name_External_Tag =>
               Result_Kind := An_External_Tag_Attribute;
            when Name_Input =>
               Result_Kind := An_Input_Attribute;
            when Name_Machine_Radix =>
               Result_Kind := A_Machine_Radix_Attribute;
            when Name_Output =>
               Result_Kind := An_Output_Attribute;
            when Name_Read =>
               Result_Kind := A_Read_Attribute;
            when Name_Size =>
               Result_Kind := A_Size_Attribute;
            when Name_Small =>
               Result_Kind := A_Small_Attribute;
            when Name_Storage_Size =>
               Result_Kind := A_Storage_Size_Attribute;
            when Name_Storage_Pool =>
               Result_Kind := A_Storage_Pool_Attribute;
            when Name_Write =>
               Result_Kind := A_Write_Attribute;
            when others =>
               --  "others" means Name_Object_Size and Name_Value_Size
               Result_Kind := An_Implementation_Defined_Attribute;
         end case;

         Result_Element := Clause;
         Set_Int_Kind (Result_Element, Result_Kind);

         return Result_Element;

      elsif Nkind (Arg_Node) = N_Component_Clause then
         Result_Node := Component_Name (Arg_Node);
      else
         Result_Node := Sinfo.Identifier (Arg_Node);
      end if;

      return Node_To_Element_New (Node             => Result_Node,
                                  Starting_Element => Clause);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Representation_Clause_Name",
               Argument   => Clause);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name  => Package_Name & "Representation_Clause_Name",
           Ex          => Ex,
           Arg_Element => Clause);
   end Representation_Clause_Name;

end Asis.Clauses;
