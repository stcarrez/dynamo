------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                A S I S . D A T A _ D E C O M P O S I T I O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT is now maintained by AdaCore                    --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;                          use System;

with Asis.Declarations;               use Asis.Declarations;
with Asis.Definitions;                use Asis.Definitions;
with Asis.Elements;                   use Asis.Elements;
with Asis.Errors;                     use Asis.Errors;
with Asis.Exceptions;                 use Asis.Exceptions;

with Asis.Data_Decomposition.Aux;     use Asis.Data_Decomposition.Aux;
with Asis.Data_Decomposition.Set_Get; use Asis.Data_Decomposition.Set_Get;
with Asis.Data_Decomposition.Vcheck;  use Asis.Data_Decomposition.Vcheck;
with Asis.Set_Get;                    use Asis.Set_Get;

with A4G.Asis_Tables;                 use A4G.Asis_Tables;
with A4G.DDA_Aux;                     use A4G.DDA_Aux;
with A4G.Vcheck;                      use A4G.Vcheck;

with Atree;                           use Atree;
with Einfo;                           use Einfo;
with Sinfo;                           use Sinfo;
with Uintp;                           use Uintp;

package body Asis.Data_Decomposition is

   Package_Name : constant String := "Asis.Data_Decomposition.";

   --------------------------
   -- All_Named_Components --
   --------------------------

   function All_Named_Components
     (Type_Definition : Asis.Type_Definition)
      return            Asis.Defining_Name_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Discr_Part : Element;
      Root_Type  : Element;
   begin

      Check_Validity (Type_Definition, Package_Name & "All_Named_Components");

      if not (Arg_Kind = A_Record_Type_Definition or else
              Is_Derived_From_Record (Type_Definition))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "All_Named_Components",
            Wrong_Kind => Arg_Kind);
      end if;

      Discr_Part := Discriminant_Part_From_Type_Definition (Type_Definition);
      Root_Type  := Root_Record_Definition (Type_Definition);

      Set_Named_Components (Discr_Part, New_List);
      Set_Named_Components (Root_Type, Append);

      return Asis.Defining_Name_List (
         Def_N_Table (1 .. Asis_Element_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "All_Named_Components");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "All_Named_Components",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end All_Named_Components;

   ---------------------------------------------
   -- Array_Components (from Array_Component) --
   ---------------------------------------------

   function Array_Components
     (Component : Array_Component)
      return      Array_Component
   is
      Arg_Type_Model_Kind : constant Type_Model_Kinds :=
         Type_Model_Kind (Component);

      Res_Type_Definition : Element;
      Comp_Ind            : Element;
   begin
      Check_Validity
        (Component, Package_Name & "Array_Components (from Array_Component)");

      if not (Is_Array (Component)
           and then
             (Arg_Type_Model_Kind = A_Simple_Static_Model or else
              Arg_Type_Model_Kind = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Array_Components (from Array_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      --  In the current implementation, we can extract array components
      --  only from array components of A_Simple_Static_Model.

      Comp_Ind := Component_Indication (Component);

      Res_Type_Definition := Component_Type_Definition (Comp_Ind);

      if Int_Kind (Res_Type_Definition) = A_Private_Type_Definition then
         Res_Type_Definition := Enclosing_Element (Res_Type_Definition);
         Res_Type_Definition :=
           Corresponding_Type_Declaration (Res_Type_Definition);
         Res_Type_Definition := Type_Declaration_View (Res_Type_Definition);
      end if;

      pragma Assert (
         Int_Kind (Res_Type_Definition) = An_Unconstrained_Array_Definition
        or else
         Int_Kind (Res_Type_Definition) = A_Constrained_Array_Definition
        or else
         Is_Derived_From_Array (Res_Type_Definition));

      return Set_Array_Componnet
         (Array_Type_Definition      => Res_Type_Definition,
          Enclosing_Record_Component => Nil_Record_Component,
          Parent_Indication          => Comp_Ind,
          Parent_First_Bit_Offset    => Component.First_Bit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Package_Name & "Array_Components (from Array_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Components (from Array_Component)",
            Ex          => Ex);
   end Array_Components;

   ----------------------------------------------
   -- Array_Components (from Record_Component) --
   ----------------------------------------------

   function Array_Components
     (Component : Record_Component)
      return      Array_Component
   is
      Arg_Type_Model_Kind : constant Type_Model_Kinds :=
        Type_Model_Kind (Component);
      Res_Type_Definition : Element;
   begin

      Check_Validity
        (Component, Package_Name & "Array_Components (from Record_Component)");

      if not (Is_Array (Component) and then
              (Arg_Type_Model_Kind = A_Simple_Static_Model or else
               Arg_Type_Model_Kind = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Array_Components (from Record_Component)",
            Wrong_Kind => Not_An_Element); --  ???
      end if;

      Res_Type_Definition := Component_Declaration (Component);

      Res_Type_Definition := Component_Type_Definition (Res_Type_Definition);

      if Int_Kind (Res_Type_Definition) = A_Private_Type_Definition then
         Res_Type_Definition := Enclosing_Element (Res_Type_Definition);
         Res_Type_Definition :=
           Corresponding_Type_Declaration (Res_Type_Definition);
         Res_Type_Definition := Type_Declaration_View (Res_Type_Definition);
      end if;

      return Set_Array_Componnet
         (Array_Type_Definition      => Res_Type_Definition,
          Enclosing_Record_Component => Component,
          Parent_Indication          => Nil_Element,
          Parent_Discriminants       => Parent_Discrims (Component),
          Parent_First_Bit_Offset    => Component.First_Bit);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Package_Name & "Array_Components (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Components (from Record_Component)",
            Ex          => Ex);
   end Array_Components;

   ---------------------------------------------
   -- Array_Components (from Type_Definition) --
   ---------------------------------------------

   function Array_Components
     (Type_Definition : Asis.Type_Definition)
      return            Array_Component
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);

      Arg_Type_Model_Kind : constant Type_Model_Kinds :=
        Type_Model_Kind (Type_Definition);

      Is_Dynamic_Array : Boolean := False;
   begin

      Check_Validity
        (Type_Definition,
         Package_Name & "Array_Components (from Type_Definition)");

      if not ((Arg_Kind = An_Unconstrained_Array_Definition or else
               Arg_Kind = A_Constrained_Array_Definition    or else
               Is_Derived_From_Array (Type_Definition))
             and then
              (Arg_Type_Model_Kind = A_Simple_Static_Model or else
               Arg_Type_Model_Kind = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Array_Components (from Type_Definition)",
            Wrong_Kind => Arg_Kind);
      end if;

      if Arg_Type_Model_Kind = A_Simple_Dynamic_Model then
         Is_Dynamic_Array := True;
      end if;

      return Set_Array_Componnet
         (Array_Type_Definition      => Type_Definition,
          Enclosing_Record_Component => Nil_Record_Component,
          Parent_Indication          => Nil_Element,
          Parent_First_Bit_Offset    => 0,
          Dynamic_Array              => Is_Dynamic_Array);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Array_Components (from Type_Definition)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Components (from Type_Definition)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Array_Components;

   -----------------
   -- Array_Index --
   -----------------

   function Array_Index
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural
   is
   begin
      Check_Validity (Iterator.Component, Package_Name & "Array_Index");

      if Is_Nil (Iterator.Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Array_Index (Nil_Array_Component_Iterator)",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      elsif Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Array_Index (iterator is done)",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);
      end if;

      return Iterator.Index;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Array_Index");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Index",
            Ex          => Ex);
   end Array_Index;

   -------------------
   -- Array_Indexes --
   -------------------

   function Array_Indexes
     (Iterator : Array_Component_Iterator)
      return     Dimension_Indexes
   is
      Linear_Index : Asis.ASIS_Natural;
      Result       : Dimension_Indexes (1 .. Iterator.Component.Dimension);
   begin
      Check_Validity (Iterator.Component, Package_Name & "Array_Indexes");

      if Is_Nil (Iterator.Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Array_Indexes (Nil_Array_Component_Iterator)",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      elsif Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Array_Indexes (iterator is done)",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);
      end if;

      Linear_Index := Iterator.Index;

      --  ???!!! FORTRAN arrays should be taken ito account

      Result := De_Linear_Index
                  (Index       => Linear_Index,
                   D           => Iterator.Component.Dimension,
                   Ind_Lengths => Iterator.Component.Length);

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Array_Indexes");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Indexes",
            Ex          => Ex);
   end Array_Indexes;

   --------------------
   -- Array_Iterator --
   --------------------

   function Array_Iterator
     (Component : Array_Component)
      return      Array_Component_Iterator
   is
      Result : Array_Component_Iterator := Nil_Array_Component_Iterator;
   begin
      Check_Validity (Component, Package_Name & "Array_Iterator");

      Result.Component := Component;
      Result.Max_Len   := Max_Len (Component);
      Result.Index     := 1;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Array_Iterator");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Array_Iterator",
            Ex          => Ex);
   end Array_Iterator;

   -----------------------------------------
   -- Array_Length (from Array_Component) --
   -----------------------------------------

   function Array_Length
     (Component : Array_Component)
      return      Asis.ASIS_Natural
   is
      Result : Asis.ASIS_Natural := 1;
   begin
      Check_Validity
        (Component, Package_Name & "Array_Length (from Array_Component)");

      if not Is_Array (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Array_Length (from Array_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      for J in 1 .. Component.Dimension loop
         Result := Result * Component.Length (J);
      end loop;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Array_Length (from Array_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Length (from Array_Component)",
            Ex          => Ex);
   end Array_Length;

   -------------------------------------------------------
   -- Array_Length (from Array_Component and Dimension) --
   -------------------------------------------------------

   function Array_Length
     (Component : Array_Component;
      Dimension : Asis.ASIS_Natural)
      return      Asis.ASIS_Natural
   is
   begin
      Check_Validity
        (Component,
         Package_Name & "Array_Length (from Array_Component and Dimension)");

      if not Is_Array (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Array_Length (from Array_Component and Dimension)",
             Wrong_Kind => Not_An_Element);  --  ???
      elsif Dimension > Component.Dimension then
         --  ??? is it correct
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Array_Length (from Array_Component and Dimension)" &
                         " - dimension is too big",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);
      end if;

      return Component.Length (Dimension);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
                 Package_Name &
                 "Array_Length (from Array_Component and Dimension)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Length (from Array_Component and Dimension)",
            Ex          => Ex);
   end Array_Length;

   ------------------------------------------
   -- Array_Length (from Record_Component) --
   ------------------------------------------

   function Array_Length
     (Component : Record_Component)
      return      Asis.ASIS_Natural
   is
      Array_Entity : Node_Id;
      D            : ASIS_Natural;
      Result       : Asis.ASIS_Natural := 1;
   begin
      Check_Validity
        (Component, Package_Name & "Array_Length (from Record_Component)");

      if not Is_Array (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Array_Length (from Record_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Array_Entity := Get_Type_Entity   (Component);
      D            := ASIS_Natural (Number_Dimensions (Array_Entity));

      for J in 1 .. D loop
         Result := Result *
                   Get_Length (Array_Entity, J, Parent_Discrims (Component));
      end loop;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Array_Length (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Array_Length (from Record_Component)",
            Ex          => Ex);
   end Array_Length;

   --------------------------------------------------------
   -- Array_Length (from Record_Component and Dimension) --
   --------------------------------------------------------

   function Array_Length
     (Component : Record_Component;
      Dimension : Asis.ASIS_Natural)
      return      Asis.ASIS_Natural
   is
      Array_Entity : Node_Id;
      D            : ASIS_Natural;
   begin
      Check_Validity
        (Component,
         Package_Name & "Array_Length (from Record_Component and Dimension)");

      if not Is_Array (Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name &
              "Array_Length (from Record_Component and Dimension)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Array_Entity := Get_Type_Entity   (Component);
      D            := ASIS_Natural (Number_Dimensions (Array_Entity));

      if Dimension > D then
         --  ??? is it correct
         Raise_ASIS_Inappropriate_Element (
            Diagnosis  => Package_Name &
                        "Array_Length (from Record_Component and Dimension)" &
                        " - dimension is too big",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);
      end if;

      return Get_Length (Array_Entity, Dimension, Parent_Discrims (Component));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
                Package_Name &
                "Array_Length (from Record_Component and Dimension)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  =>
              Package_Name &
              "Array_Length (from Record_Component and Dimension)",
            Ex          => Ex);
   end Array_Length;

   ---------------------------------------------------
   -- Component_Data_Stream (from Record_Component) --
   ---------------------------------------------------

   function Component_Data_Stream
     (Component   : Record_Component;
      Data_Stream : Portable_Data)
      return        Portable_Data
   is
      Rec_Entity  : Entity_Id;
      Comp_Entity : Entity_Id;
   begin

      Check_Validity
        (Component,
         Package_Name & "Component_Data_Stream (from Record_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Component_Data_Stream (from Record_Component) - Nil Component",
            Wrong_Kind => Not_An_Element); --  ???
      end if;

      Comp_Entity := Get_Comp_Entity   (Component);
      Rec_Entity  := Get_Record_Entity (Component);

      declare

         Data_Discs : constant Discrim_List :=
            Build_Discrim_List
              (Rec  => Rec_Entity,
               Data => Data_Stream);
         --  A list of discriminants ecxtracted from the argument Data_Stream

         Result : constant Portable_Data :=
           Extract_Record_Component (Data  => Data_Stream,
                                     Comp  => Comp_Entity,
                                     Discs => Data_Discs);
      begin

         if not Component_Present (Comp_Entity, Data_Discs) then

            Raise_ASIS_Inappropriate_Element (Diagnosis =>
               Package_Name &
              "Component_Data_Stream (from Record_Component) - " &
              "Component and Data_Stream are incompatible",
               Wrong_Kind => Not_An_Element); --  ???

         else
            return Result;
         end if;
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Component_Data_Stream (from Record_Component)");
         end if;

         raise;
      when No_Component =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                          "Component_Data_Stream (from Record_Component) - " &
                          "Component does not exist",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      when Variable_Rep_Info =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                          "Component_Data_Stream (from Record_Component) - " &
                          "complex dynamic case?",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Component_Data_Stream (from Record_Component)",
            Ex          => Ex);
   end Component_Data_Stream;

   ------------------------------------------------------------
   -- Component_Data_Stream (from Array_Component and Index) --
   ------------------------------------------------------------

   function Component_Data_Stream
     (Component   : Array_Component;
      Index       : Asis.ASIS_Positive;
      Data_Stream : Portable_Data)
      return        Portable_Data
   is
      Parent_Discs : constant Discrim_List := Parent_Discrims (Component);
      Array_Typ    : Entity_Id;
   begin
      Check_Validity
        (Component, Package_Name & "Component_Data_Stream (by Index)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Component_Data_Stream (by Index) - Nil Component",
           Wrong_Kind => Not_An_Element);  --  ???

      elsif Index > Max_Len (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Component_Data_Stream (by Index) - Index is too big",
            Wrong_Kind => Not_An_Element); -- ???
      end if;

      Array_Typ := Get_Array_Type_Entity (Component);

      declare

         Indexes : constant Dimension_Indexes :=
            De_Linear_Index
              (Index       => Index,
               D           => Dimension (Component),
               Ind_Lengths => Component.Length);
         --  ??? Fortran convention???
         --  ??? De_Linear_Index should be simplified

         Result : constant Portable_Data :=
            Extract_Array_Component (Typ   => Array_Typ,
                                     Data  => Data_Stream,
                                     Subs  => Indexes,
                                     Discs =>  Parent_Discs);

      begin
         return Result;
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Component_Data_Stream (by Index)");
         end if;

         raise;
      when No_Component =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Component_Data_Stream (by Index) - " &
                         "Component does not exist",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      when Variable_Rep_Info =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                          "Component_Data_Stream (by Index) - " &
                          "Complex dynamic case?",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Data_Stream (by Index)",
            Ex          => Ex);
   end Component_Data_Stream;

   --------------------------------------------------------------
   -- Component_Data_Stream (from Array_Component and Indexes) --
   --------------------------------------------------------------

   function Component_Data_Stream
     (Component   : Array_Component;
      Indexes     : Dimension_Indexes;
      Data_Stream : Portable_Data)
      return        Portable_Data
   is
      Parent_Discs : constant Discrim_List := Parent_Discrims (Component);
      Array_Typ    : Entity_Id;
   begin
      Check_Validity
        (Component, Package_Name & "Component_Data_Stream (by Indexes)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                          "Component_Data_Stream (by Indexes) - Nil Component",
            Wrong_Kind => Not_An_Element, --  ???
            Status     => Data_Error);

      elsif Wrong_Indexes (Component, Indexes) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              Package_Name &
              "Component_Data_Stream (by Indexes) - Indexes out of ranges",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      end if;

      Array_Typ := Get_Array_Type_Entity (Component);

      declare

         Result : constant Portable_Data :=
           Extract_Array_Component (Typ   => Array_Typ,
                                    Data  => Data_Stream,
                                    Subs  => Indexes,
                                    Discs =>  Parent_Discs);

      begin
         return Result;
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Component_Data_Stream (by Indexes)");
         end if;

         raise;
      when No_Component =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Component_Data_Stream (by Indexes) - " &
                         "Component does not exist",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      when Variable_Rep_Info =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Component_Data_Stream (by Indexes) - " &
                         "complex dynamic case?",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Data_Stream (by Indexes)",
            Ex          => Ex);
   end Component_Data_Stream;

   -----------------------------------------------------------
   -- Component_Data_Stream from (Array_Component_Iterator) --
   -----------------------------------------------------------

   function Component_Data_Stream
     (Iterator    : Array_Component_Iterator;
      Data_Stream : Portable_Data)
      return        Portable_Data
   is
      Parent_Discs : constant Discrim_List :=
        Parent_Discrims (Iterator.Component);
      Array_Typ : Entity_Id;
   begin
      Check_Validity
        (Iterator.Component,
         Package_Name & "Component_Data_Stream (by Iterator)");

      if Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
             Package_Name &
             "Component_Data_Stream (by Iterator) - Iterator is Done",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);

      elsif Iterator = Nil_Array_Component_Iterator then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Component_Data_Stream (by Iterator) - Nil Iterator",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      end if;

      Array_Typ := Get_Array_Type_Entity (Iterator.Component);

      declare
         Indexes : constant Dimension_Indexes := Array_Indexes (Iterator);
         Result  : constant Portable_Data     :=
           Extract_Array_Component (Typ   => Array_Typ,
                                    Data  => Data_Stream,
                                    Subs  => Indexes,
                                    Discs =>  Parent_Discs);
      begin
         return Result;
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Component_Data_Stream (by Iterator)");
         end if;

         raise;
      when No_Component =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Component_Data_Stream (by Iterator) - " &
                        "Component does not exist",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      when Variable_Rep_Info =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name &
                         "Component_Data_Stream (by Iterator) - " &
                         "Complex dynamic case?",
            Wrong_Kind => Not_An_Element, --  ???
            Status    => Data_Error);
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Component_Data_Stream (by Iterator)",
            Ex          => Ex);
   end Component_Data_Stream;

   ---------------------------
   -- Component_Declaration --
   ---------------------------

   function Component_Declaration
     (Component : Record_Component)
      return      Asis.Declaration
   is
      Result : Asis.Element;
   begin

      Check_Validity (Component, Package_Name & "Component_Declaration");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Component
           (Diagnosis      => Package_Name & "Component_Declaration",
            Component_Kind => Rec);
      end if;

      Result := Component_Name (Component);
      Result := Enclosing_Element (Result);

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Declaration");

         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Declaration",
            Ex          => Ex);
   end Component_Declaration;

   --------------------------
   -- Component_Indication --
   --------------------------

   function Component_Indication
     (Component : Array_Component)
      return      Asis.Subtype_Indication
   is
      Result   : Asis.Element;
   begin

      Check_Validity (Component, Package_Name & "Component_Indication");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Component
           (Diagnosis      => Package_Name & "Component_Indication",
            Component_Kind => Arr);
      end if;

      Result := Parent_Array_Type (Component);
      Result := Root_Array_Definition (Result);
      Result := Array_Component_Definition (Result);
      Result := Component_Subtype_Indication (Result);

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Component_Indication");

         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Component_Indication",
            Ex          => Ex);
   end Component_Indication;

   --------------------------------------
   -- Construct_Artificial_Data_Stream --
   --------------------------------------

   function Construct_Artificial_Data_Stream
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data;
      Discriminant    : Record_Component;
      Value           : Portable_Data)
      return            Portable_Data
   is
      Arg_Kind : constant Internal_Element_Kinds :=
        Int_Kind (Type_Definition);

      Type_Element : Asis.Element;
      Tmp_Element  : Asis.Element;

      Disc_Entity  : Entity_Id;
      Disc_Typ     : Entity_Id;
      Disc_Val     : Uint;
   begin

      Check_Validity
        (Type_Definition,
         Package_Name & "Construct_Artificial_Data_Stream (Type_Definition)");

      if not (Arg_Kind = A_Record_Type_Definition or else
              Is_Derived_From_Record (Type_Definition))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Construct_Artificial_Data_Stream (wrong Type_Definition)",
             Wrong_Kind => Arg_Kind);
      end if;

      Check_Validity
        (Discriminant,
         Package_Name & "Construct_Artificial_Data_Stream (Discriminant)");

      if Is_Nil (Discriminant) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Construct_Artificial_Data_Stream (Nil Discriminant component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      --  We have also to check if Discriminant is a discriminant of a given
      --  type

      Type_Element := Enclosing_Element (Type_Definition);
      Tmp_Element  := Component_Declaration (Discriminant);
      Tmp_Element  := Enclosing_Element (Enclosing_Element (Tmp_Element));

      if not Is_Equal (Type_Element, Tmp_Element) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                          "Construct_Artificial_Data_Stream " &
                          "(Discriminant is from another type)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      --  And now we can create the result stream:

      Disc_Entity := R_Node (Component_Name (Discriminant));
      Disc_Typ    := Etype (Disc_Entity);
      Disc_Val    := Decode_Scalar_Value (Disc_Typ, Value);

      declare
         Result : constant Portable_Data :=
           Set_Discriminant (Data => Data_Stream,
                             Disc => Disc_Entity,
                             Val  => Disc_Val);
      begin
         return Result;
      end;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when Invalid_Data =>
         Raise_ASIS_Inappropriate_Element
           (Diagnosis => Package_Name & "Construct_Artificial_Data_Stream",
            Wrong_Kind => Not_An_Element,  --  ???
            Status    => Data_Error);
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Construct_Artificial_Data_Stream");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Construct_Artificial_Data_Stream",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Construct_Artificial_Data_Stream;

   -----------------------------------------------
   -- Discriminant_Components (Array_Component) --
   -----------------------------------------------

   function Discriminant_Components
     (Component : Array_Component)
      return      Record_Component_List
   is
      Discr_Part : Asis.Element;
      Type_Def   : Asis.Element;
   begin

      Check_Validity
        (Component,
         Package_Name & "Discriminant_Components (from Array_Component)");

      if not Is_Record (Component) then
         Raise_ASIS_Inappropriate_Component
           (Diagnosis      => Package_Name &
                             "Discriminant_Components (from Array_Component)",
            Component_Kind => Arr);
      end if;

      Type_Def   := Component_Indication (Component);
      Type_Def   := Asis.Definitions.Subtype_Mark (Type_Def);
      Type_Def   := Type_Definition_From_Subtype_Mark (Type_Def);
      Discr_Part := Discriminant_Part_From_Type_Definition (Type_Def);

      Set_Named_Components (Discr_Part, New_List);

      Set_Parent_Type_Definition (Type_Def);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit,
         Discriminants    => True);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Discriminant_Components (from Array_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Discriminant_Components (from Array_Component)",
            Ex          => Ex);
   end Discriminant_Components;

   -----------------------------------------------------
   -- Discriminant_Components (from Record_Component) --
   -----------------------------------------------------

   function Discriminant_Components
     (Component : Record_Component)
      return      Record_Component_List
   is
      Discr_Part : Asis.Element;
      Type_Def   : Asis.Element;
   begin

      Check_Validity
        (Component,
         Package_Name & "Discriminant_Components (from Record_Component)");

      if not Is_Record (Component) then
         Raise_ASIS_Inappropriate_Component
           (Diagnosis      => Package_Name &
                             "Discriminant_Components (from Record_Component)",
            Component_Kind => Rec);
      end if;

      Type_Def := Component_Declaration (Component);

      Type_Def := Object_Declaration_View (Type_Def);
      Type_Def := Asis.Definitions.Component_Subtype_Indication (Type_Def);
      Type_Def := Asis.Definitions.Subtype_Mark (Type_Def);
      Type_Def := Type_Definition_From_Subtype_Mark (Type_Def);
      Discr_Part := Discriminant_Part_From_Type_Definition (Type_Def);

      Set_Named_Components (Discr_Part, New_List);

      Set_Parent_Type_Definition (Type_Def);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit,
         Discriminants    => True);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
                 Package_Name &
                 "Discriminant_Components (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Discriminant_Components (from Record_Component)",
            Ex          => Ex);
   end Discriminant_Components;

   ---------------------------------------------------
   -- Discriminant_Components (from Type_Definition)--
   ---------------------------------------------------

   function Discriminant_Components
     (Type_Definition : Asis.Type_Definition)
      return            Record_Component_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Discr_Part : Element;
   begin

      Check_Validity
        (Type_Definition,
         Package_Name & "Discriminant_Components (from Type_Definition)");

      if not (Arg_Kind = A_Record_Type_Definition or else
              Is_Derived_From_Record (Type_Definition))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Discriminant_Components (from Type_Definition)",
            Wrong_Kind => Int_Kind (Type_Definition));
      end if;

      Discr_Part := Discriminant_Part_From_Type_Definition (Type_Definition);

      Set_Named_Components (Discr_Part, New_List);

      Set_Parent_Type_Definition (Type_Definition);
      Set_Record_Type_Entity;

      Set_Record_Components_From_Names (Discriminants => True);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Discriminant_Components (from Type_Definition)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Discriminant_Components (from Type_Definition)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Discriminant_Components;

   ----------
   -- Done --
   ----------

   function Done (Iterator : Array_Component_Iterator) return Boolean is
   begin
      Check_Validity (Iterator.Component, Package_Name & "Done");

      return Iterator.Index > Iterator.Max_Len or else
             Is_Nil (Iterator.Component);
   end Done;

   ------------------------------------------------
   -- First_Bit (from Array_Component and Index) --
   ------------------------------------------------

   function First_Bit
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Natural
   is
      Result : Asis.ASIS_Natural;
   begin

      Check_Validity
        (Component,
         Package_Name & "First_Bit (from Array_Component and Index)");

      if Is_Nil (Component) or else
         Index > Max_Len (Component)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "First_Bit (from Array_Component and Index)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Result := Component.First_Bit;
      Result := Result + Component.Size * (Index - 1);
      Result := Result mod Storage_Unit;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "First_Bit (from Array_Component and Index)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "First_Bit (from Array_Component and Index)",
            Ex          => Ex);
   end First_Bit;

   --------------------------------------------------
   -- First_Bit (from Array_Component and Indexes) --
   --------------------------------------------------

   function First_Bit
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Natural
   is
      Ind    : Asis.ASIS_Positive;
   begin

      Check_Validity
        (Component,
         Package_Name & "First_Bit (from Array_Component and Indexes)");

      if Is_Nil (Component) or else
         Wrong_Indexes (Component, Indexes)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "First_Bit (from Array_Component and Indexes)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      --  ??? FORTRAN convention shoule be taken into account

      Ind := Linear_Index (Inds        => Indexes,
                           Ind_Lengths => Component.Length);

      return First_Bit (Component, Ind);

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "First_Bit (from Array_Component and Indexes)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "First_Bit (from Array_Component and Indexes)",
            Ex          => Ex);
   end First_Bit;

   -------------------------------
   -- First_Bit (from Iterator) --
   -------------------------------

   function First_Bit
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural
   is
      Component : Array_Component;
      Index     : Asis.ASIS_Positive;
   begin
      Check_Validity
        (Iterator.Component, Package_Name & "First_Bit (from Iterator)");

      if Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "First_Bit (from Iterator)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Component := Iterator.Component;
      Index     := Array_Index (Iterator);

      return First_Bit (Component, Index);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "First_Bit (from Iterator)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "First_Bit (from Iterator)",
            Ex          => Ex);
   end First_Bit;

   ---------------------------------------
   -- First_Bit (from Record_Component) --
   ---------------------------------------

   function First_Bit
     (Component : Record_Component)
      return      Asis.ASIS_Natural
   is
   begin
      Check_Validity
        (Component, Package_Name & "First_Bit (from Record_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "First_Bit (from Record_Component)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      return Component.First_Bit;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "First_Bit (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "First_Bit (from Record_Component)",
            Ex          => Ex);
   end First_Bit;

   --------------------------------
   -- Is_Array (Array_Component) --
   --------------------------------

   function Is_Array (Component : Array_Component) return Boolean is
   begin
      Check_Validity (Component, Package_Name & "Is_Array (Array_Component)");

      return Is_Array_Comp (Component);
   end Is_Array;

   ---------------------------------
   -- Is_Array (Record_Component) --
   ---------------------------------

   function Is_Array (Component : Record_Component) return Boolean is
   begin
      Check_Validity (Component, Package_Name & "Is_Array (Record_Component)");

      return Is_Array_Comp (Component);
   end Is_Array;

   --------------------------------
   -- Is_Equal (Array_Component) --
   --------------------------------

   function Is_Equal
     (Left  : Array_Component;
      Right : Array_Component)
      return  Boolean
   is
   begin
      Check_Validity (Left,  Package_Name & "Is_Equal (Array_Component)");
      Check_Validity (Right, Package_Name & "Is_Equal (Array_Component)");

      --  Is this check really enough?
      return
         Is_Equal (Left.Parent_Array_Type, Right.Parent_Array_Type) and then
         Is_Equal (Left.Parent_Component_Name,
                   Right.Parent_Component_Name)                     and then
         Left.Position  = Right.Position                            and then
         Left.First_Bit = Right.First_Bit                           and then
         Left.Last_Bit  = Right.Last_Bit                            and then
         Left.Size      = Right.Size;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Equal (Array_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Equal (Array_Component)",
            Ex          => Ex);
   end Is_Equal;

   ---------------------------------
   -- Is_Equal (Record_Component) --
   ---------------------------------

   function Is_Equal
     (Left  : Record_Component;
      Right : Record_Component)
      return  Boolean
   is
   begin
      Check_Validity (Left,  Package_Name & "Is_Equal (Record_Component)");
      Check_Validity (Right, Package_Name & "Is_Equal (Record_Component)");

      --  Is this check really enough?
      return
         Is_Equal (Left.Parent_Record_Type, Right.Parent_Record_Type) and then
         Is_Equal (Left.Component_Name, Right.Component_Name)         and then
         Left.Position  = Right.Position                              and then
         Left.First_Bit = Right.First_Bit                             and then
         Left.Last_Bit  = Right.Last_Bit                              and then
         Left.Size      = Right.Size;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Equal (Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Equal (Record_Component)",
            Ex          => Ex);
   end Is_Equal;

   ------------------------------------
   -- Is_Identical (Array_Component) --
   ------------------------------------

   function Is_Identical
     (Left  : Array_Component;
      Right : Array_Component)
      return  Boolean
   is
   begin
      Check_Validity (Left,  Package_Name & "Is_Identical (Array_Component)");
      Check_Validity (Right, Package_Name & "Is_Identical (Array_Component)");

      --  Is this check really enough?
      return
         Is_Identical (Left.Parent_Array_Type, Right.Parent_Array_Type)
       and then
         Is_Identical (Left.Parent_Component_Name, Right.Parent_Component_Name)
       and then
         Left.Position       = Right.Position
       and then
         Left.First_Bit      = Right.First_Bit
       and then
         Left.Last_Bit       = Right.Last_Bit
       and then
         Left.Size           = Right.Size
       and then
         Left.Parent_Context = Right.Parent_Context;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Identical (Array_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Identical (Array_Component)",
            Ex          => Ex);
   end Is_Identical;

   -------------------------------------
   -- Is_Identical (Record_Component) --
   -------------------------------------

   function Is_Identical
     (Left  : Record_Component;
      Right : Record_Component)
      return  Boolean
   is
   begin
      Check_Validity (Left,  Package_Name & "Is_Identical (Record_Component)");
      Check_Validity (Right, Package_Name & "Is_Identical (Record_Component)");

      --  Is this check really enough?
      return
         Is_Identical (Left.Parent_Record_Type, Right.Parent_Record_Type)
       and then
         Is_Identical (Left.Component_Name, Right.Component_Name)
       and then
         Left.Position       = Right.Position
       and then
         Left.First_Bit      = Right.First_Bit
       and then
         Left.Last_Bit       = Right.Last_Bit
       and then
         Left.Size           = Right.Size
       and then
         Left.Parent_Context = Right.Parent_Context;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Identical (Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Identical (Record_Component)",
            Ex          => Ex);
   end Is_Identical;

   ------------------------------
   -- Is_Nil (Array_Component) --
   ------------------------------

   function Is_Nil (Right : Array_Component) return Boolean is
   begin
      --  This should be enough to decide, that the argument is nil
      return Is_Nil (Right.Parent_Array_Type);
   end Is_Nil;

   -------------------------------
   -- Is_Nil (Record_Component) --
   -------------------------------

   function Is_Nil (Right : Record_Component) return Boolean is
   begin
      --  This should be enough to decide, that the argument is nil
      return Is_Nil (Right.Parent_Record_Type);
   end Is_Nil;

   ---------------------------------
   -- Is_Record (Array_Component) --
   ---------------------------------

   function Is_Record (Component : Array_Component) return Boolean is
   begin
      Check_Validity (Component, Package_Name & "Is_Record (Array_Component)");

      return Is_Record_Comp (Component);
   end Is_Record;

   ----------------------------------
   -- Is_Record (Record_Component) --
   ----------------------------------

   function Is_Record (Component : Record_Component) return Boolean is
   begin
      Check_Validity
        (Component, Package_Name & "Is_Record (Record_Component)");

      return Is_Record_Comp (Component);
   end Is_Record;

   -----------------------------------------------
   -- Last_Bit (from Array_Component and Index) --
   -----------------------------------------------

   function Last_Bit
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Integer
   is
      Result : Asis.ASIS_Natural;
   begin
      Check_Validity
        (Component, Package_Name &
                    "Last_Bit (from Array_Component and Index)");

      if Is_Nil (Component) or else
         Index > Max_Len (Component)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Last_Bit (from Array_Component and Index)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Result := First_Bit (Component, Index) + Component.Size - 1;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Last_Bit (from Array_Component and Index)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Last_Bit (from Array_Component and Index)",
            Ex          => Ex);
   end Last_Bit;

   -------------------------------------------------
   -- Last_Bit (from Array_Component and Indexes) --
   -------------------------------------------------

   function Last_Bit
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Integer
   is
      Ind    : Asis.ASIS_Positive;
   begin
      Check_Validity
        (Component, Package_Name &
                    "Last_Bit (from Array_Component and Indexes)");

      if Is_Nil (Component) or else
         Wrong_Indexes (Component, Indexes)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Last_Bit (from Array_Component and Indexes)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      --  ??? FORTRAN convention shoule be taken into account

      Ind := Linear_Index (Inds        => Indexes,
                           Ind_Lengths => Component.Length);

      return Last_Bit (Component, Ind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Last_Bit (from Array_Component and Indexes)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Last_Bit (from Array_Component and Indexes)",
            Ex          => Ex);
   end Last_Bit;

   ------------------------------
   -- Last_Bit (from Iterator) --
   ------------------------------

   function Last_Bit
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Integer
   is
      Component : Array_Component;
      Index     : Asis.ASIS_Positive;
   begin
      Check_Validity
        (Iterator.Component, Package_Name & "Last_Bit (from Iterator)");

      if Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Last_Bit (from Iterator)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Component := Iterator.Component;
      Index     := Array_Index (Iterator);

      return Last_Bit (Component, Index);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Last_Bit (from Iterator)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Last_Bit (from Iterator)",
            Ex          => Ex);
   end Last_Bit;

   --------------------------------------
   -- Last_Bit (from Record_Component) --
   --------------------------------------

   function Last_Bit
     (Component : Record_Component)
      return      Asis.ASIS_Integer
   is
   begin
      Check_Validity
        (Component, Package_Name & "Last_Bit (from Record_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Last_Bit (from Record_Component)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      return Component.Last_Bit;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Last_Bit (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Last_Bit (from Record_Component)",
            Ex          => Ex);
   end Last_Bit;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Array_Component_Iterator) is
   begin
      Check_Validity (Iterator.Component, Package_Name & "Next");

      if Iterator.Index <= Iterator.Max_Len then
         Iterator.Index := Iterator.Index + 1;
      end if;
   end Next;

   ----------------------------------
   -- Portable_Constrained_Subtype --
   ----------------------------------

   function Portable_Constrained_Subtype
     (Data_Stream : Portable_Data)
      return        Constrained_Subtype
   is
      --  This is achieved with a Data_Stream unchecked conversion to a
      --  constrained Portable_Data subtype and not with a dereference of
      --  To_Constrained_Subtype_Access (Data_Stream'Address).
      --
      --  In the latter case, ensuring that the address conversion is always
      --  valid would require the Portable_Data type to be maximally aligned
      --  or some other guarantee that the Data_Stream input is necessarily
      --  aligned enough for the target Constrained_Subtype. Forcing maximum
      --  alignment on the Portable_Data type is not desireable as it would
      --  imply annoying constraints on target objects for address conversions
      --  the other way around (from object access to Portable_Data address).
      --  Assuming that Data_Stream is always properly aligned otherwise is
      --  not clearly part of the spec and would be a nice restriction to lift
      --  anyway.

      subtype Constrained_Portable_Data is Portable_Data (Data_Stream'Range);

      function To_Constrained_Subtype is new
        Ada.Unchecked_Conversion
        (Constrained_Portable_Data, Constrained_Subtype);

      Result : constant Constrained_Subtype :=
         To_Constrained_Subtype (Data_Stream);
   begin

      return Result;
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Portable_Constrained_Subtype",
            Ex          => Ex);
   end Portable_Constrained_Subtype;

   -----------------------------------------------
   -- Position (from Array_Component and Index) --
   -----------------------------------------------

   function Position
     (Component : Array_Component;
      Index     : Asis.ASIS_Positive)
      return      Asis.ASIS_Natural
   is
      Result : Asis.ASIS_Natural;
   begin
      Check_Validity
        (Component, Package_Name &
                    "Position (from Array_Component and Index)");

      if Is_Nil (Component) or else
         Index > Max_Len (Component)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Position (from Array_Component and Index)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Result := Component.First_Bit;
      Result := Result + Component.Size * (Index - 1);
      Result := Result / Storage_Unit;

      return Result;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Position (from Array_Component and Index)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Position (from Array_Component and Index)",
            Ex          => Ex);
   end Position;

   -------------------------------------------------
   -- Position (from Array_Component and Indexes) --
   -------------------------------------------------

   function Position
     (Component : Array_Component;
      Indexes   : Dimension_Indexes)
      return      Asis.ASIS_Natural
   is
      Ind    : Asis.ASIS_Positive;
   begin
      Check_Validity
        (Component, Package_Name &
                    "Position (from Array_Component and Indexes)");

      if Is_Nil (Component) or else
         Wrong_Indexes (Component, Indexes)
      then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                        "Position (from Array_Component and Indexes)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      --  ??? FORTRAN convention shoule be taken into account

      Ind := Linear_Index (Inds        => Indexes,
                           Ind_Lengths => Component.Length);

      return Position (Component, Ind);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Position (from Array_Component and Indexes)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Position (from Array_Component and Indexes)",
            Ex          => Ex);
   end Position;

   ------------------------------
   -- Position (from Iterator) --
   ------------------------------

   function Position
     (Iterator : Array_Component_Iterator)
      return     Asis.ASIS_Natural
   is
      Component : Array_Component;
      Index     : Asis.ASIS_Positive;
   begin
      Check_Validity
        (Iterator.Component, Package_Name &  "Position (from Iterator)");

      if Done (Iterator) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name & "Position (from Iterator)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      Component := Iterator.Component;
      Index     := Array_Index (Iterator);

      return Position (Component, Index);
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Position (from Iterator)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Position (from Iterator)",
            Ex          => Ex);
   end Position;

   --------------------------------------
   -- Position (from Record_Component) --
   --------------------------------------

   function Position
     (Component : Record_Component)
      return      Asis.ASIS_Natural
   is
   begin
      Check_Validity
        (Component, Package_Name &  "Position (from Record_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Diagnosis  => Package_Name &
                         "Position (from Record_Component)",
            Wrong_Kind => Not_An_Element,  --  ???
            Status     => Data_Error);
      end if;

      return Component.Position;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Position (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Position (from Record_Component)",
            Ex          => Ex);
   end Position;

   ----------------------------------------------
   -- Record_Components (from Array_Component) --
   ----------------------------------------------

   function Record_Components
     (Component : Array_Component)
      return      Record_Component_List
   is
      Comp_Type   : Element;
   begin

      Check_Validity
        (Component, Package_Name & "Record_Components (from Array_Component)");

      if not (Is_Record (Component) and then
              Type_Model_Kind (Component) = A_Simple_Static_Model)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Record_Components (from Array_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Comp_Type := Component_Indication (Component);

      Comp_Type := Component_Type_Definition (Comp_Type);

      if Int_Kind (Comp_Type) = A_Private_Type_Definition then
         Comp_Type := Enclosing_Element (Comp_Type);
         Comp_Type := Corresponding_Type_Declaration (Comp_Type);
         Comp_Type := Type_Declaration_View (Comp_Type);
      end if;

      pragma Assert (Int_Kind (Comp_Type) = A_Record_Type_Definition or else
                     Is_Derived_From_Record (Comp_Type));

      Set_All_Named_Components   (Comp_Type);
      Set_Parent_Type_Definition (Comp_Type);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception

      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                            "Record_Components (from Array_Component)");
         end if;

         raise;
      when Ex : others =>

         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Record_Components (from Array_Component)",
            Ex          => Ex);
   end Record_Components;

   --------------------------------------------------------------
   -- Record_Components (from Array_Component and Data_Stream) --
   --------------------------------------------------------------

   function Record_Components
     (Component   : Array_Component;
      Data_Stream : Portable_Data)
      return        Record_Component_List
   is
      Comp_Type   : Element;
      Type_Model  : constant Type_Model_Kinds := Type_Model_Kind (Component);
   begin

      Check_Validity
        (Component,
         Package_Name &
         "Record_Components (from Array_Component and Data_Stream)");

      if not (Is_Record (Component) and then
              (Type_Model = A_Simple_Static_Model
            or else
               Type_Model = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Record_Components (from Array_Component and Data_Stream)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Comp_Type := Component_Indication (Component);

      Comp_Type := Component_Type_Definition (Comp_Type);

      if Int_Kind (Comp_Type) = A_Private_Type_Definition then
         Comp_Type := Enclosing_Element (Comp_Type);
         Comp_Type := Corresponding_Type_Declaration (Comp_Type);
         Comp_Type := Type_Declaration_View (Comp_Type);
      end if;

      pragma Assert (Int_Kind (Comp_Type) = A_Record_Type_Definition or else
                     Is_Derived_From_Record (Comp_Type));

      Set_All_Named_Components   (Comp_Type);
      Set_Parent_Type_Definition (Comp_Type);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit,
         Data_Stream      => Data_Stream);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
                 Package_Name &
                 "Record_Components (from Array_Component and Data_Stream)");
         end if;

         raise;
      when Ex : others =>

         Report_ASIS_Bug
           (Query_Name  =>
              Package_Name &
              "Record_Components (from Array_Component and Data_Stream)",
            Ex          => Ex);
   end Record_Components;

   -----------------------------------------------
   -- Record_Components (from Record_Component) --
   -----------------------------------------------

   function Record_Components
     (Component : Record_Component)
      return      Record_Component_List
   is
      Comp_Type   : Element;
   begin

      Check_Validity
        (Component,
         Package_Name & "Record_Components (from Record_Component)");

      if not (Is_Record (Component) and then
              Type_Model_Kind (Component) = A_Simple_Static_Model)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Record_Components (from Record_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Comp_Type := Component_Declaration (Component);

      Comp_Type := Component_Type_Definition (Comp_Type);

      if Int_Kind (Comp_Type) = A_Private_Type_Definition then
         Comp_Type := Enclosing_Element (Comp_Type);
         Comp_Type := Corresponding_Type_Declaration (Comp_Type);
         Comp_Type := Type_Declaration_View (Comp_Type);
      end if;

      Set_All_Named_Components   (Comp_Type);
      Set_Parent_Type_Definition (Comp_Type);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Record_Components (from Record_Component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Record_Components (from Record_Component)",
            Ex          => Ex);
   end Record_Components;

   ---------------------------------------------------------------
   -- Record_Components (from Record_Component and Data_Stream) --
   ---------------------------------------------------------------

   function Record_Components
     (Component   : Record_Component;
      Data_Stream : Portable_Data)
      return        Record_Component_List
   is
      Comp_Type   : Element;
      Type_Model  : Type_Model_Kinds;
   begin

      Check_Validity
        (Component,
         Package_Name &
         "Record_Components (from Record_Component and Data_Stream)");

      Type_Model := Type_Model_Kind (Component);

      if not (Is_Record (Component) and then
              (Type_Model = A_Simple_Static_Model
            or else
               Type_Model = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Record_Components (from Record_Component and Data_Stream)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      Comp_Type := Component_Declaration (Component);

      Comp_Type := Component_Type_Definition (Comp_Type);

      if Int_Kind (Comp_Type) = A_Private_Type_Definition then
         Comp_Type := Enclosing_Element (Comp_Type);
         Comp_Type := Corresponding_Type_Declaration (Comp_Type);
         Comp_Type := Type_Declaration_View (Comp_Type);
      end if;

      Set_All_Named_Components   (Comp_Type);
      Set_Parent_Type_Definition (Comp_Type);
      Set_Record_Type_Entity     (Component);

      Set_Record_Components_From_Names
        (Parent_First_Bit => Component.First_Bit,
         Data_Stream         => Data_Stream);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
                 Package_Name &
                 "Record_Components (from Record_Component and Data_Stream)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  =>
              Package_Name &
              "Record_Components (from Record_Component and Data_Stream)",
            Ex          => Ex);
   end Record_Components;

   ----------------------------------------------
   -- Record_Components (from Type_Definition) --
   ----------------------------------------------

   function Record_Components
     (Type_Definition : Asis.Type_Definition)
      return            Record_Component_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
   begin

      Check_Validity
        (Type_Definition,
         Package_Name & "Record_Components (from Type_Definition)");

      if not ((Arg_Kind = A_Record_Type_Definition or else
               Is_Derived_From_Record (Type_Definition))
             and then
               Type_Model_Kind (Type_Definition) = A_Simple_Static_Model)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Record_Components (from Type_Definition)",
            Wrong_Kind => Arg_Kind);
      end if;

      Set_All_Named_Components   (Type_Definition);
      Set_Parent_Type_Definition (Type_Definition);
      Set_Record_Type_Entity;

      Set_Record_Components_From_Names;

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Record_Components (from Type_Definition)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Record_Components (from Type_Definition)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Record_Components;

   --------------------------------------------------------------
   -- Record_Components (from Type_Definition and Data_Stream) --
   --------------------------------------------------------------

   function Record_Components
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data)
      return            Record_Component_List
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Type_Model : Type_Model_Kinds;

   begin
      Check_Validity
        (Type_Definition,
         Package_Name &
         "Record_Components (from Type_Definition and Data_Stream)");

      Type_Model := Type_Model_Kind (Type_Definition);

      if not ((Arg_Kind = A_Record_Type_Definition or else
               Is_Derived_From_Record (Type_Definition))
             and then
               (Type_Model = A_Simple_Static_Model or else
                Type_Model = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name &
            "Record_Components (from Type_Definition and Data_Stream)",
             Wrong_Kind => Arg_Kind);
      end if;

      Set_All_Named_Components   (Type_Definition);
      Set_Parent_Type_Definition (Type_Definition);
      Set_Record_Type_Entity;

      Set_Record_Components_From_Names (Data_Stream => Data_Stream);

      return Record_Component_List (
         RC_Table (1 .. Record_Component_Table.Last));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call =>
                 Package_Name &
                 "Record_Components (from Type_Definition and Data_Stream)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  =>
              Package_Name &
              "Record_Components (from Type_Definition and Data_Stream)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Record_Components;

   -----------
   -- Reset --
   -----------

   procedure Reset (Iterator : in out Array_Component_Iterator) is
   begin
      Check_Validity (Iterator.Component, Package_Name & "Reset");

      --  ??? what about checking for being not-null???
      Iterator.Index := 0;
   end Reset;

   ---------------------------------
   -- Size (from Array_Component) --
   ---------------------------------

   function Size
     (Component : Array_Component)
      return      Asis.ASIS_Natural
   is
   begin
      Check_Validity (Component, Package_Name & "Size (from Array_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Size (from Array_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      return Component.Size;

   end Size;

   ----------------------------------
   -- Size (from Record_Component) --
   ----------------------------------

   function Size
     (Component : Record_Component)
      return      Asis.ASIS_Natural
   is
   begin
      Check_Validity
        (Component, Package_Name & "Size (from Record_Component)");

      if Is_Nil (Component) then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Size (from Record_Component)",
            Wrong_Kind => Not_An_Element);  --  ???
      end if;

      return Component.Size;

   end Size;

   -------------------------------------------------
   -- Size (from Type_Definition and Data_Stream) --
   -------------------------------------------------

   function Size
     (Type_Definition : Asis.Type_Definition;
      Data_Stream     : Portable_Data)
      return            Asis.ASIS_Natural
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Type_Mod : constant Type_Model_Kinds       :=
         Type_Model_Kind (Type_Definition);
      Type_Ent : Node_Id;
      Result   : Asis.ASIS_Natural;
   begin
      Check_Validity
        (Type_Definition,
         Package_Name & "Size (from Type_Definition and Data_Stream)");

      if not (Arg_Kind in Internal_Type_Kinds and then
             (Type_Mod = A_Simple_Static_Model
            or else
              Type_Mod = A_Simple_Dynamic_Model))
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Size (from Type_Definition and Data_Stream)",
            Wrong_Kind => Arg_Kind);
      end if;

      Type_Ent := R_Node (Type_Definition);
      Type_Ent := Defining_Identifier (Parent (Type_Ent));

      declare
         Discs : constant Discrim_List :=
            Build_Discrim_List_If_Data_Presented (Type_Ent, Data_Stream);
      begin
         Result := UI_To_Aint (Get_Esize (Type_Ent, Discs));
      end;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Size (from Type_Definition and Data_Stream)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Size (from Type_Definition and Data_Stream)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Size;

   ---------------------------------
   -- Size (from Type_Definition) --
   ---------------------------------

   function Size
     (Type_Definition : Asis.Type_Definition)
      return            Asis.ASIS_Natural
   is
      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
      Type_Ent : Node_Id;
   begin
      Check_Validity
        (Type_Definition, Package_Name & "Size (from Type_Definition)");

      if not ((Arg_Kind in Internal_Type_Kinds or else
               Arg_Kind = A_Subtype_Indication)
            and then
              Type_Model_Kind (Type_Definition) = A_Simple_Static_Model)
      then
         Raise_ASIS_Inappropriate_Element
           (Package_Name & "Size (from Type_Definition)",
            Wrong_Kind => Arg_Kind);
      end if;

      --  The idea is to compute a type (or subtype) entity node
      --  (it may correspond to implicit type created by the compiler)
      --  and to get the size information from it

      if Arg_Kind in Internal_Type_Kinds then
         Type_Ent := R_Node (Type_Definition);
         Type_Ent := Defining_Identifier (Parent (Type_Ent));
      else
         Type_Ent := Subtype_Entity (Type_Definition);
      end if;

      return ASIS_Natural (UI_To_Int (Get_Esize (Type_Ent)));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name & "Size (from Type_Definition)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Size (from Type_Definition",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Size;

   --------------------------------------------
   -- Type_Model_Kind (from Array_Component) --
   --------------------------------------------

   function Type_Model_Kind
     (Component : Array_Component)
      return      Type_Model_Kinds
   is
      Component_Subtype_Indication : Asis.Element;
      Result                       : Type_Model_Kinds := Not_A_Type_Model;
   begin
      Check_Validity
        (Component, Package_Name & "Type_Model_Kind (from array component)");

      if not Is_Nil (Component) then
         Component_Subtype_Indication := Component_Indication (Component);
         Result := Subtype_Model_Kind (Component_Subtype_Indication);
      end if;

      return Result;

   exception

      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Type_Model_Kind (from array component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Type_Model_Kind (from array component)",
            Ex          => Ex);
   end Type_Model_Kind;

   ---------------------------------------------
   -- Type_Model_Kind (from Record_Component) --
   ---------------------------------------------

   function Type_Model_Kind
     (Component : Record_Component)
      return      Type_Model_Kinds
   is
      Comp_Constraint : Asis.Element;
      Comp_Entity     : Entity_Id;
      Result          : Type_Model_Kinds := Not_A_Type_Model;
   begin

      Check_Validity
        (Component, Package_Name & "Type_Model_Kind (from record component)");

      if not Is_Nil (Component) then

         Comp_Entity     := R_Node (Component_Name (Component));
         Comp_Constraint := Component_Declaration (Component);

         if Einfo.Size_Known_At_Compile_Time (Comp_Entity)            or else
            Int_Kind (Comp_Constraint) = A_Discriminant_Specification or else
            Einfo.Esize (Comp_Entity) > 0
         then
            Result := A_Simple_Static_Model;

         elsif Is_Record (Component) or else Is_Array (Component) then
            --  Here we can have an index or a discriminant constraint which
            --  may give us A_Simple_Static_Model, if the constraint depends
            --  on discriminants only

            Comp_Constraint := Object_Declaration_View (Comp_Constraint);
            Comp_Constraint := Component_Subtype_Indication (Comp_Constraint);
            Comp_Constraint := Subtype_Constraint (Comp_Constraint);

            if Constraint_Model_Kind (Comp_Constraint) = External then
               Result := A_Complex_Dynamic_Model;
            else
               Result := A_Simple_Dynamic_Model;
            end if;

         else
            Result := A_Complex_Dynamic_Model;
         end if;

      end if;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name &
                             "Type_Model_Kind (from record component)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Type_Model_Kind (from record component)",
            Ex          => Ex);
   end Type_Model_Kind;

   --------------------------------------------
   -- Type_Model_Kind (from Type_Definition) --
   --------------------------------------------

   function Type_Model_Kind
     (Type_Definition : Asis.Type_Definition)
      return            Type_Model_Kinds
   is
      Type_Entity : Node_Id;
      Result      : Type_Model_Kinds       := Not_A_Type_Model;

      Arg_Kind : constant Internal_Element_Kinds := Int_Kind (Type_Definition);
   begin

      Check_Validity (Type_Definition, Package_Name & "Type_Model_Kind");

      case Arg_Kind is

         when A_Derived_Type_Definition =>

            Result := Subtype_Model_Kind
                        (Parent_Subtype_Indication (Type_Definition)); --  ???

         when An_Enumeration_Type_Definition     |
              A_Signed_Integer_Type_Definition   |
              A_Modular_Type_Definition          |
              A_Floating_Point_Definition        |
              An_Ordinary_Fixed_Point_Definition |
              A_Decimal_Fixed_Point_Definition   |
              A_Root_Integer_Definition          |
              A_Root_Real_Definition             |
              A_Universal_Integer_Definition     |
              A_Universal_Real_Definition        |
              A_Universal_Fixed_Definition       |
              A_Pool_Specific_Access_To_Variable |
              An_Access_To_Variable              |
              An_Access_To_Constant              |
              An_Access_To_Procedure             |
              An_Access_To_Protected_Procedure   |
              An_Access_To_Function              |
              An_Access_To_Protected_Function =>

            Result := A_Simple_Static_Model;

         when An_Unconstrained_Array_Definition =>

            Result := Subtype_Model_Kind
                         (Component_Subtype_Indication
                         (Array_Component_Definition (Type_Definition)));

            if Result = A_Simple_Static_Model then
               Result := A_Simple_Dynamic_Model;
            else
               Result := A_Complex_Dynamic_Model;
            end if;

         when A_Constrained_Array_Definition =>

            --  Actually, for A_Constrained_Array_Definition we have only
            --  two possibilities: either A_Simple_Static_Model or
            --  A_Complex_Dynamic_Model, because in DDA no Data Stream
            --  can be supplied to the query extracting array components
            --  from A_Constrained_Array_Definition

            Type_Entity := R_Node (Type_Definition);
            Type_Entity := Sinfo.Defining_Identifier (Parent (Type_Entity));

            if Ekind (Type_Entity) in Object_Kind then
               --  Array definition as a part of an object definition, here we
               --  have an anonymous array type
               Type_Entity := Etype (Type_Entity);
            end if;

            if Einfo.Size_Known_At_Compile_Time (Type_Entity) or else
               Esize (Type_Entity) > 0
            then
               Result := A_Simple_Static_Model;
            else

               Result := Subtype_Model_Kind
                            (Component_Subtype_Indication
                            (Array_Component_Definition (Type_Definition)));

               if Result = A_Simple_Static_Model then
                  Result := A_Simple_Dynamic_Model;
               else
                  Result := A_Complex_Dynamic_Model;
               end if;

            end if;

         when A_Record_Type_Definition =>
            Result := Record_Model_Kind (Type_Definition);

         when A_Derived_Record_Extension_Definition |
              A_Tagged_Record_Type_Definition =>

            --  We consider tagged types as too complex for DDA without any
            --  further analysis
            Result := A_Complex_Dynamic_Model;

         when A_Subtype_Indication =>

            Result := Subtype_Model_Kind (Type_Definition); --  ???

         when others =>
            null;

      end case;

      return Result;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Argument   => Type_Definition,
               Outer_Call => Package_Name &
                             "Type_Model_Kind (from type definition)");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name &
                           "Type_Model_Kind (from type definition)",
            Ex          => Ex,
            Arg_Element => Type_Definition);
   end Type_Model_Kind;

end Asis.Data_Decomposition;
