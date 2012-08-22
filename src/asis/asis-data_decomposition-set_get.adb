------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--       A S I S . D A T A _ D E C O M P O S I T I O N . S E T _ G E T      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2009, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

with Asis.Declarations;              use Asis.Declarations;
with Asis.Definitions;               use Asis.Definitions;
with Asis.Elements;                  use Asis.Elements;
with Asis.Extensions;                use Asis.Extensions;
with Asis.Iterator;                  use Asis.Iterator;

with Asis.Set_Get;                   use Asis.Set_Get;

with Asis.Data_Decomposition.Aux;    use Asis.Data_Decomposition.Aux;

with A4G.Contt;                      use A4G.Contt;

with Atree;                          use Atree;
with Sinfo;                          use Sinfo;
with Einfo;                          use Einfo;
with Nlists;                         use Nlists;
with Uintp;                          use Uintp;

package body Asis.Data_Decomposition.Set_Get is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Derived_Type_Components (E : Asis.Element);
   --  Provided that E is a derived type definition, this procedure sets in
   --  Asis_Element_Table the defining identifiers of the components of the
   --  corresponding type. (For inherited components it sets the corresponding
   --  implicit names)

   --------------------
   -- Component_Name --
   --------------------

   function Component_Name (Comp : RC) return Asis.Defining_Name is
   begin
      return Comp.Component_Name;
   end Component_Name;

   ---------------
   -- Dimension --
   ---------------

   function Dimension (Comp : AC) return ASIS_Natural
   is
   begin
      return Comp.Dimension;
   end  Dimension;

   ---------------------------
   -- Get_Array_Type_Entity --
   ---------------------------

   function Get_Array_Type_Entity (Comp : AC) return  Entity_Id is
      Result : Entity_Id;
      pragma Warnings (Off, Result);
   begin
      --  ???!!! This is a trick needed to reset the right tree!
      --  ???!!! Should be replaced by a proper tree handling for
      --  ???!!! array components

      Result := Node (Comp.Parent_Array_Type);

      Result := Comp.Array_Type_Entity;

      return Result;
   end Get_Array_Type_Entity;

   ---------------------
   -- Get_Comp_Entity --
   ---------------------

   function Get_Comp_Entity (Comp : RC) return Entity_Id is
   begin
      return R_Node (Component_Name (Comp));
   end Get_Comp_Entity;

   -----------------------
   -- Get_Record_Entity --
   -----------------------

   function Get_Record_Entity (Comp : RC) return Entity_Id is
      Result : Entity_Id;
   begin
      Result := R_Node (Parent_Record_Type (Comp));

      while Nkind (Result) /= N_Full_Type_Declaration loop
         Result := Parent (Result);
      end loop;

      Result := Defining_Identifier (Result);

      return Result;

   end Get_Record_Entity;

   ---------------------
   -- Get_Type_Entity --
   ---------------------

   function Get_Type_Entity    (Comp : RC) return Node_Id is
      Result : Node_Id;
   begin
      Result := Etype (R_Node (Component_Name (Comp)));

      if Ekind (Result) = E_Private_Type then
         Result := Full_View (Result);
      end if;

      return Result;
   end Get_Type_Entity;

   -------------------
   -- Is_Array_Comp --
   -------------------

   function Is_Array_Comp (Comp : AC) return Boolean is
   begin
      return Comp.Is_Array_Comp;
   end Is_Array_Comp;

   function Is_Array_Comp (Comp : RC) return Boolean is
   begin
      return Comp.Is_Array_Comp;
   end Is_Array_Comp;

   --------------------
   -- Is_Record_Comp --
   --------------------

   function Is_Record_Comp (Comp : AC) return Boolean is
   begin
      return Comp.Is_Record_Comp;
   end Is_Record_Comp;

   function Is_Record_Comp (Comp : RC) return Boolean is
   begin
      return Comp.Is_Record_Comp;
   end Is_Record_Comp;

   -----------------------
   -- Parent_Array_Type --
   -----------------------

   function Parent_Array_Type (Comp : AC) return Asis.Declaration is
   begin
      return Comp.Parent_Array_Type;
   end Parent_Array_Type;

   ---------------------
   -- Parent_Discrims --
   ---------------------

   function Parent_Discrims (Comp : AC) return Discrim_List is
   begin
      if Comp.Parent_Discrims = null then
         return Null_Discrims;
      else
         return Comp.Parent_Discrims.all;
      end if;
   end Parent_Discrims;

   function Parent_Discrims (Comp : RC) return Discrim_List is
   begin
      if Comp.Parent_Discrims = null then
         return Null_Discrims;
      else
         return Comp.Parent_Discrims.all;
      end if;
   end Parent_Discrims;

   ------------------------
   -- Parent_Record_Type --
   ------------------------

   function Parent_Record_Type (Comp : RC) return Asis.Declaration is
   begin
      return Comp.Parent_Record_Type;
   end Parent_Record_Type;

   -------------------------
   -- Set_Array_Componnet --
   -------------------------

   function Set_Array_Componnet
     (Array_Type_Definition      : Element;
      Enclosing_Record_Component : Record_Component := Nil_Record_Component;
      Parent_Indication          : Element          := Nil_Element;
      Parent_Discriminants       : Discrim_List     := Null_Discrims;
      Parent_First_Bit_Offset    : ASIS_Natural     := 0;
      Dynamic_Array              : Boolean          := False)
      return Array_Component
   is
      Comp_Node        : Node_Id;
      Comp_Type_Entity : Node_Id;
      Result           : Array_Component := Nil_Array_Component;

      Enclosing_Array_Type : Element;
      Array_Entity     : Entity_Id := Empty;
      --  This should be a type entity defining the enclosed
      --  array type. This may be an implicit type created by the compiler,
      --  but the point is that in should contain real ranges for
      --  this component

      Dim       : Asis.ASIS_Positive;
      Tmp_Node  : Node_Id;
      Comp_Size : ASIS_Natural;
   begin
      Result.Parent_Array_Type     := Array_Type_Definition;
      Result.Parent_Component_Name :=
         Component_Name (Enclosing_Record_Component);

      Comp_Node        := Node (Array_Type_Definition);
      Comp_Type_Entity := Defining_Identifier (Parent (Comp_Node));

      if Ekind (Comp_Type_Entity) in Object_Kind then
         --  Array definition as a part of an object definition, here we have
         --  an anonymous array type
         Comp_Type_Entity := Etype (Etype (Comp_Type_Entity));
         Array_Entity     := Comp_Type_Entity;
      end if;

      Comp_Type_Entity := Component_Type (Comp_Type_Entity);

      if Ekind (Comp_Type_Entity) = E_Private_Type then
         Comp_Type_Entity := Full_View (Comp_Type_Entity);
      end if;

      Result.Is_Record_Comp := Is_Record_Type (Comp_Type_Entity);
      Result.Is_Array_Comp  := Is_Array_Type  (Comp_Type_Entity);

      if not Is_Nil (Enclosing_Record_Component) then
         Array_Entity := R_Node (Enclosing_Record_Component.Component_Name);
         Array_Entity := Etype (Array_Entity);

      elsif not Is_Nil (Parent_Indication) then
         Enclosing_Array_Type := Enclosing_Element (Parent_Indication);

         Enclosing_Array_Type := Enclosing_Element (Enclosing_Array_Type);
         Enclosing_Array_Type := Enclosing_Element (Enclosing_Array_Type);

         Array_Entity := Defining_Identifier (R_Node (Enclosing_Array_Type));
         Array_Entity := Component_Type (Array_Entity);

      elsif No (Array_Entity) then
         Enclosing_Array_Type := Enclosing_Element (Array_Type_Definition);
         Array_Entity := Defining_Identifier (R_Node (Enclosing_Array_Type));
      end if;

      if Ekind (Array_Entity) = E_Private_Type then
         Array_Entity := Full_View (Array_Entity);
      end if;

      Result.Array_Type_Entity := Array_Entity;

      --  Computing dimentions and lengths:

      Tmp_Node := First_Index (Array_Entity);

      Dim := ASIS_Positive (List_Length (List_Containing (Tmp_Node)));

      Result.Dimension := Dim;

      Result.Length := (others => 0);

      for I in 1 .. Dim loop

         if Dynamic_Array then
            Result.Length (I) := 0;
         else
            Result.Length (I) := Get_Length (Typ   => Array_Entity,
                                             Sub   => I,
                                             Discs => Parent_Discriminants);
         end if;

      end loop;

      Comp_Size := ASIS_Natural (UI_To_Int (
         Get_Component_Size (Array_Entity)));

      Result.Position := Parent_First_Bit_Offset / Storage_Unit;

      Result.First_Bit := Parent_First_Bit_Offset mod Storage_Unit;

      Result.Last_Bit  := Result.First_Bit + Comp_Size - 1;

      Result.Size      := Comp_Size;

      Set_Parent_Discrims (Result, Parent_Discriminants);

      Result.Parent_Context := Get_Current_Cont;
      Result.Obtained       := A_OS_Time;

      return Result;

   end Set_Array_Componnet;

   ------------------------------
   -- Set_All_Named_Components --
   ------------------------------

   procedure Set_All_Named_Components (E : Element) is
      Discr_Part  : Element;
   begin
      if Asis.Elements.Type_Kind (E) = A_Derived_Type_Definition then
         Set_Derived_Type_Components (E);
      else
         Discr_Part := Discriminant_Part (Enclosing_Element (E));
         Set_Named_Components (Discr_Part, New_List);
         Set_Named_Components (E, Append);
      end if;

   end Set_All_Named_Components;

   ---------------------------------
   -- Set_Derived_Type_Components --
   ---------------------------------

   procedure Set_Derived_Type_Components (E : Asis.Element) is
      Discr_Part : constant Asis.Element :=
                     Discriminant_Part (Enclosing_Element (E));
      Impl_Comps : constant Asis.Element_List :=
                     Implicit_Inherited_Declarations (E);
   begin

      Set_Named_Components (Discr_Part, New_List);

      for J in Impl_Comps'Range loop
         Asis_Element_Table.Append (Names (Impl_Comps (J)) (1));
      end loop;

   end Set_Derived_Type_Components;

   --------------------------
   -- Set_Named_Components --
   --------------------------

   procedure Set_Named_Components (E : Element; List_Kind : List_Kinds) is

      Control  : Traverse_Control := Continue;
      State    : No_State := Not_Used;

      procedure Set_Def_Name
        (Element : Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State);
      --  If Element is of A_Defining_Identifier kind, this procedure stores
      --  it in the Asis Element Table. Used as Pre-Operation

      procedure Set_Def_Name
        (Element : Asis.Element;
         Control : in out Traverse_Control;
         State   : in out No_State)
      is
      begin
         pragma Unreferenced (Control);
         pragma Unreferenced (State);

         if Int_Kind (Element) /= A_Defining_Identifier then
            return;
         end if;

         Asis_Element_Table.Append (Element);
      end Set_Def_Name;

      procedure Create_Name_List is new Traverse_Element (
         State_Information => No_State,
         Pre_Operation     => Set_Def_Name,
         Post_Operation    => No_Op);

   begin

      if List_Kind = New_List then
         Asis_Element_Table.Init;
      end if;

      if Is_Nil (E) then
         return;
      end if;

      Create_Name_List
        (Element => E,
         Control => Control,
         State   => State);

   end Set_Named_Components;

   -------------------------
   -- Set_Parent_Discrims --
   -------------------------

   procedure Set_Parent_Discrims (Comp : in out AC; Discs : Discrim_List) is
   begin

      if Discs = Null_Discrims then
         Comp.Parent_Discrims := null;
      else
         Comp.Parent_Discrims := new Discrim_List'(Discs);
      end if;

   end Set_Parent_Discrims;

   ----------------------------
   -- Set_Record_Type_Entity --
   ----------------------------

   procedure Set_Record_Type_Entity (AC : Array_Component) is
   begin
      Record_Type_Entity := Get_Array_Type_Entity (AC);
      Record_Type_Entity := Component_Type (Record_Type_Entity);
   end Set_Record_Type_Entity;

   procedure Set_Record_Type_Entity (RC : Record_Component) is
   begin
      Record_Type_Entity := R_Node (Component_Name (RC));
      Record_Type_Entity := Etype (Record_Type_Entity);
   end Set_Record_Type_Entity;

   procedure Set_Record_Type_Entity is
   begin
      Record_Type_Entity :=
         Defining_Identifier (Parent (R_Node (Parent_Type_Definition)));
   end Set_Record_Type_Entity;

   --------------------------------
   -- Set_Parent_Type_Definition --
   --------------------------------

   procedure Set_Parent_Type_Definition (E : Element) is
   begin
      Parent_Type_Definition := E;
   end Set_Parent_Type_Definition;

   --------------------------------------
   -- Set_Record_Components_From_Names --
   --------------------------------------

   procedure Set_Record_Components_From_Names
     (Parent_First_Bit : ASIS_Natural  := 0;
      Data_Stream      : Portable_Data := Nil_Portable_Data;
      Discriminants    : Boolean       := False)
   is
      New_Comp         : Asis.List_Index;
      Component_Name   : Element;
      Comp_Entity      : Node_Id;

      Discs : constant Discrim_List :=
         Build_Discrim_List_If_Data_Presented
           (Rec          => Record_Type_Entity,
            Data         => Data_Stream,
            Ignore_Discs => Discriminants);

      Comp_Type_Entity : Node_Id;

      Comp_First_Bit_Offset : ASIS_Natural;
      Comp_Position         : ASIS_Natural;
      Comp_Size             : ASIS_Natural;
   begin
      Record_Component_Table.Init;

      for I in 1 .. Asis_Element_Table.Last loop
         Component_Name := Def_N_Table (I);
         Comp_Entity    := Node (Component_Name);

         if Discs = Null_Discrims or else
            Component_Present (Comp_Entity, Discs)
         then
            Record_Component_Table.Increment_Last;
            New_Comp       := Record_Component_Table.Last;
            RC_Table (New_Comp).Parent_Record_Type := Parent_Type_Definition;
            RC_Table (New_Comp).Component_Name     := Component_Name;

            Comp_Type_Entity := Etype (Comp_Entity);

            if Ekind (Comp_Type_Entity) = E_Private_Type then
               Comp_Type_Entity := Full_View (Comp_Type_Entity);
            end if;

            RC_Table (New_Comp).Is_Record_Comp :=
               Is_Record_Type (Comp_Type_Entity);

            RC_Table (New_Comp).Is_Array_Comp  :=
               Is_Array_Type  (Comp_Type_Entity);

            if Discs = Null_Discrims then
               RC_Table (New_Comp).Parent_Discrims := null;
            else
               RC_Table (New_Comp).Parent_Discrims  :=
                  new Discrim_List'(Discs);
            end if;

            Comp_First_Bit_Offset := Parent_First_Bit +
               ASIS_Natural (UI_To_Int (
                  Get_Component_Bit_Offset (Comp_Entity, Discs)));

            Comp_Position := Comp_First_Bit_Offset / Storage_Unit;

            Comp_Size := ASIS_Natural (UI_To_Int
                            (Get_Esize (Comp_Entity, Discs)));

            RC_Table (New_Comp).Position := Comp_Position;

            RC_Table (New_Comp).First_Bit :=
               Comp_First_Bit_Offset mod Storage_Unit;

            RC_Table (New_Comp).Last_Bit  :=
               RC_Table (New_Comp).First_Bit + Comp_Size - 1;

            RC_Table (New_Comp).Size      := Comp_Size;

            RC_Table (New_Comp).Parent_Context := Get_Current_Cont;
            RC_Table (New_Comp).Obtained       := A_OS_Time;

         end if;

      end loop;
   end Set_Record_Components_From_Names;

end Asis.Data_Decomposition.Set_Get;
