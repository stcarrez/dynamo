------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A S I S . S E T _ G E T                          --
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
-- CHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License  distributed with ASIS-for-GNAT; see   file --
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
-- (http://www.adaccore.com).                                               --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with A4G.A_Sem;               use A4G.A_Sem;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Contt.TT;            use A4G.Contt.TT;
with A4G.GNAT_Int;            use A4G.GNAT_Int;
with A4G.Knd_Conv;            use A4G.Knd_Conv;

with Atree;                   use Atree;
with Sinfo;                   use Sinfo;
with Stand;                   use Stand;
with Uintp;                   use Uintp;

package body Asis.Set_Get is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Element_In_Current_Tree (E : Element) return Boolean;
   --  Checks if the currently accessed tree is the tree from which the
   --  argument has been obtained.

   ---------------------
   -- To_Program_Text --
   ---------------------

   function To_Program_Text (S : String) return Program_Text is
      Result       : Wide_String (1 .. S'Length);
      Result_Len   : Natural := 0;
      Next_Char    : Natural := S'First;
      Tmp          : Natural;

      function To_Wide_Char (S : String) return Wide_Character;
      --  Converts ["hh"] and ["hhhh"] notation into Wide_Character

      function To_Wide_Char (S : String) return Wide_Character is
         Numerical : Natural := 0;

         type Xlate is array (Character range '0' .. 'F') of Natural;

         Xlation : constant Xlate :=
           ('0' =>  0, '1' =>  1, '2' =>  2, '3' =>  3, '4' =>  4,
            '5' =>  5, '6' =>  6, '7' =>  7, '8' =>  8, '9' =>  9,
            'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14,
            'F' => 15,  others => 0);
      begin
         for I in S'Range loop
            Numerical := Numerical * 16 + Xlation (S (I));
         end loop;

         return Wide_Character'Val (Numerical);
      end To_Wide_Char;

   begin

      while Next_Char <= S'Last loop

         Result_Len := Result_Len + 1;

         if S (Next_Char) = '[' then
            Next_Char := Next_Char + 2;
            Tmp       := Next_Char;

            while S (Tmp + 1) /= '"' loop
               Tmp := Tmp + 1;
            end loop;

            Result (Result_Len) :=
               To_Wide_Char (S (Next_Char .. Tmp));

            Next_Char := Tmp + 3;

         else
            Result (Result_Len) :=
               To_Wide_Character (S (Next_Char));
            Next_Char := Next_Char + 1;
         end if;

      end loop;

      return Result (1 .. Result_Len);

   end To_Program_Text;

   -------------
   -- CONTEXT --
   -------------

   -------------------------------------
   -- Id <-> ASIS Context conversions --
   -------------------------------------

   function Get_Cont_Id (C  : Context)    return Context_Id is
   begin
      return C.Id;
   end Get_Cont_Id;

   function Get_Cont (Id : Context_Id) return Context is
   begin
      return (Id => Id);
   end Get_Cont;

   procedure Set_Cont   (C  : out Context; Id : Context_Id) is
   begin
      C.Id := Id;
   end Set_Cont;

   function Valid (C : Context) return Boolean is
   begin
      return Is_Opened (C.Id);
   end Valid;

----------------------
-- COMPILATION_UNIT --
----------------------

   ----------------------------------------------
   -- Id <-> ASIS Compilation Unit conversions --
   ----------------------------------------------

   function Get_Unit_Id (C_U : Compilation_Unit) return Unit_Id is
   begin
      return C_U.Id;
   end Get_Unit_Id;

   function Get_Comp_Unit
     (U  : Unit_Id;
      C  : Context_Id)
      return Compilation_Unit
   is
      Result_Unit : Compilation_Unit;
   begin
      if U = Nil_Unit then
         return Nil_Compilation_Unit;
      end if;
      Result_Unit := (Cont_Id => C, Id => U, Obtained => A_OS_Time);
      return Result_Unit;
   end Get_Comp_Unit;

   ------------------------
   -- Get_Comp_Unit_List --
   ------------------------

   function Get_Comp_Unit_List
     (U_List : Unit_Id_List;
      C      : Context_Id)
      return Compilation_Unit_List
   is
      Result_Len  : constant Natural := U_List'Length;
      Result_List : Compilation_Unit_List (1 .. Result_Len);
      U_L_First   : constant Natural := U_List'First;
   begin
      for I in 1 .. Result_Len loop
         Result_List (I) := Get_Comp_Unit (U_List (U_L_First + I - 1), C);
      end loop;
      return Result_List;
   end Get_Comp_Unit_List;

   -----------------------------------------
   -- Getting Compilation Unit Attributes --
   -----------------------------------------

   function Not_Nil (C_U : Compilation_Unit) return Boolean is
   begin
      return Get_Unit_Id (C_U) /= Nil_Unit;
   end Not_Nil;

   function Nil (C_U : Compilation_Unit) return Boolean  is
   begin
      return Get_Unit_Id (C_U) = Nil_Unit;
   end Nil;

   function Is_Standard (C_U : Compilation_Unit) return Boolean is
   begin
      return Get_Unit_Id (C_U) = Standard_Id;
   end Is_Standard;

   function Kind (C_U : Compilation_Unit) return Asis.Unit_Kinds is
   begin
      if C_U.Id = Nil_Unit then
         return Not_A_Unit;
      else
         return Kind (C_U.Cont_Id, C_U.Id);
      end if;
   end Kind;

   function Class (C_U : Compilation_Unit) return Unit_Classes is
   begin
      if C_U.Id = Nil_Unit then
         return Not_A_Class;
      else
         return Class (C_U.Cont_Id, C_U.Id);
      end if;
   end Class;

   function Origin (C_U : Compilation_Unit) return Unit_Origins is
   begin
      if C_U.Id = Nil_Unit then
         return Not_An_Origin;
      else
         return Origin (C_U.Cont_Id, C_U.Id);
      end if;
   end Origin;

   function Is_Main_Unit (C_U : Compilation_Unit) return Boolean  is
   begin
      return Is_Main_Unit (C_U.Cont_Id, C_U.Id);
   end Is_Main_Unit;

   function Top (C_U : Compilation_Unit) return Node_Id is
   begin
      if not Unit_In_Current_Tree (C_U.Cont_Id, C_U.Id) then
         Reset_Tree_For_Unit (C_U.Cont_Id, C_U.Id);
      end if;

      return Top (C_U.Id);
   end Top;

   function Is_Body_Required (C_U : Compilation_Unit) return Boolean is
   begin
      return Is_Body_Required (C_U.Cont_Id, C_U.Id);
   end Is_Body_Required;

   function Encl_Cont (C_U : Compilation_Unit) return Context is
   begin
      return Get_Cont (C_U.Cont_Id);
   end Encl_Cont;

   function Unit_Name (C_U : Compilation_Unit) return String is
   begin
      Get_Name_String (C_U.Id, Ada_Name);
      return A4G.Contt.A_Name_Buffer (1 ..  A4G.Contt.A_Name_Len);
   end Unit_Name;

   function Encl_Cont_Id (C_U : Compilation_Unit) return Context_Id is
   begin
      return C_U.Cont_Id;
   end Encl_Cont_Id;

   function Source_File (C_U : Compilation_Unit) return String
   is
   begin
      if Length_Of_Name (C_U.Id, Source_File_Name) = 0 then
         return Nil_Asis_String;
      else
         Get_Name_String (C_U.Id, Source_File_Name);
         return A4G.Contt.A_Name_Buffer (1 ..  A4G.Contt.A_Name_Len);
      end if;
   end Source_File;

   function Ref_File (C_U : Compilation_Unit) return String
   is
   begin
      if Length_Of_Name (C_U.Id, Ref_File_Name) = 0 then
         return Nil_Asis_String;
      else
         Get_Name_String (C_U.Id, Ref_File_Name);
         return A4G.Contt.A_Name_Buffer (1 ..  A4G.Contt.A_Name_Len);
      end if;
   end Ref_File;

   function Context_Info (C_U : Compilation_Unit) return String
   is
   begin
      return Context_Info (C_U.Cont_Id);
   end Context_Info;

   function Time_Stamp (C_U : Compilation_Unit) return Time
   is
   begin
      return A_Time (Time_Stamp (C_U.Cont_Id, C_U.Id));
   end Time_Stamp;

   function Source_Status (C_U : Compilation_Unit)
      return Source_File_Statuses
   is
   begin
      if C_U.Id = Nil_Unit then
         return No_File_Status;
      else
         return Source_Status (C_U.Cont_Id, C_U.Id);
      end if;
   end Source_Status;

   function Main_Tree (C_U : Compilation_Unit) return Tree_Id is
   begin
      return Main_Tree (C_U.Cont_Id, C_U.Id);
   end Main_Tree;

   -------------------
   -- Miscellaneous --
   -------------------

   function "=" (Left, Right : Compilation_Unit) return Boolean is
      Result : Boolean;
   begin
      Result :=
         Left.Id       = Right.Id and then
         Left.Cont_Id  = Right.Cont_Id and then
         Left.Obtained = Right.Obtained;

      return Result;

   end "=";

   --------------------------
   -- Get_Configuration_CU --
   --------------------------

   function Get_Configuration_CU
     (C_U :  Compilation_Unit)
      return Compilation_Unit
   is
   begin
      return Get_Comp_Unit (Config_Comp_Id, Encl_Cont_Id (C_U));
   end Get_Configuration_CU;

   -------------------
   -- Set_Main_Tree --
   -------------------

   procedure Reset_Main_Tree (C_U : Compilation_Unit) is
      Main_Tree_Id : constant Tree_Id := Main_Tree (C_U);
   begin
      if Main_Tree_Id /= Nil_Tree then
         Reset_Tree (C_U.Cont_Id, Main_Tree_Id);
      end if;
   end Reset_Main_Tree;

   -----------
   -- Valid --
   -----------

   function Valid (C_U : Compilation_Unit) return Boolean is
   begin
      return Is_Opened (C_U.Cont_Id) and then
             Later (Opened_At (C_U.Cont_Id), C_U.Obtained);
   end Valid;

-------------
-- ELEMENT --
-------------

   function "=" (Left, Right : Element) return Boolean is
      Result : Boolean;
   begin
      --  just literal field-by-field comparison
      Result :=
         Left.Node                 = Right.Node                 and then
         Left.R_Node               = Right.R_Node               and then
         Left.Node_Field_1         = Right.Node_Field_1         and then
         Left.Node_Field_2         = Right.Node_Field_2         and then
         Left.Enclosing_Unit       = Right.Enclosing_Unit       and then
         Left.Enclosing_Context    = Right.Enclosing_Context    and then
         Left.Internal_Kind        = Right.Internal_Kind        and then
         Left.Is_Part_Of_Implicit  = Right.Is_Part_Of_Implicit  and then
         Left.Is_Part_Of_Inherited = Right.Is_Part_Of_Inherited and then
         Left.Is_Part_Of_Instance  = Right.Is_Part_Of_Instance  and then
         Left.Special_Case         = Right.Special_Case         and then
         Left.Enclosing_Tree       = Right.Enclosing_Tree       and then
         Left.Rel_Sloc             = Right.Rel_Sloc             and then
         Left.Character_Code       = Right.Character_Code       and then
         Left.Obtained             = Right.Obtained;

      return Result;
   end "=";

   ---------
   -- Get --
   ---------

   function Node   (E : Element) return Node_Id is
   begin
      if E.Internal_Kind /= Not_An_Element and then
         not Element_In_Current_Tree (E)
      then
         Reset_Tree (E.Enclosing_Context, E.Enclosing_Tree);
      end if;

      return E.Node;
   end Node;

   function R_Node (E : Element) return Node_Id is
   begin
      if E.Internal_Kind /= Not_An_Element and then
         not Element_In_Current_Tree (E)
      then
         Reset_Tree (E.Enclosing_Context, E.Enclosing_Tree);
      end if;

      return E.R_Node;
   end R_Node;

   function Node_Field_1 (E : Element) return Node_Id is
   begin
      if E.Internal_Kind /= Not_An_Element and then
         not Element_In_Current_Tree (E)
      then
         Reset_Tree (E.Enclosing_Context, E.Enclosing_Tree);
      end if;

      return E.Node_Field_1;
   end Node_Field_1;

   function Node_Field_2 (E : Element) return Node_Id is
   begin
      if E.Internal_Kind /= Not_An_Element and then
         not Element_In_Current_Tree (E)
      then
         Reset_Tree (E.Enclosing_Context, E.Enclosing_Tree);
      end if;

      return E.Node_Field_2;
   end Node_Field_2;

   function Node_Value (E : Element) return Node_Id is
   begin
      return E.Node;
   end Node_Value;

   function R_Node_Value (E : Element) return Node_Id is
   begin
      return E.R_Node;
   end R_Node_Value;

   function Node_Field_1_Value (E : Element) return Node_Id is
   begin
      return E.Node_Field_1;
   end Node_Field_1_Value;

   function Node_Field_2_Value (E : Element) return Node_Id is
   begin
      return E.Node_Field_2;
   end Node_Field_2_Value;

   function Encl_Unit    (E : Element) return Compilation_Unit is
   begin
      return Get_Comp_Unit (E.Enclosing_Unit, E.Enclosing_Context);
   end Encl_Unit;

   function Encl_Unit_Id (E : Element) return Unit_Id is
   begin
      return E.Enclosing_Unit;
   end Encl_Unit_Id;

   function Encl_Cont (E : Element) return Context is
   begin
      return Get_Cont (E.Enclosing_Context);
   end Encl_Cont;

   function Encl_Cont_Id (E : Element) return Context_Id is
   begin
      return E.Enclosing_Context;
   end Encl_Cont_Id;

   function Kind (E : Element) return Asis.Element_Kinds is
   begin
      return Asis_From_Internal_Kind (E.Internal_Kind);
   end Kind;

   function Int_Kind (E : Element) return Internal_Element_Kinds is
   begin
      return E.Internal_Kind;
   end Int_Kind;

   function Is_From_Implicit  (E : Element) return Boolean is
   begin
      return E.Is_Part_Of_Implicit;
   end Is_From_Implicit;

   function Is_From_Inherited (E : Element) return Boolean is
   begin
      return E.Is_Part_Of_Inherited;
   end Is_From_Inherited;

   function Is_From_Instance  (E : Element) return Boolean is
   begin
      return E.Is_Part_Of_Instance;
   end Is_From_Instance;

   function Special_Case      (E : Element) return Special_Cases is
   begin
      return E.Special_Case;
   end Special_Case;

   function Normalization_Case (E : Element) return Normalization_Cases is
   begin
      return E.Normalization_Case;
   end Normalization_Case;

   function Parenth_Count (E : Element) return Nat is
   begin
      return E.Parenth_Count;
   end Parenth_Count;

   function Encl_Tree (E : Element) return Tree_Id is
   begin
      return E.Enclosing_Tree;
   end Encl_Tree;

   function Rel_Sloc (E : Element) return Source_Ptr is
   begin
      return E.Rel_Sloc;
   end Rel_Sloc;

   function Character_Code (E : Element) return Char_Code is
   begin
      return E.Character_Code;
   end Character_Code;

   function Obtained (E : Element) return ASIS_OS_Time is
   begin
      return E.Obtained;
   end Obtained;

   function Location (E : Element) return Source_Ptr is
   begin
      return Sloc (Node (E));
   end Location;

   function Valid (E : Element) return Boolean is
   begin
      return Is_Opened (E.Enclosing_Context) and then
             Later (Opened_At (E.Enclosing_Context), E.Obtained);
   end Valid;

   ---------
   -- Set --
   ---------

   procedure Set_Node (E : in out Element; N : Node_Id) is
      Rel_Sloc : Source_Ptr;
   begin
      E.Node := N;

      --  If we reset the node, we have to recompute Sloc as well:
      Rel_Sloc := Sloc (N);

      if Rel_Sloc > Standard_Location then
         Rel_Sloc := Rel_Sloc - Sloc (Top (E.Enclosing_Unit));

         E.Rel_Sloc := Rel_Sloc;
      end if;

   end Set_Node;

   procedure Set_R_Node
      (E : in out Element; N : Node_Id) is
   begin
      E.R_Node := N;
   end Set_R_Node;

   procedure Set_Node_Field_1
      (E : in out Element; N : Node_Id) is
   begin
      E.Node_Field_1 := N;
   end Set_Node_Field_1;

   procedure Set_Node_Field_2
      (E : in out Element; N : Node_Id) is
   begin
      E.Node_Field_2 := N;
   end Set_Node_Field_2;

   procedure Set_Encl_Unit_Id
      (E : in out Element; U : Unit_Id) is
   begin
      E.Enclosing_Unit := U;
   end Set_Encl_Unit_Id;

   procedure Set_Enclosing_Context
      (E : in out Element; C : Context_Id) is
   begin
      E.Enclosing_Context := C;
   end Set_Enclosing_Context;

   procedure Set_Obtained
      (E : in out Element; T : ASIS_OS_Time) is
   begin
      E.Obtained := T;
   end Set_Obtained;

   procedure Set_Int_Kind
      (E : in out Element; K : Internal_Element_Kinds) is
   begin
      E.Internal_Kind := K;
   end Set_Int_Kind;

   procedure Set_From_Implicit
      (E : in out Element; I : Boolean := True) is
   begin
      E.Is_Part_Of_Implicit := I;
   end Set_From_Implicit;

   procedure Set_From_Inherited
      (E : in out Element; I : Boolean := True) is
   begin
      E.Is_Part_Of_Inherited := I;
   end Set_From_Inherited;

   procedure Set_From_Instance
      (E : in out Element; I : Boolean := True) is
   begin
      E.Is_Part_Of_Instance := I;
   end Set_From_Instance;

   procedure Set_Special_Case
      (E : in out Element; S : Special_Cases) is
   begin
      E.Special_Case := S;
   end Set_Special_Case;

   procedure Set_Normalization_Case
      (E : in out Element; N : Normalization_Cases) is
   begin
      E.Normalization_Case := N;
   end Set_Normalization_Case;

   procedure Set_Parenth_Count (E : in out Element; Val : Nat) is
   begin
      E.Parenth_Count := Val;
   end Set_Parenth_Count;

   procedure Set_Rel_Sloc
      (E : in out Element; S : Source_Ptr) is
   begin
      E.Rel_Sloc := S;
   end Set_Rel_Sloc;

   procedure Set_Character_Code
      (E : in out Element; C : Char_Code) is
   begin
      E.Character_Code := C;
   end Set_Character_Code;

   procedure Set_Encl_Tree
      (E : in out Element; T : Tree_Id) is
   begin
      E.Enclosing_Tree := T;
   end Set_Encl_Tree;

   -----------------
   -- Set_Element --
   -----------------

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
      return Element
   is
      Cont_Id      : constant Context_Id := Encl_Unit.Cont_Id;
      Un_Id        : constant Unit_Id    := Encl_Unit.Id;
      Arg_N_Kind   : Node_Kind;
      Rel_Sloc     : Source_Ptr          := No_Location;
      Ch_Code      : Char_Code           := 0;
      --  Character_Code is set "by hand" for defining character literals
      --  from Standard, when the corresponding element is created

      --  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! --
      --  ??????????????????????????????????????????????????????????????? --
      --                                                                  --
      --       Temporary solution for the problem with generics:          --
      --                                                                  --
      --  The problem consists in following: GNAT rewrites all the        --
      --  structures related to generics: generic specifications,         --
      --  generic bodies and generic instantiations, and the              --
      --  corresponding original tree structures ARE NOT fully            --
      --  decorated by semantic information.                              --
      --                                                                  --
      --  The rough fix suggested here is to use the original tree        --
      --  structures for everything except the cases mentioned above,     --
      --  and to use the rewritten structures for these cases             --
      --  when original structures are not fully decorated.               --
      --                                                                  --
      --  This fix should definitely be revised when the new model        --
      --  for generics are implemented!                                   --
      --                                                                  --
      --  ??????????????????????????????????????????????????????????????? --
      --  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! --

      --  start of the patch code for generics -----------------------------

      Element_Node : Node_Id := Node;

      function Is_Generic (Node : Node_Id) return Boolean;
      --  This function checks is its argument represents a generic unit in
      --  a source code

      function Is_Generic (Node : Node_Id) return Boolean is
         Kind    : constant Node_Kind := Nkind (Node);
         Or_Kind : constant Node_Kind := Nkind (Original_Node (Node));
         Result  : Boolean            := False;
      begin

         --  first, general condition:
         Result := Is_Rewrite_Substitution (Node) and then
                  (Kind = N_Generic_Subprogram_Declaration or else
                   Kind = N_Generic_Package_Declaration    or else
                   Kind = N_Subprogram_Body                or else
                   Kind = N_Package_Body);

         --  and now - some special exceptions (fixes in fixes -
         --  this makes me crazy!!!!
         if Result                                  and then
            Kind    = N_Generic_Package_Declaration and then
            Or_Kind = N_Formal_Package_Declaration
         then
            --  this is the case of a formal package declaration with the
            --  box rewritten into N_Generic_Package_Declaration
            Result := False;
         elsif Result and then
            (Or_Kind = N_Subprogram_Renaming_Declaration)
            --  this is the case of renaming a subprogram-attribute
--              or else  SCz
--               Or_Kind = N_Expression_Function)
         then
            Result := False;
         elsif Result and then
               Or_Kind in N_Generic_Instantiation and then
               Nkind (Parent (Node)) = N_Compilation_Unit
         then
            --  Library-level instantiation
            Result := False;
         end if;

         return Result;
      end Is_Generic;

      --  end of the patch code for generics -------------------------------

   begin
      --  start of the patch code for generics -----------------------------
      if  Is_Generic (R_Node) then
         Element_Node := R_Node;
      end if;
      --  end of the patch code for generics -------------------------------

      Arg_N_Kind := Nkind (Node);

      if Spec_Case in Predefined then
         Rel_Sloc := Standard_Location;
         --  does it really make any sense???

      --  elsif???

      else
         if Arg_N_Kind = N_Object_Declaration         or else
            Arg_N_Kind = N_Number_Declaration         or else
            Arg_N_Kind = N_Discriminant_Specification or else
            Arg_N_Kind = N_Component_Declaration      or else
            Arg_N_Kind = N_Parameter_Specification    or else
            Arg_N_Kind = N_Exception_Declaration      or else
            Arg_N_Kind = N_Formal_Object_Declaration
         then
            --  GNAT normalizes these multi-identifier declarations in the
            --  equivalent sets of one-identifier declarations, so we have to
            --  use the defining identifier node for setting Rel_Sloc

            Rel_Sloc := Sloc (Defining_Identifier (Node));
         elsif Arg_N_Kind = N_With_Clause then
            --  the same story for with clauses, but here we have to use
            --  the Name field
            Rel_Sloc := Sloc (Sinfo.Name (Node));
         else
            Rel_Sloc := Sloc (Node);
         end if;

         if Spec_Case /= Configuration_File_Pragma then
            Rel_Sloc := Rel_Sloc - Sloc (Top (Un_Id));
         end if;

      end if;

      if Arg_N_Kind = N_Character_Literal then
         Ch_Code := UI_To_CC (Char_Literal_Value (Node));
      elsif Nkind (R_Node) = N_Character_Literal then -- ???
         Ch_Code := UI_To_CC (Char_Literal_Value (R_Node));
      else
         Ch_Code := Character_Code;
      end if;

      return Element'(
      --  start of the patch code for generics -----------------------------
         Node                 => Element_Node,                            --
      --  Node                 => Node, -- the original code              --
      --  end of the patch code for generics -------------------------------
         R_Node               => R_Node,
         Node_Field_1         => Node_Field_1,
         Node_Field_2         => Node_Field_2,
         Enclosing_Unit       => Un_Id,
         Enclosing_Context    => Cont_Id,
         Internal_Kind        => Int_Kind,
         Is_Part_Of_Implicit  => Implicit,
         Is_Part_Of_Inherited => Inherited,
         Is_Part_Of_Instance  => Instance,
         Special_Case         => Spec_Case,
         Normalization_Case   => Norm_Case,
         Parenth_Count        => Par_Count,
         Enclosing_Tree       => Get_Current_Tree,
         Rel_Sloc             => Rel_Sloc,
         Character_Code       => Ch_Code,
         Obtained             => A_OS_Time);

   end Set_Element;

   -----------------------------
   -- Convert_To_Limited_View --
   -----------------------------

   procedure Convert_To_Limited_View (El : in out Asis.Element) is
   begin
      Set_From_Implicit (El, True);
      Set_Special_Case  (El, From_Limited_View);
      Set_Int_Kind      (El, Limited_View_Kind (El));
   end Convert_To_Limited_View;

   -----------------
   -- Set_In_List --
   -----------------

   function Set_In_List
     (EL           : Element_List;
      Node_Field_1 : Node_Id := Empty;
      Implicit     : Boolean := False;
      Inherited    : Boolean := False)
      return         Element_List
   is
      Result : Element_List := EL;
   begin

      for J in Result'Range loop
         Set_Node_Field_1   (Result (J), Node_Field_1);
         Set_From_Implicit  (Result (J), Implicit);
         Set_From_Inherited (Result (J), Inherited);
      end loop;

      return Result;

   end Set_In_List;

   -----------------------------
   -- Element_In_Current_Tree --
   -----------------------------

   function Element_In_Current_Tree (E : Element) return Boolean is
   begin
      return (E.Enclosing_Unit = Standard_Id)
          or else
            (E.Enclosing_Context = Get_Current_Cont and then
             E.Enclosing_Tree    = Get_Current_Tree);
   end Element_In_Current_Tree;

   -----------------------------------------------------------
   -- Special processing for Elements representing root and --
   -- universal numeric types in ASIS                       --
   -----------------------------------------------------------

   ----------------------
   -- Is_Root_Num_Type --
   ----------------------

   function Is_Root_Num_Type
     (Declaration : Asis.Declaration)
      return Boolean
   is
   begin
      return
        (Declaration.Node = Empty                                 and then
         Declaration.R_Node = Empty                               and then
         Declaration.Enclosing_Unit = Standard_Id                 and then
         Declaration.Internal_Kind = An_Ordinary_Type_Declaration and then
         Declaration.Is_Part_Of_Implicit                          and then
         Declaration.Special_Case = Implicit_From_Standard);
      --  several conditions are checked in this test - just in case
   end Is_Root_Num_Type;

   --------------------------
   -- Root_Type_Definition --
   --------------------------

   function Root_Type_Definition
     (Declaration : Asis.Declaration)
      return Asis.Definition
   is
      Result : Asis.Definition := Declaration;
   begin
      --  only two fields should be corrected:
      Result.Internal_Kind := Internal_Element_Kinds'Val (Result.Rel_Sloc);
      Result.Obtained      := A_OS_Time;
      return Result;
   end Root_Type_Definition;

   -------------------------------
   -- Set_Root_Type_Declaration --
   -------------------------------

   Root_Type_Declaration_Template : constant Element :=
      Element'(Node                 => Empty,
               R_Node               => Empty,
               Node_Field_1         => Empty,
               Node_Field_2         => Empty,
               Enclosing_Unit       => Standard_Id,
               Enclosing_Context    => Nil_Context_Id,   --  should be set
               Internal_Kind        => An_Ordinary_Type_Declaration,
               Is_Part_Of_Implicit  => True,
               Is_Part_Of_Inherited => False,
               Is_Part_Of_Instance  => False,
               Special_Case         => Implicit_From_Standard,
               Normalization_Case   => Is_Not_Normalized,
               Parenth_Count        => 0,
               Enclosing_Tree       => Nil_Tree,
               Rel_Sloc             => -1,                -- should be set
               Character_Code       => 0,
               Obtained             => Nil_ASIS_OS_Time);

   function Set_Root_Type_Declaration
     (Int_Kind : Internal_Element_Kinds;
      Cont     : Context_Id)
      return Element
   is
      Result : Element := Root_Type_Declaration_Template;
   begin
      if Int_Kind in Internal_Root_Type_Kinds then
         Result.Enclosing_Context := Cont;
         Result.Rel_Sloc          := Internal_Element_Kinds'Pos (Int_Kind);
         --  we use Rel_Sloc field to keep the ("encoded") kind of
         --  the type definition. Bad style, I see... Let me know if
         --  you have a better idea for these crazy Root_Type_Kinds!...
         Result.Obtained          := A_OS_Time;
         return Result;
      else
         return Nil_Element;
      end if;
   end Set_Root_Type_Declaration;

begin
   Char_Literal_Spec_Template :=
      Element'(Node                 => Empty,            --  should be set
               R_Node               => Empty,            --  should be set
               Node_Field_1         => Empty,
               Node_Field_2         => Empty,
               Enclosing_Unit       => Standard_Id,
               Enclosing_Context    => Nil_Context_Id,   --  should be set
               Internal_Kind        => An_Enumeration_Literal_Specification,
               Is_Part_Of_Implicit  => False,
               Is_Part_Of_Inherited => False,
               Is_Part_Of_Instance  => False,
               Special_Case         => Stand_Char_Literal,
               Normalization_Case   => Is_Not_Normalized,
               Parenth_Count        => 0,
               Enclosing_Tree       => No_Tree_Name,
               Rel_Sloc             => -2,
               Character_Code       => 0,                 --  should be set
               Obtained             => Nil_ASIS_OS_Time); --  should be set

   Numeric_Error_Template :=
      Element'(Node                 => Standard_Package_Node,
               R_Node               => Standard_Package_Node,
               Node_Field_1         => Empty,
               Node_Field_2         => Empty,
               Enclosing_Unit       => Standard_Id,
               Enclosing_Context    => Nil_Context_Id,
               Internal_Kind        => An_Exception_Renaming_Declaration,
               Is_Part_Of_Implicit  => False,
               Is_Part_Of_Inherited => False,
               Is_Part_Of_Instance  => False,
               Special_Case         => Numeric_Error_Renaming,
               Normalization_Case   => Is_Not_Normalized,
               Parenth_Count        => 0,
               Enclosing_Tree       => No_Tree_Name,
               Rel_Sloc             => -2,
               Character_Code       => 0,
               Obtained             => Nil_ASIS_OS_Time);

end Asis.Set_Get;
