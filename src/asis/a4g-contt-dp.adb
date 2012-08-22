------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . C O N T T . D P                          --
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

pragma Ada_2005;

with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;

with Asis.Set_Get; use Asis.Set_Get;

with A4G.Contt.UT; use A4G.Contt.UT;
with A4G.Get_Unit; use A4G.Get_Unit;

with Atree;        use Atree;
with Nlists;       use Nlists;
with Namet;        use Namet;
with Sinfo;        use Sinfo;
with Lib;          use Lib;

package body A4G.Contt.Dp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_First_Stub (Body_Node : Node_Id) return Node_Id;
   function Get_Next_Stub  (Stub_Node : Node_Id) return Node_Id;
   --  these two functions implement the iterator through the body stubs
   --  contained in the given compilation unit. The iterator should
   --  be started from calling Get_First_Stub for the node pointed to
   --  the body (that is, for the node of ..._Body kind). The Empty node
   --  is returned if there is no first/next body stub node

   procedure Set_All_Unit_Dependencies (U : Unit_Id);
   --  Computes the full lists of supporters and dependents of U in the current
   --  Context from the list of direct supporters of U and sets these lists as
   --  values of Supporters and Dependents lists in the Unit Table

   procedure Add_Unit_Supporters (U : Unit_Id; L : in out Elist_Id);
   --  Add all the supporters of U, excluding U itself to L. This procedure
   --  traverses all the transitive semantic dependencies.

   procedure Fix_Direct_Supporters (Unit : Unit_Id);
   --  This procedure adds missed direct dependencies to the unit. It is
   --  supposed that before the call the list of direct supporters contains
   --  only units extracted from the unit context clause. So, if U is a body,
   --  this procedure adds the spec to the list of direct supporters, if it is
   --  a subunit - the parent body is added, if it is a child unit - the
   --  parent spec is added etc. The procedure adds these supporters in a
   --  transitive manner - that is, in case of a subunit, it adds the parent
   --  body, its spec (if any), its parent (if any) etc.
   --  This function supposes that Current Context is correctly set before
   --  the call.

   function In_List
     (U     : Unit_Id;
      L     : Unit_Id_List;
      Up_To : Natural)
      return  Boolean;
   --  Checks if U is a member of the first Up_To components of L. (If
   --  Up_To is 0, False is returned

   procedure CU_To_Unit_Id_List
     (CU_List             :        Compilation_Unit_List;
      Result_Unit_Id_List : in out Unit_Id_List;
      Result_List_Len     : out    Natural);
   --  Converts the ASIS Compilation Unit list into the list of Unit Ids and
   --  places this list into Result_Unit_Id_List. (Probably, we should replace
   --  this routine with a function...)
   --  For each ASIS Compilation Unit from CU_List the Result_Unit_Id_List
   --  contains exactly one Id for the corresponding unit. Result_List_Len is
   --  set to represent the index of the last Unit Id in Result_List_Len (0
   --  in case if Result_List_Len is empty). This routine expects that
   --  Result_Unit_Id_List'Length >= CU_List'Length

   --------------------------------------
   -- Dynamic Unit_Id list abstraction --
   --------------------------------------
   --  All the subprograms implementing Unit_Id list abstraction do not
   --  reset Context

   --  Is this package body the right place for defining this abstraction?
   --  May be, we should move it into A4G.A_Types???

   type Unit_Id_List_Access is access Unit_Id_List;
   Tmp_Unit_Id_List_Access : Unit_Id_List_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unit_Id_List, Unit_Id_List_Access);

   function In_Unit_Id_List
     (U : Unit_Id;
      L : Unit_Id_List_Access)
       return Boolean;
   --  Checks if U is a member of L.

   procedure Append_Unit_To_List
     (U : Unit_Id;
      L : in out Unit_Id_List_Access);
   --  (Unconditionally) appends U to L.

   procedure Add_To_Unit_Id_List
     (U : Unit_Id;
      L : in out Unit_Id_List_Access);
   --  If not In_Unit_Id_List (U, L), U is appended to L (if L is null,
   --  new Unit_Id_List value is created)

   procedure Reorder_Sem_Dependencies (Units : Unit_Id_List_Access);
   --  This procedure takes the unit list with is supposed to be the result of
   --  one of the Set_All_<Relation> functions above (that is, its parameter
   --  is not supposed to be null and it contains only existing units). It
   --  reorders it in the way required by
   --  Asis.Compilation_Units.Relations.Semantic_Dependence_Order - that is,
   --  with no forward semantic dependencies.

   -------------------
   -- Add_To_Parent --
   -------------------

   procedure Add_To_Parent (C : Context_Id; U : Unit_Id) is
      Parent_Id : Unit_Id;
      Unit_Kind : constant Unit_Kinds := Kind (C, U);
   begin

      if U = Standard_Id then
         return;
      end if;

      Reset_Context (C); -- ???

      Get_Name_String (U, Norm_Ada_Name);

      if Not_Root then
         Form_Parent_Name;

         if Unit_Kind in A_Subunit then
            A_Name_Buffer (A_Name_Len) := 'b';
         end if;

         Parent_Id := Name_Find (C);
         --  Parent_Id cannot be Nil_Unit here

            Append_Elmt
              (Unit => U,
               To   => Unit_Table.Table (Parent_Id).Subunits_Or_Childs);
      else
         Append_Elmt
           (Unit => U,
            To   => Unit_Table.Table (Standard_Id).Subunits_Or_Childs);
      end if;

   end Add_To_Parent;

   -------------------------
   -- Add_Unit_Supporters --
   -------------------------

   procedure Add_Unit_Supporters (U : Unit_Id; L : in out Elist_Id) is
      Supporters : Elist_Id        renames Unit_Table.Table (U).Supporters;
      Direct_Supporters : Elist_Id renames
         Unit_Table.Table (U).Direct_Supporters;

      Next_Support_Elmt : Elmt_Id;
      Next_Support_Unit : Unit_Id;

   begin

      if Is_Empty_Elmt_List (Direct_Supporters) then
         --  end of the recursion
         return;

      elsif not Is_Empty_Elmt_List (Supporters) then
         --  no need to traverse indirect dependencies

         Next_Support_Elmt := First_Elmt (Supporters);

         while Present (Next_Support_Elmt) loop
            Next_Support_Unit := Unit (Next_Support_Elmt);

            Add_To_Elmt_List
              (Unit => Next_Support_Unit,
               List => L);

            Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);

         end loop;

      else
         --  And here we have to traverse the recursive dependencies:

         Next_Support_Elmt := First_Elmt (Direct_Supporters);

         while Present (Next_Support_Elmt) loop
            Next_Support_Unit := Unit (Next_Support_Elmt);

            --  The old code currently commented out caused a huge delay
            --  when opening one tree context (8326-002). We will keep it
            --  till the new code is tested for queries from
            --  Asis.Compilation_Units.Relations

            --  ???Old code start

            --  Here we can not be sure, that if Next_Support_Unit already
            --  is in the list, all its supporters also are in the list
            --  Add_To_Elmt_List
            --    (Unit => Next_Support_Unit,
            --     List => L);

            --  Add_Unit_Supporters (Next_Support_Unit, L);

            --  ???Old code end

            --  ???New code start
            if not In_Elmt_List (Next_Support_Unit, L) then
               Append_Elmt
                 (Unit => Next_Support_Unit,
                  To   => L);

               Add_Unit_Supporters (Next_Support_Unit, L);
            end if;

            --  ???New code end

            Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);

         end loop;

      end if;

   end Add_Unit_Supporters;

   -------------------------
   -- Append_Subunit_Name --
   -------------------------

   procedure Append_Subunit_Name (Def_S_Name : Node_Id) is
   begin
      --  Here we need unqualified name, because the name
      --  which comes from the stub is qualified by parent body
      --  name

      Get_Unqualified_Decoded_Name_String (Chars (Def_S_Name));

      A_Name_Buffer (A_Name_Len - 1) := '.';
      A_Name_Buffer (A_Name_Len .. A_Name_Len + Name_Len - 1) :=
         Name_Buffer (1 .. Name_Len);
      A_Name_Len := A_Name_Len + Name_Len + 1;
      A_Name_Buffer (A_Name_Len - 1) := '%';
      A_Name_Buffer (A_Name_Len)     := 'b';
   end Append_Subunit_Name;

   ------------------------
   -- CU_To_Unit_Id_List --
   ------------------------

   procedure CU_To_Unit_Id_List
     (CU_List             :        Compilation_Unit_List;
      Result_Unit_Id_List : in out Unit_Id_List;
      Result_List_Len     : out    Natural)
   is
      Next_Unit       : Unit_Id;
   begin
      Result_List_Len     := 0;

      for I in CU_List'Range loop
         Next_Unit := Get_Unit_Id (CU_List (I));

         if not In_List (Next_Unit, Result_Unit_Id_List, Result_List_Len) then
            Result_List_Len := Result_List_Len + 1;
            Result_Unit_Id_List (Result_List_Len) := Next_Unit;
         end if;

      end loop;

   end CU_To_Unit_Id_List;

   ---------------------------
   -- Fix_Direct_Supporters --
   ---------------------------

   procedure Fix_Direct_Supporters (Unit : Unit_Id) is

      function Next_Supporter (U : Unit_Id) return Unit_Id;
      --  Computes the next supporter to be added (from subunit to the parent
      --  body, from body to the spec, from child to the parent etc). Ends up
      --  with Standard and then with Nil_Unit as its parent

      Next_Supporter_Id : Unit_Id;

      function Next_Supporter (U : Unit_Id) return Unit_Id is
         C             : constant Context_Id := Current_Context;
         Arg_Unit_Kind : constant Unit_Kinds := Kind (C, U);
         Result_Id     : Unit_Id             := Nil_Unit;
      begin

         case Arg_Unit_Kind is

            when A_Procedure                  |
                 A_Function                   |
                 A_Package                    |
                 A_Generic_Procedure          |
                 A_Generic_Function           |
                 A_Generic_Package            |
                 A_Procedure_Instance         |
                 A_Function_Instance          |
                 A_Package_Instance           |
                 A_Procedure_Renaming         |
                 A_Function_Renaming          |
                 A_Package_Renaming           |
                 A_Generic_Procedure_Renaming |
                 A_Generic_Function_Renaming  |
                 A_Generic_Package_Renaming   =>

               Result_Id := Get_Parent_Unit (C, U);

            when  A_Procedure_Body |
                  A_Function_Body  =>

               if Class (C, U) = A_Public_Declaration_And_Body then
                  Result_Id := Get_Parent_Unit (C, U);
               else
                  Result_Id := Get_Declaration (C, U);
               end if;

            when  A_Package_Body =>
               Result_Id := Get_Declaration (C, U);

            when A_Procedure_Body_Subunit |
                 A_Function_Body_Subunit  |
                 A_Package_Body_Subunit   |
                 A_Task_Body_Subunit      |
                 A_Protected_Body_Subunit =>
               Result_Id := Get_Subunit_Parent_Body (C, U);

            when others =>
               pragma Assert (False);
               null;
         end case;

         return Result_Id;
      end Next_Supporter;

   begin
      Next_Supporter_Id := Next_Supporter (Unit);

      while Present (Next_Supporter_Id) loop

         Append_Elmt (Unit => Next_Supporter_Id,
                      To   => Unit_Table.Table (Unit).Direct_Supporters);

         Next_Supporter_Id := Next_Supporter (Next_Supporter_Id);
      end loop;

   end Fix_Direct_Supporters;

   --------------------
   -- Get_First_Stub --
   --------------------

   function Get_First_Stub (Body_Node : Node_Id) return Node_Id is
      Decls : List_Id;
      Decl  : Node_Id;
   begin
      Decls := Declarations (Body_Node);

      if No (Decls) then
         return Empty;
      else
         Decl := Nlists.First (Decls);

         while Present (Decl) loop

            if Nkind (Decl) in N_Body_Stub then
               return Decl;
            end if;

            Decl := Next (Decl);
         end loop;
         return Empty;
      end if;

   end Get_First_Stub;

   -------------------
   -- Get_Next_Stub --
   -------------------

   function Get_Next_Stub  (Stub_Node : Node_Id) return Node_Id is
      Next_Decl : Node_Id;
   begin
      Next_Decl := Next (Stub_Node);

      while Present (Next_Decl) loop

         if Nkind (Next_Decl) in N_Body_Stub then
            return Next_Decl;
         end if;

         Next_Decl := Next (Next_Decl);
      end loop;
      return Empty;
   end Get_Next_Stub;

   -------------
   -- In_List --
   -------------

   function In_List
     (U     : Unit_Id;
      L     : Unit_Id_List;
      Up_To : Natural)
      return  Boolean
   is
      Len    : constant Natural := Natural'Min (Up_To, L'Length);
      Result : Boolean          := False;
   begin
      for I in 1 .. Len loop
         if L (I) = U then
            Result := True;
            exit;
         end if;
      end loop;

      return Result;

   end In_List;

   ------------------
   -- Process_Stub --
   ------------------

   procedure Process_Stub (C : Context_Id; U : Unit_Id; Stub : Node_Id) is
      Def_S_Name     : Node_Id;
      Subunit_Id     : Unit_Id;
   begin
      --  We should save (and then restore) the content of A_Name_Buffer in
      --  case when more than one stub is to be processed. (A_Name_Buffer
      --  contains the Ada name of the parent body)

      NB_Save;

      if Nkind (Stub) = N_Subprogram_Body_Stub then
         Def_S_Name := Defining_Unit_Name (Specification (Stub));
      else
         Def_S_Name := Defining_Identifier (Stub);
      end if;

      Append_Subunit_Name (Def_S_Name);

      Subunit_Id := Name_Find (C);

      if No (Subunit_Id) then
         Subunit_Id := Allocate_Nonexistent_Unit_Entry (C);
         Append_Elmt (Unit => Subunit_Id,
                      To   => Unit_Table.Table (U).Subunits_Or_Childs);
      end if;

      NB_Restore;

   end Process_Stub;

   ------------------------------
   -- Reorder_Sem_Dependencies --
   ------------------------------

   procedure Reorder_Sem_Dependencies (Units : Unit_Id_List_Access) is
      More_Inversion : Boolean := True;
      Tmp_Unit       : Unit_Id;
   begin

      if Units'Length = 0 then
         return;
      end if;

      --  The idea is simple: for all the units in Units list we have the
      --  lists of all the unit's supporters already computed. If we order
      --  units so that the lengths of supporter lists will increase we will
      --  get the order in which there will be no forward semantic
      --  dependencies: if unit A depends on unit B, then A also depends on
      --  all the supporters of B, so it has the list of supporters longer
      --  then B has

      while More_Inversion loop

         More_Inversion := False;

         for J in Units'First .. Units'Last - 1 loop

            if List_Length (Unit_Table.Table (Units (J)).Supporters) >
               List_Length (Unit_Table.Table (Units (J + 1)).Supporters)
            then
               Tmp_Unit       := Units (J + 1);
               Units (J + 1)  := Units (J);
               Units (J)      := Tmp_Unit;
               More_Inversion := True;
            end if;

         end loop;

      end loop;

   end Reorder_Sem_Dependencies;

   --------------------------
   -- Set_All_Dependencies --
   --------------------------

   procedure Set_All_Dependencies (Use_First_New_Unit : Boolean := False) is
      Starting_Unit : Unit_Id;
   begin

      if Use_First_New_Unit then
         Starting_Unit := First_New_Unit;

         if No (Starting_Unit) then
            --  This may happen, when, for the incremental Context, we
            --  process the tree which is the main tree for some body unit,
            --  and this body unit has been already included in the Context
            --  (See Lib (spec, (h))
            return;
         end if;

      else
         Starting_Unit := Config_Comp_Id + 1;
         --  Config_Comp_Id corresponds to last predefined unit set in the
         --  unit table
      end if;

      for U in Starting_Unit .. Last_Unit loop
         Set_All_Unit_Dependencies (U);
      end loop;
   end Set_All_Dependencies;

   -------------------------------
   -- Set_All_Unit_Dependencies --
   -------------------------------

   procedure Set_All_Unit_Dependencies (U : Unit_Id) is
      Supporters        : Elist_Id renames Unit_Table.Table (U).Supporters;
      Direct_Supporters : Elist_Id renames
         Unit_Table.Table (U).Direct_Supporters;

      Next_Support_Elmt : Elmt_Id;
      Next_Support_Unit : Unit_Id;

   begin

      Fix_Direct_Supporters (U);

      --  Setting all the unit supporters
      Next_Support_Elmt := First_Elmt (Direct_Supporters);

      while Present (Next_Support_Elmt) loop
         Next_Support_Unit := Unit (Next_Support_Elmt);

         --  If Next_Support_Unit already is in Supporters list,
         --  all its supporters also are already included in Supporters.

         if not In_Elmt_List (Next_Support_Unit, Supporters) then
            Append_Elmt
              (Unit => Next_Support_Unit,
               To   => Supporters);

            Add_Unit_Supporters (Next_Support_Unit, Supporters);
         end if;

         Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);

      end loop;

      --  And now - adding U as depended unit to the list of Dependents for
      --  all its supporters

      Next_Support_Elmt := First_Elmt (Supporters);

      while Present (Next_Support_Elmt) loop
         Next_Support_Unit := Unit (Next_Support_Elmt);

         Append_Elmt
           (Unit => U,
            To   => Unit_Table.Table (Next_Support_Unit).Dependents);

         Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);
      end loop;

   end Set_All_Unit_Dependencies;

   ---------------------------
   -- Set_Direct_Dependents --
   ---------------------------

   procedure Set_Direct_Dependents (U : Unit_Id) is
      Next_Support_Elmt : Elmt_Id;
      Next_Support_Unit : Unit_Id;
   begin
      Next_Support_Elmt := First_Elmt (Unit_Table.Table (U).Direct_Supporters);

      while Present (Next_Support_Elmt) loop
         Next_Support_Unit := Unit (Next_Support_Elmt);

         Append_Elmt
           (Unit => U,
            To   => Unit_Table.Table (Next_Support_Unit).Direct_Dependents);

         Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);
      end loop;

   end Set_Direct_Dependents;

   -----------------------
   -- Set_All_Ancestors --
   -----------------------

   procedure Set_All_Ancestors
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access)
   is
      Cont     : constant Context_Id := Current_Context;

      Arg_List : Unit_Id_List (1 .. Compilation_Units'Length) :=
        (others => Nil_Unit);

      Arg_List_Len       : Natural             := 0;
      Result_List        : Unit_Id_List_Access := null;
      Next_Ancestor_Unit : Unit_Id;

   begin
      --  For the current version, we are supposing, that we have only one
      --  Context opened at a time

      CU_To_Unit_Id_List (Compilation_Units, Arg_List, Arg_List_Len);

      --  Standard is an ancestor of any unit, and if we are here,
      --  Compilation_Units can not be Nil_Compilation_Unit_List. So we set
      --  it as the first element of the result list:

      Append_Unit_To_List (Standard_Id, Result_List);

      for I in 1 .. Arg_List_Len loop

         Next_Ancestor_Unit := Arg_List (I);

         if Next_Ancestor_Unit /= Standard_Id then

            while Kind (Cont, Next_Ancestor_Unit) in A_Subunit loop
               Next_Ancestor_Unit :=
                  Get_Subunit_Parent_Body (Cont, Next_Ancestor_Unit);
            end loop;

            if Class (Cont, Next_Ancestor_Unit) = A_Public_Body or else
               Class (Cont, Next_Ancestor_Unit) = A_Private_Body
            then
               Next_Ancestor_Unit :=
                  Get_Declaration (Cont, Next_Ancestor_Unit);
            end if;

            while Next_Ancestor_Unit /= Standard_Id loop

               if not In_Unit_Id_List (Next_Ancestor_Unit, Result_List) then

                  Append_Unit_To_List (Next_Ancestor_Unit, Result_List);
                  Next_Ancestor_Unit :=
                     Get_Parent_Unit (Cont, Next_Ancestor_Unit);
               else
                  exit;
               end if;

            end loop;

         end if;

      end loop;

      --  And here we have to order Result_List to eliminate forward
      --  semantic dependencies

      --  Result_List can not be null - it contains at least Standard_Id

      Reorder_Sem_Dependencies (Result_List);

      Result := new Compilation_Unit_List'
                     (Get_Comp_Unit_List (Result_List.all, Cont));
      Free (Result_List);

   end Set_All_Ancestors;

   ------------------------
   -- Set_All_Dependents --
   ------------------------

   procedure Set_All_Dependents
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Dependent_Units   :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access)
   is
      Cont            : constant Context_Id := Current_Context;

      Arg_List : Unit_Id_List (1 .. Compilation_Units'Length) :=
        (others => Nil_Unit);

      Arg_List_Len : Natural := 0;

      Dep_List : Unit_Id_List (1 .. Dependent_Units'Length) :=
        (others => Nil_Unit);

      Dep_List_Len        : Natural := 0;
      Result_List         : Unit_Id_List_Access := null;
      Next_Dependent_Elmt : Elmt_Id;
      Next_Dependent_Unit : Unit_Id;

   begin
      --  For the current version, we are supposing, that we have only one
      --  Context opened at a time

      CU_To_Unit_Id_List (Compilation_Units, Arg_List, Arg_List_Len);
      CU_To_Unit_Id_List (Dependent_Units,   Dep_List, Dep_List_Len);

      --  Now, collecting all the dependents for Compilation_Units

      for I in 1 .. Arg_List_Len loop

         Next_Dependent_Elmt :=
            First_Elmt (Unit_Table.Table (Arg_List (I)).Dependents);

         while Present (Next_Dependent_Elmt) loop
            Next_Dependent_Unit := Unit (Next_Dependent_Elmt);

            if Dep_List_Len = 0 or else
               In_List (Next_Dependent_Unit, Dep_List, Dep_List_Len)
            then
               Add_To_Unit_Id_List (Next_Dependent_Unit, Result_List);
            end if;

            Next_Dependent_Elmt := Next_Elmt (Next_Dependent_Elmt);

         end loop;

      end loop;

      --  And here we have to order Result_List to eliminate forward
      --  semantic dependencies

      if Result_List /= null then
         Reorder_Sem_Dependencies (Result_List);

         Result := new Compilation_Unit_List'
                        (Get_Comp_Unit_List (Result_List.all, Cont));
         Free (Result_List);
      else
         Result := new Compilation_Unit_List (1 .. 0);
      end if;

   end Set_All_Dependents;

   -------------------------
   -- Set_All_Descendants --
   -------------------------

   procedure Set_All_Descendants
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access)
   is
      Cont : constant Context_Id := Current_Context;

      Arg_List : Unit_Id_List (1 .. Compilation_Units'Length) :=
        (others => Nil_Unit);

      Arg_List_Len         : Natural             := 0;
      Result_List          : Unit_Id_List_Access := null;
      Next_Descendant_Elmt : Elmt_Id;
      Next_Unit            : Unit_Id;

      procedure Add_All_Descendants
        (Desc_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access);
      --  If Desc_Unit is not in Result_List, this procedure adds it and
      --  (recursively) all its descendants which are not in Result_List to
      --  the list.

      procedure Add_All_Descendants
        (Desc_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access)
      is
         Child_Elmt : Elmt_Id;
         Child_Unit : Unit_Id;
      begin

         if not In_Unit_Id_List (Desc_Unit, Result_List) then
            Append_Unit_To_List (Desc_Unit, Result_List);

            if Kind (Cont, Desc_Unit) = A_Package          or else
               Kind (Cont, Desc_Unit) = A_Generic_Package  or else
               Kind (Cont, Desc_Unit) = A_Package_Renaming or else
               Kind (Cont, Desc_Unit) = A_Generic_Package_Renaming
            then
               Child_Elmt :=
                  First_Elmt (Unit_Table.Table (Desc_Unit).Subunits_Or_Childs);

               while Present (Child_Elmt) loop
                  Child_Unit := Unit (Child_Elmt);

                  Add_All_Descendants (Child_Unit, Result_List);

                  Child_Elmt := Next_Elmt (Child_Elmt);
               end loop;

            end if;

         end if;

      end Add_All_Descendants;

   begin

      --  We can not use CU_To_Unit_Id_List routine, because we have to
      --  filter out subunits, nonexistent units (?) and bodies for which the
      --  Context does not contain a spec - such units can not have
      --  descendants. For bodies, only the corresponding specs contain the
      --  lists of descendants.

      for I in Compilation_Units'Range loop
         Next_Unit := Get_Unit_Id (Compilation_Units (I));

         if Kind (Cont, Next_Unit) not in A_Procedure_Body_Subunit ..
                                    A_Nonexistent_Body
         then

            if Kind (Cont, Next_Unit) in A_Library_Unit_Body then
               Next_Unit := Get_Declaration (Cont, Next_Unit);
            end if;

            if Present (Next_Unit) and then
               (not In_List (Next_Unit, Arg_List, Arg_List_Len))
            then
               Arg_List_Len := Arg_List_Len + 1;
               Arg_List (Arg_List_Len) := Next_Unit;
            end if;

         end if;

      end loop;

      for J in 1 .. Arg_List_Len loop
         Next_Descendant_Elmt :=
            First_Elmt (Unit_Table.Table (Arg_List (J)).Subunits_Or_Childs);

         while Present (Next_Descendant_Elmt) loop
            Next_Unit := Unit (Next_Descendant_Elmt);
            Add_All_Descendants (Next_Unit, Result_List);
            Next_Descendant_Elmt := Next_Elmt (Next_Descendant_Elmt);
         end loop;

      end loop;

      if Result_List /= null then
         Reorder_Sem_Dependencies (Result_List);

         Result := new Compilation_Unit_List'
                        (Get_Comp_Unit_List (Result_List.all, Cont));
         Free (Result_List);
      else
         Result := new Compilation_Unit_List (1 .. 0);
      end if;

   end Set_All_Descendants;

   ----------------------
   -- Set_All_Families --
   ----------------------

   procedure Set_All_Families
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access)
   is
      Cont : constant Context_Id := Current_Context;

      Arg_List : Unit_Id_List (1 .. Compilation_Units'Length) :=
        (others => Nil_Unit);

      Arg_List_Len : Natural := 0;
      Result_List  : Unit_Id_List_Access := null;

      procedure Collect_Spec_Family
        (Spec_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access);
      --  If Spec_Unit is not in Result_List, this procedure adds it and
      --  (recursively) all members of its family which are not in Result_List
      --  to the list. In case of a spec, the corresponding body's family is
      --  also added

      procedure Collect_Body_Family
        (Body_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access);
      --  If Body_Unit is not in Result_List, this procedure adds it and
      --  (recursively) all members of its family which are not in Result_List
      --  to the list. In case of a body, only the subunit tree rooted by this
      --  body may be added

      procedure Collect_Spec_Family
        (Spec_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access)
      is
         Child_Elmt : Elmt_Id;
         Child_Unit : Unit_Id;
      begin

         if not In_Unit_Id_List (Spec_Unit, Result_List) then
            Append_Unit_To_List (Spec_Unit, Result_List);

            --  We have to add all descendants (if any) and their families

            if Kind (Cont, Spec_Unit) = A_Package          or else
               Kind (Cont, Spec_Unit) = A_Generic_Package  or else
               Kind (Cont, Spec_Unit) = A_Package_Renaming or else
               Kind (Cont, Spec_Unit) = A_Generic_Package_Renaming
            then
               Child_Elmt :=
                  First_Elmt (Unit_Table.Table (Spec_Unit).Subunits_Or_Childs);

               while Present (Child_Elmt) loop
                  Child_Unit := Unit (Child_Elmt);

                  if Kind (Cont, Child_Unit) in
                     A_Procedure .. A_Generic_Package_Renaming
                  then

                     Collect_Spec_Family (Child_Unit, Result_List);

                  elsif Kind (Cont, Child_Unit) in
                     A_Procedure_Body .. A_Protected_Body_Subunit
                  then

                     Collect_Body_Family (Child_Unit, Result_List);

                  end if;

                  Child_Elmt := Next_Elmt (Child_Elmt);
               end loop;

            end if;

         end if;

      end Collect_Spec_Family;

      procedure Collect_Body_Family
        (Body_Unit   : Unit_Id;
         Result_List : in out Unit_Id_List_Access)
      is
         Child_Elmt : Elmt_Id;
         Child_Unit : Unit_Id;
      begin

         if not In_Unit_Id_List (Body_Unit, Result_List) then
            Append_Unit_To_List (Body_Unit, Result_List);

            --  We have to add all descendants (if any) and their families

            if Kind (Cont, Body_Unit) in
               A_Procedure_Body .. A_Protected_Body_Subunit
            then
               Child_Elmt :=
                  First_Elmt (Unit_Table.Table (Body_Unit).Subunits_Or_Childs);

               while Present (Child_Elmt) loop
                  Child_Unit := Unit (Child_Elmt);
                  Collect_Body_Family (Child_Unit, Result_List);
                  Child_Elmt := Next_Elmt (Child_Elmt);
               end loop;

            end if;

         end if;

      end Collect_Body_Family;

   begin
      CU_To_Unit_Id_List (Compilation_Units, Arg_List, Arg_List_Len);

      for J in 1 .. Arg_List_Len loop

         case Class (Cont, Arg_List (J)) is

            when A_Public_Declaration |
                 A_Private_Declaration =>

               Collect_Spec_Family (Arg_List (J), Result_List);

            when Not_A_Class =>
               --  This should never happen, so just in case we
               --  raise an exception
               null;
               pragma Assert (False);

            when others =>
               --  Here we can have only a body or a separate body
               Collect_Body_Family (Arg_List (J), Result_List);
         end case;

      end loop;

      --  And here we have to order Result_List to eliminate forward
      --  semantic dependencies

      if Result_List /= null then
         Reorder_Sem_Dependencies (Result_List);

         Result := new Compilation_Unit_List'
                        (Get_Comp_Unit_List (Result_List.all, Cont));
         Free (Result_List);
      else
         Result := new Compilation_Unit_List (1 .. 0);
      end if;

   end Set_All_Families;

   ------------------------
   -- Set_All_Supporters --
   ------------------------

   package Unit_Container is new Ada.Containers.Ordered_Sets
     (Element_Type => Unit_Id);

   procedure Unit_List_To_Set
     (Unit_List :        Elist_Id;
      Unit_Set  : in out Unit_Container.Set);
   --  Assuming that Unit_List does not contain repeating elements, creates
   --  Unit_Set as the set containing Unit IDs from Unit_List. If Unit_Set is
   --  non-empty before the call, the old content of the set is lost.

   function Unit_Set_To_List
     (Unit_Set : Unit_Container.Set)
      return    Unit_Id_List;
   --  Converts the unit id set into array

   Result_Set            : Unit_Container.Set;
   New_Set               : Unit_Container.Set;
   Newer_Set             : Unit_Container.Set;
   Next_Direct_Supporter : Unit_Container.Cursor;

   procedure Unit_List_To_Set
     (Unit_List :        Elist_Id;
      Unit_Set  : in out Unit_Container.Set)
   is
      Next_El   : Elmt_Id;
   begin
      Unit_Container.Clear (Unit_Set);

      Next_El := First_Elmt (Unit_List);

      while Present (Next_El) loop
         Unit_Container.Insert (Unit_Set, Unit (Next_El));
         Next_El := Next_Elmt (Next_El);
      end loop;
   end Unit_List_To_Set;

   function Unit_Set_To_List
     (Unit_Set : Unit_Container.Set)
      return    Unit_Id_List
   is
      Next_Unit : Unit_Container.Cursor;
      Result : Unit_Id_List (1 .. Natural (Unit_Container.Length (Unit_Set)));
      Next_Idx : Natural := Result'First;
   begin
      Next_Unit := Unit_Container.First (Unit_Set);

      while Unit_Container.Has_Element (Next_Unit) loop
         Result (Next_Idx) := Unit_Container.Element (Next_Unit);
         Next_Idx          := Next_Idx + 1;
         Next_Unit         := Unit_Container.Next (Next_Unit);
      end loop;

      return Result;
   end Unit_Set_To_List;

   procedure Set_All_Supporters
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access)

   is
      Cont            : constant Context_Id := Current_Context;

      Arg_List : Unit_Id_List (1 .. Compilation_Units'Length) :=
        (others => Nil_Unit);

      Result_List  : Unit_Id_List_Access := null;
      Arg_List_Len : Natural := 0;
      pragma Unreferenced (Arg_List_Len);

      procedure Collect_Supporters (U : Unit_Id);
      --  If U is not presented in Result, adds (recursively) all its
      --  supporters to Result_List
      --  Uses workpile algorithm to avoid cycling (cycling is possible because
      --  of limited with)

      procedure Collect_Supporters (U : Unit_Id) is
         Next_Supporter : Elmt_Id;
      begin

         Unit_Container.Clear (New_Set);
         Unit_Container.Clear (Newer_Set);

         Unit_List_To_Set
           (Unit_List => Unit_Table.Table (U).Supporters,
            Unit_Set  => New_Set);

         Unit_Container.Union
           (Target => Result_Set,
            Source => New_Set);

         while not Unit_Container.Is_Empty (New_Set) loop
            Next_Direct_Supporter := Unit_Container.First (New_Set);

            Next_Supporter :=
              First_Elmt (Unit_Table.Table
                (Unit_Container.Element (Next_Direct_Supporter)).Supporters);

            while Present (Next_Supporter) loop
               if not Unit_Container.Contains
                        (Result_Set, Unit (Next_Supporter))
               then
                  Unit_Container.Insert (Newer_Set, Unit (Next_Supporter));
               end if;

               Next_Supporter := Next_Elmt (Next_Supporter);
            end loop;

            Unit_Container.Delete_First (New_Set);

            if not Unit_Container.Is_Empty (Newer_Set) then
               Unit_Container.Union (Result_Set, Newer_Set);
               Unit_Container.Union (New_Set, Newer_Set);
               Unit_Container.Clear (Newer_Set);
            end if;
         end loop;

      end Collect_Supporters;

   begin
      Unit_Container.Clear (Result_Set);
      Unit_Container.Insert (Result_Set, Standard_Id);

      --  For the current version, we are supposing, that we have only one
      --  Context opened at a time

      CU_To_Unit_Id_List (Compilation_Units, Arg_List, Arg_List_Len);

      --  Now, collecting all the supporters for Compilation_Units

      --  Standard is a supporter of any unit, and if we are here,
      --  Compilation_Units can not be Nil_Compilation_Unit_List. So we set
      --  it as the first element of the result list:

      for J in Compilation_Units'Range loop
         Collect_Supporters (Get_Unit_Id (Compilation_Units (J)));
      end loop;

      Result_List := new Unit_Id_List'(Unit_Set_To_List (Result_Set));

      --  And here we have to order Result_List to eliminate forward
      --  semantic dependencies

      --  Result_List can not be null - it contains at least Standard_Id

      Reorder_Sem_Dependencies (Result_List);

      Result := new Compilation_Unit_List'
                     (Get_Comp_Unit_List (Result_List.all, Cont));
      Free (Result_List);

   end Set_All_Supporters;

   --------------------------
   -- Set_All_Needed_Units --
   --------------------------

   procedure Set_All_Needed_Units
     (Compilation_Units :        Asis.Compilation_Unit_List;
      Result            : in out Compilation_Unit_List_Access;
      Missed            : in out Compilation_Unit_List_Access)
   is
      Cont            : constant Context_Id := Current_Context;
      Cont_Tree_Mode  : constant Tree_Mode  := Tree_Processing_Mode (Cont);

      Arg_List        : Unit_Id_List (1 .. Compilation_Units'Length) :=
                        (others => Nil_Unit);
      Arg_List_Len    : Natural := 0;

      Result_List     : Unit_Id_List_Access := null;
      Missed_List     : Unit_Id_List_Access := null;

      procedure Set_One_Unit (U : Unit_Id);
      --  Provided that U is an (existing) unit which is not in the
      --  Result_List, this procedure adds this unit and all the units
      --  needed by it to result lists.

      procedure Add_Needed_By_Spec (Spec_Unit : Unit_Id);
      --  Provided that Spec_Unit denotes an (existing) spec, this procedure
      --  adds to the result lists units which are needed by this unit only,
      --  that is, excluding this unit (it is supposed to be already added at
      --  the moment of the call), its body and units needed by the body (if
      --  any, they are processed separately)

      procedure Add_Needed_By_Body (Body_Unit : Unit_Id);
      --  Provided that Body_Unit denotes an (existing) body, this procedure
      --  adds to the result lists units which are needed by this unit,
      --  excluding the unit itself (it is supposed to be already added at
      --  the moment of the call). That is, the spec of this unit and units
      --  which are needed by the spec (if any) are also needed, if they have
      --  not been added before

      ------------------------
      -- Add_Needed_By_Body --
      ------------------------

      procedure Add_Needed_By_Body (Body_Unit : Unit_Id) is
         Spec_Unit : Unit_Id;

         Subunit_List : constant Unit_Id_List := Subunits (Cont, Body_Unit);

         Next_Support_Elmt : Elmt_Id;
         Next_Support_Unit : Unit_Id;

      begin

         --  First, check if there is a separate spec then it has to be
         --  processed

         if Class (Cont, Body_Unit) /= A_Public_Declaration_And_Body then

            Spec_Unit := Body_Unit;

            while Class (Cont, Spec_Unit) = A_Separate_Body loop
               Spec_Unit := Get_Subunit_Parent_Body (Cont, Spec_Unit);
            end loop;

            Spec_Unit := Get_Declaration (Cont, Spec_Unit);
            --  We can not get Nil or nonexistent unit here

            if not In_Unit_Id_List (Spec_Unit, Result_List) then
               Add_Needed_By_Spec (Spec_Unit);
            end if;

         end if;

         --  Now process body's supporters:

         Next_Support_Elmt :=
            First_Elmt (Unit_Table.Table (Body_Unit).Supporters);

         while Present (Next_Support_Elmt) loop

            Next_Support_Unit := Unit (Next_Support_Elmt);

            if not In_Unit_Id_List (Next_Support_Unit, Result_List) then
               Set_One_Unit (Next_Support_Unit);
            end if;

            Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);

         end loop;

         --  And, finally, subunits:

         for J in Subunit_List'Range loop

            if Kind (Cont, Subunit_List (J)) = A_Nonexistent_Body then
               Append_Unit_To_List (Subunit_List (J), Missed_List);

            elsif not In_Unit_Id_List (Subunit_List (J), Result_List) then
               Append_Unit_To_List (Subunit_List (J), Result_List);
               Add_Needed_By_Body  (Subunit_List (J));
            end if;

         end loop;

      end Add_Needed_By_Body;

      ------------------------
      -- Add_Needed_By_Spec --
      ------------------------

      procedure Add_Needed_By_Spec (Spec_Unit : Unit_Id) is
         Next_Support_Elmt : Elmt_Id;
         Next_Support_Unit : Unit_Id;
      begin

         Next_Support_Elmt :=
            First_Elmt (Unit_Table.Table (Spec_Unit).Supporters);

         while Present (Next_Support_Elmt) loop

            Next_Support_Unit := Unit (Next_Support_Elmt);

            if not In_Unit_Id_List (Next_Support_Unit, Result_List) then
               Set_One_Unit (Next_Support_Unit);
            end if;

            Next_Support_Elmt := Next_Elmt (Next_Support_Elmt);

         end loop;

      end Add_Needed_By_Spec;

      ------------------
      -- Set_One_Unit --
      ------------------

      procedure Set_One_Unit (U : Unit_Id) is
         U_Body : Unit_Id;
      begin
         Append_Unit_To_List (U, Result_List);

         case Class (Cont, U) is

            when A_Public_Declaration |
                 A_Private_Declaration =>

               Add_Needed_By_Spec (U);

               if Is_Body_Required (Cont, U) then
                  U_Body := Get_Body (Cont, U);

                  if No (U_Body) and then
                     (Cont_Tree_Mode = On_The_Fly
                    or else
                      Cont_Tree_Mode = Mixed)
                  then
                     --  Is it a correct thing to compile something on the fly
                     --  Inside the query from Relations???
                     U_Body := Get_One_Unit
                       (Name    => To_Program_Text
                                     (Unit_Name (Get_Comp_Unit (U, Cont))),
                        Context => Cont,
                        Spec    => False);
                  end if;

                  if Present (U_Body) then

                     if Kind (Cont, U_Body) in A_Nonexistent_Declaration ..
                                               A_Nonexistent_Body
                     then
                        Add_To_Unit_Id_List (U_Body, Missed_List);

                     elsif not In_Unit_Id_List (U_Body, Result_List) then
                        Append_Unit_To_List (U_Body, Result_List);
                        Add_Needed_By_Body  (U_Body);
                     end if;

                  else
                     U_Body := Get_Nonexistent_Unit (Cont);
                     Append_Unit_To_List (U_Body, Missed_List);
                  end if;

               end if;

            when Not_A_Class =>
               --  This should never happen, so just in case we
               --  raise an exception
               null;
               pragma Assert (False);

            when others =>
               Add_Needed_By_Body (U);
         end case;

      end Set_One_Unit;

   begin --  Set_All_Needed_Units

      CU_To_Unit_Id_List (Compilation_Units, Arg_List, Arg_List_Len);

      --  Standard is a supporter of any unit, and if we are here,
      --  Compilation_Units can not be Nil_Compilation_Unit_List. So we set
      --  it as the first element of the result list:

      Append_Unit_To_List (Standard_Id, Result_List);

      for J in 1 .. Arg_List_Len loop

         if not In_Unit_Id_List (Arg_List (J), Result_List) then
            Set_One_Unit (Arg_List (J));
         end if;

      end loop;

      --  Result_List can not be null - it contains at least Standard_Id

      Reorder_Sem_Dependencies (Result_List);

      Result := new Compilation_Unit_List'
                     (Get_Comp_Unit_List (Result_List.all, Cont));
      Free (Result_List);

      if Missed_List /= null then
         Missed := new Compilation_Unit_List'
                        (Get_Comp_Unit_List (Missed_List.all, Cont));
         Free (Missed_List);
      else
         Missed := new Compilation_Unit_List (1 .. 0);
      end if;

   end Set_All_Needed_Units;

   ------------------
   -- Set_Subunits --
   ------------------

   procedure Set_Subunits (C : Context_Id; U : Unit_Id; Top : Node_Id) is
      Body_Node : Node_Id;
      Stub_Node : Node_Id;
   begin
      Get_Name_String (U, Norm_Ada_Name);
      Body_Node := Unit (Top);

      if Nkind (Body_Node) = N_Subunit then
         Body_Node := Proper_Body (Body_Node);
      end if;

      Stub_Node := Get_First_Stub (Body_Node);

      if No (Stub_Node) then
         return;
      end if;

      while Present (Stub_Node) loop
         Process_Stub (C, U, Stub_Node);
         Stub_Node := Get_Next_Stub (Stub_Node);
      end loop;

      Unit_Table.Table (U).Subunits_Computed := True;

   end Set_Subunits;

   --------------------
   -- Set_Supporters --
   --------------------

   procedure Set_Supporters (C : Context_Id; U : Unit_Id; Top : Node_Id) is
   begin
      Set_Withed_Units      (C, U, Top);
      Set_Direct_Dependents (U);
   end Set_Supporters;

   ----------------------
   -- Set_Withed_Units --
   ----------------------

   procedure Set_Withed_Units (C : Context_Id; U : Unit_Id; Top : Node_Id)
   is
      With_Clause_Node  : Node_Id;
      Cunit_Node        : Node_Id;
      Cunit_Number      : Unit_Number_Type;
      Current_Supporter : Unit_Id;
      Tmp               : Unit_Id;
      Include_Unit      : Boolean := False;
   begin
      --  the maim control structure - cycle through the with clauses
      --  in the tree
      if No (Context_Items (Top)) then
         return;
      end if;

      With_Clause_Node := First_Non_Pragma (Context_Items (Top));

      while Present (With_Clause_Node) loop
         --  here we simply get the name of the next supporting unit from
         --  the GNAT Units Table (defined in Lib)
         Cunit_Node    := Library_Unit (With_Clause_Node);
         Cunit_Number  := Get_Cunit_Unit_Number (Cunit_Node);
         Get_Decoded_Name_String (Unit_Name (Cunit_Number));

         Set_Norm_Ada_Name_String_With_Check (Cunit_Number, Include_Unit);

         if Include_Unit then

            Current_Supporter := Name_Find (C);

            if A_Name_Buffer (A_Name_Len) = 'b' then
               A_Name_Buffer (A_Name_Len) := 's';
               Tmp := Name_Find (C);

               if Present (Tmp) then
                  --  OPEN PROBLEM: is this the best solution for this problem?
                  --
                  --  Here we are in the potentially hard-to-report-about and
                  --  definitely involving inconsistent unit set situation.
                  --  The last version of U depends on subprogram body at least
                  --  in one of the consistent trees, but the Context contains
                  --  a spec (that is, a library_unit_declaration or a
                  --  library_unit_renaming_declaration) for the same full
                  --  expanded Ada name. The current working decision is
                  --  to set this dependency as if U depends on the spec.
                  --
                  --  Another (crazy!) problem: in one consistent tree
                  --  U depends on the package P (and P does not require a
                  --  body), and in another consistent tree U depends on
                  --  the procedure P which is presented by its body only.
                  --  It may be quite possible, if these trees were created
                  --  with different search paths. Is our decision reasonable
                  --  for this crazy situation :-[ ??!!??

                  Current_Supporter := Tmp;
               end if;

            end if;

            --  and now we store this dependency - we have to use
            --  Add_To_Elmt_List instead of Append_Elmt - some units
            --  may be mentioned several times in the context clause:
            if Implicit_With (With_Clause_Node) then
               Add_To_Elmt_List
                 (Unit => Current_Supporter,
                  List => Unit_Table.Table (U).Implicit_Supporters);
            else
               Add_To_Elmt_List
                 (Unit => Current_Supporter,
                  List => Unit_Table.Table (U).Direct_Supporters);
            end if;
         end if;

         With_Clause_Node := Next_Non_Pragma (With_Clause_Node);

         while Present (With_Clause_Node) and then
               Nkind (With_Clause_Node) /= N_With_Clause
         loop
            With_Clause_Node := Next_Non_Pragma (With_Clause_Node);
         end loop;

      end loop;
   end Set_Withed_Units;

   -------------------------------------------------------
   -- Dynamic Unit_Id list abstraction (implementation) --
   -------------------------------------------------------

   ----------------------
   --  In_Unit_Id_List --
   ----------------------

   function In_Unit_Id_List
     (U    : Unit_Id;
      L    : Unit_Id_List_Access)
      return Boolean
   is
   begin

      if L /= null then

         for I in L'Range loop

            if U = L (I) then
               return True;
            end if;

         end loop;

      end if;

      return False;
   end In_Unit_Id_List;

   --------------------------
   --  Add_To_Unit_Id_List --
   --------------------------

   procedure Add_To_Unit_Id_List
     (U : Unit_Id;
      L : in out Unit_Id_List_Access)
   is
   begin

      if not In_Unit_Id_List (U, L) then
         Append_Unit_To_List (U, L);
      end if;

   end Add_To_Unit_Id_List;

   -------------------------
   -- Append_Unit_To_List --
   -------------------------

   procedure Append_Unit_To_List
     (U : Unit_Id;
      L : in out Unit_Id_List_Access)
   is
   begin

      if L = null then
         L := new Unit_Id_List'(1 => U);
      else
         Free (Tmp_Unit_Id_List_Access);
         Tmp_Unit_Id_List_Access := new Unit_Id_List'(L.all & U);
         Free (L);
         L := new Unit_Id_List'(Tmp_Unit_Id_List_Access.all);
      end if;

   end Append_Unit_To_List;

end A4G.Contt.Dp;
