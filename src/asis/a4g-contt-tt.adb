------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C O N T T . T T                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines Tree Table, which contains the information
--  about the tree output files needed for swapping the ASTs accessed
--  by ASIS. This information includes such things as Asis Compilation
--  Units, and their top nodes in the tree.

with Asis;            use Asis;
with Asis.Compilation_Units;
with Asis.Errors;     use Asis.Errors;

with Asis.Set_Get;    use Asis.Set_Get;

with A4G.A_Debug;     use A4G.A_Debug;
with A4G.A_Output;    use A4G.A_Output;
with A4G.Asis_Tables; use A4G.Asis_Tables;
with A4G.Contt.UT;    use A4G.Contt.UT;
with A4G.Get_Unit;    use A4G.Get_Unit;
with A4G.Vcheck;      use A4G.Vcheck;

with Atree;           use Atree;
with Lib;             use Lib;
with Namet;           use Namet;
with Nlists;          use Nlists;
with Output;          use Output;
with Sinfo;           use Sinfo;
with Sinput;          use Sinput;
with Tree_In;

package body A4G.Contt.TT is

   procedure Set_Nil_Tree_Names (T : Tree_Id);
   --  Sets all the fields related to Source File Name Table as indicating
   --  empty  strings

   procedure Set_Nil_Tree_Attributes (T : Tree_Id);
   --  Sets all the attributes of T as if T is an ASIS Nil_Tree

   function Find_Enclosed_Decl
     (Scope : Node_Id;
      J         : Int)
      return Node_Id;
   --  Starting from Scope, looks for the nested scope which is stored
   --  in Node_Trace table as Node_Trase.Table (J). Node, that expanded
   --  generic specs are considered as ordinary scopes.

   -------------------------
   -- Allocate_Tree_Entry --
   -------------------------

   function Allocate_Tree_Entry return Tree_Id is
      New_Last : Tree_Id;
      --  the Id of the new entry being allocated in the Unit Table
   begin

      Tree_Table.Increment_Last;
      New_Last := Tree_Table.Last;

      Set_Nil_Tree_Names      (New_Last);
      Set_Nil_Tree_Attributes (New_Last);

      Tree_Table.Table (New_Last).Tree_Name_Chars_Index := A_Name_Chars.Last;

      Tree_Table.Table (New_Last).Tree_Name_Len := Short (A_Name_Len);

      --  Set corresponding string entry in the Name_Chars table

      for I in 1 .. A_Name_Len loop
         A_Name_Chars.Increment_Last;

         A_Name_Chars.Table (A_Name_Chars.Last) := A_Name_Buffer (I);
      end loop;

      A_Name_Chars.Increment_Last;
      A_Name_Chars.Table (A_Name_Chars.Last) := ASCII.NUL;
      return New_Last;

   end Allocate_Tree_Entry;

   ------------------------------------------
   -- Current_Tree_Consistent_With_Sources --
   ------------------------------------------

   function Current_Tree_Consistent_With_Sources return Boolean is
      Result       : Boolean := True;
      Source_Stamp : Time_Stamp_Type;
      Tree_Stamp   : Time_Stamp_Type;
      Source       : File_Name_Type;
   begin

      for J in 2 .. Last_Source_File loop
         --  We start from 2, because the entry 1 in the Source File Table
         --  is always for system.ads (see Sinput, spec).
         Tree_Stamp := Time_Stamp (J);

         Source := Full_File_Name (J);

         Get_Name_String (Source);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ASCII.NUL;

         if not Is_Regular_File (Name_Buffer) then
            --  The source file was (re)moved
            Result := False;
            exit;

         else
            Source_Stamp := TS_From_OS_Time (File_Time_Stamp (Name_Buffer));

            if Source_Stamp /= Tree_Stamp then
               --  The source file has been changed
               Result := False;
               exit;
            end if;

         end if;

      end loop;

      return Result;

   end Current_Tree_Consistent_With_Sources;

   ------------------------
   -- Find_Enclosed_Decl --
   ------------------------

   function Find_Enclosed_Decl
     (Scope : Node_Id;
      J     : Int)
      return Node_Id
   is
      Result : Node_Id := Empty;

      List_To_Search : List_Id;
      Kind_To_Search : constant Node_Kind := Node_Trace.Table (J).Kind;

      Line_To_Search : constant Physical_Line_Number :=
        Node_Trace.Table (J).Node_Line;

      Col_To_Search  : constant Column_Number :=
        Node_Trace.Table (J).Node_Col;

      function Check_Node (N : Node_Id) return Traverse_Result;
      --  Check if N is the needed node. If it is, Sets Result equial to N and
      --  returns Abandon. Othervise returns OK.

      function Find_In_List (L : List_Id) return Node_Id;
      --  Looks for the needed scope in a node list

      procedure Traverse_Scope is new
        Atree.Traverse_Proc (Process => Check_Node);

      function Check_Node (N : Node_Id) return Traverse_Result is
         N_Sloc       : Source_Ptr;
         Traverse_Res : Traverse_Result := OK;
      begin

         if Nkind (N) = Kind_To_Search then
            N_Sloc := Sloc (N);

            if Get_Physical_Line_Number (N_Sloc) = Line_To_Search
              and then
               Get_Column_Number (N_Sloc)        = Col_To_Search
            then
               Result       := N;
               Traverse_Res := Abandon;
            end if;

         end if;

         return Traverse_Res;
      end Check_Node;

      function Find_In_List (L : List_Id) return Node_Id is
         Res       : Node_Id := Empty;
         Next_Node : Node_Id;
         Next_Sloc : Source_Ptr;
      begin
         Next_Node := First_Non_Pragma (L);

         while Present (Next_Node) loop

            if Nkind (Next_Node) = Kind_To_Search then
               Next_Sloc := Sloc (Next_Node);

               if Get_Physical_Line_Number (Next_Sloc) = Line_To_Search
                 and then
                  Get_Column_Number (Next_Sloc)        = Col_To_Search
               then
                  Res := Next_Node;
                  exit;
               end if;

            end if;

            Next_Node := Next_Non_Pragma (Next_Node);

         end loop;

         return Res;
      end Find_In_List;

   begin

      if Nkind (Scope) = N_Package_Instantiation then
         Result := Scope;

         while Nkind (Result) /= N_Package_Declaration loop
            Result := Prev_Non_Pragma (Result);
         end loop;

         return Result;

      end if;

      if Nkind (Scope) = N_Package_Body
        or else
         Nkind (Scope) = N_Subprogram_Body
        or else
         Nkind (Scope) = N_Block_Statement
      then
         List_To_Search := Sinfo.Declarations (Scope);
      else
         List_To_Search := Visible_Declarations (Scope);
      end if;

      Result := Find_In_List (List_To_Search);

      if No (Result) then

         if Nkind (Scope) = N_Package_Specification then
            List_To_Search := Private_Declarations (Scope);
            Result         := Find_In_List (List_To_Search);

            if No (Result)
              and then
               Nkind (Parent (Scope)) = N_Generic_Package_Declaration
            then
               List_To_Search := Generic_Formal_Declarations (Parent (Scope));
               Result         := Find_In_List (List_To_Search);
            end if;

         elsif Nkind (Scope) = N_Block_Statement
            or else
               Nkind (Scope) = N_Subprogram_Body
         then
            --  We can have an instantiation nested in some block statement in
            --  tne library subprogram body. This should not happen too often,
            --  so we can use this performance-expensive approach here.
            Traverse_Scope (Scope);
         end if;

      end if;

      pragma Assert (Present (Result));

      return Result;

   end Find_Enclosed_Decl;

   -------------------
   -- Get_Tree_Name --
   -------------------

   function Get_Tree_Name (C : Context_Id; Id : Tree_Id) return String is
   begin
      Get_Name_String (C, Id);
      return A_Name_Buffer (1 ..  A_Name_Len);
   end Get_Tree_Name;

   -----------------------------
   -- Restore_Node_From_Trace --
   -----------------------------

   function Restore_Node_From_Trace
     (In_Body : Boolean               := False;
      CU      : Asis.Compilation_Unit := Asis.Nil_Compilation_Unit)
      return    Node_Id
   is
      Start_Node : Node_Id;
      Result     : Node_Id := Empty;
   begin

      if Asis.Compilation_Units.Is_Nil (CU) then
         Start_Node := Unit (Cunit (Main_Unit));

         if Nkind (Start_Node) = N_Package_Body and then
            not In_Body
         then
            Start_Node := Corresponding_Spec (Start_Node);

            while not (Nkind (Start_Node) = N_Package_Declaration
                     or else
                        Nkind (Start_Node) = N_Generic_Package_Declaration)
            loop
               Start_Node := Parent (Start_Node);
            end loop;

         end if;
      else
         Start_Node := Unit (Top (CU));
      end if;

      if Node_Trace.First = Node_Trace.Last then
         --  One-element trace means, that we have a library-level package
         --  instantiation
         Result := Start_Node;
      else

         if Nkind (Start_Node) = N_Package_Declaration
           or else
             Nkind (Start_Node) = N_Generic_Package_Declaration
         then
            Start_Node := Specification (Start_Node);
         end if;

         for J in reverse Node_Trace.First + 1 .. Node_Trace.Last - 1 loop
            Start_Node := Find_Enclosed_Decl (Start_Node, J);

            if Nkind (Start_Node) = N_Package_Declaration
              or else
               Nkind (Start_Node) = N_Generic_Package_Declaration
            then
               Start_Node := Specification (Start_Node);
            end if;

         end loop;

         Result := Find_Enclosed_Decl (Start_Node, Node_Trace.First);

      end if;

      pragma Assert (Present (Result));

      return Result;

   end Restore_Node_From_Trace;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (C : Context_Id; Id : Tree_Id) is
      S : Int;
      L : Short;

   begin

      Reset_Context (C); --  ???

      S := Tree_Table.Table (Id).Tree_Name_Chars_Index;
      L := Tree_Table.Table (Id).Tree_Name_Len;

      A_Name_Len := Natural (L);

      for I in 1 .. A_Name_Len loop
         A_Name_Buffer (I) := A_Name_Chars.Table (S + Int (I));
      end loop;
   end Get_Name_String;

   -----------------
   -- Print_Trees --
   -----------------
   procedure Print_Trees (C : Context_Id) is
   begin
      Write_Str ("Tree Table for Context number: ");
      Write_Int (Int (C));
      Write_Eol;

      if C = Non_Associated then
         Write_Str ("   Nil Context, it can never be associated ");
         Write_Str ("with any tree");
         Write_Eol;
         return;
      end if;

      if Is_Opened (C) then
         for Tr in First_Tree_Id .. Last_Tree (C) loop
            Output_Tree (C, Tr);
         end loop;
         Write_Eol;
      else
         Write_Str ("This Context is closed");
         Write_Eol;
      end if;
   end Print_Trees;

   -----------------------------
   -- Set_Nil_Tree_Attributes --
   -----------------------------

   procedure Set_Nil_Tree_Attributes (T : Tree_Id) is
   begin
      Set_Main_Unit_Id (T, Nil_Unit);
      Set_Main_Top (T, Empty);
      Tree_Table.Table (T).Units := No_Elist;
   end Set_Nil_Tree_Attributes;

   ------------------------
   -- Set_Nil_Tree_Names --
   ------------------------

   procedure Set_Nil_Tree_Names (T : Tree_Id) is
      Tr : constant Tree_Id := T;
   begin
      Tree_Table.Table (Tr).Tree_Name_Chars_Index := 0;
      Tree_Table.Table (Tr).Tree_Name_Len         := 0;
   end Set_Nil_Tree_Names;

   ---------------------------------------------------------------
   --  Internal Tree Unit Attributes Access and Update Routines --
   ---------------------------------------------------------------

   function Main_Unit_Id (T : Tree_Id) return Unit_Id is
   begin
      return Tree_Table.Table (T).Main_Unit;
   end Main_Unit_Id;

   function Main_Unit_Id return Unit_Id is
   begin
      return Tree_Table.Table (Current_Tree).Main_Unit;
   end Main_Unit_Id;

   procedure Set_Main_Unit_Id (T : Tree_Id; U : Unit_Id) is
   begin
      Tree_Table.Table (T).Main_Unit := U;
   end Set_Main_Unit_Id;

   procedure Set_Main_Top  (T : Tree_Id; N : Node_Id) is
   begin
      Tree_Table.Table (T).Main_Top := N;
   end Set_Main_Top;

   procedure Set_Main_Unit_Id (U : Unit_Id) is
   begin
      Tree_Table.Table (Current_Tree).Main_Unit := U;
   end Set_Main_Unit_Id;

   procedure Set_Main_Top  (N : Node_Id) is
   begin
      Tree_Table.Table (Current_Tree).Main_Top := N;
   end Set_Main_Top;

   -----------------------------------
   -- Subprograms for Tree Swapping --
   -----------------------------------

   -----------------------------------
   -- Append_Full_View_Tree_To_Unit --
   -----------------------------------

   procedure Append_Full_View_Tree_To_Unit (C : Context_Id; U : Unit_Id) is
   begin
      Reset_Context (C);
      Add_To_Elmt_List (Unit_Id (Current_Tree),
                        Unit_Table.Table (U).Full_View_Trees);
   end Append_Full_View_Tree_To_Unit;

   --------------------------------------
   -- Append_Limited_View_Tree_To_Unit --
   --------------------------------------

   procedure Append_Limited_View_Tree_To_Unit (C : Context_Id; U : Unit_Id) is
   begin
      Reset_Context (C);
      Add_To_Elmt_List (Unit_Id (Current_Tree),
                        Unit_Table.Table (U).Limited_View_Trees);
   end Append_Limited_View_Tree_To_Unit;

   -------------------
   -- Reorder_Trees --
   -------------------

   procedure Reorder_Trees (C : Context_Id) is
      Main_Unit : Unit_Id;
      --  The unit which main tree should be moved to the first position in
      --  the list of trees for the unit being processed in a loop

      First_Tree : Tree_Id;
      Success    : Boolean;
      C_Mode     : constant Context_Mode := Context_Processing_Mode (C);
   begin

      for U in First_Unit_Id + 1 .. Last_Unit loop
         --  First_Unit_Id corresponds to Standard

         Success   := True;
         Main_Unit := Nil_Unit;

         case Kind (C, U) is
            when A_Subunit =>
               --  (1)
               Main_Unit := Get_Subunit_Parent_Body (C, U);

               while Kind (C, Main_Unit) in A_Subunit loop
                  Main_Unit := Get_Subunit_Parent_Body (C, Main_Unit);
               end loop;

               if No (Main_Tree (C, Main_Unit)) then

                  if C_Mode in Partition .. All_Trees then
                     Get_Name_String (U, Ada_Name);

                     ASIS_Warning
                        (Message =>
                           "Asis.Ada_Environments.Open: " &
                           "ancestor body is not compiled for subunit " &
                            A_Name_Buffer (1 .. A_Name_Len),
                         Error   => Data_Error);
                  end if;

                  Success := False;
               end if;

            when A_Package           |
                 A_Generic_Package   |
                 A_Procedure         |
                 A_Function          |
                 A_Generic_Procedure |
                 A_Generic_Function  =>

               --  (2), (3) and (5)

               if Is_Body_Required (C, U)           or else
                  Kind (C, U) = A_Procedure         or else
                  Kind (C, U) = A_Function          or else
                  Kind (C, U) = A_Generic_Procedure or else
                  Kind (C, U) = A_Generic_Function
               then
                  --  (2) and (5)
                  Main_Unit := Get_Body (C, U);

                  if No (Main_Unit) or else
                     No (Main_Tree (C, Main_Unit))
                  then
                     --  The second condition corresponds to the situation when
                     --  the tree is created for library-level generic spec
                     --  which requires the body

                     if C_Mode in Partition .. All_Trees and then
                        Origin (C, U) = An_Application_Unit
                     then
                        Get_Name_String (U, Ada_Name);

                        ASIS_Warning
                           (Message =>
                                "Asis.Ada_Environments.Open: "
                              & "body is not compiled for "
                              &  A_Name_Buffer (1 .. A_Name_Len),
                            Error   => Data_Error);
                     end if;

                     Success := False;
                  end if;

               else
                  --  (3)
                  Main_Unit := U;

                  if No (Main_Tree (C, Main_Unit)) then
                     --  We do not generate any warning in this case, because
                     --  we do not know whether or not this package
                     --  declaration has to be compiled on its own. So we only
                     --  set Success OFF to prevent any change in the tree
                     --  list
                     Success := False;
                  end if;

               end if;

            when A_Generic_Unit_Instance =>
               --  (4)
               Main_Unit := U;

               if No (Main_Tree (C, Main_Unit)) then

                  if C_Mode in Partition .. All_Trees and then
                     Origin (C, U) = An_Application_Unit
                  then
                     Get_Name_String (U, Ada_Name);

                     ASIS_Warning
                        (Message =>
                            "Asis.Ada_Environments.Open: "
                          & "library-level instance "
                          &  A_Name_Buffer (1 .. A_Name_Len)
                          & " is not compiled",
                         Error   => Data_Error);
                  end if;

                  Success := False;
               end if;

            when A_Library_Unit_Body =>
               --  There are some situations when the body is compiled because
               --  the corresponding spec is a supporter of the main unit of
               --  the compilation. See Lib (spec), (h)
               Main_Unit := U;

               if No (Main_Tree (C, Main_Unit)) then
                  --  We do notr generate a warning here - if needed, the
                  --  warning is generated for the corresponding spec
                  Success := False;
               end if;

            when others =>
               null;
         end case;

         if Success and then Present (Main_Unit) then
            --  Here we have to reorder the trees for U. Currently the
            --  simplest solution is used - we just prepend the right tree
            --  to the tree list, if it is not already the first tree in
            --  the list. So this tree may be duplicated in the list.
            First_Tree := Main_Tree (C, Main_Unit);

            if First_Tree /=
              Tree_Id
                (Unit (First_Elmt (Unit_Table.Table (U).Full_View_Trees)))
            then
               Prepend_Elmt
                 (Unit_Id (First_Tree), Unit_Table.Table (U).Full_View_Trees);
            end if;

         end if;

      end loop;

   end Reorder_Trees;

   ----------------
   -- Reset_Tree --
   ----------------

   procedure Reset_Tree (Context : Context_Id; Tree : Tree_Id) is
      Tree_File_FD : File_Descriptor;
      File_Closed  : Boolean := False;
   begin
      --  Special processing for GNSA mode:

      if Tree_Processing_Mode (Current_Context) = GNSA then
         --  This is no more than a workaround for -GNSA C1 Context when we
         --  have exactly one tree (and exactly one (GNSA) Context!
         return;
      end if;

      if Context = Current_Context and then
         Tree    = Current_Tree
      then
         return;
      end if;

      if Debug_Flag_T then
         Write_Str ("In Context ");
         Write_Int (Int (Context));
         Write_Str (" resetting the tree ");
         Write_Int (Int (Tree));
         Write_Eol;
      end if;

      --  the following call to Reset_Context is redundant, because the next
      --  call to Get_Name_String also resets Context, but this is the right
      --  place for Reset_Context
      Reset_Context (Context);

      Get_Name_String (Context, Tree);
      --  should be always successful, because Tree may correspond only to
      --  some tree file, which has been investigated by ASIS

      A_Name_Buffer (A_Name_Len + 1) := ASCII.NUL;

      if Debug_Flag_T then
         Write_Str (" (");
         Write_Str (A_Name_Buffer (1 .. A_Name_Len));
         Write_Str (")");
         Write_Eol;

      end if;

      Tree_File_FD := Open_Read (A_Name_Buffer'Address, Binary);

      if Tree_File_FD = Invalid_FD then
         Raise_ASIS_Failed
           (Diagnosis => "A4G.Contt.TT.Reset_Tree: "      &
                         "Cannot open tree file: "        &
                          A_Name_Buffer (1 .. A_Name_Len) &
                          ASIS_Line_Terminator            &
                          "ASIS external environment may have been changed",
            Stat      => Data_Error);
      end if;

      begin
         Tree_In (Tree_File_FD);
      exception
         when others =>
            Close (Tree_File_FD, File_Closed);

            --  We did not chech File_Closed here, because the problem in
            --  Tree_In seems to be more important for ASIS

            Raise_ASIS_Failed
              (Diagnosis => "A4G.Contt.TT.Reset_Tree: "      &
                            "Can not read tree file: "       &
                             A_Name_Buffer (1 .. A_Name_Len) &
                             ASIS_Line_Terminator            &
                             "ASIS external environment may have been changed",
               Stat      => Data_Error);
      end;

      Close (Tree_File_FD, File_Closed);

      if not File_Closed then
         Raise_ASIS_Failed
           (Diagnosis => "A4G.Contt.TT.Reset_Tree: "      &
                         "Can not close tree file: "      &
                          A_Name_Buffer (1 .. A_Name_Len) &
                          ASIS_Line_Terminator            &
                          "disk is full or file may be used by other program",
            Stat      => Data_Error);
      end if;

      --  if we are here, then the required tree has been successfully
      --  re-retrieved. So:

      Current_Context := Context;
      Current_Tree    := Tree;

      if Debug_Flag_T then
         Write_Str ("In Context ");
         Write_Int (Int (Context));
         Write_Str (" the tree ");
         Write_Int (Int (Tree));
         Write_Str (" has been reset");
         Write_Eol;
      end if;

   end Reset_Tree;

   -----------------------------
   --  Reset_Tree_For_Element --
   -----------------------------

   procedure Reset_Tree_For_Element (E : Asis.Element) is
   begin
      Reset_Tree (Encl_Cont_Id (E), Encl_Tree (E));
   end Reset_Tree_For_Element;

   -------------------------
   -- Reset_Tree_For_Unit --
   -------------------------

   procedure Reset_Tree_For_Unit (C : Context_Id; U : Unit_Id) is
      Tree_List   : Elist_Id;
      Tree_To_Set : Tree_Id;
   begin
      --  Special processing for GNSA mode:

      if Tree_Processing_Mode (Get_Current_Cont) = GNSA then
         --  This is no more than a workaround for -GNSA C1 Context when we
         --  have exactly one tree (and exactly one (GNSA) Context!
         return;
      end if;

      Tree_List  := Unit_Table.Table (U).Full_View_Trees;

      if No (Tree_List) or else No (First_Elmt (Tree_List)) then
         Tree_List  := Unit_Table.Table (U).Limited_View_Trees;
      end if;
      --  it cannot be No_List or Empty_List!

      Tree_To_Set := Tree_Id (Unit (First_Elmt (Tree_List)));

      if Debug_Flag_T then
         Write_Str ("For unit ");
         Write_Int (Int (U));
         Write_Str (" ");
      end if;

      Reset_Tree (Context => C,
                  Tree    => Tree_To_Set);
   end Reset_Tree_For_Unit;

   procedure Reset_Tree_For_Unit (Unit : Asis.Compilation_Unit) is
   begin
      Reset_Tree_For_Unit (Encl_Cont_Id (Unit), Get_Unit_Id (Unit));
   end Reset_Tree_For_Unit;

   -------------------------
   -- Reset_Instance_Tree --
   -------------------------

   procedure Reset_Instance_Tree
     (Lib_Level_Instance : Asis.Compilation_Unit;
      Decl_Node          : in out Node_Id)
   is
      U            : Unit_Id := Get_Unit_Id (Lib_Level_Instance);
      Tree_To_Set  : Tree_Id;
      Curr_Context : constant Context_Id := Get_Current_Cont;
      Curr_Tree    : constant Tree_Id    := Get_Current_Tree;
      In_Body      : Boolean := False;
   begin
      --  Special processing for GNSA mode:

      if Tree_Processing_Mode (Curr_Context) = GNSA then
         --  This is no more than a workaround for -GNSA C1 Context when we
         --  have exactly one tree (and exactly one (GNSA) Context!
         return;
      end if;

      Tree_To_Set :=
         Unit_Table.Table (U).Main_Tree;

      if No (Tree_To_Set) then

         if Kind (Lib_Level_Instance) in A_Package .. A_Generic_Package or else
            Kind (Lib_Level_Instance) in A_Library_Unit_Body
         then
            U := Get_Body (Current_Context, U);

            if Tree_Processing_Mode (Curr_Context) = Incremental and then
               (No (U) or else
                No (Unit_Table.Table (U).Main_Tree))
            then
               --  In this situation we try to compile the needed body on the
               --  fly
               if Is_Body_Required (Lib_Level_Instance) or else
                  Kind (Lib_Level_Instance) in A_Library_Unit_Body
               then

                  U := Get_Main_Unit_Tree_On_The_Fly
                        (Start_Unit => Get_Unit_Id (Lib_Level_Instance),
                        Cont       => Curr_Context,
                        Spec       => False);
               else
                  U := Get_Main_Unit_Tree_On_The_Fly
                        (Start_Unit => Get_Unit_Id (Lib_Level_Instance),
                        Cont       => Curr_Context,
                        Spec       => True);
               end if;

            end if;

         elsif Kind (Lib_Level_Instance) in A_Generic_Unit_Instance and then
               Tree_Processing_Mode (Encl_Cont_Id (Lib_Level_Instance)) =
               Incremental
         then
               U := Get_Main_Unit_Tree_On_The_Fly
                      (Start_Unit => Get_Unit_Id (Lib_Level_Instance),
                       Cont       => Curr_Context,
                       Spec       => True);
         end if;

         if Present (U) then

            Tree_To_Set := Unit_Table.Table (U).Main_Tree;

            Reset_Tree (Context => Get_Current_Cont,
                        Tree    => Curr_Tree);
         end if;

      end if;

      if No (Tree_To_Set) or else Tree_To_Set = Current_Tree then
         return;
      end if;

      Create_Node_Trace (Decl_Node);

      Reset_Tree (Context => Get_Current_Cont,
                  Tree    => Tree_To_Set);

      if Kind (Lib_Level_Instance) in A_Library_Unit_Body then
         In_Body := True;
      end if;

      Decl_Node := Restore_Node_From_Trace (In_Body);

   end Reset_Instance_Tree;

   ----------------------------------
   -- Tree_Consistent_With_Sources --
   ----------------------------------

   function Tree_Consistent_With_Sources
     (E :    Asis.Element)
      return Boolean
   is
   begin

      Reset_Tree (Encl_Cont_Id (E), Encl_Tree (E));

      return Current_Tree_Consistent_With_Sources;

   end Tree_Consistent_With_Sources;

   function Tree_Consistent_With_Sources
     (CU :   Asis.Compilation_Unit)
      return Boolean
   is
   begin
      Reset_Tree_For_Unit (CU);
      return Current_Tree_Consistent_With_Sources;
   end Tree_Consistent_With_Sources;

   --------------------------
   -- Unit_In_Current_Tree --
   --------------------------

   function Unit_In_Current_Tree (C : Context_Id; U : Unit_Id) return Boolean
   is
   begin
      if U = Standard_Id then
         return True;
      end if;

      if Current_Context /= C then
         return False;
      end if;

      return
        In_Elmt_List
          (Unit_Id (Current_Tree), Unit_Table.Table (U).Full_View_Trees)
       or else
          (No (Unit_Table.Table (U).Full_View_Trees)
           and then
           In_Elmt_List
            (Unit_Id (Current_Tree), Unit_Table.Table (U).Limited_View_Trees));

   end Unit_In_Current_Tree;

--------------------------------------------------
--    General-Purpose Tree Table Subprograms    --
--------------------------------------------------

   ---------------
   -- Last_Tree --
   ---------------

   function Last_Tree (C : Context_Id) return Tree_Id is
   begin
      Reset_Context (C);
      return Tree_Table.Last;
   end Last_Tree;

   --------
   -- No --
   --------

   function No (Tree : Tree_Id) return Boolean is
   begin
      return Tree = Nil_Tree;
   end No;

   -----------------
   -- Output_Tree --
   -----------------

   procedure Output_Tree (C : Context_Id; Tree : Tree_Id) is
   begin

      --  ???  Check for Debug_Mode should be moved into the context(s) where
      --  ???  Output_Tree is called

      if Debug_Mode   or else
         Debug_Flag_C or else
         Debug_Lib_Model
      then
         Write_Str ("Debug output for Tree Id " & Tree_Id'Image (Tree));
         Write_Eol;

         if Tree = Nil_Tree then
            Write_Str ("This is a Nil Tree");
            Write_Eol;
            return;
         end if;

         Get_Name_String (C, Tree);

         Write_Str ("Tree File Name is: " & A_Name_Buffer (1 ..  A_Name_Len));
         Write_Eol;

         Write_Str ("Main Unit Id : ");
         Write_Str (Main_Unit_Id (Tree)'Img);
         Write_Eol;

         Write_Str ("The list of the Units contained in the tree:");
         Write_Eol;

         Print_List (Tree_Table.Table (Tree).Units);

         Write_Eol;
      end if;

   end Output_Tree;

   -------------
   -- Present --
   -------------

   function Present (Tree : Tree_Id) return Boolean is
   begin
      return Tree /= No_Tree_Name;
   end Present;

end A4G.Contt.TT;
