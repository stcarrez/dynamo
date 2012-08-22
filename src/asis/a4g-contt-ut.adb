------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C O N T T . U T                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore.                  --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;

with Asis.Errors;     use Asis.Errors;
with Asis.Exceptions; use Asis.Exceptions;

with Asis.Set_Get;    use Asis.Set_Get;

with A4G.A_Debug;     use A4G.A_Debug;
with A4G.Contt.Dp;    use A4G.Contt.Dp;
with A4G.Contt.TT;    use A4G.Contt.TT;
with A4G.Vcheck;      use A4G.Vcheck;

with Atree;           use Atree;
with Einfo;           use Einfo;
with Lib;
with Namet;           use Namet;
with Output;          use Output;
with Sinfo;           use Sinfo;
with Sinput;          use Sinput;
with Snames;          use Snames;
with Table;

package body A4G.Contt.UT is

   -----------------------------------------
   -- Local Subprograms (general-purpose) --
   -----------------------------------------

   function Allocate_New_Entry (C : Context_Id) return Unit_Id;
   --  allocates and returns a new entry in the Context Unit table
   --  No setting or any other changes are done

   procedure Set_Nil_Unit_Names (U : Unit_Id);
   --  Sets all the fields related to Unit Name Table as indicating empty
   --  strings
   --  The body is in "Unit Name Table Data and Subprograms" section

   procedure Set_Nil_Unit_Attributes (C : Context_Id; U : Unit_Id);
   --  Sets all the attributes, dependency lists and tree lists of U as
   --  if U is an ASIS Nil_Compilation_Unit.
   --  The body is in "Black-Box Unit Attributes Routines" section

   procedure Set_No_Source_File (U : Unit_Id);
   --  Makes settings corresponding to the absence of the source file
   --  name

   function Same_Names return Boolean;
   --  Compares the contents of the ASIS and GNAT Name Buffers.

   procedure Make_Unit_Name;
   --  Supposing that A_Name_Buffer contains the normalized name of a
   --  nonexistent unit (with a suffix ending with 'n', this procedure
   --  sets the content of A_Name_Buffer as equal to the Ada name of
   --  this nonexistent unit

   function Is_Spec (U : Unit_Id) return Boolean;
   --  Checks if U denotes a unit that is a library_unit_declaration

   function Get_Unit_Id_List (List : Elist_Id) return Unit_Id_List;
   --  Transforms the unit list into one-dimensional array of unit Ids.
   --  Returns Nil_Unit_Id_List for No_Elist

   function Absolute_Full_File_Name return Boolean;
   --  Checks that a source file name currently contained in the GNAT Name
   --  Table contains directory information in an absolute form

   procedure Store_Tree_For_Unit
     (C   : Context_Id;
      U   : Unit_Id;
      N_U : Unit_Number_Type);
   --  Provided that N_U is the unit number in the current tree that
   --  corresponds to U, stores the currently accessed tree in the list of full
   --  or limited view trees for the given unit.

   type Top_Node_Rec is record
      Tree     : Tree_Id;
      Top_Node : Node_Id;
   end record;

   package Top_Node_Cache is new Table.Table (
     Table_Component_Type => Top_Node_Rec,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Id,
     Table_Initial        => 1000,
     Table_Increment      => 100,
     Table_Name           => "Top Node Cache");
   --  Used to cache the already computed results of the Top function

   -----------------------------
   -- Absolute_Full_File_Name --
   -----------------------------

   function Absolute_Full_File_Name return Boolean is
      Result : Boolean := False;
   begin
      if Namet.Name_Buffer (1) /= '.' then

         for I in 1 .. Namet.Name_Len loop

            if Namet.Name_Buffer (I) = '/'
              or else
               Namet.Name_Buffer (I) = '\'
            then
               Result := True;
               exit;
            end if;

         end loop;

      end if;

      return Result;

   end Absolute_Full_File_Name;

   ------------------------
   -- Allocate_New_Entry --
   ------------------------

   function Allocate_New_Entry (C : Context_Id) return Unit_Id is
      Hash_Index : Hash_Index_Type;
      --  Computed hash index

      Curr_Id    : Unit_Id;
      --  Id of entries in hash chain, if any

      New_Last : Unit_Id;
      --  the Id of the new entry being allocated in the Unit Table
   begin

      Hash_Index := Hash;
      Curr_Id    := Contexts.Table (C).Hash_Table (Hash_Index);

      Unit_Table.Increment_Last;
      New_Last   := Unit_Table.Last;

      --  correcting the hash chain, if any

      if Curr_Id = No_Unit_Id then
         Contexts.Table (C).Hash_Table (Hash_Index) := New_Last;
         --  no hash chain to correct
      else
         while Unit_Table.Table (Curr_Id).Hash_Link /= No_Unit_Id
         loop
            Curr_Id := Unit_Table.Table (Curr_Id).Hash_Link;
         end loop;

         --  now Curr_Id is the last entry in the hash chain
         Unit_Table.Table (Curr_Id).Hash_Link := New_Last;
      end if;

      return New_Last;

   end Allocate_New_Entry;

   -------------------------------------
   -- Allocate_Nonexistent_Unit_Entry --
   -------------------------------------

   function Allocate_Nonexistent_Unit_Entry (C : Context_Id) return Unit_Id
   is
      New_Unit_Id : Unit_Id;
   begin
      --  first we should modify the normalized unit name to make it the
      --  name of a nonexistent unit:

      A_Name_Len := A_Name_Len + 1;
      A_Name_Buffer (A_Name_Len) := 'n';

      if Debug_Flag_O    or else
         Debug_Lib_Model or else
         Debug_Mode
      then
         Write_Str ("Allocating new nonexistent unit: ");
         Write_Str (A_Name_Buffer (1 .. A_Name_Len));
         Write_Eol;
         Write_Eol;
      end if;

      --  DO WE REALLY NEED A SPECIAL SUFFIX FOR THE NAMES OF NONEXISTENT
      --  UNITS ???

      New_Unit_Id :=  Allocate_New_Entry (C);

      Set_Nil_Unit_Names      (New_Unit_Id);
      Set_Nil_Unit_Attributes (C, New_Unit_Id);
      Set_Norm_Ada_Name       (New_Unit_Id);

      if A_Name_Buffer (A_Name_Len - 1) = 'b' then
         Set_Kind (C, New_Unit_Id, A_Nonexistent_Body);
      else
         Set_Kind (C, New_Unit_Id, A_Nonexistent_Declaration);
      end if;

      Make_Unit_Name;
      Set_Ada_Name (New_Unit_Id);

      return New_Unit_Id;

   end Allocate_Nonexistent_Unit_Entry;

   -------------------------
   -- Allocate_Unit_Entry --
   -------------------------

   function Allocate_Unit_Entry (C : Context_Id) return Unit_Id is
      New_Unit_Id : Unit_Id;
   begin
      New_Unit_Id :=  Allocate_New_Entry (C);

      Set_Nil_Unit_Names      (New_Unit_Id);
      Set_Nil_Unit_Attributes (C, New_Unit_Id);

      Set_Norm_Ada_Name       (New_Unit_Id);

      if A_Name_Buffer (A_Name_Len) = 's' then
         Contexts.Table (C).Specs :=  Contexts.Table (C).Specs + 1;
      elsif A_Name_Buffer (A_Name_Len) = 'b' then
         Contexts.Table (C).Bodies :=  Contexts.Table (C).Bodies + 1;
      end if;

      if Debug_Mode then
         Write_Str ("Allocate_Unit_Entry: in context ");
         Write_Int (Int (C));
         Write_Str (" unit ");
         Write_Int (Int (New_Unit_Id));
         Write_Str (" is allocated...");
         Write_Eol;
      end if;

      return New_Unit_Id;

   end Allocate_Unit_Entry;

   -----------------------
   -- Already_Processed --
   -----------------------

   function Already_Processed (C : Context_Id; U : Unit_Id) return Boolean is
   begin
      return Kind (C, U) /= Not_A_Unit;
   end Already_Processed;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency
     (C     : Context_Id;
      U_Id  : Unit_Id;
      U_Num : Unit_Number_Type)
   is
      Old_Stamp     : Time_Stamp_Type;
      New_Stamp     : Time_Stamp_Type;
      C_Tree_Mode   : constant Tree_Mode := Tree_Processing_Mode (C);
      Tmp           : Elmt_Id;
      Unit_Is_Older : Boolean;
   begin
      Old_Stamp := Time_Stamp (C, U_Id);
      New_Stamp := Sinput.Time_Stamp (Lib.Source_Index (U_Num));

      if not (Old_Stamp = New_Stamp) then
         --  note, this is "=" explicitly defied in Types

         Unit_Is_Older := New_Stamp > Old_Stamp;

         if C_Tree_Mode = Incremental then
            raise Inconsistent_Incremental_Context;
         else
            --  There is a special case that requires a specific diagnostic
            --  message - (re)compilation of another version of System
            --  (See D617-017)

            Get_Name_String (U_Id, Norm_Ada_Name);

            if A_Name_Buffer (1 .. A_Name_Len) = "system%s" then
               Raise_ASIS_Failed
                 (Diagnosis => "Asis.Ada_Environments.Open - " &
                               "System is recompiled",
                  Stat      => Use_Error);
            else
               --  Generate the full details about detected inconsistency.
               Set_Standard_Error;

               Write_Str ("Different versions of unit ");
               Get_Name_String (U_Id, Ada_Name);
               Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
               Write_Eol;
               Write_Str ("(source file ");
               Get_Name_String (U_Id, Ref_File_Name);
               Write_Str (A_Name_Buffer (1 ..  A_Name_Len) & ")");
               Write_Eol;
               Write_Str ("used to create the following tree files:");
               Write_Eol;

               if Unit_Is_Older then
                  Write_Str ("Older version used for:");
                  Write_Eol;
               else
                  Write_Str ("Newer version used for:");
                  Write_Eol;
               end if;

               Write_Str ("Full view trees:");

               Tmp := First_Elmt (Unit_Table.Table (U_Id).Full_View_Trees);

               if Present (Tmp) then
                  Write_Eol;

                  while Present (Tmp) loop
                     A4G.Contt.TT.Get_Name_String (C, Tree_Id (Unit (Tmp)));
                     Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
                     Write_Eol;
                     Tmp := Next_Elmt (Tmp);
                  end loop;
               else
                  Write_Str (" no");
                  Write_Eol;
               end if;

               Write_Str ("Limited view trees:");

               Tmp := First_Elmt (Unit_Table.Table (U_Id).Limited_View_Trees);

               if Present (Tmp) then
                  Write_Eol;

                  while Present (Tmp) loop
                     A4G.Contt.TT.Get_Name_String (C, Tree_Id (Unit (Tmp)));
                     Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
                     Write_Eol;
                     Tmp := Next_Elmt (Tmp);
                  end loop;
               else
                  Write_Str (" no");
                  Write_Eol;
               end if;

               if Unit_Is_Older then
                  Write_Str ("Newer version used for:");
                  Write_Eol;
               else
                  Write_Str ("Older version used for:");
                  Write_Eol;
               end if;

               A4G.Contt.TT.Get_Name_String (C, Current_Tree);
               Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
               Write_Eol;

               Set_Standard_Output;

               Raise_ASIS_Failed
                 (Diagnosis => "Asis.Ada_Environments.Open - " &
                               "a set of tree files is inconsistent",
                  Stat      => Use_Error);
            end if;
         end if;

      end if;

   end Check_Consistency;

   ------------------------------
   -- Check_Source_Consistency --
   ------------------------------

   procedure Check_Source_Consistency
     (C : Context_Id;
      U_Id : Unit_Id)
   is
      Tree_Stamp     : Time_Stamp_Type;
      Source_Stamp   : Time_Stamp_Type;
      C_Source_Mode  : constant Source_Mode := Source_Processing_Mode (C);
      C_Tree_Mode    : constant Tree_Mode   := Tree_Processing_Mode (C);
      Source_Status  : Source_File_Statuses := No_File_Status;
   begin
      Get_Name_String (U_Id, Source_File_Name);
      A_Name_Len := A_Name_Len + 1;
      A_Name_Buffer (A_Name_Len) := ASCII.NUL;

      if Is_Regular_File (A_Name_Buffer) then
         Source_Stamp := TS_From_OS_Time (File_Time_Stamp (A_Name_Buffer));
         Tree_Stamp   := Time_Stamp (C, U_Id);

         if Source_Stamp > Tree_Stamp then
            Source_Status := Newer;
         elsif Source_Stamp < Tree_Stamp then
            Source_Status := Older;
         else
            Source_Status := Up_To_Date;
         end if;

      else
         Source_Status := Absent;
      end if;

      Set_Source_Status (C, U_Id, Source_Status);

      if C_Source_Mode = All_Sources and then
         Source_Status = Absent
      then

         if C_Tree_Mode = Incremental then
            raise Inconsistent_Incremental_Context;
         else
            Set_Error_Status
              (Status    => Asis.Errors.Use_Error,
               Diagnosis =>  "Asis.Ada_Environments.Open - source file "
                           &  A_Name_Buffer (1 .. A_Name_Len - 1)
                           & " does not exist");
            raise ASIS_Failed;
         end if;

      end if;

      if (C_Source_Mode = All_Sources or else C_Source_Mode = Existing_Sources)
        and then
         (Source_Status = Newer or else Source_Status = Older)
      then

         if C_Tree_Mode = Incremental then
            raise Inconsistent_Incremental_Context;
         else
            Set_Error_Status
              (Status    => Asis.Errors.Use_Error,
               Diagnosis =>  "Asis.Ada_Environments.Open - source file "
                           &  A_Name_Buffer (1 .. A_Name_Len - 1)
                           & " is inconsistent with a tree file "
                           & Get_Tree_Name (C, Current_Tree));
            raise ASIS_Failed;
         end if;

      end if;

   end Check_Source_Consistency;

   --------------------
   -- Enclosing_Unit --
   --------------------

   function Enclosing_Unit
     (Cont : Context_Id;
      Node : Node_Id)
      return Asis.Compilation_Unit
   is
      Current_Node   : Node_Id := Node;
      Result_Unit_Id : Unit_Id := Nil_Unit;
      Success        : Boolean := False;
   begin
      --  First, correct Current_Node in case if itreresents the defining
      --  operator of implicitly declared "/=" (as a consequence of explicit
      --  "=" definition

      if Nkind (Current_Node) = N_Defining_Operator_Symbol
        and then
          not Comes_From_Source (Current_Node)
        and then
         Chars (Current_Node) = Name_Op_Ne
        and then
         Present (Corresponding_Equality (Current_Node))
      then
         Current_Node := Corresponding_Equality (Current_Node);
      end if;

      --  Then, checking if we are or are not in the package Standard:
      if Sloc (Node) <= Standard_Location then
         Result_Unit_Id := Standard_Id;
      else
         --  we are not in the package Standard here, therefore we have to
         --  find the top node of the enclosing subtree:
         while not (Nkind (Current_Node) = N_Compilation_Unit) loop
            pragma Assert (Present (Parent (Current_Node)));
            Current_Node := Parent (Current_Node);
         end loop;

         if Is_Rewrite_Substitution (Unit (Current_Node))
           and then
            Is_Rewrite_Substitution (Original_Node (Unit (Current_Node)))
           and then
            Nkind (Original_Node (Unit (Current_Node))) = N_Package_Body
         then
            --  This corresponds to the situation when a library-level
            --  instantiation is a supporter of a main unit, and the expanded
            --  body of this instantiation is required according to Lib (h).
            --  (See 7523-A19, 7624-A06 9418-015 and 9416-A01). In this case we
            --  Have to go to the compilation unit created for the
            --  instantiation

            Current_Node := Library_Unit (Current_Node);
         end if;

         --  now - getting the normalized unit name
         Namet.Get_Decoded_Name_String (Lib.Unit_Name (
                      Lib.Get_Cunit_Unit_Number (Current_Node)));

         Set_Norm_Ada_Name_String_With_Check
           (Lib.Get_Cunit_Unit_Number (Current_Node), Success);

         if not Success then
            --  This means, that we most probably are in the unit created for
            --  expanded package spec in case of library-level package
            --  instantiation, ASIS skips such units and processes only
            --  units rooted by expanded bodies, so let's try this

            Current_Node := Unit (Current_Node);

            pragma Assert
              (Nkind (Current_Node) = N_Package_Declaration and then
               not Comes_From_Source (Current_Node));

            Current_Node := Corresponding_Body  (Current_Node);

            if Nkind (Parent (Current_Node)) =
              N_Defining_Program_Unit_Name
            then
               Current_Node := Parent (Current_Node);
            end if;

            Current_Node := Parent (Parent (Current_Node));

            Set_Norm_Ada_Name_String_With_Check
              (Lib.Get_Cunit_Unit_Number (Current_Node), Success);

         end if;

         if Success then
            Result_Unit_Id := Name_Find (Cont);
         end if;

      end if;

      if No (Result_Unit_Id) then
         raise Internal_Implementation_Error;
      else
         return Get_Comp_Unit (Result_Unit_Id, Cont);
      end if;
   end Enclosing_Unit;

   ----------------------
   -- Form_Parent_Name --
   ----------------------

   procedure Form_Parent_Name is
      New_Len : Integer := 0;
   begin
      for I in reverse 1 .. A_Name_Len loop
         if A_Name_Buffer (I) = '.' then
            New_Len := I;
            exit;
         end if;
      end loop;

      A_Name_Len := New_Len;
      if A_Name_Len = 0 then
         return;
      end if;

      A_Name_Buffer (A_Name_Len) := '%';
      A_Name_Len := A_Name_Len + 1;
      A_Name_Buffer (A_Name_Len) := 's';
   end Form_Parent_Name;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (Id : Unit_Id; Col : Column) is
      S : Int;
      L : Short;

   begin

      case Col is
         when Ada_Name =>
            S := Unit_Table.Table (Id).Ada_Name_Chars_Index;
            L := Unit_Table.Table (Id).Ada_Name_Len;
         when Norm_Ada_Name =>
            S := Unit_Table.Table (Id).Norm_Ada_Name_Chars_Index;
            L := Unit_Table.Table (Id).Norm_Ada_Name_Len;
         when Source_File_Name =>
            S := Unit_Table.Table (Id).File_Name_Chars_Index;
            L := Unit_Table.Table (Id).File_Name_Len;
         when Ref_File_Name =>
            S := Unit_Table.Table (Id).Ref_Name_Chars_Index;
            L := Unit_Table.Table (Id).Ref_Name_Len;
      end case;

      A_Name_Len := Natural (L);

      for I in 1 .. A_Name_Len loop
         A_Name_Buffer (I) := A_Name_Chars.Table (S + Int (I));
      end loop;
   end Get_Name_String;

   -----------------
   -- Get_Subunit --
   -----------------

   function Get_Subunit
     (Parent_Body : Asis.Compilation_Unit;
      Stub_Node   : Node_Id)
      return Asis.Compilation_Unit
   is
      Def_S_Name     : Node_Id;
      Arg_Unit_Id    : constant Unit_Id := Get_Unit_Id (Parent_Body);
      Result_Unit_Id : Unit_Id;
      Result_Cont_Id : constant Context_Id := Encl_Cont_Id (Parent_Body);
   begin
      Get_Name_String (Arg_Unit_Id, Norm_Ada_Name);

      if Nkind (Stub_Node) = N_Subprogram_Body_Stub then
         Def_S_Name := Defining_Unit_Name (Specification (Stub_Node));
      else
         Def_S_Name := Defining_Identifier (Stub_Node);
      end if;
      Append_Subunit_Name (Def_S_Name);

      --  Now we have a name of a subunit in A_Name_Buffer. Let's try
      --  to find this subunit out:
      Result_Unit_Id := Name_Find (Result_Cont_Id);

         return Get_Comp_Unit (Result_Unit_Id, Result_Cont_Id);
   end Get_Subunit;

   ----------------------
   -- Get_Unit_Id_List --
   ----------------------

   function Get_Unit_Id_List (List : Elist_Id) return Unit_Id_List is
      Res_Len      : Natural;
      Next_Element : Elmt_Id;
   begin

      if No (List) then
         return Nil_Unit_Id_List;
      end if;

      Res_Len := List_Length (List);

      declare
         Result : Unit_Id_List (1 .. Res_Len);
      begin
         Next_Element := First_Elmt (List);

         for I in 1 .. Res_Len loop
            Result (I) := Unit (Next_Element);

            Next_Element  := Next_Elmt (Next_Element);
         end loop;

         return Result;
      end;
   end Get_Unit_Id_List;

   -----------------------------------
   -- GNAT_Compilation_Dependencies --
   -----------------------------------

   function GNAT_Compilation_Dependencies (U : Unit_Id) return Unit_Id_List is
   begin
      return Get_Unit_Id_List (Unit_Table.Table (U).Compilation_Dependencies);
   end GNAT_Compilation_Dependencies;

   -------------
   -- Is_Spec --
   -------------

   function Is_Spec (U : Unit_Id) return Boolean is
   begin
      Get_Name_String (U, Norm_Ada_Name);

      --  The second condition is needed to filter out
      --  A_Configuration_Compiation unit having the name
      --  "__configuration_compilation%s"
      return A_Name_Buffer (A_Name_Len) = 's'
        and then A_Name_Buffer (1) /= '_';
   end Is_Spec;

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Unit_Id; Col : Column) return Nat is
      L : Short;
   begin

      case Col is
         when Ada_Name =>
            L := Unit_Table.Table (Id).Ada_Name_Len;
         when Norm_Ada_Name =>
            L := Unit_Table.Table (Id).Norm_Ada_Name_Len;
         when Source_File_Name =>
            L := Unit_Table.Table (Id).File_Name_Len;
         when Ref_File_Name =>
            L := Unit_Table.Table (Id).Ref_Name_Len;
      end case;

      return Nat (L);

   end Length_Of_Name;

   --------------------
   -- Make_Unit_Name --
   --------------------

   procedure Make_Unit_Name is
   begin
      --  getting rid of the suffix:
      A_Name_Len := A_Name_Len - 3;
      A_Name_Buffer (1) := Ada.Characters.Handling.To_Upper
                             (A_Name_Buffer (1));
      --  "normalizing" the name:
      for I in 1 .. A_Name_Len - 1 loop
         if A_Name_Buffer (I) = '.' or else
            A_Name_Buffer (I) = '_'
         then
            A_Name_Buffer (I + 1) :=
               Ada.Characters.Handling.To_Upper (A_Name_Buffer (I + 1));
         end if;
      end loop;
   end Make_Unit_Name;

   ---------------
   -- Name_Find --
   ---------------

   --  The code has been borrowed from the GNAT Namet package. The quick
   --  search for one character names was removed and allocating of a new
   --  entry in case when no name has been found is changed to returning
   --  Nil_Unit

   function Name_Find (C : Context_Id) return Unit_Id is
      New_Id : Unit_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      Hash_Index := Hash;
      New_Id := Contexts.Table (C).Hash_Table (Hash_Index);

      if New_Id = No_Unit_Id then
         return Nil_Unit;

      else
         Search : loop
            if A_Name_Len /=
               Integer (Unit_Table.Table (New_Id).Norm_Ada_Name_Len)
            then
               goto No_Match;
            end if;

            S := Unit_Table.Table (New_Id).Norm_Ada_Name_Chars_Index;

            for I in 1 .. A_Name_Len loop
               if A_Name_Chars.Table (S + Int (I)) /=
                  A_Name_Buffer (I) then
                  goto No_Match;
               end if;
            end loop;

            return New_Id;

            --  Current entry in hash chain does not match

            <<No_Match>>
               if Unit_Table.Table (New_Id).Hash_Link /=
                  No_Unit_Id
               then
                  New_Id := Unit_Table.Table (New_Id).Hash_Link;
               else
                  exit Search;
               end if;

         end loop Search;
      end if;

      --  We fall through here only if a matching entry was not found in the
      --  hash table.
      --  In the GNAT Name Table a new entry in the names table is created,
      --  but we simply return Nil_Unit. Remember, we will have to
      --  maintain the consistency of hash links when we will allocate
      --  the new entry for the newly successfully compiled ASIS Compilation
      --  Unit.

      return Nil_Unit;

   end Name_Find;

   -----------------
   -- Reset_Cache --
   -----------------

   procedure Reset_Cache is
   begin

      for U in First_Unit_Id .. Top_Node_Cache.Last loop
         Top_Node_Cache.Table (U).Tree := Nil_Tree;
      end loop;

   end Reset_Cache;

   ------------------
   -- Set_Ada_Name --
   ------------------

   procedure Set_Ada_Name (Id : Unit_Id) is
   begin
      --  Set the values of Ada_Name_Chars_Index and Ada_Name_Len

      Unit_Table.Table (Id).Ada_Name_Chars_Index :=
            A_Name_Chars.Last;

      Unit_Table.Table (Id).Ada_Name_Len := Short (A_Name_Len);

      --  Set corresponding string entry in the Name_Chars table

      for I in 1 .. A_Name_Len loop
         A_Name_Chars.Increment_Last;

         A_Name_Chars.
            Table (A_Name_Chars.Last) := A_Name_Buffer (I);
      end loop;

      A_Name_Chars.Increment_Last;
      A_Name_Chars.Table (A_Name_Chars.Last) :=
            ASCII.NUL;

   end Set_Ada_Name;

   ------------------------
   -- Set_Nil_Unit_Names --
   ------------------------

   procedure Set_Nil_Unit_Names (U : Unit_Id) is
      Unit : constant Unit_Id := U;
   begin

      Unit_Table.Table (Unit).Ada_Name_Chars_Index      := 0;
      Unit_Table.Table (Unit).Norm_Ada_Name_Chars_Index := 0;
      Unit_Table.Table (Unit).File_Name_Chars_Index     := 0;
      Unit_Table.Table (Unit).Ada_Name_Len              := 0;
      Unit_Table.Table (Unit).Norm_Ada_Name_Len         := 0;
      Unit_Table.Table (Unit).File_Name_Len             := 0;
      Unit_Table.Table (Unit).Ref_Name_Len              := 0;

      Unit_Table.Table (Unit).Hash_Link                 := No_Unit_Id;
   end Set_Nil_Unit_Names;

   -----------------------
   -- Set_Norm_Ada_Name --
   -----------------------

   procedure Set_Norm_Ada_Name (Id : Unit_Id) is
   begin
      --  Set the values of Norm_Ada_Name_Chars_Index and Norm_Ada_Name_Len

      Unit_Table.Table (Id).Norm_Ada_Name_Chars_Index :=
         A_Name_Chars.Last;
      Unit_Table.Table (Id).Norm_Ada_Name_Len := Short (A_Name_Len);

      --  Set corresponding string entry in the Name_Chars table

      for I in 1 .. A_Name_Len loop
         A_Name_Chars.Increment_Last;
         A_Name_Chars.
            Table (A_Name_Chars.Last) := A_Name_Buffer (I);
      end loop;

      A_Name_Chars.Increment_Last;
      A_Name_Chars.
         Table (A_Name_Chars.Last) := ASCII.NUL;

   end Set_Norm_Ada_Name;

   ------------------------------
   -- Set_Norm_Ada_Name_String --
   ------------------------------

   procedure Set_Norm_Ada_Name_String is
   begin
      A_Name_Len := Namet.Name_Len;

      A_Name_Buffer (1 .. A_Name_Len) :=
         Namet.Name_Buffer (1 .. Namet.Name_Len);

      --  ???  The commented code caused problems for 7717-010
      --  ???  We will keep it for a while in case of possible
      --  ???  regressions (18.05.2000)
      --  A_Name_Buffer (1 .. A_Name_Len) := Ada.Characters.Handling.To_Lower
      --          (Namet.Name_Buffer (1 .. Namet.Name_Len));

   end Set_Norm_Ada_Name_String;

   -----------------------------------------
   -- Set_Norm_Ada_Name_String_With_Check --
   -----------------------------------------

   procedure Set_Norm_Ada_Name_String_With_Check
     (Unit    :     Unit_Number_Type;
      Success : out Boolean)
   is
      Unit_Node      : Node_Id;
      Unit_Node_Kind : Node_Kind;
   begin

      Set_Norm_Ada_Name_String;
      Success := True;

      Unit_Node      := Sinfo.Unit (Lib.Cunit (Unit));
      Unit_Node_Kind := Nkind (Unit_Node);

      if (Unit_Node_Kind = N_Package_Body or else
          Unit_Node_Kind = N_Package_Declaration)
        and then
          Nkind (Original_Node (Unit_Node)) in N_Generic_Instantiation
      then
         --  Unit created for library-level package or procedure instantiation
         --  It is a spec, but the compiler sets for it in the unit
         --  table suffix '%b'
         A_Name_Buffer (A_Name_Len) := 's';

      elsif not Comes_From_Source (Unit_Node) then
         --  Unit created for expanded package spec in case of
         --  library-level package instantiation, we do not need it
         Success := False;
      end if;

   end Set_Norm_Ada_Name_String_With_Check;

   ------------------------
   -- Set_No_Source_File --
   ------------------------

   procedure Set_No_Source_File (U : Unit_Id) is
   begin
      Unit_Table.Table (U).File_Name_Len := 0;
      Unit_Table.Table (U).Ref_Name_Len  := 0;
   end Set_No_Source_File;

   --------------------------
   -- Set_Source_File_Name --
   --------------------------

   procedure Set_Source_File_Name (Id  : Unit_Id; Ref : Boolean := False) is
   begin
      --  Set the values of File_Name_Chars_Index and File_Name_Len

      if Ref then
         Unit_Table.Table (Id).Ref_Name_Chars_Index :=
            A_Name_Chars.Last;
         Unit_Table.Table (Id).Ref_Name_Len := Short (A_Name_Len);
      else
         Unit_Table.Table (Id).File_Name_Chars_Index :=
            A_Name_Chars.Last;
         Unit_Table.Table (Id).File_Name_Len := Short (A_Name_Len);
      end if;

      --  Set corresponding string entry in the Name_Chars table

      for I in 1 .. A_Name_Len loop
         A_Name_Chars.Increment_Last;
         A_Name_Chars.
            Table (A_Name_Chars.Last) := A_Name_Buffer (I);
      end loop;

      A_Name_Chars.Increment_Last;
      A_Name_Chars.
         Table (A_Name_Chars.Last) := ASCII.NUL;

   end Set_Source_File_Name;

   ---------------------------------
   -- Set_Ref_File_As_Source_File --
   ---------------------------------

   procedure Set_Ref_File_As_Source_File (U : Unit_Id) is
   begin

      Unit_Table.Table (U).Ref_Name_Chars_Index :=
         Unit_Table.Table (U).File_Name_Chars_Index;

      Unit_Table.Table (U).Ref_Name_Len :=
         Unit_Table.Table (U).File_Name_Len;

   end Set_Ref_File_As_Source_File;

   ------------------------------
   -- Set_Ref_File_Name_String --
   ------------------------------

   procedure Set_Ref_File_Name_String (U : Unit_Id) is
      Last_Dir_Separator : Natural := 0;
   begin

      if not Absolute_Full_File_Name then

         Get_Name_String (U, Source_File_Name);

         for I in reverse 1 .. A_Name_Len loop

            if A_Name_Buffer (I) = Directory_Separator then
               Last_Dir_Separator := I;
               exit;
            end if;

         end loop;
      end if;

      if Last_Dir_Separator > 0 and then
         not (Last_Dir_Separator = 2 and then A_Name_Buffer (1) = '.')
      then
         A_Name_Len := Last_Dir_Separator;
      else
         A_Name_Len := 0;
      end if;

      A_Name_Buffer (A_Name_Len + 1 .. A_Name_Len + Namet.Name_Len) :=
         Namet.Name_Buffer (1 .. Namet.Name_Len);

      A_Name_Len := A_Name_Len +  Namet.Name_Len;

   end Set_Ref_File_Name_String;

   --------------
   -- Set_Unit --
   --------------

   function Set_Unit (C : Context_Id; U : Unit_Number_Type) return Unit_Id is
      New_Unit : Unit_Id;
   begin
      New_Unit := Allocate_Unit_Entry (C);
      Set_Time_Stamp (C, New_Unit, Sinput.Time_Stamp (Lib.Source_Index (U)));

      return New_Unit;
   end Set_Unit;

----------------------------------------------
--    Black-Box Unit Attributes Routines    --
----------------------------------------------

   -----------------------
   -- Local Subprograms --
   -----------------------

   ------------------------------------------------
   -- Unit Attributes Access and Update Routines --
   ------------------------------------------------

   function Top (U : Unit_Id) return Node_Id is
      Old_Last_Cache : Unit_Id;
   begin

      --  First, try to get the result from the cache
      if U <= Top_Node_Cache.Last
        and then
         Top_Node_Cache.Table (U).Tree = Get_Current_Tree
      then
         return Top_Node_Cache.Table (U).Top_Node;
      end if;

      --  we have to compute the top node of the unit on the base of the
      --  currently accessed tree. We are guaranteed here, that the currently
      --  accessed tree contains the subtree for a given Unit

      Get_Name_String (U, Norm_Ada_Name);
      --  and now we will compare it with the names of the units contained
      --  in the currently accessed tree

      for Current_Unit in Main_Unit .. Lib.Last_Unit loop
         Namet.Get_Decoded_Name_String (Lib.Unit_Name (Current_Unit));

         --  Here we have to take into account, that in case of library
         --  level package instantiations, in the tree created for such
         --  an instantiation the main unit (corresponding to this
         --  instantiation) has suffix '%b', whereas in ASIS the corresponding
         --  normalized unit name has suffix '%s'

         if Current_Unit = Main_Unit and then
            Nkind (Original_Node (Sinfo.Unit (Lib.Cunit (Current_Unit)))) in
            N_Generic_Instantiation
         then
            Namet.Name_Buffer (Namet.Name_Len) := 's';
         end if;

         if Same_Names then
            Old_Last_Cache := Top_Node_Cache.Last;

            if U > Old_Last_Cache then
               Top_Node_Cache.Set_Last (U);

               for J in Old_Last_Cache + 1 .. U - 1 loop
                  Top_Node_Cache.Table (J).Tree := Nil_Tree;
               end loop;

            end if;

            Top_Node_Cache.Table (U).Top_Node := Lib.Cunit (Current_Unit);
            Top_Node_Cache.Table (U).Tree     := Get_Current_Tree;

            return Lib.Cunit (Current_Unit);
         end if;
      end loop;

      --  we cannot be here! But if we are, the only cause may be some bug
      --  in ASIS implementation. So:

      raise Internal_Implementation_Error;
   end Top;

   function Kind (C : Context_Id; U : Unit_Id) return Unit_Kinds is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Kind;
   end Kind;

   function Class (C : Context_Id; U : Unit_Id) return Unit_Classes is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Class;
   end Class;

   function Origin (C : Context_Id; U : Unit_Id) return Unit_Origins is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Origin;
   end Origin;

   function Is_Main_Unit (C : Context_Id; U : Unit_Id) return Boolean is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Main_Unit;
   end Is_Main_Unit;

   function Is_Body_Required (C : Context_Id; U : Unit_Id) return Boolean is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Is_Body_Required;
   end Is_Body_Required;

   function Time_Stamp (C : Context_Id; U : Unit_Id) return Time_Stamp_Type is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Time_Stamp;
   end Time_Stamp;

   function Is_Consistent     (C : Context_Id; U : Unit_Id) return Boolean is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Is_Consistent;
   end Is_Consistent;

   function Source_Status (C : Context_Id; U : Unit_Id)
      return Source_File_Statuses
   is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Source_File_Status;
   end Source_Status;

   function Main_Tree (C : Context_Id; U : Unit_Id) return Tree_Id is
   begin
      Reset_Context (C);
      return Unit_Table.Table (U).Main_Tree;
   end Main_Tree;

   function Has_Limited_View_Only
     (C    : Context_Id;
      U    : Unit_Id)
      return Boolean
   is
   begin
      Reset_Context (C);
      return No (Unit_Table.Table (U).Full_View_Trees)
          or else
             No (First_Elmt (Unit_Table.Table (U).Full_View_Trees));
   end Has_Limited_View_Only;

   --------

   procedure Set_Top (C : Context_Id; U : Unit_Id; N : Node_Id) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Top := N;
   end Set_Top;

   procedure Set_Kind (C : Context_Id; U : Unit_Id; K : Unit_Kinds) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Kind := K;
   end Set_Kind;

   procedure Set_Class (C : Context_Id; U : Unit_Id; Cl : Unit_Classes) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Class := Cl;
   end Set_Class;

   procedure Set_Origin (C : Context_Id; U : Unit_Id; O : Unit_Origins) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Origin := O;
   end Set_Origin;

   procedure Set_Is_Main_Unit (C : Context_Id; U : Unit_Id; M : Boolean) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Main_Unit := M;
   end Set_Is_Main_Unit;

   procedure Set_Is_Body_Required (C : Context_Id; U : Unit_Id; B : Boolean) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Is_Body_Required := B;
   end Set_Is_Body_Required;

   procedure Set_Time_Stamp (C : Context_Id; U : Unit_Id; T : Time_Stamp_Type)
   is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Time_Stamp := T;
   end Set_Time_Stamp;

   procedure Set_Is_Consistent (C : Context_Id; U : Unit_Id; B : Boolean) is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Is_Consistent := B;
   end Set_Is_Consistent;

   procedure Set_Source_Status
     (C : Context_Id;
      U : Unit_Id;
      S : Source_File_Statuses)
   is
   begin
      Reset_Context (C);
      Unit_Table.Table (U).Source_File_Status := S;
   end Set_Source_Status;

   ----------------
   -- Same_Names --
   ----------------

   function Same_Names return Boolean is
   begin
      if Contt.A_Name_Len /= Namet.Name_Len then
         return False;
      end if;

      --  a small optimization for comparing the Unit names:
      --  we start from comparing the spec/body sign :-)
      if Contt.A_Name_Buffer (A_Name_Len) /=
         Namet.Name_Buffer (A_Name_Len)
      then
         return False;
      end if;

      for I in 1 .. Contt.A_Name_Len - 1 loop
         if Contt.A_Name_Buffer (I) /= Namet.Name_Buffer (I) then
            return False;
         end if;
      end loop;

      return True;

   end Same_Names;

   -----------------------------
   -- Set_Nil_Unit_Attributes --
   -----------------------------

   procedure Set_Nil_Unit_Attributes (C : Context_Id; U : Unit_Id) is
   begin
      Set_Top               (C, U, Empty);
      Set_Kind              (C, U, Not_A_Unit);
      Set_Class             (C, U, Not_A_Class);
      Set_Origin            (C, U, Not_An_Origin);
      Set_Is_Main_Unit      (C, U, False);
      Set_Is_Body_Required  (C, U, False);
      Set_No_Source_File    (U);
      Set_Time_Stamp        (C, U, (others => '0'));
      Set_Is_Consistent     (C, U, True);
      Set_Source_Status     (C, U, No_File_Status);

      --  setting the empty dependencies lists:
      Unit_Table.Table (U).Ancestors                := New_Elmt_List;
      Unit_Table.Table (U).Descendants              := New_Elmt_List;

      Unit_Table.Table (U).Direct_Supporters        := New_Elmt_List;
      Unit_Table.Table (U).Supporters               := New_Elmt_List;
      Unit_Table.Table (U).Implicit_Supporters      := New_Elmt_List;

      Unit_Table.Table (U).Direct_Dependents        := New_Elmt_List;
      Unit_Table.Table (U).Dependents               := New_Elmt_List;

      Unit_Table.Table (U).Subunits_Or_Childs       := New_Elmt_List;
      Unit_Table.Table (U).Subunits_Computed        := False;

      Unit_Table.Table (U).Compilation_Dependencies := New_Elmt_List;

      Unit_Table.Table (U).Full_View_Trees          := New_Elmt_List;
      Unit_Table.Table (U).Limited_View_Trees       := New_Elmt_List;
      Unit_Table.Table (U).Main_Tree                := Nil_Tree;

   end Set_Nil_Unit_Attributes;

   -------------------------
   -- Store_Tree_For_Unit --
   -------------------------

   procedure Store_Tree_For_Unit
     (C   : Context_Id;
      U   : Unit_Id;
      N_U : Unit_Number_Type)
   is
   begin
      if Analyzed (Lib.Cunit (N_U)) then
         Append_Full_View_Tree_To_Unit (C, U);
      else
         Append_Limited_View_Tree_To_Unit (C, U);
      end if;
   end Store_Tree_For_Unit;

   ---------------------
   -- TS_From_OS_Time --
   ---------------------

   function TS_From_OS_Time (T : OS_Time) return Time_Stamp_Type is
      Y   : Year_Type;
      Mon : Month_Type;
      D   : Day_Type;
      H   : Hour_Type;
      Min : Minute_Type;
      S   : Second_Type;
      Res : Time_Stamp_Type;
   begin
      GM_Split (T, Y, Mon, D, H, Min, S);
      Make_Time_Stamp
         (Nat (Y), Nat (Mon), Nat (D), Nat (H), Nat (Min), Nat (S), Res);
      return Res;
   end TS_From_OS_Time;

----------------------------------------------------------
--    Subprograms for Semantic Dependencies Handling    --
----------------------------------------------------------

   --------------
   -- Children --
   --------------

   function Children (U : Unit_Id) return Unit_Id_List is
   begin
      return Get_Unit_Id_List (Unit_Table.Table (U).Subunits_Or_Childs);
   end Children;

   --------------------------
   -- Get_Nonexistent_Unit --
   --------------------------

   function Get_Nonexistent_Unit (C : Context_Id) return Unit_Id is
      Result_Id : Unit_Id;
   begin
      --  A_Name_Buffer contains the normalized unit name ending with "%s"
      A_Name_Len := A_Name_Len + 1;
      A_Name_Buffer (A_Name_Len) := 'n';
      Result_Id := Name_Find (C);
      if No (Result_Id) then
         --  coming back to the correct initial situation for
         --  Allocate_Nonexistent_Unit_Entry:
         A_Name_Len := A_Name_Len - 1;
         Result_Id := Allocate_Nonexistent_Unit_Entry (C);
      end if;
      return Result_Id;
   end Get_Nonexistent_Unit;

   ---------------------
   -- Get_Parent_Unit --
   ---------------------

   function Get_Parent_Unit (C : Context_Id; U : Unit_Id) return Unit_Id is
   begin
      if U = Standard_Id then
         return Nil_Unit;
      end if;

      Get_Name_String (U, Norm_Ada_Name);
      Form_Parent_Name;

      if A_Name_Len = 0 then
         return Standard_Id;
      else
         return Name_Find (C);
      end if;
   end Get_Parent_Unit;

   --------------
   -- Get_Body --
   --------------

   function Get_Body (C : Context_Id; U : Unit_Id) return Unit_Id is
   begin
      Get_Name_String (U, Norm_Ada_Name);
      A_Name_Buffer (A_Name_Len) := 'b';
      return Name_Find (C);
   end Get_Body;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration (C : Context_Id; U : Unit_Id) return Unit_Id is
   begin
      Get_Name_String (U, Norm_Ada_Name);
      A_Name_Buffer (A_Name_Len) := 's';
      return Name_Find (C);
   end Get_Declaration;

   -------------------
   -- Get_Same_Unit --
   -------------------

   function Get_Same_Unit
     (Arg_C  : Context_Id;
      Arg_U  : Unit_Id;
      Targ_C : Context_Id)
   return Unit_Id
   is
      Result : Unit_Id;
   begin
      if Arg_C = Targ_C or else Arg_U = Nil_Unit then
         return Arg_U;
      end if;

      Reset_Context (Arg_C);
      Get_Name_String (Arg_U, Norm_Ada_Name);

      Reset_Context (Targ_C);
      Result := Name_Find (Targ_C);

      if Present (Result) and then
         Time_Stamp (Arg_C, Arg_U) = Time_Stamp (Targ_C, Result)
      then
         return Result;
      else
         return Nil_Unit;
      end if;
   end Get_Same_Unit;

   -----------------------------
   -- Get_Subunit_Parent_Body --
   -----------------------------

   function Get_Subunit_Parent_Body
     (C : Context_Id;
      U : Unit_Id)
      return Unit_Id
   is
   begin
      Get_Name_String (U, Norm_Ada_Name);
      Form_Parent_Name;
      A_Name_Buffer (A_Name_Len) := 'b';
      --  for subunits Form_Parent_Name cannot set A_Name_Len as 0, and it
      --  sets A_Name_Buffer (A_Name_Len) as 's'
      return Name_Find (C);
   end Get_Subunit_Parent_Body;

   --------------
   -- Not_Root --
   --------------

   function Not_Root return Boolean is
   begin

      for I in 1 .. A_Name_Len loop

         if A_Name_Buffer (I) = '.' then
            return True;
         end if;

      end loop;

      return False;
   end Not_Root;

   --------------
   -- Subunits --
   --------------

   function Subunits (C : Context_Id; U : Unit_Id) return Unit_Id_List is
   begin

      if not Unit_Table.Table (U).Subunits_Computed then

         if not Unit_In_Current_Tree (C, U) then
            Reset_Tree_For_Unit (C, U);
         end if;

         Set_Subunits (C, U, Top (U));
      end if;

      return Get_Unit_Id_List (Unit_Table.Table (U).Subunits_Or_Childs);
   end Subunits;

--------------------------------------------------
--    General-Purpose Unit Table Subprograms    --
--------------------------------------------------

   ----------------------
   -- Comp_Unit_Bodies --
   ----------------------

   function Comp_Unit_Bodies (C : Context_Id) return Natural is
   begin
      return Contexts.Table (C).Bodies;
   end Comp_Unit_Bodies;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : Context_Id) is
   begin

      if not Debug_Lib_Model then
         return;
      end if;

      for U in First_Unit_Id .. Last_Unit loop
         Output_Unit (C, U);
      end loop;

   end Finalize;

   ----------------
   -- First_Body --
   ----------------

   function First_Body return Unit_Id is
      Result : Unit_Id := Nil_Unit;
   begin
      --  Note that we start iterating after Config_Comp_Id not to count
      --  A_Configuration_Compilation unit as a body
      for U in Config_Comp_Id + 1 .. Last_Unit loop

         if not Is_Spec (U) then
            Result := U;
            exit;
         end if;

      end loop;

      return Result;
   end First_Body;

   ---------------
   -- Last_Unit --
   ---------------

   function Last_Unit return Unit_Id is
   begin
      return Unit_Table.Last;
   end Last_Unit;

   --------------------
   -- Lib_Unit_Decls --
   --------------------

   function Lib_Unit_Decls (C : Context_Id) return Natural is
   begin
      return Contexts.Table (C).Specs;
   end Lib_Unit_Decls;

   ---------------
   -- Next_Body --
   ---------------

   function Next_Body (B : Unit_Id) return Unit_Id is
      Result : Unit_Id := Nil_Unit;
   begin
      for U in B + 1 .. Last_Unit loop

         if not Is_Spec (U) then
            Result := U;
            exit;
         end if;

      end loop;

      return Result;
   end Next_Body;

   ---------------
   -- Next_Decl --
   ---------------

   function Next_Decl (D : Unit_Id) return Unit_Id is
      Result : Unit_Id := Nil_Unit;
   begin
      for U in D + 1 .. Last_Unit loop

         if Is_Spec (U) then
            Result := U;
            exit;
         end if;

      end loop;

      return Result;
   end Next_Decl;

   --------
   -- No --
   --------

   function No (Unit : Unit_Id) return Boolean is
   begin
      return Unit = Nil_Unit;
   end No;

   -------------
   -- Present --
   -------------

   function Present (Unit : Unit_Id) return Boolean is
   begin
      return Unit /= Nil_Unit;
   end Present;

   -----------------
   -- Output_Unit --
   -----------------

   procedure Output_Unit (C : Context_Id; Unit : Unit_Id) is
   begin
      Write_Str ("Debug output for Unit Id ");
      Write_Int (Int (Unit));
      Write_Eol;
      Write_Str ("----------------------------");
      Write_Eol;

      if Unit = Nil_Unit then
         Write_Str ("This is a Nil Unit");
         Write_Eol;
         return;
      end if;

      Write_Str ("Ada Unit Name:            ");
      Get_Name_String (Unit, Ada_Name);
      Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
      Write_Eol;

      Write_Str ("Normalized Ada Unit Name: ");
      Get_Name_String (Unit, Norm_Ada_Name);
      Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
      Write_Eol;

      Write_Str ("Source File Name:         ");
      Get_Name_String (Unit, Source_File_Name);
      if A_Name_Len = 0 then
         Write_Str ("no source file available");
      else
         Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
      end if;
      Write_Eol;

      Write_Str ("Reference File Name:      ");
      Get_Name_String (Unit, Ref_File_Name);
      if A_Name_Len = 0 then
         Write_Str ("no reference file available");
      else
         Write_Str (A_Name_Buffer (1 ..  A_Name_Len));
      end if;
      Write_Eol;

      Write_Str ("Unit Kind:                ");
      Write_Str (Unit_Kinds'Image (Kind (C, Unit)));
      Write_Eol;

      Write_Str ("Unit Class:               ");
      Write_Str (Unit_Classes'Image (Class (C, Unit)));
      Write_Eol;

      Write_Str ("Unit Origin:              ");
      Write_Str (Unit_Origins'Image (Origin (C, Unit)));
      Write_Eol;

      Write_Str ("Can be a main unit:       ");
      Write_Str (Boolean'Image (Is_Main_Unit (C, Unit)));
      Write_Eol;

      Write_Str ("Is body required:         ");
      Write_Str (Boolean'Image (Is_Body_Required (C, Unit)));
      Write_Eol;

      Write_Str ("Time stamp:               ");
      Write_Str (String (Time_Stamp (C, Unit)));
      Write_Eol;

      Write_Str ("Is consistent:            ");
      Write_Str (Boolean'Image (Is_Consistent (C, Unit)));
      Write_Eol;

      Write_Str ("Source file status:       ");
      Write_Str (Source_File_Statuses'Image (Source_Status (C, Unit)));
      Write_Eol;

      Write_Str ("Full view tree set:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Full_View_Trees);

      Write_Str ("Limited view tree set:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Limited_View_Trees);

      Write_Str ("Main_Tree: ");
      Write_Int (Int (Unit_Table.Table (Unit).Main_Tree));
      Write_Eol;

      Write_Str ("Dependencies:");
      Write_Eol;
      Write_Str ("=============");
      Write_Eol;

      Write_Str ("Ancestors:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Ancestors);

      Write_Str ("Descendents:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Descendants);

      Write_Str ("Direct_Supporters:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Direct_Supporters);

      Write_Str ("Supporters:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Supporters);

      Write_Str ("Implicit Supporters:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Implicit_Supporters);

      Write_Str ("Direct_Dependents:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Direct_Dependents);

      Write_Str ("Dependents:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Dependents);

      Write_Str ("Subunits_Or_Childs:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Subunits_Or_Childs);

      Write_Str ("Compilation_Dependencies:");
      Write_Eol;
      Print_List (Unit_Table.Table (Unit).Compilation_Dependencies);

      Write_Str ("==============================================");
      Write_Eol;
   end Output_Unit;

   -----------------
   -- Print_Units --
   -----------------
   procedure Print_Units (C : Context_Id) is
   begin
      Write_Str ("Unit Table for Context number: ");
      Write_Int (Int (C));
      Write_Eol;

      if C = Non_Associated then
         Write_Str ("   Nil Context, it can never contain any unit");
         Write_Eol;
         return;
      end if;

      if Is_Opened (C) then

         Write_Str ("The number of the unit entries being allocated is ");
         Write_Int (Int (Last_Unit - First_Unit_Id + 1));
         Write_Eol;
         Write_Str ("The number of existing specs is    ");
         Write_Int (Int (Contexts.Table (C).Specs));
         Write_Eol;
         Write_Str ("The number of existing bodies is   ");
         Write_Int (Int (Contexts.Table (C).Bodies));
         Write_Eol;
         Write_Str ("The number of nonexisting units is ");
         Write_Int (Int (Last_Unit - First_Unit_Id + 1) -
                    Int (Contexts.Table (C).Specs) -
                    Int (Contexts.Table (C).Bodies));
         Write_Eol;

         for U in First_Unit_Id .. Last_Unit loop
            Output_Unit (C, U);
         end loop;
         Write_Eol;
      else
         Write_Str ("This Context is closed");
         Write_Eol;
      end if;
   end Print_Units;

   --------------------
   -- Register_Units --
   --------------------

   procedure Register_Units (Set_First_New_Unit : Boolean := False) is
      Cont             : constant Context_Id := Get_Current_Cont;
      Current_Unit     : Unit_Id;
      Include_Unit     : Boolean    := False;
      Store_First_Unit : Boolean    := Set_First_New_Unit;
   begin

      First_New_Unit := Nil_Unit;

      for N_Unit in Main_Unit .. Lib.Last_Unit loop

         if Present (Lib.Cunit (N_Unit)) then

            Namet.Get_Decoded_Name_String (Lib.Unit_Name (N_Unit));
            Set_Norm_Ada_Name_String_With_Check (N_Unit, Include_Unit);

            if Include_Unit then
               Current_Unit := Name_Find (Cont);

               if No (Current_Unit) then
                  Current_Unit := Set_Unit (Cont, N_Unit);

                  if Store_First_Unit then
                     First_New_Unit := Last_Unit;
                     Store_First_Unit := False;
                  end if;

               end if;

               Store_Tree_For_Unit (Cont, Current_Unit, N_Unit);
            end if;

         end if;

      end loop;

   end Register_Units;

begin
   --  Initializing the top node cache
   Reset_Cache;
end A4G.Contt.UT;
