------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . C O N T T . S D                          --
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

with Ada.Exceptions;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Asis.Errors;               use Asis.Errors;
with Asis.Exceptions;           use Asis.Exceptions;

with A4G.A_Debug;               use A4G.A_Debug;
with A4G.GNAT_Int;
with A4G.A_Output;              use A4G.A_Output;
with A4G.Contt.TT;              use A4G.Contt.TT;
with A4G.Contt.UT;              use A4G.Contt.UT;
with A4G.CU_Info2;              use A4G.CU_Info2;
with A4G.Defaults;              use A4G.Defaults;
with A4G.Vcheck;                use A4G.Vcheck;

with Atree;
with Lib;
with Output;                    use Output;
with Sinfo;                     use Sinfo;

package body A4G.Contt.SD is

   ------------------------------------
   -- Local Subprograms  (new stuff) --
   ------------------------------------

   --  Do we need some of these local subprograms as the interface
   --  subprograms of this package?

   --  Is this package the right location for these subprograms?

   procedure Scan_Search_Path (C : Context_Id);
   --  Scans the tree search path and stores the names of the tree file
   --  candidates in the context tree table.

   procedure Scan_Tree_List (C : Context_Id);
   --  This procedure is supposed to be called for One_tree and N_Trees
   --  Context processing modes, therefore the Parameters string associated
   --  with C should contain at least one tree name. It scans the list of tree
   --  file names which have been extracted from the Parameters string when
   --  making the association for C. For each tree file name checks if the
   --  file exists and stores existing files in the context tree table. In case
   --  if this check fails, raises ASIS_Failed if C was defined as "-C1"
   --  ("one tree") context, or generates Asis Warning for "-CN" Context.
   --  This procedure does not reset a context.

   procedure Read_and_Check_New
     (C       : Context_Id;
      Tree    : Tree_Id;
      Success : out Boolean);
   --  Tries to read in Tree and to check if this tree is compile-only.
   --  if both of these attempts are successful, sets Success ON and
   --  sets Current_Tree as Tree. If either of these actions fails, then
   --  depending on the Context operation mode, either raises ASIS_Failed
   --  and forms the Diagnosis string on behalf on Asis.Ada_Environments.Open,
   --  or only sets Success OFF, in both cases Current_Context and Current_Tree
   --  are set to nil values.

   procedure Process_Unit_New (U : Unit_Number_Type);
   --  Does the general unit processing in one-pass Context opening. If this
   --  unit is "new", it creates the new entry in the unit table and checks,
   --  if the unit in the tree is consistent with the unit source (if needed).
   --  If U corresponds to a "known" unit, it makes the consistency check.
   --  If this procedure raises ASIS_Failed, it forms the Diagnosis string
   --  on behalf on Asis.Ada_Environments.Open
   --  ????????

   procedure Investigate_Unit_New
     (C   : Context_Id;
      U   : Unit_Id;
      U_N : Unit_Number_Type);
   --  Computes the basic unit attributes for U_N and stores them for the
   --  ASIS unit U in the ASIS Context C.

   procedure Store_Tree (Path : String);
   --  Stores the full name of the tree file in the Context Tree table for
   --  the current Context. It supposes, that when it is called,
   --  Namet.Name_Table contains the name of the tree file to be stored,
   --  but without any directory information, and Path contains the path to
   --  the tree search directory (followed by directory separator) where this
   --  file was found.

   ---------------------------
   -- Investigate_Trees_New --
   ---------------------------

   procedure Investigate_Trees_New (C : Context_Id) is
      Success : Boolean := False;
      --  flag indicating if the next tree file has been successfully read in
      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
   begin
      --  here we have all the names of tree files stored in the tree table
      --  for C
      for T in First_Tree_Id .. Last_Tree (C) loop
         Read_and_Check_New (C, T, Success);

         if Success then
            Get_Name_String (C, T);
            Change_Dir (Dir_Name (A_Name_Buffer (1 .. A_Name_Len)));

            Register_Units;
            Scan_Units_New;

            Change_Dir (Current_Dir);
         end if;

      end loop;
   end Investigate_Trees_New;

   --------------------------
   -- Investigate_Unit_New --
   --------------------------

   procedure Investigate_Unit_New
     (C   : Context_Id;
      U   : Unit_Id;
      U_N : Unit_Number_Type)
   is
      Top : constant Node_Id := Lib.Cunit (U_N);
      --  pointer to the N_Compilation_Unit node for U in the currently
      --  accessed tree
   begin

      Set_S_F_Name_and_Origin (C, U, Top);
      Check_Source_Consistency (C, U);

      Set_Kind_and_Class      (C, U, Top);
      Get_Ada_Name            (Top);
      Set_Ada_Name            (U);
      Set_Is_Main_Unit        (C, U, Is_Main (Top, Kind (C, U)));
      Set_Is_Body_Required    (C, U, Sinfo.Body_Required (Top));

      Set_Dependencies        (C, U, Top);

   end Investigate_Unit_New;

   ----------------------
   -- Process_Unit_New --
   ----------------------

   procedure Process_Unit_New (U : Unit_Number_Type) is
      Cont         : constant Context_Id := Get_Current_Cont;
      Include_Unit : Boolean             := False;
      Current_Unit : Unit_Id;
   begin
      Namet.Get_Decoded_Name_String (Lib.Unit_Name (U));
      Set_Norm_Ada_Name_String_With_Check (U, Include_Unit);

      if not Include_Unit then
         return;
      end if;

      Current_Unit := Name_Find (Cont);
      --  all the units in the current tree are already registered, therefore
      --  Current_Unit should not be Nil_Unit

      if Already_Processed (Cont, Current_Unit) then
         Check_Consistency   (Cont, Current_Unit, U);
         --  Append_Tree_To_Unit (Cont, Current_Unit);
      else
         Investigate_Unit_New (Cont, Current_Unit, U);
      end if;
   end Process_Unit_New;

   ------------------------
   -- Read_and_Check_New --
   ------------------------

   procedure Read_and_Check_New
     (C       : Context_Id;
      Tree    : Tree_Id;
      Success : out Boolean)
   is
      Tree_File_D : File_Descriptor;

   begin
      --  Special processing for GNSA mode:
      if Tree_Processing_Mode (C) = GNSA then

         if Context_Processing_Mode (C) = One_Tree then
            Set_Current_Cont (C);
            Set_Current_Tree (Tree);
            Success := True;
            return;
         else
            --  Other possibilites are not implemented now, so
            pragma Assert (False);
            null;
         end if;

      end if;

      Get_Name_String (C, Tree);
      A_Name_Buffer (A_Name_Len + 1) := ASCII.NUL;
      Tree_File_D := Open_Read (A_Name_Buffer'Address, Binary);

      A4G.GNAT_Int.Tree_In_With_Version_Check (Tree_File_D, C, Success);

      Set_Current_Cont (C);
      Set_Current_Tree (Tree);

   exception
      when Program_Error |
           ASIS_Failed   =>

         Set_Current_Cont (Nil_Context_Id);
         Set_Current_Tree (Nil_Tree);

         raise;
      when Ex : others =>

         --  If we are here, we are definitely having a serious problem:
         --  we have a tree file which is version-compartible with ASIS,
         --  and we can not read it because of some unknown reason.

         Set_Current_Cont (Nil_Context_Id);
         Set_Current_Tree (Nil_Tree);

         --  debug stuff...
         if Debug_Flag_O    or else
            Debug_Lib_Model or else
            Debug_Mode
         then
            Write_Str ("The tree file ");
            Write_Str (A_Name_Buffer (1 .. A_Name_Len));
            Write_Str (" was not read in and checked successfully");
            Write_Eol;
            Write_Str (Ada.Exceptions.Exception_Name (Ex));
            Write_Str (" was raised");
            Write_Eol;
            Write_Str ("Exception message: ");
            Write_Str (Ada.Exceptions.Exception_Message (Ex));
            Write_Eol;
         end if;

         Report_ASIS_Bug
           (Query_Name  => "A4G.Contt.SD.Read_and_Check_New" &
                           " (tree file " &
                           A_Name_Buffer (1 .. A_Name_Len) & ")",
            Ex          => Ex);
   end Read_and_Check_New;

   --------------------
   -- Scan_Tree_List --
   --------------------

   procedure Scan_Tree_List (C : Context_Id) is
      Cont_Mode   : constant Context_Mode := Context_Processing_Mode (C);
      Tree_List   : Tree_File_List_Ptr renames
        Contexts.Table (C).Context_Tree_Files;

      GNSA_Tree_Name : constant String := "GNSA-created tree";
      --  Can be used for -C1 COntext only.

--      Success : Boolean;
   begin
      --  Special processing for GNSA mode:

      if Tree_Processing_Mode (C) = GNSA then

         if Context_Processing_Mode (C) = One_Tree then
            Name_Len := GNSA_Tree_Name'Length;
            Name_Buffer (1 .. Name_Len) := GNSA_Tree_Name;
            Store_Tree ("");

            return;
         else
            --  Other possibilites are not implemented now, so
            pragma Assert (False);
            null;
         end if;

      end if;

      for I in Tree_List'Range loop

         exit when Tree_List (I) = null;

         if not Is_Regular_File (Tree_List (I).all) then

--            --  A loop needed to deal with possible raise conditions

--            Success := False;

--            for J in 1 .. 100 loop
--               if Is_Regular_File (Tree_List (I).all) then
--                  Success := True;
--                  exit;
--               end if;

--               delay 0.05;
--            end loop;

--            if not Success then

            if Cont_Mode  = One_Tree then
               Set_Error_Status
                 (Status    => Asis.Errors.Use_Error,
                  Diagnosis => "Asis.Ada_Environments.Open:"
                             &  ASIS_Line_Terminator
                             & "tree file "
                             & Tree_List (I).all
                             & " does not exist");
               raise ASIS_Failed;
            elsif Cont_Mode = N_Trees then
               ASIS_Warning
                  (Message => "Asis.Ada_Environments.Open: "
                             &  ASIS_Line_Terminator
                             & "tree file "
                             & Tree_List (I).all
                             & " does not exist",
                   Error   => Use_Error);
            end if;
--            end if;

         else
            Name_Len := Tree_List (I)'Length;
            Name_Buffer (1 .. Name_Len) := Tree_List (I).all;
            Store_Tree ("");
         end if;

      end loop;

   end Scan_Tree_List;

   ----------------------
   -- Scan_Search_Path --
   ----------------------

   procedure Scan_Search_Path (C : Context_Id) is
      Curr_Dir    : GNAT.Directory_Operations.Dir_Type;
      Search_Path : constant Directory_List_Ptr :=
        Contexts.Table (C).Tree_Path;

      procedure Scan_Dir (Path : String);
      --  scans tree files in Curr_Dir. Puts in the Name Table all
      --  the files having names of the form *.at?, which have not been
      --  scanned before. Sets the global variable Last_Tree_File equal to
      --  the Name_Id of the last scanned tree file. The names of the tree
      --  files stores in the Name Table are also stored in the ASIS tree
      --  table with the directory information passed as the actual for Path
      --  parameter

      procedure Read_Tree_File
        (Dir  : in out GNAT.Directory_Operations.Dir_Type;
         Str  : out String;
         Last : out Natural);
      --  This procedure is the modification of GNAT.Directory_Operations.Read
      --  which reads only tree file entries from the directory. A Tree file
      --  is any file having the extension '.[aA][dD][tT]' (We are
      --  considering upper case letters because of "semi-case-sensitiveness"
      --  of Windows 95/98/NT.)

      procedure Read_Tree_File
        (Dir  : in out GNAT.Directory_Operations.Dir_Type;
         Str  : out String;
         Last : out Natural)
      is
         function Is_Tree_File return Boolean;
         --  Checks if the file name stored in Str is the name of some tree
         --  file. This function assumes that Str'First is 1, and that
         --  Last > 0

         function Is_Tree_File return Boolean is
            Result : Boolean := False;
         begin

            if Last >= 5                     and then
               Str (Last - 3) = '.'          and then

               (Str (Last) = 't' or else
                Str (Last) = 'T')            and then

               (Str (Last - 1) = 'd' or else
                Str (Last - 1) = 'D')        and then

               (Str (Last - 2) = 'a' or else
                Str (Last - 2) = 'A')
            then
               Result := True;
            end if;

            return Result;

         end Is_Tree_File;

      begin
         GNAT.Directory_Operations.Read (Dir, Str, Last);
         while Last > 0 loop
            exit when Is_Tree_File;
            GNAT.Directory_Operations.Read (Dir, Str, Last);
         end loop;

      end Read_Tree_File;

      procedure Scan_Dir (Path : String) is
         T_File        : Name_Id;
         Is_First_Tree : Boolean := True;
      begin
         --  looking for the first tree file in this directory

         Read_Tree_File
           (Dir  => Curr_Dir,
            Str  => Namet.Name_Buffer,
            Last => Namet.Name_Len);

         while Namet.Name_Len > 0 loop
            T_File := Name_Find;

            if Is_First_Tree then
               Is_First_Tree := False;
               First_Tree_File := T_File;
            end if;

            if T_File > Last_Tree_File then
               Last_Tree_File := T_File;
               Store_Tree (Path);
            end if;

            Read_Tree_File
              (Dir  => Curr_Dir,
               Str  => Namet.Name_Buffer,
               Last => Namet.Name_Len);

         end loop;

      end Scan_Dir;

   begin  --  Scan_Search_Path
      if Search_Path = null then
         GNAT.Directory_Operations.Open (Curr_Dir, "." & Directory_Separator);
         Scan_Dir ("");
         GNAT.Directory_Operations.Close (Curr_Dir);
      else

         for I in 1 .. Search_Path'Last loop
            GNAT.Directory_Operations.Open (Curr_Dir, Search_Path (I).all);
            Scan_Dir (Search_Path (I).all);
            GNAT.Directory_Operations.Close (Curr_Dir);
         end loop;

      end if;

      if Use_Default_Trees (C) then

         for J in First_Dir_Id .. ASIS_Tree_Search_Directories.Last loop

            GNAT.Directory_Operations.Open
              (Curr_Dir,
               ASIS_Tree_Search_Directories.Table (J).all);

            Scan_Dir (ASIS_Tree_Search_Directories.Table (J).all);
            GNAT.Directory_Operations.Close (Curr_Dir);

         end loop;

      end if;

   end Scan_Search_Path;

   -------------------------
   -- Scan_Tree_Files_New --
   -------------------------

   procedure Scan_Tree_Files_New (C : Context_Id) is
      C_Mode : constant Context_Mode := Context_Processing_Mode (C);

      GNSA_Tree_Name : constant String := "GNSA-created tree";
      --  Can be used for -C1 Context only
   begin
      --  Special processing for GNSA mode:

      if Tree_Processing_Mode (C) = GNSA then

         if Context_Processing_Mode (C) = One_Tree then
            Name_Len := GNSA_Tree_Name'Length;
            Name_Buffer (1 .. Name_Len) := GNSA_Tree_Name;
            Store_Tree ("");

            return;
            --  to avoid GNAT Name Table corruption
         else
            --  Other possibilites are not implemented now, so
            pragma Assert (False);
            null;
         end if;

      end if;

      --  first, initialization which is (may be?) common for all context
      --  modes:
      First_Tree_File := First_Name_Id;
      Last_Tree_File  := First_Name_Id - 1;
      Namet.Initialize;

      --  now for different context modes we call individual scan procedures.
      --  all of them first put names of tree files into the GNAT Name table
      --  and then transfer them into Context tree table, but we cannot
      --  factor this out because of the differences in processing a search
      --  path (if any) and forming the full names of the tree files

      case C_Mode is
         when All_Trees =>
            Scan_Search_Path (C);
         when One_Tree | N_Trees =>
            Scan_Tree_List (C);
            --  all the tree file names have already been stored in the
            --  context tree table when association parameters were processed
            null;
         when Partition =>
            Not_Implemented_Yet ("Scan_Tree_Files_New (Partition)");
      end case;

      --  debug output:...
      if Debug_Flag_O    or else
         Debug_Lib_Model or else
         Debug_Mode
      then
         Write_Str ("Scanning tree files for Context ");
         Write_Int (Int (C));
         Write_Eol;
         if Context_Processing_Mode (C) = All_Trees then

            if Last_Tree_File < First_Tree_File then
               Write_Str ("   no tree file has been found");
               Write_Eol;
            else
               Write_Str ("   the content of the Name Table is:");
               Write_Eol;

               for I in First_Tree_File .. Last_Tree_File loop
                  Get_Name_String (I);
                  Write_Str ("      ");
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end loop;

            end if;

         else
            Write_Str ("Trees already stored in the tree table:");
            Write_Eol;

            for Tr in First_Tree_Id .. Last_Tree (C) loop
               Get_Name_String (C, Tr);
               Write_Str ("   " & A_Name_Buffer (1 ..  A_Name_Len));
               Write_Eol;
            end loop;

         end if;
      end if;

   end Scan_Tree_Files_New;

   --------------------
   -- Scan_Units_New --
   --------------------

   procedure Scan_Units_New is
      Main_Unit_Id : Unit_Id;
      Next_Unit_Id : Unit_Id;
      Include_Unit : Boolean := False;
   begin

      for N_Unit in Main_Unit .. Lib.Last_Unit loop

         if Atree.Present (Lib.Cunit (N_Unit)) then
            Process_Unit_New (N_Unit);
         end if;

      end loop;

      --  And here we collect compilation dependencies for the main unit in
      --  the tree:

      Namet.Get_Decoded_Name_String (Lib.Unit_Name (Main_Unit));
      Set_Norm_Ada_Name_String_With_Check (Main_Unit, Include_Unit);

      if not Include_Unit then
         return;
      end if;

      Main_Unit_Id := Name_Find (Current_Context);

      for N_Unit in Main_Unit .. Lib.Last_Unit loop

         if Atree.Present (Lib.Cunit (N_Unit)) then

            Namet.Get_Decoded_Name_String (Lib.Unit_Name (N_Unit));
            Set_Norm_Ada_Name_String_With_Check (N_Unit, Include_Unit);

            if Include_Unit then
               Next_Unit_Id := Name_Find (Current_Context);

               Add_To_Elmt_List
                 (Unit => Next_Unit_Id,
                  List =>
                     Unit_Table.Table (Main_Unit_Id).Compilation_Dependencies);
            end if;

         end if;

      end loop;

      Unit_Table.Table (Main_Unit_Id).Main_Tree := Current_Tree;
      Set_Main_Unit_Id (Main_Unit_Id);

   end Scan_Units_New;

   ----------------
   -- Store_Tree --
   ----------------

   procedure Store_Tree (Path : String) is
      New_Tree : Tree_Id;
      --  we do not need it, but Allocate_Tree_Entry is a function...
      pragma Warnings (Off, New_Tree);
   begin
      if Path = "" then
         Set_Name_String (Normalize_Pathname (Name_Buffer (1 .. Name_Len)));
      else
         Set_Name_String
           (Normalize_Pathname
              (Path & Directory_Separator & Name_Buffer (1 .. Name_Len)));
      end if;

      New_Tree := Allocate_Tree_Entry;
   end Store_Tree;

end A4G.Contt.SD;
