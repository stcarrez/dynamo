------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . G E T _ U N I T                         --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Asis.Errors;
with Asis.Exceptions;           use Asis.Exceptions;

with Asis.Set_Get;              use  Asis.Set_Get;

with A4G.A_Debug;               use A4G.A_Debug;
with A4G.A_Opt;                 use A4G.A_Opt;
with A4G.U_Conv;                use A4G.U_Conv;
with A4G.Contt.Dp;              use A4G.Contt.Dp;
with A4G.Contt.SD;              use A4G.Contt.SD;
with A4G.Contt.TT;              use A4G.Contt.TT;
with A4G.Contt.UT;              use A4G.Contt.UT; use A4G.Contt;
with A4G.Defaults;              use A4G.Defaults;
with A4G.GNAT_Int;              use A4G.GNAT_Int;
with A4G.Vcheck;                use A4G.Vcheck;

with Output;                    use Output;

package body A4G.Get_Unit is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Convert_To_String (Wide_Name : Wide_String) return String;
   --  Takes the name (of a compilation unit) and converts it into String
   --  following the GNAT encoding rules as described in Namet.

   -----------------------
   -- Convert_To_String --
   -----------------------

   function Convert_To_String (Wide_Name : Wide_String) return String is
   begin

      if Is_String (Wide_Name) then
         return To_String (Wide_Name);
      else
         Not_Implemented_Yet ("Wide_Character in Unit name");
      end if;

   end Convert_To_String;

   ----------------------------
   -- Fetch_Unit_By_Ada_Name --
   ----------------------------

   function Fetch_Unit_By_Ada_Name
     (Name      : String;
      Norm_Name : String;
      Context   : Context_Id;
      Spec      : Boolean)
      return Unit_Id
   is
      Result_Unit_Id : Unit_Id;
      Result_Tree    : Tree_Id;

      File_Name : String_Access;
      --  We use File_Name to obtain the source file name according to
      --  the GNAT file name rules (including the krunching rules)

      Predefined : Boolean := False;
      Success    : Boolean := False;

      Source_File  : String_Access;
      --  source to compile, contains all the path information
      Tree_File_N  : String_Access;
      --  tree output file to be retrieved (name)
      Tree_File_D  : File_Descriptor;
      --  tree output file to be retrieved (file descriptor)

      Cont_Tree_Mode : constant Tree_Mode := Tree_Processing_Mode (Context);

      procedure Get_Source_File;
      --  This procedure creates the value of Source_File using the values
      --  of Name and Spec. The procedure first tries to get the file
      --  containing the body regardless of the value of the Spec parameter.
      --  The idea is to get for Spec => True the tree file containing both
      --  the spec and the body of the compilation unit. If we get the tree
      --  file for the spec only, then latter on we may get into
      --  the following problem - the attempt to get the body of the
      --  compilation unit will create the tree file for the body, and
      --  this tree file will have the same name. As a result, all the
      --  references into the tree for spec will become erroneous, and
      --  we will have the problem similar to what was reported as 7504-002.
      --
      --  ??? All this "compile-on-the-fly" Context mode becomes a real
      --  mess. May be, we have to get rid of it???

      procedure Get_Source_File is
      begin
         --  The code is really awfull, but probably it would be better to
         --  get rid of the "compila-on-the-fly" mode, then to polish
         --  the code.

         File_Name := Source_From_Unit_Name (Name, False);
         --  if needed, the name is krunched here (??? may be, we can already
         --  forget about systems with the limitations on the file name
         --  length???)
         --  Note also, that ASIS cannot work with files which names override
         --  the standard GNAT file name convention as a result of
         --  Source_File_Name pragma.

         Source_File := Locate_In_Search_Path
                          (C         => Context,
                           File_Name => To_String (File_Name),
                           Dir_Kind  => Source);

         if Source_File = null then
            Source_File := Locate_Default_File
                             (File_Name => File_Name,
                              Dir_Kind  => Source);
         end if;

         if Source_File /= null then
            return;
         elsif Spec then
            File_Name := Source_From_Unit_Name (Name, True);

            Source_File := Locate_In_Search_Path
                             (C         => Context,
                              File_Name => To_String (File_Name),
                              Dir_Kind  => Source);

            if Source_File = null then
               Source_File := Locate_Default_File
                                (File_Name => File_Name,
                                 Dir_Kind  => Source);
            end if;

         end if;

         if Source_File = null and then
            Is_Predefined_File_Name (File_Name)
            --  not GNAT, but ASIS Is_Predefined_File_Name function is called
            --  here!
         then
            --  if File_Name is the name of a predefined unit, we shall try
            --  the default source search path. See also
            --  A4G.Contt.Set_Predefined_Units
            File_Name := Source_From_Unit_Name (Name, False);
            Predefined  := True;
            Source_File := Locate_Default_File (File_Name, Source);

            if Source_File = null then
               File_Name := Source_From_Unit_Name (Name, True);
               Source_File := Locate_Default_File (File_Name, Source);
            end if;

         end if;

      end Get_Source_File;

   begin
      --  we start from looking for a unit in the Unit Name Table
      Set_Name_String (Norm_Name);
      Result_Unit_Id := Name_Find (Context);

      if Result_Unit_Id /= Nil_Unit then
         return Result_Unit_Id;
      elsif Cont_Tree_Mode = Pre_Created then
         if Norm_Name = "standard%s" then
            --  For Standard, we have to search twice - first for possible
            --  redefinition of Standard, then - for predefined Standard
            return Standard_Id;
         else
            return Nil_Unit;
         end if;
      end if;

      if Spec then
         --  The idea is to avoid non-needed recompilations in case if we
         --  already have the (tree for the) corresponding body, see F823-027.
         A_Name_Buffer (A_Name_Len) := 'b';
         Result_Unit_Id := Name_Find (Context);

         if Result_Unit_Id /= Nil_Unit then
            return Nil_Unit;
         end if;

      end if;

      --  We can be here only if Context was associated in On_The_Fly or Mixed
      --  tree processing mode mode, and we have failed to find the required
      --  unit among units which are already known to ASIS. Therefore, we
      --  have to (try to) create the tree by compiling the source:

      Get_Source_File;

      if Source_File = null then
         if Debug_Mode then
            Write_Str ("Fetch_Unit_By_Ada_Name: cannot locate a source file ");
            Write_Str ("for " & Name);
            Write_Eol;
         end if;

         return Nil_Unit;
      end if;

      --  And trying to compile - Source_File contains the reference
      --  to the existing source file which has been already successfully
      --  located in the Context:

      if Debug_Mode then
         Write_Str ("Fetch_Unit_By_Ada_Name: "
                  & "Trying to create a tree on the fly:");
         Write_Eol;
         Write_Str ("Source file is " & To_String (Source_File));
         Write_Eol;
      end if;

      Create_Tree (Source_File   => Source_File,
                   Context       => Context,
                   Is_Predefined => Predefined,
                   Success       => Success);

      if not Success then

         if Debug_Mode then
            Write_Str ("Failure...");
            Write_Eol;
         end if;

         return Nil_Unit;
      end if;

      if Debug_Mode then
         Write_Str ("Success...");
         Write_Eol;
      end if;

      --  here we have a new tree, successfully created just here. We will
      --  read it in, then we will investigate it just in the same way as
      --  during opening a Context and then we look into the unit table for
      --  the needed unit again

      Tree_File_N := Tree_From_Source_Name (File_Name);
      --  ??? IT WOULD BE BETTER TO USE STRIP_DIRECTORY (SOURCE_FILE)
      --  INSTEAD OF FILE_NAME HERE!!!!!

      Tree_File_D := Open_Read (Tree_File_N.all'Address, Binary);

      begin
         if Debug_Mode then
            Write_Str ("Fetch_Unit_By_Ada_Name: trying to read in a tree...");
            Write_Eol;
         end if;

         Tree_In_With_Version_Check (Tree_File_D, Context, Success);

         if Success then

            if Debug_Mode then
               Write_Str ("Fetch_Unit_By_Ada_Name: a tree is read in...");
               Write_Eol;
            end if;

         else
            --  This should never happen!

            Set_Error_Status
              (Status    => Asis.Errors.Use_Error,
               Diagnosis => "A4G.Get_Unit.Fetch_Unit_By_Ada_Name:"
               &  ASIS_Line_Terminator
               & "problems reading tree file "
               & To_String (Tree_File_N));

            raise ASIS_Failed;
         end if;

      exception
         when Program_Error |
              ASIS_Failed   =>

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
               Write_Str ("A4G.Get_Unit.Fetch_Unit_By_Ada_Name:");
               Write_Eol;
               Write_Str ("Cannot read the tree file ");
               Write_Str (To_String (Tree_File_N));
               Write_Str (" newly created for a source file ");
               Write_Str (To_String (Source_File));
               Write_Eol;
            end if;

            Report_ASIS_Bug
              (Query_Name  => "A4G.Get_Unit.Fetch_Unit_By_Ada_Name" &
                              " (tree file " & To_String (Tree_File_N) & ")",
               Ex          => Ex);
      end;

      Set_Name_String (To_String (Tree_File_N));
      Set_Current_Cont (Context);
      Result_Tree := Allocate_Tree_Entry;
      Set_Current_Tree (Result_Tree);

      --  here we have to investigate the newly created tree:

      if Tree_Processing_Mode (Context) = Incremental then

         begin
            NB_Save;
            Register_Units (Set_First_New_Unit => True);
            Scan_Units_New;

            Set_All_Dependencies (Use_First_New_Unit => True);

            Reorder_Trees (Context);
         exception

            when Inconsistent_Incremental_Context =>
               --  Erase the old Context information
               Increase_ASIS_OS_Time;
               Pre_Initialize (Context);
               A4G.Contt.Initialize (Context);

               --  We still have one good tree
               Result_Tree := Allocate_Tree_Entry;
               NB_Restore;
               Set_Current_Tree (Result_Tree);

               --  And we have to extract the information from this tree
               Register_Units;
               Scan_Units_New;
               Set_All_Dependencies;
         end;

      else
         --  For old "on the fly" and mixed mode no enhansment
         Register_Units;
         Scan_Units_New;
      end if;

      --  and now - the final attempt to get the needed unit. We have to reset
      --  the name buffer - it may be changed by Scan_Units_New:
      Set_Name_String (Norm_Name);
      Result_Unit_Id := Name_Find (Context);

      if Result_Unit_Id = Nil_Unit
        and then
         Norm_Name = "standard%s"
      then
         --  For Standard, we have to search twice - first for possible
         --  redefinition of Standard, then - for predefined Standard
         Result_Unit_Id := Standard_Id;
      end if;

      return Result_Unit_Id;

   end Fetch_Unit_By_Ada_Name;

   -----------------------------------
   -- Get_Main_Unit_Tree_On_The_Fly --
   -----------------------------------

   function Get_Main_Unit_Tree_On_The_Fly
     (Start_Unit : Unit_Id;
      Cont       : Context_Id;
      Spec       : Boolean)
      return       Unit_Id
   is
      Start_CU : constant Asis.Compilation_Unit :=
        Get_Comp_Unit (Start_Unit, Cont);

      Result_Unit_Id : Unit_Id;
      Result_Tree    : Tree_Id;

      File_Name : String_Access;
      --  We use File_Name to obtain the source file name according to
      --  the GNAT file name rules (including the krunching rules)

      Predefined : Boolean := False;
      Success    : Boolean := False;

      S_File_To_Compile : String_Access;
      --  source to compile, contains all the path information

      Tmp_S_File, Tmp_S_File1 : String_Access;

      Tree_File_N  : String_Access;
      --  tree output file to be retrieved (name)
      Tree_File_D  : File_Descriptor;
      --  tree output file to be retrieved (file descriptor)

      procedure Get_Source_File_To_Compile;
      --  This procedure creates the value of Source_File using the values
      --  of Start_Unit and Spec. If Spec is ON, the full source name of
      --  Start_Unit is used. If Spec is OFF, we try to create the source
      --  file name from the unit name by applying the standard GNAT file
      --  naming rules. This is the weak point of the implementation, because
      --  this does not wor for non-standard names

      procedure Get_Source_File_To_Compile is
      begin

         if Spec then
            S_File_To_Compile :=
               new String'(Source_File (Start_CU) & ASCII.NUL);
         else
            --  The first thing to do is to try to locate the file with the
            --  name computed on the base of the standard GNAT name rules

            File_Name := Source_From_Unit_Name (Unit_Name (Start_CU), False);

            S_File_To_Compile := Locate_In_Search_Path
                             (C         => Cont,
                              File_Name => To_String (File_Name),
                              Dir_Kind  => Source);

            if  S_File_To_Compile = null then
               --  Two possibilities:
               --  - Start_Unit is a predefined unit
               --  - Try to convert spec name into unit body name and to
               --    locate it

               Tmp_S_File := new String'(Source_File (Start_CU));

               Tmp_S_File (Tmp_S_File'Last) := 'b';

               Tmp_S_File1 := new String'(Base_Name (Tmp_S_File.all));

               if not Is_Predefined_File_Name (Tmp_S_File1) then

                  S_File_To_Compile := Locate_In_Search_Path
                                   (C         => Cont,
                                    File_Name => Tmp_S_File.all,
                                    Dir_Kind  => Source);
               else
                  Predefined        := True;
                  S_File_To_Compile :=
                     Locate_Default_File (Tmp_S_File, Source);
               end if;

            end if;

         end if;

      end Get_Source_File_To_Compile;

   begin

      Get_Source_File_To_Compile;

      if S_File_To_Compile = null then
         if Debug_Mode then
            Write_Str ("Get_Main_Unit_Tree_On_The_Fly: cannot create a main ");
            Write_Str ("tree for " & Unit_Name (Start_CU));
            Write_Eol;
         end if;

         return Nil_Unit;
      end if;

      --  Now we try to compile - Source_File contains the reference
      --  to the existing source file which has been already successfully
      --  located in the Context:

      if Debug_Mode then
         Write_Str ("Get_Main_Unit_Tree_On_The_Fly: "
                  & "Trying to create a main tree on the fly:");
         Write_Eol;
         Write_Str ("Source file is " & S_File_To_Compile.all);
         Write_Eol;
      end if;

      Create_Tree (Source_File   => S_File_To_Compile,
                   Context       => Cont,
                   Is_Predefined => Predefined,
                   Success       => Success);

      if not Success then

         if Debug_Mode then
            Write_Str ("Failure...");
            Write_Eol;
         end if;

         return Nil_Unit;
      end if;

      if Debug_Mode then
         Write_Str ("Success...");
         Write_Eol;
      end if;

      --  Now we have a new tree, successfully created just here. We have to
      --  add it to the incremental Context

      Tree_File_N := new String'(Base_Name (S_File_To_Compile.all));

      Tree_File_N (Tree_File_N'Last - 1) := 't';
      --  ??? What about non-standart file names????

      Tree_File_D := Open_Read (Tree_File_N.all'Address, Binary);

      begin
         if Debug_Mode then
            Write_Str ("Get_Main_Unit_Tree_On_The_Fly: read in a tree...");
            Write_Eol;
         end if;

         Tree_In_With_Version_Check (Tree_File_D, Cont, Success);

         if Success then

            if Debug_Mode then
               Write_Str ("Get_Main_Unit_Tree_On_The_Fly: done...");
               Write_Eol;
            end if;

         else
            --  This should never happen!

            Set_Error_Status
              (Status    => Asis.Errors.Use_Error,
               Diagnosis => "A4G.Get_Unit.Get_Main_Unit_Tree_On_The_Fly:"
               &  ASIS_Line_Terminator
               & "problems reading tree file "
               & To_String (Tree_File_N));

            raise ASIS_Failed;
         end if;

      exception
         when Program_Error |
              ASIS_Failed   =>

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
               Write_Str ("A4G.Get_Unit.Get_Main_Unit_Tree_On_The_Fly:");
               Write_Eol;
               Write_Str ("Cannot read the tree file ");
               Write_Str (To_String (Tree_File_N));
               Write_Str (" newly created for a source file ");
               Write_Str (S_File_To_Compile.all);
               Write_Eol;
            end if;

            Report_ASIS_Bug
              (Query_Name  => "A4G.Get_Unit.Get_Main_Unit_Tree_On_The_Fly" &
                              " (tree file " & To_String (Tree_File_N) & ")",
               Ex          => Ex);
      end;

      Set_Name_String (To_String (Tree_File_N));

      Set_Current_Cont (Cont);
      Result_Tree := Allocate_Tree_Entry;
      Set_Current_Tree (Result_Tree);

      --  here we have to investigate the newly created tree:

      begin
         NB_Save;
         Register_Units (Set_First_New_Unit => True);
         Scan_Units_New;

         Set_All_Dependencies (Use_First_New_Unit => True);

         Reorder_Trees (Cont);
      exception

         when Inconsistent_Incremental_Context =>
            --  Erase the old Context information
            Increase_ASIS_OS_Time;
            Pre_Initialize (Cont);
            A4G.Contt.Initialize (Cont);

            --  We still have one good tree
            Result_Tree := Allocate_Tree_Entry;
            NB_Restore;
            Set_Current_Tree (Result_Tree);

            --  And we have to extract the information from this tree
            Register_Units;
            Scan_Units_New;
            Set_All_Dependencies;
      end;

      --  and now - forming the result Unit_Id

      if Spec then
         Result_Unit_Id := Start_Unit;
      else
         Result_Unit_Id :=  Get_Body (Cont, Start_Unit);
      end if;

      return Result_Unit_Id;

   end Get_Main_Unit_Tree_On_The_Fly;

   ------------------
   -- Get_One_Unit --
   ------------------

   function Get_One_Unit
     (Name    : Wide_String;
      Context : Context_Id;
      Spec    : Boolean)
      return Unit_Id
   is
      Unit_Name      : constant String := Convert_To_String (Name);
      Name_Length    : constant Natural                   := Unit_Name'Length;
      Norm_Unit_Name : String (1 .. Name_Length + 2); -- "+ 2" for %(s|b)

      Result_Id      : Unit_Id;

      Is_Unit_Name   : Boolean := False;

   begin

      --  first of all, we have to check if Name can really be
      --  treated as Ada unit name:

      if Name_Length = 0 then
         return Nil_Unit;
      end if;

      Get_Norm_Unit_Name (U_Name           => Unit_Name,
                          N_U_Name         => Norm_Unit_Name,
                          Spec             => Spec,
                          May_Be_Unit_Name => Is_Unit_Name);

      if not Is_Unit_Name then
         return Nil_Unit;
      end if;

      --  Now we are sure that Name has the syntax structure of the Ada
      --  unit name, and we have to check whether ASIS has already got
      --  to know about this unit in its Unit Table, Norm_Unit_Name has
      --  already been prepared for the corresponding search in the
      --  Unit Table. If this check fails, we will
      --  have to try to compile the Unit from its source:
      Result_Id := Fetch_Unit_By_Ada_Name
                        (Name      => Unit_Name,
                         Norm_Name => Norm_Unit_Name,
                         Context   => Context,
                         Spec      => Spec);

      return Result_Id;

   end Get_One_Unit;

end A4G.Get_Unit;
