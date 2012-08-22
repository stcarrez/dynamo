------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                             A 4 G . C O N T T                            --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore.                  --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;

with GNAT.Directory_Operations;

with Asis;            use Asis;
with Asis.Errors;     use Asis.Errors;
with Asis.Exceptions; use Asis.Exceptions;
with Asis.Extensions; use Asis.Extensions;

with A4G.A_Debug;     use A4G.A_Debug;
with A4G.A_Osint;     use A4G.A_Osint;
with A4G.A_Output;    use A4G.A_Output;
with A4G.Contt.Dp;    use A4G.Contt.Dp;
with A4G.Contt.SD;    use A4G.Contt.SD;
with A4G.Contt.TT;    use A4G.Contt.TT;
with A4G.Contt.UT;    use A4G.Contt.UT;
with A4G.Defaults;    use A4G.Defaults;
with A4G.Vcheck;      use A4G.Vcheck;

with Namet;           use Namet;
with Output;          use Output;

package body A4G.Contt is

   -------------------------------------------
   -- Local Subprograms and Data Structures --
   -------------------------------------------

   procedure Set_Empty_Context (C : Context_Id);
   --  Set all the attribute of the Context indicated by C as for a
   --  Context having no associations (being empty)

   procedure Set_Predefined_Units;
   --  Sets in the Unit Table the unit entries corresponding to the predefined
   --  Ada environment. For now it sets the entries for the package Standard
   --  and for A_Configuration_Compilation only.

   procedure Print_Context_Search_Dirs
     (C : Context_Id;
      Dir_Kind : Search_Dir_Kinds);
   --  outputs the list of the directories making up the Dir_Kind search path
   --  for the context C; is intended to be used to produce a part of the
   --  Context debug output

   procedure Process_Dir (Dir_Name : String; Dir_Kind : Search_Dir_Kinds);
   --  verifies the part of the context association parameter following the
   --  two leading "-<option>" by checking if it is the name of the
   --  existing directory. If this check fails, this routine raises
   --  ASIS_Failed with Status setting as Parameter_Error (as required by
   --  Asis.Ada_Environmemts.Associate. Otherwise this value is stored in
   --  a normalized form in some temporary data structures as a part of the
   --  search path for the current Context.
   --
   --  For now, normalization consists on appending the directory separator
   --  for the stored name, if Dir_Name does not end with the separator.
   --
   --  To store the search paths for the given context, the Set_Search_Paths
   --  procedure should be called after processing all the actual for the
   --  Parameters parameter of Asis.Ada_Environment.Associate query

   -----------------------------------------------------------------
   -- Structures for temporary handling search directories names  --
   -- during processing the Parameters of the Context Association --
   -----------------------------------------------------------------

   type Dir_Rec;

   type Link is access Dir_Rec;

   type Dir_Rec is record
      Dir_Name : String_Access;
      Next     : Link;
   end record;

   type Dir_List is record
      First : Link;
      Last  : Link;
   end record;

   Source_Dirs : Dir_List;
   Object_Dirs : Dir_List;
   Tree_Dirs   : Dir_List;

   Source_Dirs_Count : Natural := 0;
   Object_Dirs_Count : Natural := 0;
   Tree_Dirs_Count   : Natural := 0;

   procedure Append_Dir (Dirs : in out Dir_List; Dir : Link);
   --  appends a new element with the directory name to a directory list

   GNSA_Source : String_Ptr;
   --  Temporary variable for storing the source name for GNSA Context, may
   --  be used for -C1 Context only, multiple-trees Contexts will need some
   --  general solution

   Config_File : String_Ptr;
   --  Here we keep the '-gnatec<file_name> option when processing context
   --  parameters

   GnatA_Set : Boolean := False;
   --  Flag indicating if '-gnatA' option is provided as a Context parameter

   --  ??? Handling of '-gnatec and -gnatA Context parameters is really awful
   --  It was added to the rather hard-wired processing of Context parameters
   --  coded in the very beginning of the ASIS project. This stuff should be
   --  reimplemented at some point

   --------------------------
   -- Allocate_New_Context --
   --------------------------

   function Allocate_New_Context return Context_Id is
      C : Context_Id;
   begin
      Contexts.Increment_Last;
      C := Contexts.Last;
      Set_Empty_Context (C);

      return Contexts.Last;
   end Allocate_New_Context;

   ----------------
   -- Append_Dir --
   ----------------

   procedure Append_Dir (Dirs : in out Dir_List; Dir : Link) is
   begin
      if Dirs.First = null then
         Dirs.First := Dir;
      else
         Dirs.Last.Next := Dir;
      end if;
      Dirs.Last := Dir;

   end Append_Dir;

   ------------------
   -- Context_Info --
   ------------------

   function Context_Info (C : Context_Id) return String is
      Cont_Id_Image : constant String := Context_Id'Image (C);
      First_Digit   : Natural;
   begin
      for I in Cont_Id_Image'Range loop
         if Cont_Id_Image (I) /= ' ' then
            First_Digit := I;
            exit;
         end if;
      end loop;

      return  "ASIS Context " &
               Cont_Id_Image (First_Digit .. Cont_Id_Image'Last);
   end Context_Info;

   ---------------
   -- Erase_Old --
   ---------------

   procedure Erase_Old (C : Context_Id) is
   begin
      --  Old (previously associated) Context Name and Parameter values
      Free (Contexts.Table (C).Name);
      Free (Contexts.Table (C).Parameters);

      Free (Contexts.Table (C).GCC);

      --  Context search paths
      Free_Argument_List (Contexts.Table (C).Source_Path);
      Free_Argument_List (Contexts.Table (C).Object_Path);
      Free_Argument_List (Contexts.Table (C).Tree_Path);
      --  Context "-I" options for the compiler
      Free_Argument_List (Contexts.Table (C).Context_I_Options);
      --  a list of tree files for C1/CN modes (if any)
      Free_Argument_List (Contexts.Table (C).Context_Tree_Files);
   end Erase_Old;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      for C in First_Context_Id .. Contexts.Last loop
         Finalize (C);
      end loop;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (C : Context_Id) is
   begin
      Reset_Context (C);
      if Debug_Lib_Model then
         Print_Context_Info (C);
      end if;

      if Is_Associated (C) then
         Erase_Old (C);
         --  probably, some more cleaning up is needed...
      end if;
      --  at least we have to put off these flags:
      Contexts.Table (C).Is_Associated := False;
      Contexts.Table (C).Is_Opened     := False;
   end Finalize;

   ----------------------
   -- Get_Context_Name --
   ----------------------

   function Get_Context_Name (C : Context_Id) return String is
      S : constant String_Access := Contexts.Table (C).Name;
   begin
      if S = null then
         return "";
      else
         return S.all;
      end if;
   end Get_Context_Name;

   ----------------------------
   -- Get_Context_Parameters --
   ----------------------------

   function Get_Context_Parameters (C : Context_Id) return String is
      S : constant String_Access := Contexts.Table (C).Parameters;
   begin
      if S = null then
         return "";
      else
         return S.all;
      end if;
   end Get_Context_Parameters;

   ---------------------
   -- Get_Current_Cont --
   ---------------------

   function Get_Current_Cont return Context_Id is
   begin
      return Current_Context;
   end Get_Current_Cont;

   ----------------------
   -- Get_Current_Tree --
   ----------------------

   function Get_Current_Tree return Tree_Id is
   begin
      return Current_Tree;
   end Get_Current_Tree;

   ----------
   -- Hash --
   ----------

   function Hash return Hash_Index_Type is
      subtype Int_1_12 is Int range 1 .. 12;
      --  Used to avoid when others on case jump below

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if A_Name_Len > 12 then
         Even_Name_Len := (A_Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (A_Name_Buffer (01))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (A_Name_Buffer (03))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (A_Name_Buffer (05))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (A_Name_Buffer (07))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (A_Name_Buffer (09))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (A_Name_Buffer (11))) * 2 +
           Character'Pos (A_Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case (Int_1_12 (A_Name_Len)) is

         when 1 =>
            return
               Character'Pos (A_Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (A_Name_Buffer (1))) * 64 +
              Character'Pos (A_Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (A_Name_Buffer (1))) * 16 +
              Character'Pos (A_Name_Buffer (3))) * 16 +
              Character'Pos (A_Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (A_Name_Buffer (1))) * 8 +
              Character'Pos (A_Name_Buffer (2))) * 8 +
              Character'Pos (A_Name_Buffer (3))) * 8 +
              Character'Pos (A_Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (A_Name_Buffer (4))) * 8 +
              Character'Pos (A_Name_Buffer (1))) * 4 +
              Character'Pos (A_Name_Buffer (3))) * 4 +
              Character'Pos (A_Name_Buffer (5))) * 8 +
              Character'Pos (A_Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (A_Name_Buffer (5))) * 4 +
              Character'Pos (A_Name_Buffer (1))) * 4 +
              Character'Pos (A_Name_Buffer (4))) * 4 +
              Character'Pos (A_Name_Buffer (2))) * 4 +
              Character'Pos (A_Name_Buffer (6))) * 4 +
              Character'Pos (A_Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (A_Name_Buffer (4))) * 4 +
              Character'Pos (A_Name_Buffer (3))) * 4 +
              Character'Pos (A_Name_Buffer (1))) * 4 +
              Character'Pos (A_Name_Buffer (2))) * 2 +
              Character'Pos (A_Name_Buffer (5))) * 2 +
              Character'Pos (A_Name_Buffer (7))) * 2 +
              Character'Pos (A_Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (A_Name_Buffer (2))) * 4 +
              Character'Pos (A_Name_Buffer (1))) * 4 +
              Character'Pos (A_Name_Buffer (3))) * 2 +
              Character'Pos (A_Name_Buffer (5))) * 2 +
              Character'Pos (A_Name_Buffer (7))) * 2 +
              Character'Pos (A_Name_Buffer (6))) * 2 +
              Character'Pos (A_Name_Buffer (4))) * 2 +
              Character'Pos (A_Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (A_Name_Buffer (2))) * 4 +
              Character'Pos (A_Name_Buffer (1))) * 4 +
              Character'Pos (A_Name_Buffer (3))) * 4 +
              Character'Pos (A_Name_Buffer (4))) * 2 +
              Character'Pos (A_Name_Buffer (8))) * 2 +
              Character'Pos (A_Name_Buffer (7))) * 2 +
              Character'Pos (A_Name_Buffer (5))) * 2 +
              Character'Pos (A_Name_Buffer (6))) * 2 +
              Character'Pos (A_Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (A_Name_Buffer (01))) * 2 +
              Character'Pos (A_Name_Buffer (02))) * 2 +
              Character'Pos (A_Name_Buffer (08))) * 2 +
              Character'Pos (A_Name_Buffer (03))) * 2 +
              Character'Pos (A_Name_Buffer (04))) * 2 +
              Character'Pos (A_Name_Buffer (09))) * 2 +
              Character'Pos (A_Name_Buffer (06))) * 2 +
              Character'Pos (A_Name_Buffer (05))) * 2 +
              Character'Pos (A_Name_Buffer (07))) * 2 +
              Character'Pos (A_Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (A_Name_Buffer (05))) * 2 +
              Character'Pos (A_Name_Buffer (01))) * 2 +
              Character'Pos (A_Name_Buffer (06))) * 2 +
              Character'Pos (A_Name_Buffer (09))) * 2 +
              Character'Pos (A_Name_Buffer (07))) * 2 +
              Character'Pos (A_Name_Buffer (03))) * 2 +
              Character'Pos (A_Name_Buffer (08))) * 2 +
              Character'Pos (A_Name_Buffer (02))) * 2 +
              Character'Pos (A_Name_Buffer (10))) * 2 +
              Character'Pos (A_Name_Buffer (04))) * 2 +
              Character'Pos (A_Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (A_Name_Buffer (03))) * 2 +
              Character'Pos (A_Name_Buffer (02))) * 2 +
              Character'Pos (A_Name_Buffer (05))) * 2 +
              Character'Pos (A_Name_Buffer (01))) * 2 +
              Character'Pos (A_Name_Buffer (06))) * 2 +
              Character'Pos (A_Name_Buffer (04))) * 2 +
              Character'Pos (A_Name_Buffer (08))) * 2 +
              Character'Pos (A_Name_Buffer (11))) * 2 +
              Character'Pos (A_Name_Buffer (07))) * 2 +
              Character'Pos (A_Name_Buffer (09))) * 2 +
              Character'Pos (A_Name_Buffer (10))) * 2 +
              Character'Pos (A_Name_Buffer (12))) mod Hash_Num;

         when others =>
            --  ??? !!! ???
            --  this alternative can never been reached, but it looks like
            --  there is something wrong here with the compiler, it does not
            --  want to compile the code without this line (up to 3.10b)
            return 0;

      end case;
   end Hash;

   ---------------
   -- I_Options --
   ---------------

   function I_Options (C : Context_Id) return Argument_List is
      Nul_Argument_List : constant Argument_List (1 .. 0) := (others => null);
   begin
      if Contexts.Table (C).Context_I_Options = null then
         return Nul_Argument_List;
      else
         return Contexts.Table (C).Context_I_Options.all;
      end if;
   end I_Options;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Contexts.Init;
      Current_Context := Non_Associated;
      Current_Tree    := Nil_Tree;
   end Initialize;

   --------------------
   -- Pre_Initialize --
   --------------------

   procedure Pre_Initialize (C : Context_Id) is
   begin

      Backup_Current_Context;

      --  Clearing the Context Hash Table:
      for J in Hash_Index_Type loop
         Contexts.Table (C).Hash_Table (J) := No_Unit_Id;
      end loop;

      --  Initializing Context's internal tables:
      A_Name_Chars.Init;
      Unit_Table.Init;
      Tree_Table.Init;
      A4G.A_Elists.Initialize;

      Current_Context := C;
      Current_Tree    := Nil_Tree;
   end Pre_Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : Context_Id) is
   begin

      Contexts.Table (C).Opened_At := A_OS_Time;
      Contexts.Table (C).Specs     := 0;
      Contexts.Table (C).Bodies    := 0;

      --  Clearing the Context Hash Table:
      for J in Hash_Index_Type loop
         Contexts.Table (C).Hash_Table (J) := No_Unit_Id;
      end loop;

      Set_Predefined_Units;
   end Initialize;

   ---------------------------
   -- Locate_In_Search_Path --
   ---------------------------

   function Locate_In_Search_Path
     (C         : Context_Id;
      File_Name : String;
      Dir_Kind : Search_Dir_Kinds)
      return String_Access
   is
      Curr_Dir    : String_Access;
      Search_Path : Directory_List_Ptr;
   begin

      case Dir_Kind is
         when Source =>
            Search_Path := Contexts.Table (C).Source_Path;
         when Object =>
            Search_Path := Contexts.Table (C).Object_Path;
         when Tree   =>
            Search_Path := Contexts.Table (C).Tree_Path;
      end case;

      if Search_Path = null then
         --  this means that the current directory only should be used
         --  for locating the file
         if Is_Regular_File (File_Name) then
            return new String'(File_Name & ASCII.NUL);
         else
            return null;
         end if;
      end if;

      --  and here we have to look through the directory search path

      for I in 1 .. Search_Path'Last loop

         Curr_Dir := Search_Path (I);

         if Is_Regular_File
              (Curr_Dir.all & Directory_Separator & File_Name)
         then
            return new String'
                         (Curr_Dir.all & Directory_Separator &
                          File_Name & ASCII.NUL);
         end if;

      end loop;

      return null;
   end Locate_In_Search_Path;

   -------------
   -- NB_Save --
   -------------

   procedure NB_Save is
   begin
      Backup_Name_Len := A_Name_Len;
      Backup_Name_Buffer (1 .. Backup_Name_Len) :=
         A_Name_Buffer (1 .. A_Name_Len);
   end NB_Save;

   ----------------
   -- NB_Restore --
   ----------------

   procedure NB_Restore is
   begin
      A_Name_Len := Backup_Name_Len;
      A_Name_Buffer (1 .. A_Name_Len) :=
         Backup_Name_Buffer (1 .. Backup_Name_Len);
   end NB_Restore;

   ------------------------
   -- Print_Context_Info --
   ------------------------

   procedure Print_Context_Info is
   begin
      Write_Str ("ASIS Context Table - general information:");
      Write_Eol;
      Write_Eol;
      Write_Str ("The number of contexts which have been allocated: ");
      Write_Int (Int (Contexts.Last - First_Context_Id + 1));
      Write_Eol;
      Write_Eol;
      Write_Str ("Default search paths:");
      Write_Eol;
      Write_Eol;
      Write_Str ("Source search path:");
      Write_Eol;
      Print_Source_Defaults;
      Write_Eol;
      Write_Str ("Object/ALI search path:");
      Write_Eol;
      Print_Lib_Defaults;
      Write_Eol;
      Write_Str ("Tree search path:");
      Write_Eol;
      Print_Tree_Defaults;
      Write_Eol;
      Write_Str ("=====================================================");
      Write_Eol;

      for C in First_Context_Id .. Contexts.Last loop
         Print_Context_Info (C);
         Write_Eol;
      end loop;

   end Print_Context_Info;

   ------------------------
   -- Print_Context_Info --
   ------------------------

   procedure Print_Context_Info (C : Context_Id) is
   begin
      Reset_Context (C);
      Write_Str ("Debug output for context number: ");
      Write_Int (Int (C));
      Write_Eol;

      if C = Non_Associated then
         Write_Str ("   Nil Context, it can never be associated");
         Write_Eol;
         return;
      end if;

      if Is_Associated (C) then
         Print_Context_Parameters (C);

         if Is_Opened (C) then
            Print_Units (C);
            Print_Trees (C);
         else
            Write_Str ("This Context is closed");
            Write_Eol;
         end if;

      else
         Write_Str ("This Context is dissociated");
         Write_Eol;
      end if;

   end Print_Context_Info;

   ------------------------------
   -- Print_Context_Parameters --
   ------------------------------

   procedure Print_Context_Parameters (C : Context_Id) is
   begin

      Write_Str ("Association parameters for Context number: ");
      Write_Int (Int (C));
      Write_Eol;

      if C = Non_Associated then
         Write_Str ("   Nil Context, it can never be associated");
         Write_Eol;
         return;
      end if;

      if Is_Associated (C) then
         Write_Str ("Context name: ");

         if Contexts.Table (C).Name = null or else
            Contexts.Table (C).Name.all = ""
         then
            Write_Str ("no name has been associated");
         else
            Write_Str (Contexts.Table (C).Name.all);
         end if;

         Write_Eol;

         Write_Str ("Context parameters:");
         Write_Eol;

         if Contexts.Table (C).Parameters = null then
            Write_Str ("   no parameter has been associated");
         else
            Write_Str ("   " & Contexts.Table (C).Parameters.all);
         end if;

         Write_Eol;

         Write_Str ("Context Search Dirs:");
         Write_Eol;
         Write_Str ("--------------------");
         Write_Eol;
         Write_Str ("Source Dirs");
         Write_Eol;
         Print_Context_Search_Dirs (C, Source);
         Write_Eol;

         Write_Str ("The source search path for calling GNAT is ");
         Write_Eol;

         if Contexts.Table (C).Context_I_Options = null then
            Write_Str (" no ""-I"" option has been associated");
            Write_Eol;
         else

            for I in 1 .. Contexts.Table (C).Context_I_Options'Last loop
               Write_Str ("   " &
                          Contexts.Table (C).Context_I_Options (I).all);
               Write_Eol;
            end loop;

         end if;

         Write_Eol;

         Write_Str ("Object/ALI Dirs");
         Write_Eol;
         Print_Context_Search_Dirs (C, Object);
         Write_Eol;
         Write_Eol;
         Write_Str ("Tree Dirs");
         Write_Eol;
         Print_Context_Search_Dirs (C, Tree);
         Write_Eol;
         Write_Eol;
      else
         Write_Str ("The Context is dissociated");
         Write_Eol;
      end if;

   end Print_Context_Parameters;

   -------------------------------
   -- Print_Context_Search_Dirs --
   -------------------------------

   procedure Print_Context_Search_Dirs
     (C : Context_Id;
      Dir_Kind : Search_Dir_Kinds)
   is
      Path : Directory_List_Ptr;
      --  search path to print
   begin
      case Dir_Kind is
         when Source =>
            Path := Contexts.Table (C).Source_Path;
         when Object =>
            Path := Contexts.Table (C).Object_Path;
         when Tree   =>
            Path := Contexts.Table (C).Tree_Path;
      end case;

      if Path = null then
         Write_Str ("   No directory has been associated");
         return;
      end if;

      for I in Path'Range loop
         Write_Str ("   " & Path (I).all);
         Write_Eol;
      end loop;

      Write_Eol;
   end Print_Context_Search_Dirs;

   --------------------------------
   -- Process_Context_Parameters --
   --------------------------------

   procedure Process_Context_Parameters
     (Parameters : String;
      Cont       : Context_Id := Non_Associated)
   is
      Cont_Parameters : Argument_List_Access;

      C_Set           : Boolean := False;
      F_Set           : Boolean := False;
      S_Set           : Boolean := False;
      GCC_Set         : Boolean := False;

      Next_TF_Name    : Natural := 0;

      procedure Process_One_Parameter (Param : String);
      --  incapsulates processing of a separate parameter

      procedure Check_Parameters;
      --  Checks, that context options are compatible with each other and with
      --  the presence of tree files (if any). The check made by this procedure
      --  is not very smart - it detects only one error, and it does not try to
      --  provide a very detailed diagnostic

      procedure Process_Tree_File_Name (TF_Name : String);
      --  Checks, that TF_Name has tree file name suffix (.ats or .atb), and
      --  generates an ASIS warning if this check fails. Stores TF_Name in
      --  Context_Tree_Files list for the Context Cont.

      procedure Process_Source_File_For_GNSA (SF_Name : String);
      --  Checks if SF_Name is the name of the regular file, and if it is,
      --  stores it in the temporary variable

      procedure Process_gnatec_Option (Option : String);
      --  Checks if the string after '-gnatec' is the name of some file. If
      --  it is, frees Config_File and stores the -gnatec option into this
      --  variable. Otherwise raises ASIS_Failed with Status setting as
      --  Parameter_Error.

      ----------------------
      -- Check_Parameters --
      ----------------------

      procedure Check_Parameters is
         Mode_Str : String := "-C?";
      begin
         --  first, set defaults if needed:
         if not C_Set then
            Set_Default_Context_Processing_Mode (Cont);
            C_Set := True;
         end if;

         if not F_Set then
            Set_Default_Tree_Processing_Mode (Cont);
            F_Set := True;
         end if;

         if not S_Set then
            Set_Default_Source_Processing_Mode (Cont);
            S_Set := True;
         end if;

         --  Special processing for GNSA mode:

         if Tree_Processing_Mode (Cont) = GNSA and then
            Context_Processing_Mode (Cont) /= One_Tree
         then
            Set_Error_Status
              (Status    => Asis.Errors.Parameter_Error,
               Diagnosis => "Asis.Ada_Environments.Associate:"
                          &  ASIS_Line_Terminator
                          &  "only -C1 mode can be set for -GNSA mode");
            raise ASIS_Failed;
         end if;

         case Context_Processing_Mode (Cont) is

            when One_Tree | N_Trees =>
               if Context_Processing_Mode (Cont) = One_Tree then
                  Mode_Str (3) := '1';
               else
                  Mode_Str (3) := 'N';
               end if;

               if not (Tree_Processing_Mode (Cont) = Pre_Created
                      or else
                      (Tree_Processing_Mode (Cont) = GNSA and then
                       Context_Processing_Mode (Cont) = One_Tree))
               then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                &  "only -FT mode can be set for "
                                &  Mode_Str & " mode");
                  raise ASIS_Failed;
               end if;

               --  Process_Association_Option already checks, that at most one
               --  tree file can be set for this mode, and here we have to
               --  check, that at least one tree file is set GNSA is a special
               --  case at the moment):

               if Last_Tree_File < First_Tree_File and then
                  Tree_Processing_Mode (Cont) /= GNSA
               then
                  --  this means, that first tree file just has not been
                  --  processed
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                &  "no tree file is set for "
                                &  Mode_Str & " mode");
                  raise ASIS_Failed;
               end if;
            when Partition =>
               --  for now, this is not implemented :-(
               Not_Implemented_Yet (Diagnosis =>
                          "Asis.Ada_Environments.Associate (-CP option)");
            when All_Trees =>

               --  all tree processing modes are allowed for All_Trees
               --  contexts, but no tree files should be explicitly set:

               if  Last_Tree_File >= First_Tree_File then
                  --  this means, that at least one tree file has been
                  --  processed
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                &  "no tree file must be set for -CA mode");
                  raise ASIS_Failed;
               end if;
         end case;

         if (Tree_Processing_Mode (Cont) = Mixed       or else
             Tree_Processing_Mode (Cont) = On_The_Fly  or else
             Tree_Processing_Mode (Cont) = Incremental or else
             Tree_Processing_Mode (Cont) = GNSA)
           and then
            Source_Processing_Mode (Cont) /= All_Sources
         then
            Set_Error_Status
              (Status    => Asis.Errors.Parameter_Error,
               Diagnosis => "Asis.Ada_Environments.Associate:"
                          &  ASIS_Line_Terminator
                          &  "only -SA option is allowed if trees can be "
                          &  "created on the fly");
            raise ASIS_Failed;
         end if;

         --  If we can create trees on the fly and the GCC field for the given
         --  context is not set, try to define from the ASIS tool name
         --  if we have to use some specific gcc

         if (Tree_Processing_Mode (Cont) = Mixed       or else
             Tree_Processing_Mode (Cont) = On_The_Fly  or else
             Tree_Processing_Mode (Cont) = Incremental)
            and then
             Contexts.Table (Cont).GCC = null
         then
            declare
               Tool_Name : constant String :=
                 GNAT.Directory_Operations.Base_Name
                   (Normalize_Pathname (Ada.Command_Line.Command_Name));
               Dash_Idx  : Natural         := 0;
            begin

               for J in reverse Tool_Name'Range loop

                  if Tool_Name (J) = '-' then
                     Dash_Idx := J;
                     exit;
                  end if;

               end loop;

               if Dash_Idx > 0 then
                  Contexts.Table (Cont).GCC :=
                    Locate_Exec_On_Path
                      (Tool_Name (Tool_Name'First .. Dash_Idx) & "gcc");
               end if;

            end;

         end if;

      end Check_Parameters;

      ---------------------------
      -- Process_gnatec_Option --
      ---------------------------

      procedure Process_gnatec_Option (Option : String) is
         File_Name_Start : Natural := Option'First + 7;
      begin

         if Option (File_Name_Start) = '=' then
            File_Name_Start := File_Name_Start + 1;
         end if;

         if File_Name_Start <= Option'Last and then
            Is_Regular_File (Option (File_Name_Start .. Option'Last))
         then
            Free (Config_File);
            Config_File := new String'(Option);
         else
            Set_Error_Status
              (Status    => Asis.Errors.Parameter_Error,
               Diagnosis => "Asis.Ada_Environments.Associate:"
                          &  ASIS_Line_Terminator
                          &  "cannot find configuration pragmas file "
                          &  Option (File_Name_Start .. Option'Last));

            raise ASIS_Failed;
         end if;

      end Process_gnatec_Option;

      ---------------------------
      -- Process_One_Parameter --
      ---------------------------

      procedure Process_One_Parameter (Param : String) is
         Parameter : constant String (1 .. Param'Length) := Param;
         Par_Len   : constant Positive := Parameter'Length;

         procedure Process_Parameter;
         procedure Process_Option;
         --  Process_Option works if Param starts from '-', and
         --  Process_Parameter works otherwise

         procedure Process_Parameter is
         begin
            --  the only parameter currently available for Context association
            --  is a tree file (or source file in case of GNSA context) name

            --  Special processing for GNSA mode:

            if Tree_Processing_Mode (Cont) = GNSA then
               Process_Source_File_For_GNSA (Parameter);
               return;
            end if;

            if Last_Tree_File < First_Tree_File then
               --  This means, that we've just encountered the first candidate
               --  for a tree file name as a part of the Parameters string.
               --  Therefore, we should set the default Context, tree and
               --  source processing options (if needed) and the corresponding
               --  flags:

               if not C_Set then
                  Set_Default_Context_Processing_Mode (Cont);
                  C_Set := True;
               end if;

               if not F_Set then
                  Set_Default_Tree_Processing_Mode (Cont);
                  F_Set := True;
               end if;

               if not S_Set then
                  Set_Default_Source_Processing_Mode (Cont);
                  S_Set := True;
               end if;
            else
               --  more than one tree file is illegal in -C1 mode
               if Context_Processing_Mode (Cont) = One_Tree then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                &  "only one tree file is allowed in "
                                &  "-C1 mode");
                  raise ASIS_Failed;
               end if;
            end if;

            Process_Tree_File_Name (Parameter);

         end Process_Parameter;

         procedure Process_Option is
            Switch_Char : Character;
         begin

            if Par_Len < 3 then
               goto Wrong_Par;
            else
               Switch_Char := Parameter (2);
            end if;

            if Switch_Char = 'C' and then Par_Len = 3 then

               if C_Set then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                & "-C option is either misplaced "
                                & "or duplicated");
                  raise ASIS_Failed;

               else
                  Switch_Char := Parameter (3);

                  case Switch_Char is
                     when '1' =>
                        Set_Context_Processing_Mode (Cont, One_Tree);
                     when 'N' =>
                        Set_Context_Processing_Mode (Cont, N_Trees);
                     when 'P' =>
                        Set_Context_Processing_Mode (Cont, Partition);
                     when 'A' =>
                        Set_Context_Processing_Mode (Cont, All_Trees);
                     when others =>
                        goto Wrong_Par;
                  end case;

                  C_Set := True;
               end if;

            elsif Switch_Char = 'F' and then Par_Len = 3 then

               if F_Set then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                & "-F option is either misplaced "
                                & "or duplicated");
                  raise ASIS_Failed;

               else
                  Switch_Char := Parameter (3);

                  case Switch_Char is
                     when 'S' =>
                        Set_Tree_Processing_Mode (Cont, On_The_Fly);
                     when 'T' =>
                        Set_Tree_Processing_Mode (Cont, Pre_Created);
                     when 'M' =>
                        Set_Tree_Processing_Mode (Cont, Mixed);
                     when 'I' =>
                        Set_Tree_Processing_Mode (Cont, Incremental);
                     when others =>
                        goto Wrong_Par;
                  end case;

                  F_Set := True;
               end if;

            elsif Switch_Char = 'S' and then Par_Len = 3 then

               if S_Set then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                & "-S option is either misplaced"
                                & " or duplicated");
                  raise ASIS_Failed;
               else
                  Switch_Char := Parameter (3);

                  case Switch_Char is
                     when 'A' =>
                        Set_Source_Processing_Mode (Cont, All_Sources);
                     when 'E' =>
                        Set_Source_Processing_Mode (Cont, Existing_Sources);
                     when 'N' =>
                        Set_Source_Processing_Mode (Cont, No_Sources);
                     when others =>
                        goto Wrong_Par;
                  end case;

                  S_Set := True;
               end if;

            elsif Switch_Char = 'I' then
               Process_Dir (Parameter (3 .. Par_Len), Source);

            elsif Switch_Char = 'O' then
               Process_Dir (Parameter (3 .. Par_Len), Object);

            elsif Switch_Char = 'T' then
               Process_Dir (Parameter (3 .. Par_Len), Tree);

            elsif Switch_Char = 'g' and then
                  Par_Len >= 8      and then
                  Parameter (1 .. 7) = "-gnatec"
            then
               Process_gnatec_Option (Parameter);

            elsif Parameter = "-AOP" then
               Set_Use_Default_Trees (Cont, True);

            elsif Switch_Char = '-'
               and then
                  Parameter (1 .. 6) = "--GCC="
            then

               if GCC_Set then
                  Set_Error_Status
                    (Status    => Asis.Errors.Parameter_Error,
                     Diagnosis => "Asis.Ada_Environments.Associate:"
                                &  ASIS_Line_Terminator
                                & "--GCC option is duplicated");
                  raise ASIS_Failed;
               else
                  GCC_Set := True;
                  Contexts.Table (Cont).GCC :=
                    Locate_Exec_On_Path (Parameter (7 .. Parameter'Last));
               end if;

            elsif Parameter = "-gnatA" then
               GnatA_Set := True;

            elsif Parameter = "-GNSA" then
               --  Special processing for GNSA

               Set_Tree_Processing_Mode    (Cont, GNSA);
               Set_Source_Processing_Mode  (Cont, All_Sources);
               Set_Context_Processing_Mode (Cont, One_Tree);
               F_Set := True;
               C_Set := True;
               S_Set := True;
            else
               goto Wrong_Par;
            end if;

            return;

            <<Wrong_Par>>
               ASIS_Warning
                  (Message => "Asis.Ada_Environments.Associate: "
                             & "unknown option "
                             &  Parameter,
                   Error   => Parameter_Error);

         end Process_Option;

      begin --  Process_One_Parameter
         if Parameter (1) = '-' then
            Process_Option;
         else
            Process_Parameter;
         end if;
      end Process_One_Parameter;

      ----------------------------------
      -- Process_Source_File_For_GNSA --
      ----------------------------------

      procedure Process_Source_File_For_GNSA (SF_Name : String) is
      begin

         if not Is_Regular_File (SF_Name) then

            Set_Error_Status
              (Status    => Asis.Errors.Parameter_Error,
               Diagnosis => "Asis.Ada_Environments.Associate: "
                          & "file " & SF_Name & "does not exist");

            raise ASIS_Failed;
         end if;

         Free (GNSA_Source);
         GNSA_Source := new String'(SF_Name);

      end Process_Source_File_For_GNSA;

      ----------------------------
      -- Process_Tree_File_Name --
      ----------------------------

      procedure Process_Tree_File_Name (TF_Name : String) is
         TF_First    : Positive := TF_Name'First;
         TF_Last     : Positive := TF_Name'Last;
         TF_Len      : Positive;
         Wrong_Name  : Boolean;
         T_File_Name : Name_Id;

      begin
         if TF_Name (TF_First) = '"'
           and then
            TF_Name (TF_Last) = '"'
         then
            TF_First := TF_First + 1;
            TF_Last  := TF_Last  - 1;
         end if;

         TF_Len := TF_Last - TF_First + 1;

         Wrong_Name := not (
               TF_Len >= 5
            and then
               (TF_Name (TF_Last) = 't' or else TF_Name (TF_Last) = 'T')
            and then
               (TF_Name (TF_Last - 1) = 'd'
                  or else TF_Name (TF_Last - 1) = 'D')
            and then
               (TF_Name (TF_Last - 2) = 'a'
                  or else TF_Name (TF_Last - 2) = 'A')
            and then
               TF_Name (TF_Last - 3) = '.');

         if Wrong_Name then
            ASIS_Warning
               (Message => "Asis.Ada_Environments.Associate: "
                         & TF_Name
                         & " does not have a form of a tree file name",
                Error   => Parameter_Error);
         end if;

         for I in TF_First .. TF_Last loop
            Name_Buffer (I) := TF_Name (I);
         end loop;

         Name_Len := TF_Len;

         T_File_Name := Name_Find;

         if T_File_Name > Last_Tree_File then
            Last_Tree_File := T_File_Name;
            Next_TF_Name := Next_TF_Name + 1;
            Contexts.Table (Cont).Context_Tree_Files (Next_TF_Name) :=
               new String'(TF_Name (TF_First .. TF_Last));
         end if;

      end Process_Tree_File_Name;

   begin  -- Process_Context_Parameters

      Free (Config_File);
      GnatA_Set := False;

      if Tree_Processing_Mode (Cont) /= GNSA  then
         --  In GNSA mode we should not destroy the GNAT name table.
         --  ??? But why? We run GNSA after that?
         --  Should be revised for non -C1 GNSA modes, if any

         Namet.Initialize; --  ???
         First_Tree_File := First_Name_Id;
         Last_Tree_File  := First_Name_Id - 1;
      end if;

      Set_Use_Default_Trees (Cont, False);

      if Parameters /= "" then

         Cont_Parameters := Parameter_String_To_List (Parameters);

         Contexts.Table (Cont).Context_Tree_Files :=
            new Argument_List (1 .. Cont_Parameters'Length);

         for I in Cont_Parameters'Range loop
            Process_One_Parameter (Cont_Parameters (I).all);
         end loop;

         Free_Argument_List (Cont_Parameters);
      end if;

      Check_Parameters;
      Set_Context_Parameters (Cont, Parameters);
      Set_Search_Paths (Cont);
   end Process_Context_Parameters;

   -----------------
   -- Process_Dir --
   -----------------

   procedure Process_Dir (Dir_Name : String; Dir_Kind : Search_Dir_Kinds) is
      First   : Positive := Dir_Name'First;
      Last    : Natural  := Dir_Name'Last;
      New_Dir : Link;
   begin

      if Dir_Name (First) = '"'
        and then
         Dir_Name (Last) = '"'
      then
         First := First + 1;
         Last  := Last - 1;
      end if;

      if not Is_Directory (Dir_Name (First .. Last)) then
         Set_Error_Status (Status    => Asis.Errors.Parameter_Error,
                           Diagnosis => "Asis.Ada_Environments.Associate:"
                                      &  ASIS_Line_Terminator
                                      &  "Wrong parameter for Context "
                                      &  "Association: "
                                      &  Dir_Name
                                      &  " is not a directory name");
         raise ASIS_Failed;
      end if;

      New_Dir          := new Dir_Rec;
      New_Dir.Dir_Name := new String'(Dir_Name (First .. Last));

      case Dir_Kind is
         when Source =>
            Source_Dirs_Count := Source_Dirs_Count + 1;
            Append_Dir (Source_Dirs, New_Dir);
         when Object =>
            Object_Dirs_Count := Object_Dirs_Count + 1;
            Append_Dir (Object_Dirs, New_Dir);
         when Tree   =>
            Tree_Dirs_Count   := Tree_Dirs_Count   + 1;
            Append_Dir (Tree_Dirs, New_Dir);
      end case;
   end Process_Dir;

   --------------------
   -- Scan_Trees_New --
   --------------------

   procedure Scan_Trees_New (C : Context_Id) is
   begin
      Scan_Tree_Files_New (C);
      Investigate_Trees_New (C);

      --  And now, when all the unit attributes are set, we compute integrated
      --  dependencies
      Set_All_Dependencies;

      Reorder_Trees (C);
   end Scan_Trees_New;

   ----------------------
   -- Set_Context_Name --
   ----------------------

   procedure Set_Context_Name (C : Context_Id; Name : String) is
   begin
      Contexts.Table (C).Name := new String'(Name);
   end Set_Context_Name;

   ----------------------------
   -- Set_Context_Parameters --
   ----------------------------

   procedure Set_Context_Parameters (C : Context_Id; Parameters : String)
   is
   begin
      Contexts.Table (C).Parameters := new String'(Parameters);
   end Set_Context_Parameters;

   -----------------------
   -- Set_Empty_Context --
   -----------------------

   procedure Set_Empty_Context (C : Context_Id) is
      Cont : constant Context_Id := C;
   begin
      --  We explicitly set all the fields of the context record

      Contexts.Table (C).Name       := null;
      Contexts.Table (C).Parameters := null;
      Contexts.Table (C).GCC        := null;

      Set_Is_Associated     (Cont, False);
      Set_Is_Opened         (Cont, False);
      Set_Use_Default_Trees (Cont, False);

      Contexts.Table (C).Opened_At := Last_ASIS_OS_Time;
      Contexts.Table (C).Specs     := 0;
      Contexts.Table (C).Bodies    := 0;

      for J in Hash_Index_Type loop
         Contexts.Table (C).Hash_Table (J) := Nil_Unit;
      end loop;

      Contexts.Table (C).Current_Main_Unit  := Nil_Unit;

      Contexts.Table (C).Source_Path        := null;
      Contexts.Table (C).Object_Path        := null;
      Contexts.Table (C).Tree_Path          := null;
      Contexts.Table (C).Context_I_Options  := null;
      Contexts.Table (C).Context_Tree_Files := null;

      Contexts.Table (C).Mode               := All_Trees;
      Contexts.Table (C).Tree_Processing    := Pre_Created;
      Contexts.Table (C).Source_Processing  := All_Sources;

   end Set_Empty_Context;

   ---------------------
   -- Set_Current_Cont --
   ---------------------

   procedure Set_Current_Cont (L : Context_Id) is
   begin
      Current_Context := L;
   end Set_Current_Cont;

   ----------------------
   -- Set_Current_Tree --
   ----------------------

   procedure Set_Current_Tree (Tree : Tree_Id) is
   begin
      Current_Tree := Tree;
   end Set_Current_Tree;

   ----------------------
   -- Set_Name_String --
   ----------------------

   procedure Set_Name_String (S : String) is
   begin
      A_Name_Len                      := S'Length;
      A_Name_Buffer (1 .. A_Name_Len) := S;
   end Set_Name_String;

   --------------------------
   -- Set_Predefined_Units --
   --------------------------

   procedure Set_Predefined_Units is
      Cont : constant Context_Id := Get_Current_Cont;
      C_U  : Unit_Id;
   begin

      --  set the entry for the package Standard:

      --  The problem here is that Ada allows to redefine Standard, so we use
      --  a special normalized name for predefined Standard, and a "normal"
      --  normalized name for redefinition of Standard. See also
      --  A4G.Get_Unit.Fetch_Unit_By_Ada_Name

      Set_Name_String ("__standard%s");
      C_U := Allocate_Unit_Entry (Cont);
      --  C_U should be equal to Standard_Id. Should we check this here?

      Set_Name_String ("Standard");
      Set_Ada_Name (C_U);
      Set_Kind     (Cont, C_U, A_Package);
      Set_Class    (Cont, C_U, A_Public_Declaration);
      Set_Top      (Cont, C_U, Empty);
      --  What is the best solution for computing the top node of the
      --  subtree for the package Standard? Now we compute it in
      --  Asis.Set_Get.Top...

      Set_Time_Stamp  (Cont, C_U, Empty_Time_Stamp);
      Set_Origin      (Cont, C_U, A_Predefined_Unit);

      Set_Is_Main_Unit (Cont, C_U, False);
      Set_Is_Body_Required (Cont, C_U, False);

      Set_Source_Status    (Cont, C_U, Absent);

      --  as for the source file, it was set to Nil when allocating the
      --  unit entry

      --  Set the entry for A_Configuration_Compilation

      Set_Name_String ("__configuration_compilation%s");
      C_U := Allocate_Unit_Entry (Cont);
      --  C_U should be equal to Config_Comp_Id. Should we check this here?

      --  Allocate_Unit_Entry counts A_Configuration_Compilation as a spec
      --  unit, but actually it is not a spec, so we have to decrease the
      --  counter back:
      Contexts.Table (Cont).Specs := Contexts.Table (Cont).Specs - 1;

      Set_Name_String ("__Configuration_Compilation");
      --  This name will never be displayed by ASIS

      Set_Ada_Name (C_U);
      Set_Kind     (Cont, C_U, A_Configuration_Compilation);
      Set_Class    (Cont, C_U, A_Public_Declaration);
      Set_Top      (Cont, C_U, Empty);

      Set_Time_Stamp  (Cont, C_U, Empty_Time_Stamp);
      Set_Origin      (Cont, C_U, A_Predefined_Unit);
      --  A_Predefined_Unit? It does not matter, actually...

      Set_Is_Main_Unit (Cont, C_U, False);
      Set_Is_Body_Required (Cont, C_U, False);

      Set_Source_Status    (Cont, C_U, Absent);

      --  as for the source file, it was set to Nil when allocating the
      --  unit entry

   end Set_Predefined_Units;

   ----------------------
   -- Set_Search_Paths --
   ----------------------

   procedure Set_Search_Paths (C : Context_Id) is

      I_Opt_Len : constant Natural := Source_Dirs_Count;

      N_Config_File_Options : Natural := 0;
      Idx                   : Natural;

      procedure Set_Path
        (Path : in out Directory_List_Ptr;
         From : in out Dir_List;
         N    : in out Natural);
      --  Sets the given search path, N is the count of the directories.
      --  resets the temporary data structures used to keep and to count
      --  directory names

      procedure Set_Path
        (Path : in out Directory_List_Ptr;
         From : in out Dir_List;
         N    : in out Natural)
      is
         Next_Dir : Link := From.First;
      begin
         if N = 0 then
            From.First := null;  -- just in case
            From.Last  := null;  -- just in case
            return;
            --  we have nothing to do, and the corresponding search path
            --  will remain null, as it should have been before the call
         end if;

         Path := new Argument_List (1 .. N);

         for I in 1 .. N loop
            Path (I) := new String'(Next_Dir.Dir_Name.all);
            Free (Next_Dir.Dir_Name);
            Next_Dir := Next_Dir.Next;
         end loop;

         From.First := null;
         From.Last  := null;
         N          := 0;

         --  we free the memory occupied by strings stored in this temporary
         --  list of directories, but we do not free the memory used by the
         --  links. We hope we can skip this optimization

      end Set_Path;

   begin --  Set_Search_Paths

      Set_Path
        (Contexts.Table (C).Source_Path, Source_Dirs, Source_Dirs_Count);
      Set_Path
        (Contexts.Table (C).Object_Path, Object_Dirs, Object_Dirs_Count);
      Set_Path
        (Contexts.Table (C).Tree_Path,   Tree_Dirs,   Tree_Dirs_Count);

      --  And the last thing to do is to set for a given Context its
      --  Context_I_Options field:

      if I_Opt_Len = 0      and then
         Config_File = null and then
         not GnatA_Set      and then
         Tree_Processing_Mode (C) /= GNSA
      then
         Contexts.Table (C).Context_I_Options := null; -- just in case
         return;
      end if;

      if Config_File /= null then
         N_Config_File_Options := N_Config_File_Options + 1;
      end if;

      if GnatA_Set then
         N_Config_File_Options := N_Config_File_Options + 1;
      end if;

      Contexts.Table (C).Context_I_Options :=
         new Argument_List (1 .. I_Opt_Len + N_Config_File_Options + 1);

      for I in 1 .. I_Opt_Len loop
         Contexts.Table (C).Context_I_Options (I) :=
            new String'("-I" & Contexts.Table (C).Source_Path (I).all);
      end loop;

      Idx := I_Opt_Len;

      if Config_File /= null then
         Idx := Idx + 1;
         Contexts.Table (C).Context_I_Options (Idx) :=
            new String'(Config_File.all);
      end if;

      if GnatA_Set then
         Idx := Idx + 1;
         Contexts.Table (C).Context_I_Options (Idx) :=
            new String'("-gnatA");
      end if;

      Idx := Idx + 1;

      if Tree_Processing_Mode (C) = GNSA then
         Contexts.Table (C).Context_I_Options (Idx) :=
           new String'(GNSA_Source.all);
      else
         --  For non-GNSA on the fly compilation we always set -I-
         Contexts.Table (C).Context_I_Options (Idx) :=
            new String'("-I-");
      end if;

   end Set_Search_Paths;

   ---------------------------------------------------
   -- Context Attributes Access and Update Routines --
   ---------------------------------------------------

   function Is_Associated (C : Context_Id) return Boolean is
   begin
      return C /= Non_Associated and then
             Contexts.Table (C).Is_Associated;
   end Is_Associated;

   function Is_Opened (C : Context_Id) return Boolean is
   begin
      return C /= Non_Associated and then
             Contexts.Table (C).Is_Opened;
   end Is_Opened;

   function Opened_At (C : Context_Id) return ASIS_OS_Time is
   begin
      return Contexts.Table (C).Opened_At;
   end Opened_At;

   function Context_Processing_Mode (C : Context_Id) return Context_Mode is
   begin
      return Contexts.Table (C).Mode;
   end Context_Processing_Mode;

   function Tree_Processing_Mode   (C : Context_Id) return Tree_Mode is
   begin
      return Contexts.Table (C).Tree_Processing;
   end Tree_Processing_Mode;

   function Source_Processing_Mode (C : Context_Id) return  Source_Mode is
   begin
      return Contexts.Table (C).Source_Processing;
   end Source_Processing_Mode;

   function Use_Default_Trees       (C : Context_Id) return Boolean is
   begin
      return Contexts.Table (C).Use_Default_Trees;
   end Use_Default_Trees;

   function Gcc_To_Call             (C : Context_Id) return String_Access is
   begin
      return Contexts.Table (C).GCC;
   end Gcc_To_Call;

   --------

   procedure Set_Is_Associated (C : Context_Id; Ass : Boolean) is
   begin
      Contexts.Table (C).Is_Associated := Ass;
   end Set_Is_Associated;

   procedure Set_Is_Opened     (C : Context_Id; Op  : Boolean) is
   begin
      Contexts.Table (C).Is_Opened := Op;
   end Set_Is_Opened;

   procedure Set_Context_Processing_Mode (C : Context_Id; M : Context_Mode) is
   begin
      Contexts.Table (C).Mode := M;
   end Set_Context_Processing_Mode;

   procedure Set_Tree_Processing_Mode   (C : Context_Id; M : Tree_Mode) is
   begin
      Contexts.Table (C).Tree_Processing := M;
   end Set_Tree_Processing_Mode;

   procedure Set_Source_Processing_Mode (C : Context_Id; M :  Source_Mode) is
   begin
      Contexts.Table (C).Source_Processing := M;
   end Set_Source_Processing_Mode;

   procedure Set_Use_Default_Trees       (C : Context_Id; B : Boolean) is
   begin
      Contexts.Table (C).Use_Default_Trees := B;
   end Set_Use_Default_Trees;

   procedure Set_Default_Context_Processing_Mode           (C : Context_Id) is
   begin
      Contexts.Table (C).Mode := All_Trees;
   end Set_Default_Context_Processing_Mode;

   procedure Set_Default_Tree_Processing_Mode   (C : Context_Id) is
   begin
      Contexts.Table (C).Tree_Processing := Pre_Created;
   end Set_Default_Tree_Processing_Mode;

   procedure Set_Default_Source_Processing_Mode (C : Context_Id) is
   begin
      Contexts.Table (C).Source_Processing := All_Sources;
   end Set_Default_Source_Processing_Mode;

-----------------
--  NEW STUFF  --
-----------------

   ----------------------------
   -- Backup_Current_Context --
   ----------------------------

   procedure Backup_Current_Context is
   begin
      if Current_Context /= Nil_Context_Id then
         Save_Context (Current_Context);
      end if;
   end Backup_Current_Context;

   -------------------
   -- Reset_Context --
   -------------------

   procedure Reset_Context (C : Context_Id) is
   begin
      if C = Nil_Context_Id then
         return;
      elsif C /= Current_Context then

         if Is_Opened (Current_Context) then
            Save_Context (Current_Context);
         end if;

         if Is_Opened (C) then
            Restore_Context (C);
         end if;

         Current_Context := C;
         --  we have to do also this:
         Current_Tree := Nil_Tree;
         --  otherwise node/tree access in a new Context may not reset the tree
         --  in case in tree Ids in the old and new Contexts are the same
      end if;
   end Reset_Context;

   ---------------------
   -- Restore_Context --
   ---------------------

   procedure Restore_Context (C : Context_Id) is
   begin
      A_Name_Chars.Restore
         (Contexts.Table (C).Back_Up.Context_Name_Chars);
      Unit_Table.Restore (Contexts.Table (C).Back_Up.Units);
      Tree_Table.Restore (Contexts.Table (C).Back_Up.Trees);

      --  restoring lists tables:
      A4G.A_Elists.Elmts.Restore
         (Contexts.Table (C).Back_Up.Context_Unit_Lists.Saved_Elmts);
      A4G.A_Elists.Elists.Restore
         (Contexts.Table (C).Back_Up.Context_Unit_Lists.Saved_Elists);
   end Restore_Context;

   ------------------
   -- Save_Context --
   ------------------

   procedure Save_Context (C : Context_Id) is
   begin
      if Is_Opened (C) then
         Contexts.Table (C).Back_Up.Context_Name_Chars := A_Name_Chars.Save;
         Contexts.Table (C).Back_Up.Units              := Unit_Table.Save;
         Contexts.Table (C).Back_Up.Trees              := Tree_Table.Save;

         --  saving lists tables:
         Contexts.Table (C).Back_Up.Context_Unit_Lists.Saved_Elmts :=
            A4G.A_Elists.Elmts.Save;
         Contexts.Table (C).Back_Up.Context_Unit_Lists.Saved_Elists :=
            A4G.A_Elists.Elists.Save;
      end if;
   end Save_Context;

   -------------------------
   -- Verify_Context_Name --
   -------------------------

   procedure Verify_Context_Name (Name : String; Cont : Context_Id) is
   begin
      --  no verification is performed now - we simply have no idea, what
      --  and how to verify :-I

      Set_Context_Name (Cont, Name);
   end Verify_Context_Name;

end A4G.Contt;
