------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . D E F A U L T S                         --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
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

with Unchecked_Deallocation;

with A4G.A_Osint; use A4G.A_Osint;
with A4G.U_Conv;  use A4G.U_Conv;

with Output;      use Output;

package body A4G.Defaults is

   procedure Free_String is new Unchecked_Deallocation
     (String, String_Access);

   procedure Add_Src_Search_Dir (Dir : String);
   --  Add Dir at the end of the default source file search path.

   procedure Add_Lib_Search_Dir (Dir : String);
   --  Add Dir at the end of the default library (=object+ALI) file search
   --  path.

   ------------------------
   -- Add_Lib_Search_Dir --
   ------------------------

   procedure Add_Lib_Search_Dir (Dir : String) is
   begin
      ASIS_Lib_Search_Directories.Increment_Last;
      ASIS_Lib_Search_Directories.Table (ASIS_Lib_Search_Directories.Last) :=
        new String'(Normalize_Directory_Name (Dir));
   end Add_Lib_Search_Dir;

   ------------------------
   -- Add_Src_Search_Dir --
   ------------------------

   procedure Add_Src_Search_Dir (Dir : String) is
   begin
      ASIS_Src_Search_Directories.Increment_Last;
      ASIS_Src_Search_Directories.Table (ASIS_Src_Search_Directories.Last) :=
        new String'(Normalize_Directory_Name (Dir));
   end Add_Src_Search_Dir;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      --  finalise ASIS_Src_Search_Directories:
      for I in First_Dir_Id .. ASIS_Src_Search_Directories.Last loop
         Free_String (ASIS_Src_Search_Directories.Table (I));
      end loop;

      --  finalize ASIS_Lib_Search_Directories:
      for I in First_Dir_Id .. ASIS_Lib_Search_Directories.Last loop
         Free_String (ASIS_Lib_Search_Directories.Table (I));
      end loop;

      --  finalize ASIS_Tree_Search_Directories
      for I in First_Dir_Id .. ASIS_Tree_Search_Directories.Last loop
         Free_String (ASIS_Tree_Search_Directories.Table (I));
      end loop;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Search_Path : String_Access;
   begin
      --  just in case:
      Finalize;

      ASIS_Src_Search_Directories.Init;
      ASIS_Lib_Search_Directories.Init;
      ASIS_Tree_Search_Directories.Init;

      --  stroring the defaults for: the code is stolen from Osint
      --  (body, rev. 1.147) and then adjusted

      for Dir_kind in Search_Dir_Kinds loop

         case Dir_kind is
            when Source =>
               Search_Path := Getenv ("ADA_INCLUDE_PATH");
            when Object =>
               Search_Path := Getenv ("ADA_OBJECTS_PATH");
            when Tree =>
               --  There is no environment variable for separate
               --  tree path at the moment;
               exit;
         end case;

         if Search_Path'Length > 0 then
            declare
               Lower_Bound : Positive := 1;
               Upper_Bound : Positive;

            begin
               loop
                  while Lower_Bound <= Search_Path'Last
                    and then Search_Path.all (Lower_Bound) =
                    ASIS_Path_Separator
                  loop
                     Lower_Bound := Lower_Bound + 1;
                  end loop;

                  exit when Lower_Bound > Search_Path'Last;

                  Upper_Bound := Lower_Bound;
                  while Upper_Bound <= Search_Path'Last
                    and then Search_Path.all (Upper_Bound) /=
                    ASIS_Path_Separator
                  loop
                     Upper_Bound := Upper_Bound + 1;
                  end loop;

                  case Dir_kind is
                     when Source =>
                        Add_Src_Search_Dir
                          (Search_Path.all (Lower_Bound .. Upper_Bound - 1));
                     when Object =>
                        Add_Lib_Search_Dir
                          (Search_Path.all (Lower_Bound .. Upper_Bound - 1));
                     when Tree =>
                        exit; --  non implemented yet;
                  end case;

                  Lower_Bound := Upper_Bound + 1;
               end loop;
            end;
         end if;
      end loop;

      --  ???  TEMPORARY SOLUTION: the default objects search path
      --  is also used as the default tree path

      for J in First_Dir_Id .. ASIS_Lib_Search_Directories.Last loop

         ASIS_Tree_Search_Directories.Increment_Last;

         ASIS_Tree_Search_Directories.Table
           (ASIS_Tree_Search_Directories.Last) :=
            new String'(ASIS_Lib_Search_Directories.Table (J).all);

      end loop;

      Free (Search_Path);

   end Initialize;

   -------------------------
   -- Locate_Default_File --
   -------------------------

   function Locate_Default_File
     (File_Name : String_Access;
      Dir_Kind  : Search_Dir_Kinds)
      return String_Access
   is
      function Is_Here_In_Src  (File_Name : String_Access; Dir : Dir_Id)
         return Boolean;
      function Is_Here_In_Lib  (File_Name : String_Access; Dir : Dir_Id)
         return Boolean;
      --  funtion Is_Here_In_Tree (File_Name : String_Access; Dir : Dir_Id)
      --     return Boolean;

      function Is_Here_In_Src  (File_Name : String_Access; Dir : Dir_Id)
         return Boolean
      is
      begin
         return Is_Regular_File (ASIS_Src_Search_Directories.Table (Dir).all
                               & To_String (File_Name));
      end Is_Here_In_Src;

      function Is_Here_In_Lib  (File_Name : String_Access; Dir : Dir_Id)
         return Boolean
      is
      begin
         return Is_Regular_File (ASIS_Lib_Search_Directories.Table (Dir).all
                               & To_String (File_Name));
      end Is_Here_In_Lib;

   begin
      case Dir_Kind is

         when Source =>
            for Dir in First_Dir_Id .. ASIS_Src_Search_Directories.Last loop
               if Is_Here_In_Src (File_Name, Dir) then
                  return new String'
                       (ASIS_Src_Search_Directories.Table (Dir).all
                      & File_Name.all);
               end if;
            end loop;

         when Object =>
            for Dir in First_Dir_Id .. ASIS_Lib_Search_Directories.Last loop
               if Is_Here_In_Lib (File_Name, Dir) then
                  return new String'
                       (ASIS_Lib_Search_Directories.Table (Dir).all
                      & File_Name.all);
               end if;
            end loop;
         when Tree =>
            null; --  non implemented yet;
      end case;

      return null;
   end Locate_Default_File;

   ------------------------
   -- Print_Lib_Defaults --
   ------------------------

   procedure Print_Lib_Defaults is
   begin
      if ASIS_Lib_Search_Directories.Last < First_Dir_Id then
         Write_Str ("   No default library files search path");
         Write_Eol;
      else
         for Dir in First_Dir_Id .. ASIS_Lib_Search_Directories.Last loop
            Write_Str ("   " & ASIS_Lib_Search_Directories.Table (Dir).all);
            Write_Eol;
         end loop;
      end if;
   end Print_Lib_Defaults;

   ---------------------------
   -- Print_Source_Defaults --
   ---------------------------

   procedure Print_Source_Defaults is
   begin
      if ASIS_Src_Search_Directories.Last < First_Dir_Id then
         Write_Str ("   No default source search path");
         Write_Eol;
      else
         for Dir in First_Dir_Id .. ASIS_Src_Search_Directories.Last loop
            Write_Str ("   " & ASIS_Src_Search_Directories.Table (Dir).all);
            Write_Eol;
         end loop;
      end if;
   end Print_Source_Defaults;

   -------------------------
   -- Print_Tree_Defaults --
   -------------------------

   procedure Print_Tree_Defaults is
   begin
      if ASIS_Tree_Search_Directories.Last < First_Dir_Id then
         Write_Str ("   No default tree files search path");
         Write_Eol;
      else
         for Dir in First_Dir_Id .. ASIS_Tree_Search_Directories.Last loop
            Write_Str ("   " & ASIS_Tree_Search_Directories.Table (Dir).all);
            Write_Eol;
         end loop;
      end if;
   end Print_Tree_Defaults;

end A4G.Defaults;
