------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ O S I N T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-1999, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  Ada Core Technologies Inc --
-- (http://www.gnat.com).                                                   --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;

package body A4G.A_Osint is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Free_String is new Unchecked_Deallocation (String, String_Access);

   procedure Free_List is new Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   ------------------------
   -- Free_Argument_List --
   ------------------------

   procedure Free_Argument_List (List : in out Argument_List_Access) is
   begin
      if List = null then
         return;
      end if;

      for J in List'Range loop
         Free_String (List (J));
      end loop;

      Free_List (List);
   end Free_Argument_List;

   ------------------------------
   -- Get_Max_File_Name_Length --
   ------------------------------

   function Get_Max_File_Name_Length return Int is
      function Get_Maximum_File_Name_Length return Int;
      pragma Import (C, Get_Maximum_File_Name_Length,
                    "__gnat_get_maximum_file_name_length");
      --  This function does what we want, but it returns -1 when there
      --  is no restriction on the file name length
      --
      --  The implementation has been "stolen" from the body of GNAT
      --  Osint.Initialize

   begin
      if Get_Maximum_File_Name_Length = -1 then
         return Int'Last;
      else
         return Get_Maximum_File_Name_Length;
      end if;
   end Get_Max_File_Name_Length;

   ------------------------------
   -- Normalize_Directory_Name --
   ------------------------------

   function Normalize_Directory_Name
     (Directory : String)
      return String
   is
   begin
      --  For now this just insures that the string is terminated with
      --  the directory separator character. Add more later?

      if Directory (Directory'Last) = Directory_Separator then
         return Directory;
      elsif Directory'Length = 0 then
         --  now we do not need this, but it is no harm to keep it
         return '.' & Directory_Separator;
      else
         return Directory & Directory_Separator;
      end if;
   end Normalize_Directory_Name;

end A4G.A_Osint;
