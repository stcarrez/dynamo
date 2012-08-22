------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                    A S I S . T E X T . S E T _ G E T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2008, Free Software Foundation, Inc.       --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis.Set_Get;            use Asis.Set_Get;

with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.TT;            use A4G.Contt.TT;

with Sinput;                  use Sinput;
with Widechar;                use Widechar;

package body Asis.Text.Set_Get is

   ---------
   -- Get --
   ---------

   function Line_Length (L : Line) return Character_Position is
   begin
      return L.Length;
   end Line_Length;

   function Line_Location (L : Line) return Source_Ptr is
   begin
      if not (L.Enclosing_Context = Get_Current_Cont and then
              L.Enclosing_Tree    = Get_Current_Tree)
      then
         Reset_Tree (L.Enclosing_Context, L.Enclosing_Tree);
      end if;

      return L.Sloc;
   end Line_Location;

   function Valid (L : Line) return Boolean is
   begin
      return Is_Opened (L.Enclosing_Context) and then
             Later (Opened_At (L.Enclosing_Context), L.Obtained);
   end Valid;

   ------------------------
   -- Debug_Image (Span) --
   ------------------------

   function Debug_Image (The_Span : Span) return String is
      LT : String renames ASIS_Line_Terminator;
   begin
      return    LT
            & "Debug image for Asis.Text.Span:"
            & LT
            & "First Line   : "
            & Int'Image (Int (The_Span.First_Line))
            & LT
            & "First Column : "
            & Int'Image (Int (The_Span.First_Column))
            & LT
            & "Last Line    : "
            & Int'Image (Int (The_Span.Last_Line))
            & LT
            & "Last Column  : "
            & Int'Image (Int (The_Span.Last_Column))
            & LT
            & LT;
   end Debug_Image;

   ---------------------
   -- Line_Wide_Image --
   ---------------------

   function Line_Wide_Image (L : Line) return Wide_String is
   begin
      if Line_Length (L) = 0 then
         return "";
      end if;

      declare
         Result : Wide_String (1 .. Line_Length (L));

         S       : Source_Ptr := L.Sloc;
         SFI     : constant Source_File_Index := Get_Source_File_Index (S);
         Src     : constant Source_Buffer_Ptr := Source_Text (SFI);
         Next_Ch : Char_Code;
         Success : Boolean;
         pragma Unreferenced (Success);
      begin

         for J in Result'Range loop

               if Is_Start_Of_Wide_Char_For_ASIS (Src, S, L.Comment_Sloc) then
                  Scan_Wide (Src, S, Next_Ch, Success);

                  Result (J) := Wide_Character'Val (Next_Ch);

               else
                  Result (J) := To_Wide_Character (Src (S));
                  S := S + 1;
               end if;

         end loop;

         return Result;
      end;

   end Line_Wide_Image;

   ---------
   -- Set --
   ---------

   procedure Set_Line_Length (L : in out Line; N : Character_Position) is
   begin
      L.Length := N;
   end Set_Line_Length;

   procedure Set_Line_Location (L : in out Line; S : Source_Ptr) is
   begin
      L.Rel_Sloc := L.Rel_Sloc + (S - L.Sloc);
      L.Sloc := S;
   end Set_Line_Location;

   ---------------
   -- Set_Lines --
   ---------------

   procedure Set_Lines (LList : in out Line_List; El : Element) is
      First_Line : constant Line_Number := LList'First;
      Last_Line  : constant Line_Number := LList'Last;
      El_Sloc    : constant Source_Ptr  := Location (El);
      --  This call to Location resets the tree for El, if needed;
      --  and this makes all the routine "tree-swapping-safe"
      Sloc_Move : constant Source_Ptr   := Rel_Sloc (El) - El_Sloc;
      --  Sloc_Move in fact is equal to - Sloc (Top (Enclosing_CU)),
      --  so by adding Sloc_Move we can get relative Sloc for lines:

      --  We define local variables for Element characteristics in order
      --  not to compute this in the loop:
      El_Encl_Unit : constant Unit_Id      := Encl_Unit_Id (El);
      El_Encl_Cont : constant Context_Id   := Encl_Cont_Id (El);
      El_Encl_Tree : constant Tree_Id      := Encl_Tree    (El);
      El_Obtained  : constant ASIS_OS_Time := Obtained     (El);

      SFI : constant Source_File_Index := Get_Source_File_Index (El_Sloc);
      Src_First : constant Source_Ptr := Source_First (SFI);
      Src       : constant Source_Buffer_Ptr := Source_Text (SFI);

      S           : Source_Ptr;
      Wide_Length : Character_Position;
      --  Line length counted in wide characters

      Comment_Pos : Source_Ptr;
      --  Start of the comment in the next line image. Zero if there is no
      --  comment as a part of the line image
   begin
      --  the only thing which requires special processing is
      --  setting of the length of the last Line in LList if
      --  this Line corresponds to the last line in the compilation
      --  containing El.

      --  We start from settings which do not require any
      --  special processing. We take from Element all which can
      --  be safely "transferred" into Lines. Note, that we know,
      --  that El is valid, that is, the Context from which it had been
      --  obtained was not closed after obtaining this Element. So we
      --  simply copy the time when El was obtained in all the Lines
      --  in Line list
      for LN in First_Line .. Last_Line loop
         LList (LN).Enclosing_Unit    := El_Encl_Unit;
         LList (LN).Enclosing_Context := El_Encl_Cont;
         LList (LN).Enclosing_Tree    := El_Encl_Tree;
         LList (LN).Obtained          := El_Obtained;

         LList (LN).Sloc     := Line_Start (Physical_Line_Number (LN), SFI);
         LList (LN).Rel_Sloc := LList (LN).Sloc + Sloc_Move;
      end loop;

      --  Counting Line lengths. Firtst we count length in the internal
      --  representation:
      for LN in First_Line .. Last_Line - 1 loop
         S := LList (LN + 1).Sloc - 1;

         while S > Src_First
            and then
               Is_EOL_Char (Src (S))
            and then
               S >= LList (LN).Sloc
         loop
            S := S - 1;
         end loop;

         if S = Src_First then
            --  Empty lines in the beginning of a source file
            LList (LN).Length := 0;
         else
            LList (LN).Length :=
               Character_Position (S - LList (LN).Sloc + 1);
         end if;

      end loop;

      --  The special case of the last Line in the list:
      S := LList (Last_Line).Sloc;

      while S < Source_Last (SFI)
         and then
            not Is_EOL_Char (Src (S))
      loop
         S := S + 1;
      end loop;

      LList (Last_Line).Length :=
         Character_Position (S - LList (Last_Line).Sloc);

      --  Recompute line lengths, taking into account possible encodings of
      --  upper half characters. The result should be the lengths of the lines
      --  in the original source

      for LN in First_Line .. Last_Line loop

         if LList (LN).Length /= 0 then
            Wide_Length := 0;
            S           := LList (LN).Sloc;
            Comment_Pos := Comment_Beginning
              (Src (S .. S + Source_Ptr (LList (LN).Length) - 1));

            LList (LN).Comment_Sloc := Comment_Pos;

            while S < LList (LN).Sloc + Source_Ptr (LList (LN).Length) loop

               if Is_Start_Of_Wide_Char_For_ASIS (Src, S, Comment_Pos) then
                  Skip_Wide_For_ASIS (Src, S);
               else
                  S := S + 1;
               end if;

               Wide_Length := Wide_Length + 1;
            end loop;

            LList (LN).Length := Wide_Length;
         end if;
      end loop;

   end Set_Lines;

end Asis.Text.Set_Get;
