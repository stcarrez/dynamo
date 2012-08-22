------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . U _ C O N V                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
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

with Namet;                   use Namet;
with Fname;                   use Fname;
with Krunch;
with Opt;                     use Opt;

package body A4G.U_Conv is

   ---------------------------------
   -- Local Types and Subprograms --
   ---------------------------------

   --  We use the trivial finite state automata to analyse and to transform
   --  strings passed as parameters to ASIS interfaces and processed by ASIS
   --  itself below there are type and routines definitions for various
   --  versions of this automata

   type State is (Beg_Ident, Mid_Ident, Und_Line);
   --  The states of the automata. Some versions may use only a part of the
   --  whole set of states.

   procedure Normalize_Char (In_Char    :        Character;
                             Curr_State : in out State;
                             Out_Char   : out    Character;
                             OK         : out    Boolean);
   --  One step of the finite-state-automata analyzing the string which is
   --  supposed to be an Ada unit name and producind the "normalized"
   --  version of the name. If In_Char under in the state Curr_State may be
   --  considered as belonging to the Ada unit name, the "low-case version"
   --  of this character is assigned to Out_Char, and OK is ste True,
   --  otherwise OK is set false

   function Convert_Char (Ch : Character) return Character;
   --  performs upper case -> lover case conversion in the GNAT file
   --  name style (see GNAT Document INTRO and Fnames.ads - only letters
   --  from the A .. Z range are folded to lower case)

   ------------------
   -- Convert_Char --
   ------------------

   function Convert_Char (Ch : Character) return Character is
   begin

      if Ch = '.' then
         return '-';
      else
         return To_Lower (Ch);
      end if;

   end Convert_Char;

   ------------------------
   -- Get_Norm_Unit_Name --
   ------------------------

   procedure Get_Norm_Unit_Name
     (U_Name           :     String;
      N_U_Name         : out String;
      Spec             :     Boolean;
      May_Be_Unit_Name : out Boolean)
   is
      Current_State : State := Beg_Ident;
   begin

      May_Be_Unit_Name := False;

      for I in U_Name'Range loop

         Normalize_Char (U_Name   (I), Current_State,
                         N_U_Name (I), May_Be_Unit_Name);

         exit when not May_Be_Unit_Name;

      end loop;

      if not May_Be_Unit_Name then
         return;

      elsif N_U_Name (U_Name'Last) = '_' or else
            N_U_Name (U_Name'Last) = '.'
      then
         --  something like "Ab_" -> "ab_" or "Ab_Cd." -> "ab_cd."
         May_Be_Unit_Name := False;
         return;
      end if;

      --  here we have all the content of U_Name parced and
      --  May_Be_Unit_Name is True. All we have to do is to append
      --  the "%s" or "%b" suffix

      N_U_Name (N_U_Name'Last - 1) := '%';

      if Spec then
         N_U_Name (N_U_Name'Last) := 's';
      else
         N_U_Name (N_U_Name'Last) := 'b';
      end if;

   end Get_Norm_Unit_Name;

   -----------------------------
   -- Is_Predefined_File_Name --
   -----------------------------

   function Is_Predefined_File_Name (S : String_Access) return Boolean is
   begin
      Namet.Name_Len := S'Length - 1;
      --  "- 1" is for trailing ASCII.NUL in the file name
      Namet.Name_Buffer (1 .. Namet.Name_Len) := To_String (S);
      return Fname.Is_Predefined_File_Name (Namet.Name_Enter);
   end Is_Predefined_File_Name;

   --------------------
   -- Normalize_Char --
   --------------------

   procedure Normalize_Char
     (In_Char    :        Character;
      Curr_State : in out State;
      Out_Char   : out    Character;
      OK         : out    Boolean)
   is
   begin

      OK := True;

      case Curr_State is

         when Beg_Ident =>

            if Is_Letter (In_Char) then
               Curr_State := Mid_Ident;
            else
               OK := False;
            end if;

         when Mid_Ident =>

            if Is_Letter (In_Char) or else
               Is_Digit (In_Char)
            then
               null;
            elsif In_Char = '_' then
               Curr_State := Und_Line;
            elsif In_Char = '.' then
               Curr_State := Beg_Ident;
            else
               OK := False;
            end if;

         when Und_Line =>
            if Is_Letter (In_Char) or else
               Is_Digit  (In_Char)
            then
               Curr_State := Mid_Ident;
            else
               OK := False;
            end if;

      end case;

      Out_Char := To_Lower (In_Char);

   end Normalize_Char;

   ---------------------------
   -- Source_From_Unit_Name --
   ---------------------------

   function Source_From_Unit_Name
     (S    : String;
      Spec : Boolean)
      return String_Access
   is
      Result_Prefix   : String (1 .. S'Length);
      Result_Selector : String (1 .. 4) := ".adb";

      Initial_Length  : constant Natural := S'Length;
      Result_Length   : Natural          := Initial_Length;
      --  this is for the name krunching
   begin
      for I in S'Range loop
         Result_Prefix (I) := Convert_Char (S (I));
      end loop;

      Krunch
       (Buffer    => Result_Prefix,
        Len       => Result_Length,
        Maxlen    => Integer (Maximum_File_Name_Length),
        No_Predef => False);

      if Spec then
         Result_Selector (4) := 's';
      end if;

      return new String'(Result_Prefix (1 .. Result_Length)
                       & Result_Selector
                       & ASCII.NUL);

   end Source_From_Unit_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : String_Access) return String is
   begin
      return S.all (S'First .. S'Last - 1);
   end To_String;

   ---------------------------
   -- Tree_From_Source_Name --
   ---------------------------

   function Tree_From_Source_Name (S : String_Access) return String_Access is
      Return_Val : String_Access;
   begin
      Return_Val := new String'(S.all);
      --  the content of S should be "*.ad?" & ASCII.NUL
      Return_Val (Return_Val'Last - 1) := 't'; -- ".ad?" -> ".adt"
      return Return_Val;
   end Tree_From_Source_Name;

end A4G.U_Conv;
