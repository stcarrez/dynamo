------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . A _ D E B U G                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-2005, Free Software Foundation, Inc.       --
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

package body A4G.A_Debug is

   ---------------------------------
   -- Summary of Debug Flag Usage --
   ---------------------------------

   --  da   Generate messages when working with normalized associations
   --  db
   --  dc   Generate messages from Context Table during finalization
   --  dd   Dynamic allocation of tables messages generated
   --  de
   --  df
   --  dg
   --  dh
   --  di   Turns off including the Element location into its Debug_Image
   --  dj
   --  dk
   --  dl   Generate debug output when converting node lists into element lists
   --  dm
   --  dn   Generate messages for list allocation
   --  do   Generate messages when opening a Context
   --  dp
   --  dq
   --  dr
   --  ds   All the debug output related to the semantic queries
   --  dt   Generate messages when a tree file is read in
   --  du
   --  dv   Generate messages when checking the validity of ASIS abstractions
   --  dw
   --  dx   Generate debug messages from inside Asis.Text routines
   --  dy
   --  dz

   --  d1   Generate the debug output for tree fragments being traversed
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   ----------------------------------------
   -- Documentation for ASIS Debug Flags --
   ----------------------------------------

   --  da   When a list of normalized ASIS association elements is created
   --       or a normalized association is further decomposed, messages
   --       representing some "control points" of this process are generated.
   --
   --  db
   --  dc   When ASIS implementation is finalized (by calling to
   --       A4G.Finalize), the content of the main
   --       Context Table and its subtables is outputted

   --  dd   Dynamic allocation of tables messages generated. Each time a
   --       table is reallocated, a line is output indicating the expansion.

   --  de
   --  df
   --  dg
   --  dh   In GNAT this flag generates a table at the end of a compilation
   --       showing how the hash table chains built by the Namet package are
   --       loaded. This is useful in ensuring that the hashing algorithm
   --       (in Namet.Hash) is working effectively with typical sets of
   --       program identifiers. In ASIS the corresponding feature is not
   --       implemented yet. The idea is to see if the hashing algorithm
   --       is working effectively with the typical set of normalized
   --       unit names

   --  di   Turns off including the Element location into its Debug_Image,
   --       this is helpful when Asis.Text queries do not work properly
   --       because of structural queries misfunction.

   --  dj
   --  dk
   --  dl   Generate debug output when converting node lists into element
   --       lists. For every tree node traversing during the list conversion
   --       some information about the node is outputted
   --  dm
   --  dn   Generate messages for list allocation. Each time a list header is
   --       allocated, a line of output is generated.

   --  do   Generate messages when opening a Context in
   --       "use pre-created trees" mode
   --  dp
   --  dq
   --  dr
   --  ds   Various debug messages related to the semantic queries
   --       implemented for now are generated

   --  dt   Generate messages when a tree file is read in during processing
   --       ASIS queries
   --  du
   --  dv   Generate messages when checking the validity of ASIS Elements,
   --       Compilation_Units, Contexts. The idea is to show, why a given
   --       ASIS abstraction is considered as being invalid

   --  dw
   --  dx   Generate debug messages from inside the routines involved in the
   --       implementation of Asis.Text
   --  dy
   --  dz

   --  d1   Generate the debug output for tree fragments being traversed
   --       when processing ASIS queries (usially - the subtrees rooted by
   --       argument's pr result's node
   --
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig is Character range '1' .. '9';
      subtype Let is Character range 'a' .. 'z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;

      else
         case Let (C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;
      end if;
   end Set_Debug_Flag;

   -------------
   -- Set_Off --
   -------------

   procedure Set_Off is
   begin
      Debug_Flag_1 := False;
      Debug_Flag_2 := False;
      Debug_Flag_3 := False;
      Debug_Flag_4 := False;
      Debug_Flag_5 := False;
      Debug_Flag_6 := False;
      Debug_Flag_7 := False;
      Debug_Flag_8 := False;
      Debug_Flag_9 := False;

      Debug_Flag_A := False;
      Debug_Flag_B := False;
      Debug_Flag_C := False;
      Debug_Flag_D := False;
      Debug_Flag_E := False;
      Debug_Flag_F := False;
      Debug_Flag_G := False;
      Debug_Flag_H := False;
      Debug_Flag_I := False;
      Debug_Flag_J := False;
      Debug_Flag_K := False;
      Debug_Flag_L := False;
      Debug_Flag_M := False;
      Debug_Flag_N := False;
      Debug_Flag_O := False;
      Debug_Flag_P := False;
      Debug_Flag_Q := False;
      Debug_Flag_R := False;
      Debug_Flag_S := False;
      Debug_Flag_T := False;
      Debug_Flag_U := False;
      Debug_Flag_V := False;
      Debug_Flag_W := False;
      Debug_Flag_X := False;
      Debug_Flag_Y := False;
      Debug_Flag_Z := False;

      Debug_Mode   := False;
      Debug_Lib_Model := False; -- TEMPORARY SOLUTION, SHOULD BE DROPPED!!!
   end Set_Off;

   ------------
   -- Set_On --
   ------------

   procedure Set_On is
   begin
      Debug_Flag_1 := True;
      Debug_Flag_2 := True;
      Debug_Flag_3 := True;
      Debug_Flag_4 := True;
      Debug_Flag_5 := True;
      Debug_Flag_6 := True;
      Debug_Flag_7 := True;
      Debug_Flag_8 := True;
      Debug_Flag_9 := True;

      Debug_Flag_A := True;
      Debug_Flag_B := True;
      Debug_Flag_C := True;
      Debug_Flag_D := True;
      Debug_Flag_E := True;
      Debug_Flag_F := True;
      Debug_Flag_G := True;
      Debug_Flag_H := True;
      Debug_Flag_I := True;
      Debug_Flag_J := True;
      Debug_Flag_K := True;
      Debug_Flag_L := True;
      Debug_Flag_M := True;
      Debug_Flag_N := True;
      Debug_Flag_O := True;
      Debug_Flag_P := True;
      Debug_Flag_Q := True;
      Debug_Flag_R := True;
      Debug_Flag_S := True;
      Debug_Flag_T := True;
      Debug_Flag_U := True;
      Debug_Flag_V := True;
      Debug_Flag_W := True;
      Debug_Flag_X := True;
      Debug_Flag_Y := True;
      Debug_Flag_Z := True;

      Debug_Mode      := True; -- TEMPORARY SOLUTION, SHOULD BE DROPPED!!!
      Debug_Lib_Model := True; -- TEMPORARY SOLUTION, SHOULD BE DROPPED!!!
   end Set_On;

end A4G.A_Debug;
