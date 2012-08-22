------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . A _ E L I S T S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2007, Free Software Foundation, Inc.       --
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

--  This is the modification of the GNAT Elists package. See spec for the
--  description of the modifications.

with A4G.A_Debug;  use A4G.A_Debug;

with Output;       use Output;

package body A4G.A_Elists is

   ----------------------
   -- Add_To_Elmt_List --
   ----------------------

   procedure Add_To_Elmt_List (Unit : Unit_Id; List : in out Elist_Id) is
   begin
      if No (List) then
         List := New_Elmt_List;
         Append_Elmt (Unit, List);
      elsif not In_Elmt_List (Unit, List) then
         Append_Elmt (Unit, List);
      end if;
   end Add_To_Elmt_List;

   -----------------
   -- Append_Elmt --
   -----------------

   procedure Append_Elmt (Unit : Unit_Id; To : Elist_Id) is
      L : constant Elmt_Id := Elists.Table (To).Last;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Unit := Unit;
      Elmts.Table (Elmts.Last).Next := Union_Id (To);

      if L = No_Elmt then
         Elists.Table (To).First := Elmts.Last;
      else
         Elmts.Table (L).Next := Union_Id (Elmts.Last);
      end if;

      Elists.Table (To).Last  := Elmts.Last;

      if Debug_Flag_N then
         Write_Str ("Append new element Elmt_Id = ");
         Write_Int (Int (Elmts.Last));
         Write_Str (" to list Elist_Id = ");
         Write_Int (Int (To));
         Write_Str (" referencing Unit_Id = ");
         Write_Int (Int (Unit));
         Write_Eol;
      end if;
   end Append_Elmt;

   ------------------
   -- Prepend_Elmt --
   ------------------

   procedure Prepend_Elmt (Unit : Unit_Id; To : Elist_Id) is
      F : constant Elmt_Id := Elists.Table (To).First;

   begin
      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Unit := Unit;

      if F = No_Elmt then
         Elists.Table (To).Last := Elmts.Last;
         Elmts.Table (Elmts.Last).Next := Union_Id (To);
      else
         Elmts.Table (Elmts.Last).Next := Union_Id (F);
      end if;

      Elists.Table (To).First  := Elmts.Last;

   end Prepend_Elmt;

   -----------------------
   -- Insert_Elmt_After --
   -----------------------

   procedure Insert_Elmt_After (Unit : Unit_Id; Elmt : Elmt_Id) is
      N : constant Union_Id := Elmts.Table (Elmt).Next;

   begin

      pragma Assert (Elmt /= No_Elmt);

      Elmts.Increment_Last;
      Elmts.Table (Elmts.Last).Unit := Unit;
      Elmts.Table (Elmts.Last).Next := N;

      Elmts.Table (Elmt).Next := Union_Id (Elmts.Last);

      if N in Elist_Range then
         Elists.Table (Elist_Id (N)).Last := Elmts.Last;
      end if;
   end Insert_Elmt_After;

   -------------
   -- Belongs --
   -------------

   function Belongs (List1 : Elist_Id; List2 : Elist_Id) return Boolean is
      Curr_Elmt : Elmt_Id;
   begin
      if No (List1) or else Is_Empty_Elmt_List (List1) then
         return True;
      end if;

      Curr_Elmt := First_Elmt (List1);

      while Present (Curr_Elmt) loop
         if not In_Elmt_List (Unit (Curr_Elmt), List2) then
            return False;
         end if;
         Curr_Elmt := Next_Elmt (Curr_Elmt);
      end loop;

      return True;

   end Belongs;

   ----------------
   -- First_Elmt --
   ----------------

   function First_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      pragma Assert (List > Elist_Low_Bound);
      return Elists.Table (List).First;
   end First_Elmt;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Elists.Init;
      Elmts.Init;
   end Initialize;

   ------------------
   -- In_Elmt_List --
   ------------------

   function In_Elmt_List (U : Unit_Id; List : Elist_Id) return Boolean is
      Curr_Elmt : Elmt_Id;
   begin
      if No (List) or else Is_Empty_Elmt_List (List) then
         return False;
      end if;

      Curr_Elmt := First_Elmt (List);
      while Present (Curr_Elmt) loop
         if U = Unit (Curr_Elmt) then
            return True;
         end if;
         Curr_Elmt := Next_Elmt (Curr_Elmt);
      end loop;

      return False;

   end In_Elmt_List;

   ---------------
   -- Intersect --
   ---------------

   function Intersect (List1 : Elist_Id; List2 : Elist_Id) return Boolean is
      Curr_Elmt : Elmt_Id;
   begin
      if No (List1) or else  No (List2) or else
         Is_Empty_Elmt_List (List1) or else
         Is_Empty_Elmt_List (List2)
      then
         return False;
      else
         Curr_Elmt := First_Elmt (List1);

         while Present (Curr_Elmt) loop
            if In_Elmt_List (Unit (Curr_Elmt), List2) then
               return True;
            end if;
            Curr_Elmt := Next_Elmt (Curr_Elmt);
         end loop;

         return False;
      end if;
   end Intersect;

   ------------------------
   -- Is_Empty_Elmt_List --
   ------------------------

   function Is_Empty_Elmt_List (List : Elist_Id) return Boolean is
   begin
      return Elists.Table (List).First = No_Elmt;
   end Is_Empty_Elmt_List;

   -------------------
   -- Last_Elist_Id --
   -------------------

   function Last_Elist_Id return Elist_Id is
   begin
      return Elists.Last;
   end Last_Elist_Id;

   ---------------
   -- Last_Elmt --
   ---------------

   function Last_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      return Elists.Table (List).Last;
   end Last_Elmt;

   ------------------
   -- Last_Elmt_Id --
   ------------------

   function Last_Elmt_Id return Elmt_Id is
   begin
      return Elmts.Last;
   end Last_Elmt_Id;

   -----------------
   -- List_Length --
   -----------------

   function List_Length (List : Elist_Id) return Natural is
      Result : Natural := 0;
      Elem   : Elmt_Id;
   begin
      Elem := First_Elmt (List);

      while Present (Elem) loop
         Result := Result + 1;
         Elem   := Next_Elmt (Elem);
      end loop;

      return Result;
   end List_Length;

   ---------------
   -- Move_List --
   ---------------

   procedure Move_List
     (List_From :        Elist_Id;
      List_To   : in out Elist_Id)
   is
   begin
      if No (List_To) then
         List_To := New_Elmt_List;
      end if;

      if No (List_From) or else Is_Empty_Elmt_List (List_From) then
         return;
      end if;

      --  if we are here, we have to move elements...

      if Is_Empty_Elmt_List (List_To) then

         Elists.Table (List_To).Last  := Elists.Table (List_From).Last;
         Elmts.Table (Elists.Table (List_From).Last).Next :=
            Union_Id (List_To);

      else
         Elmts.Table (Elists.Table (List_From).Last).Next :=
            Elmts.Table (Elists.Table (List_To).First).Next;
      end if;

      Elists.Table (List_To).First := Elists.Table (List_From).First;

      Elists.Table (List_From).First := No_Elmt;
      Elists.Table (List_From).Last  := No_Elmt;

   end Move_List;

   -------------------
   -- New_Elmt_List --
   -------------------

   function New_Elmt_List return Elist_Id is
   begin
      Elists.Increment_Last;
      Elists.Table (Elists.Last).First := No_Elmt;
      Elists.Table (Elists.Last).Last  := No_Elmt;

      if Debug_Flag_N then
         Write_Str ("Allocate new element list, returned ID = ");
         Write_Int (Int (Elists.Last));
         Write_Eol;
      end if;

      return Elists.Last;
   end New_Elmt_List;

   ---------------
   -- Next_Elmt --
   ---------------

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id is
      N : constant Union_Id := Elmts.Table (Elmt).Next;

   begin
      if N in Elist_Range then
         return No_Elmt;
      else
         return Elmt_Id (N);
      end if;
   end Next_Elmt;

   --------
   -- No --
   --------

   function No (List : Elist_Id) return Boolean is
   begin
      return List = No_Elist;
   end No;

   function No (Elmt : Elmt_Id) return Boolean is
   begin
      return Elmt = No_Elmt;
   end No;

   -----------
   -- Unit --
   -----------

   function Unit (Elmt : Elmt_Id) return Unit_Id is
   begin
      if Elmt = No_Elmt then
         return Nil_Unit;
      else
         return Elmts.Table (Elmt).Unit;
      end if;
   end Unit;

   ----------------
   -- Num_Elists --
   ----------------

   function Num_Elists return Nat is
   begin
      return Int (Elmts.Last) - Int (Elmts.First) + 1;
   end Num_Elists;

   -------------
   -- Present --
   -------------

   function Present (List : Elist_Id) return Boolean is
   begin
      return List /= No_Elist;
   end Present;

   function Present (Elmt : Elmt_Id) return Boolean is
   begin
      return Elmt /= No_Elmt;
   end Present;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (List : Elist_Id) is
      Curr_Elmt : Elmt_Id;
      Counter   : Int := 1;
   begin
      if No (List) then
         if Debug_Flag_N then
            Write_Str ("   There is no list here");
            Write_Eol;
         end if;
         return;
      end if;

      if Is_Empty_Elmt_List (List) then
         if Debug_Flag_N then
            Write_Str ("   The list is empty");
            Write_Eol;
         end if;
         return;
      end if;

      if Debug_Flag_N then
         Write_Str ("List contains the following Ids:");
         Write_Eol;
      end if;

      Curr_Elmt := First_Elmt (List);
      while Present (Curr_Elmt) loop
         if Debug_Flag_N then
            Write_Str ("   Element number ");
            Write_Int (Counter);
            Write_Str (" is ");
            Write_Int (Int (Unit (Curr_Elmt)));
            Write_Eol;
         end if;
         Curr_Elmt := Next_Elmt (Curr_Elmt);
         Counter   := Counter + 1;
      end loop;

   end Print_List;

   -----------------
   -- Remove_Elmt --
   -----------------

   procedure Remove_Elmt (List : Elist_Id; Elmt : Elmt_Id) is
      Nxt : Elmt_Id;
      Prv : Elmt_Id;

   begin
      Nxt := Elists.Table (List).First;

      --  Case of removing only element in the list

      if Elmts.Table (Nxt).Next in Elist_Range then

         pragma Assert (Nxt = Elmt);

         Elists.Table (List).First := No_Elmt;
         Elists.Table (List).Last  := No_Elmt;

      --  Case of removing the first element in the list

      elsif Nxt = Elmt then
         Elists.Table (List).First := Elmt_Id (Elmts.Table (Nxt).Next);

      --  Case of removing second or later element in the list

      else
         loop
            Prv := Nxt;
            Nxt := Elmt_Id (Elmts.Table (Prv).Next);
            exit when Nxt = Elmt
              or else Elmts.Table (Nxt).Next in Elist_Range;
         end loop;

         pragma Assert (Nxt = Elmt);

         Elmts.Table (Prv).Next := Elmts.Table (Nxt).Next;

         if Elmts.Table (Prv).Next in Elist_Range then
            Elists.Table (List).Last := Prv;
         end if;
      end if;
   end Remove_Elmt;

   ----------------------
   -- Remove_Last_Elmt --
   ----------------------

   procedure Remove_Last_Elmt (List : Elist_Id) is
      Nxt : Elmt_Id;
      Prv : Elmt_Id;

   begin
      Nxt := Elists.Table (List).First;

      --  Case of removing only element in the list

      if Elmts.Table (Nxt).Next in Elist_Range then
         Elists.Table (List).First := No_Elmt;
         Elists.Table (List).Last  := No_Elmt;

      --  Case of at least two elements in list

      else
         loop
            Prv := Nxt;
            Nxt := Elmt_Id (Elmts.Table (Prv).Next);
            exit when Elmts.Table (Nxt).Next in Elist_Range;
         end loop;

         Elmts.Table (Prv).Next   := Elmts.Table (Nxt).Next;
         Elists.Table (List).Last := Prv;
      end if;
   end Remove_Last_Elmt;

   ------------------
   -- Replace_Elmt --
   ------------------

   procedure Replace_Elmt (Elmt : Elmt_Id; New_Unit : Unit_Id) is
   begin
      Elmts.Table (Elmt).Unit := New_Unit;
   end Replace_Elmt;

end A4G.A_Elists;
