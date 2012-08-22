------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--           A S I S . D A T A _ D E C O M P O S I T I O N . D E B U G      --
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

with Asis.Elements;           use Asis.Elements;
with Asis.Declarations;       use Asis.Declarations;

with A4G.A_Output;            use A4G.A_Output;
with A4G.A_Types;

with Asis.Data_Decomposition.Aux;
with Asis.Data_Decomposition.Set_Get;

package body Asis.Data_Decomposition.Debug is

   LT : String renames A4G.A_Types.ASIS_Line_Terminator;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Rec_Comp_Debug_String (RC : Record_Component);
   procedure Arr_Comp_Debug_String (AC : Array_Component);
   --  Form the Content of the Debug_Buffer

   ---------------------------
   -- Arr_Comp_Debug_String --
   ---------------------------

   procedure Arr_Comp_Debug_String (AC : Array_Component) is
      Parent_Type_Image : String_Ptr;
   begin
      Debug_Buffer_Len := 0;
      if Is_Nil (AC) then
         Add ("This is a Nil Record Component");
         return;
      end if;

      Parent_Type_Image :=
         new String'(To_String (Debug_Image (AC.Parent_Array_Type)));

      Debug_Buffer_Len := 0;

      Add (LT);

      Add ("Parent_Array_Type");
      Add (LT);
      Add (Parent_Type_Image.all);
      Free (Parent_Type_Image);
      Add (LT);
      Add (LT);

      Add ("Parent_Component_Name: ");
      if Is_Nil (AC.Parent_Component_Name) then
         Add ("Is Nil");
      else
         Add (To_String (Defining_Name_Image (AC.Parent_Component_Name)));
      end if;
      Add (LT);

      if AC.Is_Record_Comp then
         Add (">>> IS RECORD");
         Add (LT);
      elsif AC.Is_Array_Comp then
         Add (">>> IS ARRAY");
         Add (LT);
      end if;

      Add ("Position      :");
      Add (ASIS_Natural'Image (AC.Position));
      Add (LT);

      Add ("First_Bit     :");
      Add (ASIS_Natural'Image (AC.First_Bit));
      Add (LT);

      Add ("Last_Bit      :");
      Add (ASIS_Natural'Image (AC.Last_Bit));
      Add (LT);

      Add ("Size          :");
      Add (ASIS_Natural'Image (AC.Size));
      Add (LT);

      Add ("Dimension     :");
      Add (ASIS_Natural'Image (AC.Dimension));
      Add (LT);

      for I in 1 .. AC.Dimension loop
         Add ("   Length (");
         Add (ASIS_Natural'Image (I));
         Add ("):");
         Add (ASIS_Natural'Image (AC.Length (I)));
         Add (LT);

      end loop;

   end Arr_Comp_Debug_String;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (RC : Record_Component) return Wide_String is
   begin

      Rec_Comp_Debug_String (RC);

      return To_Wide_String (
         LT & "Record Component Debug_Image: " & LT &
         Debug_Buffer (1 .. Debug_Buffer_Len));

   end Debug_Image;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (AC : Array_Component) return Wide_String is
   begin

      Arr_Comp_Debug_String (AC);

      return To_Wide_String (
         LT & "Array Component Debug_Image: " & LT &
         Debug_Buffer (1 .. Debug_Buffer_Len));

   end Debug_Image;

   ---------------------------
   -- Rec_Comp_Debug_String --
   ---------------------------

   procedure Rec_Comp_Debug_String (RC : Record_Component) is
      Parent_Type_Image : String_Ptr;
   begin

      Debug_Buffer_Len := 0;

      if Is_Nil (RC) then
         Add ("This is a Nil Record Component");
         return;
      end if;

      Parent_Type_Image :=
         new String'(To_String (Debug_Image (RC.Parent_Record_Type)));

      Debug_Buffer_Len := 0;

      Add (LT);

      Add ("Parent_Record_Type");
      Add (LT);
      Add (Parent_Type_Image.all);
      Free (Parent_Type_Image);
      Add (LT);
      Add (LT);

      Add ("Component_Name: ");
      Add (To_String (Defining_Name_Image (RC.Component_Name)));
      Add (LT);

      if RC.Is_Record_Comp then
         Add (">>> IS RECORD");
         Add (LT);
      elsif RC.Is_Array_Comp then
         Add (">>> IS ARRAY");
         Add (LT);

      end if;

      Add ("Position      :");
      Add (ASIS_Natural'Image (RC.Position));
      Add (LT);

      Add ("First_Bit     :");
      Add (ASIS_Natural'Image (RC.First_Bit));
      Add (LT);

      Add ("Last_Bit      :");
      Add (ASIS_Natural'Image (RC.Last_Bit));
      Add (LT);

      Add ("Size          :");
      Add (ASIS_Natural'Image (RC.Size));
      Add (LT);

   end Rec_Comp_Debug_String;

   ---------------------------
   -- Is_Derived_From_Array --
   ----------------------------
   function Is_Derived_From_Array (TD : Element) return Boolean renames
      Asis.Data_Decomposition.Aux.Is_Derived_From_Array;

   ----------------------------
   -- Is_Derived_From_Record --
   -----------------------------
   function Is_Derived_From_Record (TD : Element) return Boolean renames
      Asis.Data_Decomposition.Aux.Is_Derived_From_Record;

   function Dimension (Comp : Array_Component) return ASIS_Natural renames
      Asis.Data_Decomposition.Set_Get.Dimension;

--   function Linear_Index
--     (Inds        : Dimension_Indexes;
--      D           : ASIS_Natural;
--      Ind_Lengths : Dimention_Length;
--      Conv        : Convention_Id := Convention_Ada)
--      return Asis.ASIS_Natural
--      renames Asis.Data_Decomposition.Aux.Linear_Index;

--   function De_Linear_Index
--     (Index       : Asis.ASIS_Natural;
--      D           : ASIS_Natural;
--      Ind_Lengths : Dimention_Length;
--      Conv        : Convention_Id := Convention_Ada)
--      return Dimension_Indexes
--      renames Asis.Data_Decomposition.Aux.De_Linear_Index;

end Asis.Data_Decomposition.Debug;
