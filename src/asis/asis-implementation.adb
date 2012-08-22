------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                  A S I S . I M P L E M E N T A T I O N                   --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Opt;               use A4G.A_Opt;
with A4G.Contt;               use A4G.Contt;
with A4G.Defaults;
with A4G.Vcheck;              use A4G.Vcheck;

with A4G.A_Osint;             use A4G.A_Osint;

with Gnatvsn;
with Opt;

package body Asis.Implementation is

   Package_Name : constant String := "Asis.Implementation.";

   ----------------------
   -- Asis_Implementor --
   ----------------------

   function ASIS_Implementor return Wide_String is
   begin
      return "AdaCore (http://www.adacore.com)";
   end ASIS_Implementor;

   ----------------------------------
   -- ASIS_Implementor_Information --
   ----------------------------------

   function ASIS_Implementor_Information return Wide_String is
   begin
      return
        "Copyright (C) 1995-"                 &
        To_Wide_String (Gnatvsn.Current_Year) &
        ", Free Software Foundation";
   end ASIS_Implementor_Information;

   ------------------------------
   -- ASIS_Implementor_Version --
   ------------------------------

   function ASIS_Implementor_Version return Wide_String is
      GNAT_Version   : constant String   := Gnatvsn.Gnat_Version_String;
      First_Idx      : constant Positive := GNAT_Version'First;
      Last_Idx       :          Positive := GNAT_Version'Last;
      Minus_Detected : Boolean           := False;
   begin

      for J in reverse GNAT_Version'Range loop

         if GNAT_Version (J) = '-' then
            Last_Idx       := J - 1;
            Minus_Detected := True;
            exit;
         end if;

      end loop;

      if Minus_Detected then
         return ASIS_Version & " for GNAT " &
            To_Wide_String (GNAT_Version (First_Idx .. Last_Idx)) & ")";
      else
         return ASIS_Version & " for GNAT " &
            To_Wide_String (GNAT_Version (First_Idx .. Last_Idx));
      end if;

   end ASIS_Implementor_Version;

   ------------------
   -- ASIS_Version --
   ------------------

   function ASIS_Version return Wide_String is
   begin
      return "ASIS 2.0.R";
   end ASIS_Version;

   ---------------
   -- Diagnosis --
   ---------------

   function Diagnosis return Wide_String is
   begin
      --  The ASIS Diagnosis string uses only the first 256 values of
      --  Wide_Character type
      return To_Wide_String (Diagnosis_Buffer (1 .. Diagnosis_Len));
   end Diagnosis;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Parameters : Wide_String := "") is
      S_Parameters : constant String := Trim (To_String (Parameters), Both);
      --  all the valid actuals for Parametes should contain  only
      --  characters from the first 256 values of Wide_Character type
   begin

      if not A4G.A_Opt.Is_Initialized then
         return;
      end if;

      if Debug_Flag_C    or else
         Debug_Lib_Model or else
         Debug_Mode
      then
         Print_Context_Info;
      end if;

      if S_Parameters'Length > 0 then
         Process_Finalization_Parameters (S_Parameters);
      end if;

      A4G.Contt.Finalize;
      A4G.A_Opt.Set_Off;
      A4G.A_Debug.Set_Off;

      A4G.A_Opt.Is_Initialized := False;

   exception
      when ASIS_Failed =>

         A4G.A_Opt.Set_Off;
         A4G.A_Debug.Set_Off;

         A4G.A_Opt.Is_Initialized := False;

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Finalize");
         end if;

         raise;

      when Ex : others =>

         A4G.A_Opt.Set_Off;
         A4G.A_Debug.Set_Off;

         A4G.A_Opt.Is_Initialized := False;

         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Finalize",
            Ex          => Ex);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parameters : Wide_String := "") is
      S_Parameters : constant String := Trim (To_String (Parameters), Both);
      --  all the valid actuals for Parametes should contain  only
      --  characters from the first 256 values of Wide_Character type
   begin
      if A4G.A_Opt.Is_Initialized then
         return;
      end if;

      if not A4G.A_Opt.Was_Initialized_At_Least_Once then
         Opt.Maximum_File_Name_Length := Get_Max_File_Name_Length;
         A4G.A_Opt.Was_Initialized_At_Least_Once := True;
      end if;

      if S_Parameters'Length > 0 then
         Process_Initialization_Parameters (S_Parameters);
      end if;

      A4G.Contt.Initialize;
      A4G.Defaults.Initialize;
      A4G.A_Opt.Is_Initialized := True;

   exception
      when ASIS_Failed =>

         A4G.A_Opt.Set_Off;
         A4G.A_Debug.Set_Off;

         A4G.A_Opt.Is_Initialized := False;

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Initialize");
         end if;

         raise;

      when Ex : others =>

         A4G.A_Opt.Set_Off;
         A4G.A_Debug.Set_Off;

         A4G.A_Opt.Is_Initialized := False;

         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Initialize",
            Ex          => Ex);
   end Initialize;

   ------------------
   -- Is_Finalized --
   ------------------

   function Is_Finalized return Boolean is
   begin
      return not A4G.A_Opt.Is_Initialized;
   end Is_Finalized;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized return Boolean  is
   begin
      return A4G.A_Opt.Is_Initialized;
   end Is_Initialized;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Status    : Asis.Errors.Error_Kinds := Asis.Errors.Not_An_Error;
      Diagnosis : Wide_String             := "")
   is
   begin
      A4G.Vcheck.Set_Error_Status (Status    => Status,
                                   Diagnosis => To_String (Diagnosis));
   end Set_Status;

   ------------
   -- Status --
   ------------

   function Status return Asis.Errors.Error_Kinds is
   begin
      return Status_Indicator;
   end Status;

end Asis.Implementation;
