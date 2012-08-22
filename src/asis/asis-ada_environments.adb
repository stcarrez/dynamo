------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                A S I S . A D A _ E N V I R O N M E N T S                 --
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

with Asis.Set_Get;            use Asis.Set_Get;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Opt;               use A4G.A_Opt;
with A4G.A_Output;            use A4G.A_Output;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.TT;            use A4G.Contt.TT;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Vcheck;              use A4G.Vcheck;

with Output;                  use Output;

package body Asis.Ada_Environments is

   Package_Name : constant String := "Asis.Ada_Environments.";

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (The_Context : in out Asis.Context;
      Name        :        Wide_String;
      Parameters  :        Wide_String := Default_Parameters)
   is
      S_Parameters : constant String := Trim (To_String (Parameters), Both);
      Cont         :          Context_Id;
   begin

      Cont := Get_Cont_Id (The_Context);

      if not A4G.A_Opt.Is_Initialized then

         Set_Error_Status
           (Status    => Initialization_Error,
            Diagnosis => Package_Name & "Associate: "
                       & "called for non-initialized ASIS");

         raise ASIS_Failed;

      end if;

      if Is_Opened (Cont) then
         Set_Error_Status
           (Status    => Value_Error,
            Diagnosis => Package_Name & "Associate: "
                       & "the Context has already been opened");
         raise ASIS_Inappropriate_Context;
      end if;

      if Cont = Non_Associated then
         --  this is the first association for a given Context
         Cont := Allocate_New_Context;
         Set_Cont (The_Context, Cont);
      else
         Erase_Old (Cont);
      end if;

      Verify_Context_Name (To_String (Name), Cont);
      Process_Context_Parameters (S_Parameters, Cont);

      Set_Is_Associated (Cont, True);

      Save_Context (Cont);
      Set_Current_Cont (Nil_Context_Id);

   exception
      when ASIS_Inappropriate_Context =>
         Set_Is_Associated (Cont, False);
         raise;
      when ASIS_Failed =>
         Set_Is_Associated (Cont, False);

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Associate");
         end if;

         raise;
      when Ex : others =>
         Set_Is_Associated (Cont, False);

         Report_ASIS_Bug
           (Query_Name => Package_Name & "Associate",
            Ex         => Ex);
   end Associate;

   -----------
   -- Close --
   -----------

   procedure Close (The_Context : in out Asis.Context) is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      Reset_Context (Cont);

      if not Is_Opened (Cont) then
         Set_Error_Status (Status    => Value_Error,
                           Diagnosis => Package_Name & "Close: " &
                           "the Context is not open");
         raise ASIS_Inappropriate_Context;
      end if;

      if Debug_Flag_C    or else
         Debug_Lib_Model or else
         Debug_Mode
      then
         Write_Str ("Closing Context ");
         Write_Int (Int (Cont));
         Write_Eol;
         Print_Units (Cont);
         Print_Trees (Cont);
      end if;

      Set_Is_Opened (Cont, False);

      Set_Current_Cont (Nil_Context_Id);

      Reset_Cache;

   exception
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>
         Set_Current_Cont (Nil_Context_Id);

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name &  "Close");
         end if;

         raise;
      when Ex : others =>
         Set_Current_Cont (Nil_Context_Id);
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Associate",
            Ex         => Ex);
   end Close;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image
     (The_Context : Asis.Context)
      return        Wide_String
   is
      Arg_Cont : Context_Id;
      LT       : Wide_String renames A4G.A_Types.Asis_Wide_Line_Terminator;
   begin
      Arg_Cont := Get_Cont_Id (The_Context);
      Reset_Context (Arg_Cont);

      return LT & "Context Debug_Image: " &
             LT & "Context Id is" &
             Context_Id'Wide_Image (Arg_Cont) &
             LT & To_Wide_String (Debug_String (The_Context));
   exception
      when Ex : others =>
         Report_ASIS_Bug
          (Query_Name => Package_Name & "Debug_Image",
           Ex         => Ex);
   end Debug_Image;

   ------------------
   -- Default_Name --
   ------------------

   function Default_Name return Wide_String is
   begin
      return Nil_Asis_Wide_String;
   end Default_Name;

   ------------------------
   -- Default_Parameters --
   ------------------------

   function Default_Parameters return Wide_String is
   begin
      return Nil_Asis_Wide_String;
   end Default_Parameters;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate (The_Context : in out Asis.Context) is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);

      if Is_Opened (Cont) then
         Set_Error_Status (Status    => Value_Error,
                           Diagnosis => Package_Name & "Dissociate: "
                                      & "the Context is open");
         raise ASIS_Inappropriate_Context;
      end if;

      if Debug_Flag_C    or else
         Debug_Lib_Model or else
         Debug_Mode
      then
         Write_Str ("Dissociating Context ");
         Write_Int (Int (Cont));
         Write_Eol;
         Print_Context_Parameters (Cont);
      end if;

      if Is_Associated (Cont) then
         Erase_Old (Cont);
         Set_Is_Associated (Cont, False);
      end if;

   exception
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Dissociate");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Dissociate",
            Ex         => Ex);
   end Dissociate;

   ------------
   -- Exists --
   ------------

   function Exists (The_Context : Asis.Context) return Boolean is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      return Is_Associated (Cont);
   end Exists;

   ----------------------
   -- Has_Associations --
   ----------------------

   function Has_Associations
     (The_Context : Asis.Context)
      return        Boolean
   is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      return Is_Associated (Cont);
   end Has_Associations;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : Asis.Context;
      Right : Asis.Context)
      return  Boolean
   is
   begin
      return Get_Cont_Id (Left) = Get_Cont_Id (Right);
      --  Should be revised
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : Asis.Context;
      Right : Asis.Context)
      return  Boolean
   is
   begin
      return Get_Cont_Id (Left) = Get_Cont_Id (Right);
   end Is_Identical;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (The_Context : Asis.Context) return Boolean is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      return Is_Opened (Cont);
   end Is_Open;

   ----------
   -- Name --
   ----------

   function Name (The_Context : Asis.Context) return Wide_String is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      return  To_Wide_String (Get_Context_Name (Cont));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open (The_Context : in out Asis.Context) is
      Cont              : Context_Id;
      Context_Tree_Mode : Tree_Mode;
   begin
      Cont := Get_Cont_Id (The_Context);

      if not Is_Associated (Cont) then
         Set_Error_Status (Status    => Value_Error,
                           Diagnosis => Package_Name & "Open: " &
                           "the Context does not have association");
         raise ASIS_Inappropriate_Context;
      elsif Is_Opened (Cont) then
         Set_Error_Status (Status    => Value_Error,
                           Diagnosis => Package_Name & "Open: " &
                           "the Context has already been opened");
         raise ASIS_Inappropriate_Context;
      end if;

      Reset_Context (Cont);
      Context_Tree_Mode := Tree_Processing_Mode (Cont);

      if Tree_Processing_Mode (Cont) = GNSA then
         Set_Error_Status (Status    => Use_Error,
                           Diagnosis => Package_Name & "Open: " &
                           "GNSA Context mode is not allowed");
         raise ASIS_Inappropriate_Context;
      end if;

      Increase_ASIS_OS_Time;

      Pre_Initialize (Cont);
      A4G.Contt.Initialize (Cont);
      --  Having these two Pre_Initialize and A4G.Contt.Initialize calling
      --  one after another is a kind of junk, but there are some problems
      --  with multi-context processing which have not been completely
      --  detected and which does not allow to get rid of this definitely
      --  redundunt "initialization"

      case Context_Tree_Mode is
         when Pre_Created | Mixed =>
            Scan_Trees_New (Cont);

         when Incremental =>

            --  Not the best approach, unfortunately
            begin
               Scan_Trees_New (Cont);
            exception
               when Inconsistent_Incremental_Context =>
                  --  Setting empty incremental context:
                  Pre_Initialize (Cont);
                  A4G.Contt.Initialize (Cont);
            end;

         when others =>
            null;
      end case;

      Set_Is_Opened (Cont, True);

      Save_Context (Cont);

      Set_Current_Cont (Nil_Context_Id);

   exception
      when Program_Error =>
         raise;
      when ASIS_Inappropriate_Context =>
         raise;
      when ASIS_Failed =>
         Set_Is_Opened (Cont, False);
         Set_Current_Cont (Nil_Context_Id);

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Open");
         end if;

         raise;
      when Ex : others =>
         Set_Is_Opened (Cont, False);
         Set_Current_Cont (Nil_Context_Id);

         Report_ASIS_Bug
           (Query_Name => Package_Name & "Open",
            Ex         => Ex);
   end Open;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (The_Context : Asis.Context) return Wide_String is
      Cont : Context_Id;
   begin
      Cont := Get_Cont_Id (The_Context);
      return  To_Wide_String (Get_Context_Parameters (Cont));
   end Parameters;

end Asis.Ada_Environments;
