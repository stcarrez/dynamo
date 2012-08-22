------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ T Y P E S                           --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;

with System; use System;

with Hostparm;

package body A4G.A_Types is

   ---------------
   -- A_OS_Time --
   ---------------

   function A_OS_Time return ASIS_OS_Time is
   begin
      return ASIS_Clock;
   end A_OS_Time;

   -----------------------------
   -- Asis_Normalize_Pathname --
   -----------------------------

   Asis_Normalize_Pathname_Result : String_Access;

   function Asis_Normalize_Pathname
     (Name           : String;
      Directory      : String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return String
   is
      --  ???
      --  All the stuff in the declarative part is copied from Osint...

      function C_String_Length (S : Address) return Integer;
      --  Returns length of a C string (zero for a null address)

      function To_Path_String_Access
        (Path_Addr : Address;
         Path_Len  : Integer) return String_Access;
      --  Converts a C String to an Ada String. Are we doing this to avoid
      --  withing Interfaces.C.Strings ???
      --  Caller must free result.

      function To_Host_Dir_Spec
        (Canonical_Dir : String;
         Prefix_Style  : Boolean) return String_Access;
      --  Convert a canonical syntax directory specification to host syntax.
      --  The Prefix_Style flag is currently ignored but should be set to
      --  False. Note that the caller must free result.
      --  ???
      --  Copied from Osint...

      function C_String_Length (S : Address) return Integer is
         function Strlen (S : Address) return Integer;
         pragma Import (C, Strlen, "strlen");
      begin
         if S = Null_Address then
            return 0;
         else
            return Strlen (S);
         end if;
      end C_String_Length;

      function To_Path_String_Access
        (Path_Addr : Address;
         Path_Len  : Integer) return String_Access
      is
         subtype Path_String is String (1 .. Path_Len);
         type Path_String_Access is access Path_String;

         function Address_To_Access is new
           Ada.Unchecked_Conversion (Source => Address,
                                     Target => Path_String_Access);

         Path_Access : constant Path_String_Access :=
                         Address_To_Access (Path_Addr);

         Return_Val : String_Access;

      begin
         Return_Val := new String (1 .. Path_Len);

         for J in 1 .. Path_Len loop
            Return_Val (J) := Path_Access (J);
         end loop;

         return Return_Val;
      end To_Path_String_Access;

      function To_Host_Dir_Spec
        (Canonical_Dir : String;
         Prefix_Style  : Boolean) return String_Access
      is
         function To_Host_Dir_Spec
           (Canonical_Dir : Address;
            Prefix_Flag   : Integer) return Address;
         pragma Import (C, To_Host_Dir_Spec, "__gnat_to_host_dir_spec");

         C_Canonical_Dir : String (1 .. Canonical_Dir'Length + 1);
         Host_Dir_Addr   : Address;
         Host_Dir_Len    : Integer;

      begin
         C_Canonical_Dir (1 .. Canonical_Dir'Length) := Canonical_Dir;
         C_Canonical_Dir (C_Canonical_Dir'Last)      := ASCII.NUL;

         if Prefix_Style then
            Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 1);
         else
            Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 0);
         end if;
         Host_Dir_Len := C_String_Length (Host_Dir_Addr);

         if Host_Dir_Len = 0 then
            return null;
         else
            return To_Path_String_Access (Host_Dir_Addr, Host_Dir_Len);
         end if;
      end To_Host_Dir_Spec;
   begin
      if Name = "" then
         return "";
      else
         Free (Asis_Normalize_Pathname_Result);

         Asis_Normalize_Pathname_Result := To_Host_Dir_Spec (
           Canonical_Dir =>
             Normalize_Pathname (
               Name           => Name,
               Directory      => Directory,
               Resolve_Links  => Resolve_Links,
               Case_Sensitive => Case_Sensitive),
           Prefix_Style  => False);

         return Asis_Normalize_Pathname_Result.all;
      end if;
   end Asis_Normalize_Pathname;

   ---------------------------
   -- Increase_ASIS_OS_Time --
   ---------------------------

   procedure Increase_ASIS_OS_Time is
   begin
      ASIS_Clock := ASIS_Clock + 1;
   end Increase_ASIS_OS_Time;

   -----------
   -- Later --
   -----------

   function Later (L, R : ASIS_OS_Time) return Boolean is
   begin
      return L <= R;
   end Later;

   ------------------------------
   -- Parameter_String_To_List --
   ------------------------------

   function Parameter_String_To_List
     (Par_String : String)
      return       Argument_List_Access
   is
      Max_Pars : constant Integer := Par_String'Length;
      New_Parv : Argument_List (1 .. Max_Pars);
      New_Parc : Natural := 0;
      Idx      : Integer;
      Old_Idx  : Integer;

      function Move_To_Next_Par (Ind : Integer) return Integer;
      --  Provided that Ind points somewhere inside Par_String, moves
      --  it ahead to point to the beginning of the next parameter if
      --  Ind points to the character considering as a parameter separator,
      --  otherwise returns Ind unchanged. If Ind points to a separator and
      --  there is no more parameters ahead, Par_String'Last + 1 is returned.
      --  (See the definition of the syntax of the Parameters string in the
      --  ASIS Reference Manual)

      function Move_To_Par_End (Ind : Integer) return Integer;
      --  Provided that Ind points to some character of a separate parameters
      --  being a part of Par_String, returns the index of the last character
      --  of this parameter

      function Move_To_Next_Par (Ind : Integer) return Integer is
         Result : Integer := Ind;
      begin

         while Result <=             Par_String'Last and then
              (Par_String (Result) = ' '      or else
               Par_String (Result) = ASCII.HT or else
               Par_String (Result) = ASCII.LF or else
               Par_String (Result) = ASCII.CR)
         loop
            Result := Result + 1;
         end loop;

         return Result;

      end Move_To_Next_Par;

      function Move_To_Par_End (Ind : Integer) return Integer is
         Result : Integer := Ind;
         Quoted : Boolean := False;
      begin

         loop

            --  Am unquoted white space or EOL is the end of an argument
            if not Quoted
              and then
              (Par_String (Result) = ' '      or else
               Par_String (Result) = ASCII.HT or else
               Par_String (Result) = ASCII.LF or else
               Par_String (Result) = ASCII.CR)
            then
               exit;

            --  Start of quoted string
            elsif not Quoted
              and then Par_String (Result) = '"'
            then
               Quoted := True;

            --  End of a quoted string and end of an argument
            elsif Quoted
              and then Par_String (Result) = '"'
            then
               Result := Result + 1;
               exit;
            end if;

            Result := Result + 1;
            exit when Result > Par_String'Last;
         end loop;

         Result := Result - 1;

         return Result;

      end Move_To_Par_End;

   begin
      Idx := Move_To_Next_Par (Par_String'First);

      while Idx <= Par_String'Last loop
         Old_Idx := Idx;
         Idx     := Move_To_Par_End (Idx);

         New_Parc := New_Parc + 1;
         New_Parv (New_Parc) :=
           new String'(Par_String (Old_Idx .. Idx));

         Idx := Move_To_Next_Par (Idx + 1);
      end loop;

      return new Argument_List'(New_Parv (1 .. New_Parc));
   end Parameter_String_To_List;

begin
   if Hostparm.OpenVMS then
      ASIS_Path_Separator    := ',';
      ASIS_Current_Directory := new String'("[]");
   else
      ASIS_Path_Separator    := GNAT.OS_Lib.Path_Separator;
      ASIS_Current_Directory := new String'(".");
   end if;
end A4G.A_Types;
