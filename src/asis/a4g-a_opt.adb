------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                                                                          --
--                            A 4 G . A _ O P T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Errors;  use Asis.Errors;

with A4G.A_Types;  use A4G.A_Types;
with A4G.A_Osint;  use A4G.A_Osint;
with A4G.A_Output; use A4G.A_Output;
with A4G.A_Debug;  use A4G.A_Debug;
with A4G.Vcheck;   use A4G.Vcheck;

with GNAT.OS_Lib;  use GNAT.OS_Lib;

package body A4G.A_Opt is

   -------------------------------------
   -- Process_Finalization_Parameters --
   -------------------------------------

   procedure Process_Finalization_Parameters (Parameters : String) is
      Final_Parameters : Argument_List_Access;

      procedure Process_One_Parameter (Param : String);
      --  incapsulates processing of a separate parameter

      ---------------------------
      -- Process_One_Parameter --
      ---------------------------

      procedure Process_One_Parameter (Param : String) is
         Parameter : constant String (1 .. Param'Length) := Param;
      begin
         ASIS_Warning
            (Message => "Asis.Implementation.Finalize: "
                      & "unknown parameter - "
                      & Parameter,
             Error   => Parameter_Error);
      end Process_One_Parameter;

   begin  -- Process_Finalization_Parameters
      Final_Parameters := Parameter_String_To_List (Parameters);

      for I in Final_Parameters'Range loop
         Process_One_Parameter (Final_Parameters (I).all);
      end loop;

      Free_Argument_List (Final_Parameters);
   end Process_Finalization_Parameters;

   ---------------------------------------
   -- Process_Initialization_Parameters --
   ---------------------------------------

   procedure Process_Initialization_Parameters (Parameters : String) is
      Init_Parameters : Argument_List_Access;

      procedure Process_One_Parameter (Param : String);
      --  incapsulates processing of a separate parameter

      ---------------------------
      -- Process_One_Parameter --
      ---------------------------

      procedure Process_One_Parameter (Param : String) is
         Parameter : constant String (1 .. Param'Length) := Param;
         Unknown_Parameter : Boolean := False;
         subtype Dig is Character range '1' .. '9';
         subtype Let is Character range 'a' .. 'z';

         procedure Process_Parameter;
         procedure Process_Option;
         --  Process_Option works if Param starts from '-', and
         --  Process_Parameter works otherwise

         procedure Process_Parameter is
         begin
            --  no parameter is currently available as an ASIS initialization
            --  parameter
            Raise_ASIS_Failed
               (Diagnosis    => "Asis.Implementation.Initialize: "
                              & "unknown parameter - "
                              & Parameter,
                Stat         => Parameter_Error,
                Internal_Bug => False);
         end Process_Parameter;

         procedure Process_Option is
         begin

            case Parameter (2) is

               when 'a' =>

                  if Parameter = "-asis05"
                    or else
                     Parameter = "-asis12"
                    or else
                     Parameter = "-asis95"
                  then
                     --  We have to accept these parameters because of
                     --  compatibility reasons
                     null;
                  else
                     Unknown_Parameter := True;
                  end if;

               when 'd' =>

                  if   Parameter'Length = 3
                     and then
                      (Parameter (3) in Dig or else
                       Parameter (3) in Let)
                  then
                     Set_Debug_Flag (Parameter (3));
                  elsif Parameter = "-dall" then
                     A4G.A_Debug.Set_On;
                  else
                     Unknown_Parameter := True;
                  end if;

               when 'k' =>

                  if Parameter = "-k" then
                     Keep_Going := True;
                  else
                     Unknown_Parameter := True;
                  end if;

               when 'n' =>

                  if Parameter = "-nbb" then
                     Generate_Bug_Box := False;
                     Keep_Going       := True;
                  else
                     Unknown_Parameter := True;
                  end if;

               when 's' =>

                  if Parameter = "-sv" then
                     Strong_Version_Check := True;
                  else
                     Unknown_Parameter := True;
                  end if;

               when 'w' =>

                  if Parameter = "-ws" then
                     ASIS_Warning_Mode := Suppress;
                  elsif Parameter = "-we" then
                     ASIS_Warning_Mode := Treat_As_Error;
                  elsif Parameter = "-wv" then
                     Strong_Version_Check := False;
                  else
                     Unknown_Parameter := True;
                  end if;

               when others =>
                  Unknown_Parameter := True;
            end case;

            if Unknown_Parameter then
               Raise_ASIS_Failed
                  (Diagnosis    => "Asis.Implementation.Initialize: "
                                 & "unknown option - "
                                 & Parameter,
                   Stat         => Parameter_Error,
                   Internal_Bug => False);
            end if;

         end Process_Option;

      begin --  Process_One_Parameter
         if Parameter (1) = '-' then
            if Parameter'Length >= 2 then
               Process_Option;
            else
               Raise_ASIS_Failed
                  (Diagnosis => "Asis.Implementation.Initialize: "
                              & "Option is missing after ""-""",
                   Stat      => Parameter_Error,
                Internal_Bug => False);
            end if;
         else
            Process_Parameter;
         end if;
      end Process_One_Parameter;

   begin  -- Process_Initialization_Parameters
      Init_Parameters := Parameter_String_To_List (Parameters);

      for I in Init_Parameters'Range loop
         Process_One_Parameter (Init_Parameters (I).all);
      end loop;

      Free_Argument_List (Init_Parameters);
   end Process_Initialization_Parameters;

   -------------
   -- Set_Off --
   -------------

   procedure Set_Off is
   begin
      Is_Initialized       := False;
      ASIS_Warning_Mode    := Normal;
      Strong_Version_Check := True;
      Generate_Bug_Box     := True;
      Keep_Going           := False;
   end Set_Off;

end A4G.A_Opt;
