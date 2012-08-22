------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . V C H E C K                            --
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

with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Elements;           use Asis.Elements;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Implementation;     use Asis.Implementation;

with Asis.Set_Get;            use Asis.Set_Get;
with Asis.Text.Set_Get;       use Asis.Text.Set_Get;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Opt;               use A4G.A_Opt;
with A4G.A_Output;            use A4G.A_Output;

with Fname;                   use Fname;
with Gnatvsn;                 use Gnatvsn;
with Lib;                     use Lib;
with Namet;                   use Namet;
with Output;                  use Output;
with Sinput;                  use Sinput;
with Types;                   use Types;

package body A4G.Vcheck is

   ----------------
   -- Local Data --
   ----------------

   Recursion_Count : Natural := 0;
   --  Used in Report_ASIS_Bug to prevent too many runaway recursion steps to
   --  be done if something bad happens while reporting an ASIS bug. The
   --  problem is that ASIS queries are used to form the diagnostic message,
   --  and some circularities are possible here.

   Max_Recursions_Allowed : constant Positive := 1;
   --  This constant limits the number of recursion calls of Report_ASIS_Bug.
   --  When this limit is achieved, we try once again, but with turning OFF
   --  including the text position into Element's debug image. If this last
   --  step also results in resursive call to Report_ASIS_Bug, we
   --  unconditionally do OS_Abort.
   --
   --  Until we finish the revising of all the exception handlers in the
   --  ASIS implementation code, we limit the recursion depth by one, because
   --  some circularities are possible in the routines that are not "terminal"
   --  ASIS queries but which make use of ASIS queries and contain exception
   --  handlers forming or modifying diagnostic info.

   LT : String renames A4G.A_Types.ASIS_Line_Terminator;

   Current_Pos : Natural range 0 .. Diagnosis_String_Length;
   --  The pointer to the last filled position in the logical text line
   --  in the Diagnosis buffer

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Add_Str (Str : String);
   --  This procedure is similar to Add, but it tries to keep the lengths
   --  of strings stores in Diagnosis_Buffer under 76 characters. Str should
   --  not contain any character(s) caused line breaks. If (a part of) the
   --  argument can be added to the current Diagnosis string and if this string
   --  already contains some text, (a part of) the argument is separated by a
   --  space character.

   procedure Close_Str;
   --  Closes a current string in the Diagnosis buffer

   procedure Reset_Diagnosis_Buffer;
   --  Resets the Diagnosis buffer

   --  ??? The diagnosis buffer needs a proper documentation!!!!

   procedure Set_Error_Status_Internal
     (Status    : Error_Kinds      := Not_An_Error;
      Diagnosis : String := Nil_Asis_String;
      Query     : String := Nil_Asis_String);
   --  This procedure allows to avoid dynamicaly allocated strings in calls
   --  to Set_Error_Status in Check_Validity. Check_Validity is called in
   --  all ASIS structural and semantic queries, so a dynamic string as an
   --  argument of internal call results in significant performance penalties.
   --  (See E705-008).

   ---------
   -- Add --
   ---------
   procedure Add (Phrase : String) is
   begin
      if Diagnosis_Len = Max_Diagnosis_Length then
         return;
      end if;
      for I in Phrase'Range loop
         Diagnosis_Len := Diagnosis_Len + 1;
         Diagnosis_Buffer (Diagnosis_Len) := Phrase (I);
         if Diagnosis_Len = Max_Diagnosis_Length then
            exit;
         end if;
      end loop;
   end Add;

   -------------
   -- Add_Str --
   -------------

   procedure Add_Str (Str : String) is

      First_Idx : Natural := Str'First;
      Last_Idx  : Natural := First_Idx;
      --  Indexes of the first and last subwords in Str

      Word_Len  : Positive;
      Available : Positive;

      Str_Last : constant Positive := Str'Last;
   begin

      while Last_Idx < Str_Last loop

         Last_Idx := Str_Last;

         for J in First_Idx .. Str_Last loop

            if Str (J) = ' ' then
               Last_Idx := J - 1;
               exit;
            end if;

         end loop;

         Word_Len := Last_Idx - First_Idx;

         if Current_Pos = 0 then
            Available := Diagnosis_String_Length;
         else
            Available := Diagnosis_String_Length - (Current_Pos + 1);
         end if;

         if Word_Len <= Available then

            if Current_Pos > 0 then
               Add (" ");
               Current_Pos := Current_Pos + 1;
            end if;

            Add (Str (First_Idx .. Last_Idx));

            Current_Pos := Current_Pos + Word_Len;

         else

            Add (ASIS_Line_Terminator);
            Add (Str (First_Idx .. Last_Idx));
            Current_Pos := Word_Len;
         end if;

         if Current_Pos >=
            Diagnosis_String_Length - ASIS_Line_Terminator'Length
         then
            Add (ASIS_Line_Terminator);
            Current_Pos := 0;
         end if;

         First_Idx := Last_Idx + 2;

      end loop;

   end Add_Str;

   ----------------------
   --   Check_Validity --
   ----------------------

   procedure Check_Validity
     (Compilation_Unit : Asis.Compilation_Unit;
      Query            : String) is
   begin

      if Not_Nil (Compilation_Unit) and then
         not Valid (Compilation_Unit)
      then

         Set_Error_Status_Internal
           (Status    => Value_Error,
            Diagnosis => "Invalid Unit value in ",
            Query     => Query);

         raise ASIS_Inappropriate_Compilation_Unit;
      end if;

   end Check_Validity;

   procedure Check_Validity (Element : Asis.Element;
                             Query   : String) is
   begin

      if Kind (Element) /= Not_An_Element and then
         not Valid (Element)
      then
         Set_Error_Status_Internal
           (Status    => Value_Error,
            Diagnosis => "Invalid Element value in ",
            Query     => Query);

         raise ASIS_Inappropriate_Element;

      end if;

   end Check_Validity;

   procedure Check_Validity
     (Line  : Asis.Text.Line;
      Query : String)
   is
   begin
      if not Asis.Text.Is_Nil (Line) and then not Valid (Line) then
         Set_Error_Status_Internal
           (Status    => Value_Error,
            Diagnosis => "Invalid Line value in ",
            Query     =>  Query);

         raise ASIS_Inappropriate_Line;
      end if;
   end Check_Validity;

   procedure Check_Validity (Context : Asis.Context;
                             Query   : String) is
   begin
      if not Valid (Context) then
         Set_Error_Status_Internal
           (Status    => Value_Error,
            Diagnosis => "Unopened Context argument in ",
            Query     => Query);

         raise ASIS_Inappropriate_Context;

      end if;

   end Check_Validity;

   ---------------
   -- Close_Str --
   ---------------

   procedure Close_Str is
   begin
      Add (ASIS_Line_Terminator);
      Current_Pos := 0;
   end Close_Str;

   ---------------------
   -- Report_ASIS_Bug --
   ---------------------

   procedure Report_ASIS_Bug
     (Query_Name    : String;
      Ex            : Exception_Occurrence;
      Arg_Element   : Asis.Element          := Nil_Element;
      Arg_Element_2 : Asis.Element          := Nil_Element;
      Arg_CU        : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_CU_2      : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_Line      : Asis.Text.Line        := Nil_Line;
      Arg_Span      : Asis.Text.Span        := Nil_Span;
      Bool_Par_ON   : Boolean               := False;
      Context_Par   : Boolean               := False
      --  What else???
      )
   is
      Is_GPL_Version : constant Boolean := Gnatvsn.Build_Type = GPL;
      Is_FSF_Version : constant Boolean := Gnatvsn.Build_Type = FSF;

      procedure Repeat_Char (Char : Character; Col : Nat; After : Character);
      --  This procedure is similar to Comperr.Repeat_Char, but it does nothing
      --  if Generate_Bug_Box is set OFF.
      --
      --  Output Char until current column is at or past Col, and then output
      --  the character given by After (if column is already past Col on entry,
      --  then the effect is simply to output the After character).

      procedure End_Line;
      --  This procedure is similar to Comperr.End_Line, but it does nothing
      --  if Generate_Bug_Box is set OFF.
      --
      --  Add blanks up to column 76, and then a final vertical bar

      procedure Write_Char (C : Character);
      procedure Write_Str  (S : String);
      procedure Write_Eol;
      --  These three subprograms are similar to the procedures with the same
      --  names from the GNAT Output package except that they do nothing in
      --  case if Generate_Bug_Box is set OFF.

      procedure End_Line is
      begin

         if Generate_Bug_Box then
            Repeat_Char (' ', 76, '|');
            Write_Eol;
         end if;

      end End_Line;

      procedure Repeat_Char (Char : Character; Col : Nat; After : Character) is
      begin

         if Generate_Bug_Box then
            while Column < Col loop
               Write_Char (Char);
            end loop;

            Write_Char (After);
         end if;

      end Repeat_Char;

      procedure Write_Char (C : Character) is
      begin

         if Generate_Bug_Box then
            Output.Write_Char (C);
         end if;

      end Write_Char;

      procedure Write_Str  (S : String) is
      begin

         if Generate_Bug_Box then
            Output.Write_Str (S);
         end if;

      end Write_Str;

      procedure Write_Eol is
      begin

         if Generate_Bug_Box then
            Output.Write_Eol;
         end if;

      end Write_Eol;

   begin

      if Recursion_Count >= Max_Recursions_Allowed then

         if Debug_Flag_I then
            --  We can not do anything reasonable any more:
            OS_Abort;
         else
            --  We will try the last time with turning off span computing
            --  as a part of debug output
            Debug_Flag_I := True;
            --  It is not safe to put this flag OFF (it it was set OFF before
            --  the call to Report_ASIS_Bug), because it may be some
            --  circularities (see the comment for Max_Recursions_Allowed
            --  global variable). We may want to revise this decision when
            --  the revision of all the exception handlers in the ASIS code
            --  is complete.
         end if;

      end if;

      Recursion_Count := Recursion_Count + 1;

      --  This procedure is called in case of an ASIS implementation bug, so
      --  we do not care very much about efficiency
      Set_Standard_Error;

      --  Generate header for bug box

      Write_Eol;
      Write_Char ('+');
      Repeat_Char ('=', 29, 'A');
      Write_Str ("SIS BUG DETECTED");
      Repeat_Char ('=', 76, '+');
      Write_Eol;

      --  Output ASIS version identification

      Write_Str ("| ");
      Write_Str (To_String (ASIS_Implementor_Version));

      --  Output the exception info:
      Write_Str (" ");
      Write_Str (Exception_Name (Ex));
      Write_Char (' ');
      Write_Str (Exception_Message (Ex));
      End_Line;

      --  Output the query name and call details
      Write_Str ("| when processing ");
      Write_Str (Query_Name);

      if Bool_Par_ON then
         Write_Str (" (Boolean par => ON)");
      elsif Context_Par then
         Write_Str (" (with Context parameter)");
      end if;

      End_Line;

      --  Add to ASIS Diagnosis:
      Reset_Diagnosis_Buffer;
      Add_Str ("ASIS internal implementation error detected for");
      Close_Str;
      Add_Str (Query_Name);

      if Bool_Par_ON then
         Add_Str ("(Boolean par => ON)");
      elsif Context_Par then
         Add_Str ("(with Context parameter)");
      end if;

      Close_Str;

      --  Add information about the argument of the call (bug box)
      if not Is_Nil (Arg_Element) or else
         not Is_Nil (Arg_CU)
      then

         Write_Str ("| ");
         Write_Str ("called with ");

         if not Is_Nil (Arg_Element) then
            Write_Str (Int_Kind (Arg_Element)'Img);
            Write_Str (" Element");
            End_Line;

         elsif not Is_Nil (Arg_CU) then
            Write_Str (Kind (Arg_CU)'Img);
            Write_Str (" Compilation Unit");
            End_Line;
         end if;

         Write_Str ("| (for full details see the debug image after the box)");
         End_Line;
      end if;

      --  Add information about the argument of the call (Diagnosis string)

      if not Is_Nil (Arg_Element) or else
         not Is_Nil (Arg_CU)
      then
         Add_Str ("called with");
         Close_Str;

         if not Is_Nil (Arg_Element) then
            Debug_String (Arg_Element, No_Abort => True);
            Add (Debug_Buffer (1 .. Debug_Buffer_Len));

         elsif not Is_Nil (Arg_CU) then
            Debug_String (Arg_CU, No_Abort => True);
            Add (Debug_Buffer (1 .. Debug_Buffer_Len));
         end if;

         Close_Str;

         --  Note, that if we do not generate the bug box, in case if the query
         --  have two Element or CU parameters, the information about the
         --  second parameter is missed in the ASIS Diagnosis
      end if;

      Add_Str (Exception_Name (Ex));
      Add     (" ");
      Add_Str (Exception_Message (Ex));

      if not Generate_Bug_Box then
         Close_Str;
         Add_Str ("For more details activate the ASIS bug box");
      end if;

      --  Completing the bug box

      if Is_FSF_Version then
         Write_Str
           ("| Please submit a bug report; see" &
            " http://gcc.gnu.org/bugs.html.");
         End_Line;

      elsif Is_GPL_Version then

         Write_Str
           ("| Please submit a bug report by email " &
            "to report@adacore.com.");
         End_Line;

         Write_Str
           ("| GAP members can alternatively use GNAT Tracker:");
         End_Line;

         Write_Str
           ("| http://www.adacore.com/ " &
            "section 'send a report'.");
         End_Line;

         Write_Str
           ("| See gnatinfo.txt for full info on procedure " &
            "for submitting bugs.");
         End_Line;

      else
         Write_Str
           ("| Please submit a bug report using GNAT Tracker:");
         End_Line;

         Write_Str
           ("| http://www.adacore.com/gnattracker/ " &
            "section 'send a report'.");
         End_Line;

         Write_Str
           ("| alternatively submit a bug report by email " &
            "to report@adacore.com,");
         End_Line;

         Write_Str
           ("| including your customer number #nnn " &
            "in the subject line.");
         End_Line;
      end if;

      Write_Str
        ("| Use a subject line meaningful to you and us to track the bug.");
      End_Line;

      Write_Str
        ("| Include the entire contents of this bug " &
         "box and the ASIS debug info");
      End_Line;

      Write_Str ("| in the report.");
      End_Line;

      Write_Str
        ("| Include the exact list of the parameters of the ASIS queries ");
      End_Line;

      Write_Str
        ("| Asis.Implementation.Initialize and " &
         "Asis.Ada_Environments.Associate");
      End_Line;

      Write_Str
        ("| from the ASIS application for which the bug is detected");
      End_Line;

      Write_Str
        ("| Also include sources listed below in gnatchop format");
      End_Line;

      Write_Str
        ("| (concatenated together with no headers between files).");
      End_Line;

      if not Is_FSF_Version then
         Write_Str ("| Use plain ASCII or MIME attachment.");
         End_Line;
      end if;

      Write_Str
        ("| NOTE: ASIS bugs may be submitted to asis-report@adacore.com");
      End_Line;

      --  Complete output of bug box

      Write_Char ('+');
      Repeat_Char ('=', 76, '+');
      Write_Eol;
      Write_Eol;

      --  Argument debug image(s)

      if not Is_Nil (Arg_Element) or else
         not Is_Nil (Arg_CU)      or else
         not Is_Nil (Arg_Line)
      then
         Write_Str ("The debug image(s) of the argument(s) of the call");
         Write_Eol;

         if not (Is_Nil (Arg_Element_2) and then Is_Nil (Arg_CU_2)) then
            Write_Str ("***First argument***");
            Write_Eol;
         end if;

         Write_Str (Debug_Buffer (1 .. Debug_Buffer_Len));
         Write_Eol;
         Write_Eol;

         if not Is_Nil (Arg_Element_2) then
            Debug_String (Arg_Element_2, No_Abort => True);
         elsif not Is_Nil (Arg_CU_2) then
            Debug_String (Arg_CU_2, No_Abort => True);
         end if;

         if not (Is_Nil (Arg_Element_2) and then Is_Nil (Arg_CU_2)) then
            Write_Str ("***Second argument***");
            Write_Eol;
            Write_Str (Debug_Buffer (1 .. Debug_Buffer_Len));
            Write_Eol;
            Write_Eol;
         end if;

         if not Is_Nil (Arg_Line) then

            if not Is_Nil (Arg_Element) then
               Write_Str ("***Line argument***");
               Write_Eol;
            end if;

            if Recursion_Count >= Max_Recursions_Allowed and then
               Debug_Flag_I
            then
               --  There is a real possibility that we can not output the
               --  debug image of the argument line because of the bug being
               --  reported:
               Write_Str ("Line image can not be reported ");
               Write_Str ("because of the internal error");
               Write_Eol;
               Write_Eol;
            else
               Write_Str (To_String (Debug_Image (Arg_Line)));
               Write_Eol;
               Write_Eol;
            end if;

         end if;

         if not Is_Nil (Arg_Span) then

            if not Is_Nil (Arg_Element) then
               Write_Str ("***Span argument***");
               Write_Eol;
            end if;

            Write_Str ("First_Line   =>");
            Write_Str (Arg_Span.First_Line'Img);
            Write_Eol;

            Write_Str ("First_Column =>");
            Write_Str (Arg_Span.First_Column'Img);
            Write_Eol;

            Write_Str ("Last_Line    =>");
            Write_Str (Arg_Span.Last_Line'Img);
            Write_Eol;

            Write_Str ("Last_Column  =>");
            Write_Str (Arg_Span.Last_Column'Img);
            Write_Eol;
            Write_Eol;

         end if;

      end if;

      Write_Str ("Please include these source files with error report");
      Write_Eol;
      Write_Str ("Note that list may not be accurate in some cases, ");
      Write_Eol;
      Write_Str ("so please double check that the problem can still ");
      Write_Eol;
      Write_Str ("be reproduced with the set of files listed.");
      Write_Eol;
      Write_Eol;

      if Generate_Bug_Box then

         for U in Main_Unit .. Last_Unit loop
            begin
               if not Is_Internal_File_Name
                        (File_Name (Source_Index (U)))
               then
                  Write_Name (Full_File_Name (Source_Index (U)));
                  Write_Eol;
               end if;

            --  No point in double bug box if we blow up trying to print
            --  the list of file names! Output informative msg and quit.

            exception
               when others =>
                  Write_Str ("list may be incomplete");
                  exit;
            end;
         end loop;

      end if;

      Write_Eol;

      Set_Standard_Output;

      if Keep_Going then
         --  Raise ASIS_Failed and go ahead (the Diagnosis is already formed)
         Status_Indicator := Unhandled_Exception_Error;
         Recursion_Count  := Recursion_Count - 1;
         raise ASIS_Failed;
      else
         OS_Exit (1);
      end if;

   exception
      when ASIS_Failed =>
         raise;

      when  Internal_Ex : others =>

         Write_Eol;
         Write_Str ("The diagnostis can not be completed because of " &
                    "the following error:");
         Write_Eol;
         Write_Str (Exception_Name (Ex));
         Write_Char (' ');
         Write_Str (Exception_Message (Ex));
         Write_Eol;

         Close_Str;
         Add_Str ("The diagnostis can not be completed because of " &
                  "the following error:");
         Close_Str;

         Add_Str (Exception_Name (Ex));
         Add     (" ");
         Add_Str (Exception_Message (Ex));

         Add_Str (Exception_Information (Internal_Ex));

         Set_Standard_Output;

         if Keep_Going then
            Status_Indicator := Unhandled_Exception_Error;
            --  Debug_Flag_I     := Skip_Span_In_Debug_Image;
            raise ASIS_Failed;
         else
            OS_Exit (1);
         end if;

   end Report_ASIS_Bug;

   ----------------------------
   -- Reset_Diagnosis_Buffer --
   ----------------------------

   procedure Reset_Diagnosis_Buffer is
   begin
      Diagnosis_Len := 0;
      Current_Pos   := 0;
   end Reset_Diagnosis_Buffer;

   -----------------------------
   -- Raise_ASIS_Failed (new) --
   -----------------------------

   procedure Raise_ASIS_Failed
     (Diagnosis    : String;
      Argument     : Asis.Element            := Nil_Element;
      Stat         : Asis.Errors.Error_Kinds := Internal_Error;
      Bool_Par     : Boolean                 := False;
      Internal_Bug : Boolean                 := True)
   is
   begin
      Diagnosis_Len := 0;

      if Internal_Bug then
         Add ("Internal implementation error: ");
      end if;

      Add (Diagnosis);

      if Bool_Par then
         Add (LT & "(Boolean parameter is TRUE)");
      end if;

      if not Is_Nil (Argument) then
         Add (LT & "when processing ");
         Debug_String (Argument);
         Add (Debug_Buffer (1 .. Debug_Buffer_Len));
      end if;

      Status_Indicator := Stat;

      raise ASIS_Failed;

   end Raise_ASIS_Failed;

   -------------------------------------
   -- Raise_ASIS_Failed_In_Traversing --
   -------------------------------------

   procedure Raise_ASIS_Failed_In_Traversing
     (Start_Element  : Asis.Element;
      Failure_At     : Asis.Element;
      Pre_Op         : Boolean;
      Exception_Info : String)
   is
   begin
      Diagnosis_Len := 0;

      Add ("Traversal failure. Tarversal started at:" & LT);
      Debug_String (Start_Element);
      Add (Debug_Buffer (1 .. Debug_Buffer_Len) & LT);

      if Pre_Op then
         Add ("Pre-operation");
      else
         Add ("Post-operation");
      end if;

      Add (" failed at:" & LT);
      Debug_String (Failure_At);
      Add (Debug_Buffer (1 .. Debug_Buffer_Len));
      Add (LT & Exception_Info);

      Status_Indicator := Unhandled_Exception_Error;

      raise ASIS_Failed;

   end Raise_ASIS_Failed_In_Traversing;

---------------------------------------------------------------

   procedure Raise_ASIS_Inappropriate_Compilation_Unit
                         (Diagnosis : String) is
   begin

      Set_Error_Status (Status    => Value_Error,
                        Diagnosis => "Inappropriate Unit Kind in "
                                    & Diagnosis);

      raise ASIS_Inappropriate_Compilation_Unit;

   end Raise_ASIS_Inappropriate_Compilation_Unit;
----------------------------------------------------------------------
   procedure Raise_ASIS_Inappropriate_Element
     (Diagnosis  : String;
      Wrong_Kind : Internal_Element_Kinds;
      Status     : Error_Kinds      := Value_Error) is
   begin

      Set_Error_Status (Status    => Status,
                        Diagnosis => "Inappropriate Element Kind in " &
                                      Diagnosis                       &
                                     " (" & Wrong_Kind'Img & ")");

      raise ASIS_Inappropriate_Element;

   end Raise_ASIS_Inappropriate_Element;
----------------------------------------------------------------------
   procedure Raise_ASIS_Inappropriate_Line_Number
     (Diagnosis : String;
      Status    : Error_Kinds      := Value_Error) is
   begin
      Set_Error_Status (Status    => Status,
                        Diagnosis => "Inappropriate Lines/Span Kind in "
                                    & Diagnosis);
      raise ASIS_Inappropriate_Line_Number;
   end Raise_ASIS_Inappropriate_Line_Number;
----------------------------------------------------------------------
   procedure Not_Implemented_Yet (Diagnosis : String) is
   begin

      Set_Error_Status (Status    => Not_Implemented_Error,
                        Diagnosis => "Not Implemented Query:" & LT
                                    & Diagnosis);

      raise ASIS_Failed;

   end Not_Implemented_Yet;
--------------------------------------------------------------------
   procedure Set_Error_Status
     (Status    : Error_Kinds      := Not_An_Error;
      Diagnosis : String := Nil_Asis_String)
   is
   begin
      if Status     = Not_An_Error and then
         Diagnosis /= Nil_Asis_String
      then
         Status_Indicator := Internal_Error;
         Diagnosis_Len    := Incorrect_Setting_Len + ASIS_Line_Terminator_Len;
         Diagnosis_Buffer (1 .. Diagnosis_Len)
                          := Incorrect_Setting & ASIS_Line_Terminator;
         raise ASIS_Failed;
      end if;

      Status_Indicator := Status;

      Diagnosis_Len    := Diagnosis'Length;

      Diagnosis_Buffer (1 .. Diagnosis_Len) := Diagnosis;

   end Set_Error_Status;

   procedure Set_Error_Status_Internal
     (Status    : Error_Kinds      := Not_An_Error;
      Diagnosis : String := Nil_Asis_String;
      Query     : String := Nil_Asis_String)
   is
   begin
      Set_Error_Status
        (Status => Status,
         Diagnosis => Diagnosis & Query);
   end Set_Error_Status_Internal;

----------------------------------------------------------------------

   --------------------------
   -- Add_Call_Information --
   --------------------------

   procedure Add_Call_Information
     (Outer_Call : String;
      Argument   : Asis.Element := Nil_Element;
      Bool_Par   : Boolean := False)
   is
   begin
      Add (LT & "called in " & Outer_Call);

      if Bool_Par then
         Add (LT & "(Boolean parameter is TRUE)");
      end if;

      if not Is_Nil (Argument) then
         Add (LT & "with the argument : ");
         Debug_String (Argument);
         Add (Debug_Buffer (1 .. Debug_Buffer_Len));
      end if;

   end Add_Call_Information;

end A4G.Vcheck;
