------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A S I S . T E X T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis;                    use Asis;
with Asis.Compilation_Units;  use Asis.Compilation_Units;
with Asis.Elements;           use Asis.Elements;
with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;

with Asis.Set_Get;            use Asis.Set_Get;
with Asis.Text.Set_Get;       use Asis.Text.Set_Get;

with A4G.A_Debug;             use A4G.A_Debug;
with A4G.A_Sinput;            use A4G.A_Sinput;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.TT;            use A4G.Contt.TT;
with A4G.Contt.UT;            use A4G.Contt.UT;
with A4G.Span_Beginning;      use A4G.Span_Beginning;
with A4G.Span_End;            use A4G.Span_End;
with A4G.Vcheck;              use A4G.Vcheck;

with Namet;                   use Namet;
with Output;                  use Output;
with Sinput;                  use Sinput;

package body Asis.Text is

   -------------------
   -- Comment_Image --
   -------------------

   function Comment_Image
     (The_Line : Asis.Text.Line)
      return     Wide_String
   is
   begin

      Check_Validity (The_Line, "Asis.Text.Comment_Image");

      if The_Line.Length = 0 then
         --  just a small optimization
         return "";
      end if;

      declare
         The_Line_Image : Wide_String      := Line_Image (The_Line);
         Comment_Pos    : constant Source_Ptr := Comment_Beginning
           (Text_Buffer (To_String (The_Line_Image)));
      begin

         if Comment_Pos = No_Location then
            --  no comment in this string
            return "";

         else
            --  we have to pad the beginning (that is, non-comment part)
            --  of the line image by white spaces, making difference between
            --  HT and other symbols:

            for I in The_Line_Image'First .. Natural (Comment_Pos) - 1 loop

               if To_Character (The_Line_Image (I)) /= ASCII.HT then
                  The_Line_Image (I) := ' ';
               end if;

            end loop;

            return The_Line_Image;

         end if;

      end;
   exception
      when ASIS_Inappropriate_Line =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => "Asis.Text.Comment_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Comment_Image",
            Arg_Line    => The_Line,
            Ex          => Ex);
   end Comment_Image;

   ----------------------
   -- Compilation_Span --
   ----------------------

   function Compilation_Span
     (Element : Asis.Element)
      return    Asis.Text.Span
   is
      Result_Span : Asis.Text.Span := Asis.Text.Nil_Span;
      S_P         : Source_Ptr;
      S_P_Last    : Source_Ptr;
      SFI         : Source_File_Index;
      Src         : Source_Buffer_Ptr;
      Last_Line   : Physical_Line_Number;
   begin
      Check_Validity (Element, "Asis.Text.Compilation_Span");

      --  In case of GNAT compilations are source files, so there is no need
      --  to compute Result_Span.First_Line and Result_Span.First_Column -
      --  the correct values are (1, 1) , and they come from Nil_Span

      S_P := Get_Location (Element);
      --  this makes all the rest "tree-swapping-safe"

      if S_P > Standard_Location then
         SFI       := Get_Source_File_Index (S_P);
         Src       := Source_Text (SFI);

         Last_Line := Last_Source_Line (SFI);

         Result_Span.Last_Line   := Line_Number (Last_Line);
         S_P                     := Line_Start (Last_Line, SFI);
         S_P_Last                := S_P;

         while not (S_P_Last = Source_Last (SFI) or else
                    Src (S_P_Last) = ASCII.LF    or else
                    Src (S_P_Last) = ASCII.CR)
         loop
            S_P_Last := S_P_Last + 1;
         end loop;

         Result_Span.Last_Column := Character_Position (S_P_Last - S_P);
      end if;

      return Result_Span;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Compilation_Span",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Compilation_Span",
            Arg_Element => Element,
            Ex          => Ex);
   end Compilation_Span;

   ---------------------------
   -- Compilation_Unit_Span --
   ---------------------------

   function Compilation_Unit_Span
     (Element : Asis.Element)
      return    Asis.Text.Span
   is
      Result_Span    : Asis.Text.Span := Asis.Text.Nil_Span;
      CU             : Asis.Compilation_Unit;
      First_El       : Asis.Element;
      Unit           : Asis.Element;
      No_Cont_Clause : Boolean := True;

      Span_Start : Source_Ptr;
      Span_End   : Source_Ptr;
   begin
      Check_Validity (Element, "Asis.Text.Compilation_Unit_Span");

      --  First, check that the argument Element is not from the predefined
      --  Standard package.

      Span_Start := Get_Location (Element);
      --  this makes all the rest "tree-swapping-safe"

      if Span_Start > Standard_Location then
         CU := Enclosing_Compilation_Unit (Element);
         Unit := Unit_Declaration (CU);

         declare
            Cont_Cl_Elms : constant Asis.Context_Clause_List :=
               Context_Clause_Elements
                 (Compilation_Unit => CU,
                  Include_Pragmas  => True);
         begin

            if Is_Nil (Cont_Cl_Elms) then
               First_El := Unit;
            else
               First_El   := Cont_Cl_Elms (Cont_Cl_Elms'First);
               No_Cont_Clause := False;
            end if;

         end;

         Span_Start := Set_Image_Beginning (First_El);

         if No_Cont_Clause then

            --  For private unit declarations and for subunits we have to take
            --  into account syntax elements which are not a part of the
            --  corresponding unit declaration element

            case Unit_Class (CU) is
               when A_Private_Declaration =>
                  --  Taking into account leading 'private' keyword
                  Span_Start := Search_Prev_Word_Start (Span_Start);

               when A_Separate_Body =>
                  --  Taking into account 'separate (...)"
                  Span_Start := Search_Left_Parenthesis (Span_Start);
                  Span_Start := Search_Prev_Word_Start  (Span_Start);

               when others =>
                  null;
            end case;
         end if;

         Span_End := Set_Image_End (Unit);

         Result_Span := Source_Locations_To_Span (Span_Start, Span_End);
      end if;

      return Result_Span;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Compilation_Unit_Span",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Compilation_Unit_Span",
            Arg_Element => Element,
            Ex          => Ex);
   end Compilation_Unit_Span;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image
     (The_Line : Asis.Text.Line)
      return     Wide_String
   is
      LT : String renames ASIS_Line_Terminator;
      S  : constant Source_Ptr := Line_Location (The_Line);
      --  this makes the rest "tree-swapping-safe"

      Sindex : constant Source_File_Index := Get_Source_File_Index (S);
      F_Name : constant File_Name_Type    := File_Name (Sindex);

      T  : constant Tree_Id    := The_Line.Enclosing_Tree;
      C  : constant Context_Id := The_Line.Enclosing_Context;

      Tree_Name : String_Ptr;
   begin
      Check_Validity (The_Line, "Asis.Text.Debug_Image");

      if Present (T) then
         Get_Name_String (C, T);

         Tree_Name :=
           new String'(A_Name_Buffer (1 ..  A_Name_Len) &
                       " (Tree_Id =" & Tree_Id'Image (T) & ")");
      else
         Tree_Name := new String'(" <nil tree>");
      end if;

      Get_Name_String (F_Name);

      return To_Wide_String
               ("Debug image for Asis.Text.Line:"
              & LT
              & "  obtained from file " & Name_Buffer (1 .. Name_Len)
              & LT
              & "  Absolute (relative) location in source file :"
              & Source_Ptr'Image (S)
              & " ("
              & Source_Ptr'Image (The_Line.Rel_Sloc)
              & ')'
              & LT
              & "  Line  :"
              & Physical_Line_Number'Image (Get_Physical_Line_Number (S))
              & LT
              & "  Column:"
              & Source_Ptr'Image (A_Get_Column_Number (S))
              & LT
              & "  Number of characters in line:"
              & Asis.Text.Character_Position'Image (Line_Length (The_Line))
              & LT
              & "  obtained from the tree "
              & Tree_Name.all);

   exception
      when ASIS_Inappropriate_Line =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => "Asis.Text.Debug_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Debug_Image",
            Arg_Line    => The_Line,
            Ex          => Ex);
   end Debug_Image;

   ---------------------
   -- Delimiter_Image --
   ---------------------

   function Delimiter_Image return Wide_String is
   begin
      return Asis_Wide_Line_Terminator;
   end Delimiter_Image;

   -------------------
   -- Element_Image --
   -------------------

   function Element_Image
     (Element : Asis.Element)
      return    Wide_String
   is
   begin
      Check_Validity (Element, "Asis.Text.Element_Image");

      if not Is_Text_Available (Element) then
         return "";
      end if;

      declare
         LList : constant Asis.Text.Line_List := Lines (Element);
         --  We create the Element Image from Lines containing the Element

         Spaces : Natural;
         --  The number of characters in the first line of the Image, which
         --  should be padded by white spaces

         Numb : Natural;
         --  Yere we collect the whole number of characters needed in the
         --  string representing the result Element image. Note that we are
         --  counting in wide characters
      begin
         Spaces := Natural (Element_Span (Element).First_Column) - 1;
         Numb   := Asis.ASIS_Natural (Spaces) +
                   Asis.ASIS_Natural (LList'Last - LList'First) *
                   Asis.ASIS_Natural (Asis_Wide_Line_Terminator'Length);

         for I in LList'First .. LList'Last loop
            Numb := Numb + Natural (Line_Length (LList (I)));
         end loop;

         declare
            Result   : Wide_String (1 .. Numb);
            In_Str   : Positive := 1;
         begin

            --  The image of the first line may contain spaces padding the part
            --  of the physal line that precedes the element

            Result (In_Str ..
                    In_Str + Spaces + Line_Length (LList (LList'First)) - 1) :=
               Line_Image (LList (LList'First));

            In_Str := In_Str + Spaces + Line_Length (LList (LList'First));

            --  and now - filling the rest of Result_String
            --  by the "proper" Element Image
            for Linee in LList'First + 1 .. LList'Last loop
               Result (In_Str .. In_Str + ASIS_Line_Terminator'Length - 1)
                 := Asis_Wide_Line_Terminator;

               In_Str := In_Str + Asis_Wide_Line_Terminator'Length;

               Result (In_Str .. In_Str + Line_Length (LList (Linee)) - 1) :=
                  Line_Image (LList (Linee));

               In_Str := In_Str + Line_Length (LList (Linee));

            end loop;

            return Result;

         end;
      end;
   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Element_Image",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis_Text.Element_Image",
            Arg_Element => Element,
            Ex          => Ex);
   end Element_Image;

   ------------------
   -- Element_Span --
   ------------------

   --  Element_Span is "tree-swapping-safe"!
   function Element_Span (Element : Asis.Element) return Asis.Text.Span is
      Sp         : Asis.Text.Span := Asis.Text.Nil_Span;
      Span_Start : Source_Ptr;
      Span_End   : Source_Ptr;
   begin
      Check_Validity (Element, "Asis.Text.Element_Span");
      if Debug_Flag_X or else Debug_Mode then
         Write_Str  ("*** Asis.Text.Element_Span ***");
         Write_Eol;
         Write_Str  ("Element kind is ");
         Write_Str  (Internal_Element_Kinds'Image (Int_Kind (Element)));
         Write_Eol;
      end if;

      if not Is_Text_Available (Element) then

         if Debug_Flag_X or else Debug_Mode then
            Write_Str  ("!!! Text isn't available !!!");
            Write_Eol;
         end if;

         return Sp;

      end if;

      --  Set_Image_Beginning is "tree-swapping-safe"
      Span_Start := Set_Image_Beginning (Element);
      Span_End   := Set_Image_End (Element);
      Sp         := Source_Locations_To_Span (Span_Start, Span_End);

      if Debug_Flag_X or else Debug_Mode then
         Write_Str ("Returning Asis.Text.Span parameters:");
         Write_Eol;
         Write_Str (Debug_Image (Sp));
         Write_Eol;
      end if;

      return Sp;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Element_Span",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Element_Span",
            Arg_Element => Element,
            Ex          => Ex);
   end Element_Span;

   -----------------------
   -- First_Line_Number --
   -----------------------

   function First_Line_Number (Element : Asis.Element) return Line_Number is
      Sp : Asis.Text.Span;
   begin
      Check_Validity (Element, "Asis.Text.First_Line_Number");

      if Is_Text_Available (Element) then
         Sp := Element_Span (Element);

         return Sp.First_Line;

      else

         return 0;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.First_Line_Number",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.First_Line_Number",
            Arg_Element => Element,
            Ex          => Ex);
   end First_Line_Number;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : Asis.Text.Line;
      Right : Asis.Text.Line)
      return  Boolean
   is
      C_Left  : Context_Id;
      C_Right : Context_Id;
      U_Left  : Unit_Id;
      U_Right : Unit_Id;
   begin
      Check_Validity (Left, "Asis.Text.Is_Equal");
      Check_Validity (Right, "Asis.Text.Is_Equal");

      --  Two lines which are Is_Equal may be obtained from different
      --  Context, and they may be based on different trees. But to
      --  be Is_Equal, they have to represent the same portion of the
      --  source text from the same source file

      if Left.Length   /= Right.Length or else
         Left.Rel_Sloc /= Right.Rel_Sloc
      then
         return False;
      else
         --  we use just the same approach as for comparing Elements
         C_Left  := Left.Enclosing_Context;
         U_Left  := Left.Enclosing_Unit;

         C_Right := Right.Enclosing_Context;
         U_Right := Right.Enclosing_Unit;

         return Time_Stamp (C_Left, U_Left) = Time_Stamp (C_Right, U_Right);

      end if;
   exception
      when ASIS_Inappropriate_Line =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => "Asis.Text.Is_Equal");
         end if;

         raise;
      when Ex : others =>
         --  Note, that in case of failure the very primitive diagnosis will be
         --  generated. If the implementation of this query become more
         --  sophisticated, we'll probably need two Line parameters for
         --  Report_ASIS_Bug procedure

         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Is_Equal",
            Ex          => Ex);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : Asis.Text.Line;
      Right : Asis.Text.Line)
      return  Boolean
   is
   begin
      Check_Validity (Left,  "Asis.Text.Is_Identical");
      Check_Validity (Right, "Asis.Text.Is_Identical");

      return (Left.Length            = Right.Length             and then
              Left.Rel_Sloc          = Right.Rel_Sloc           and then
              Left.Enclosing_Context = Right.Enclosing_Context  and then
              Left.Enclosing_Unit    = Right.Enclosing_Unit);
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : Asis.Text.Line) return Boolean is
      Result : Boolean;
   begin
      --  Here we have to simulate the predefined "=" operation for
      --  Line as if it is called as 'Right + Nil_Line'
      Result := Right.Sloc              = No_Location      and then
                Right.Length            = 0                and then
                Right.Rel_Sloc          = No_Location      and then
                Right.Enclosing_Unit    = Nil_Unit         and then
                Right.Enclosing_Context = Non_Associated   and then
                Right.Enclosing_Tree    = Nil_Tree         and then
                Right.Obtained          = Nil_ASIS_OS_Time;

      return Result;
   end Is_Nil;

   function Is_Nil (Right : Asis.Text.Line_List) return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;

   function Is_Nil (Right : Asis.Text.Span) return Boolean is
   begin
      return (Right.Last_Line < Right.First_Line) or else
             ((Right.Last_Line = Right.First_Line) and then
              (Right.Last_Column < Right.First_Column));
   end Is_Nil;

   -----------------------
   -- Is_Text_Available --
   -----------------------

   function Is_Text_Available (Element : Asis.Element) return Boolean is
      El_Kind   : constant Internal_Element_Kinds := Int_Kind     (Element);
      Spec_Case : constant Special_Cases          := Special_Case (Element);
   begin
      Check_Validity (Element,
                      "Asis.Text.Is_Text_Available");
      if El_Kind = Not_An_Element                          or else
         Is_From_Implicit (Element)                        or else
         Is_From_Instance (Element)                        or else
         Normalization_Case (Element) /= Is_Not_Normalized or else
         Spec_Case  in Explicit_From_Standard .. Stand_Char_Literal
      then
         return False;
      else
         return True;
      end if;

   end Is_Text_Available;

   ----------------------
   -- Last_Line_Number --
   ----------------------

   function Last_Line_Number
     (Element : Asis.Element)
      return    Asis.Text.Line_Number
   is
      Sp : Asis.Text.Span;
   begin
      Check_Validity (Element, "Asis.Text.Last_Line_Number");

      if Is_Text_Available (Element) then
         Sp := Element_Span (Element);

         return Sp.Last_Line;

      else

         return 0;

      end if;

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Last_Line_Number",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Last_Line_Number",
            Arg_Element => Element,
            Ex          => Ex);
   end Last_Line_Number;

   ------------
   -- Length --
   ------------

   function Length
     (The_Line : Asis.Text.Line)
      return     Asis.Text.Character_Position
   is
   begin
      Check_Validity (The_Line, "Asis.Text.Length");

      return Line_Length (The_Line);

   end Length;

   ----------------
   -- Line_Image --
   ----------------

   function Line_Image (The_Line : Asis.Text.Line) return Wide_String is
   begin

      Check_Validity (The_Line, "Asis.Text.Line_Image");

      if Line_Length (The_Line) = 0 then
         return "";
      end if;

      declare
         ASIS_Line_Start : constant Source_Ptr := Line_Location (The_Line);
         S               :          Source_Ptr := Line_Start (ASIS_Line_Start);
         Space_Len       : constant Natural    :=
           Natural (ASIS_Line_Start - S);
         Space_String    :          Wide_String (1 .. Space_Len);
         --  We cannot have more padding white spaces then Space_Len

         Space_Num : Natural := 0;
         --  Counter for padding spaces

         Sindex : constant Source_File_Index := Get_Source_File_Index (S);
         Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);

         Line_Wide_Img : constant Wide_String := Line_Wide_Image (The_Line);
      begin
         --  first, padding the beginning of the image if needed:

         while S < ASIS_Line_Start loop
            Space_Num := Space_Num + 1;

            if Get_Character (S) = ASCII.HT then
               Space_String (Space_Num) := To_Wide_Character (ASCII.HT);
               S := S + 1;
            elsif Is_Start_Of_Wide_Char_For_ASIS (Src, S) then
               Skip_Wide_For_ASIS (Src, S);
               Space_String (Space_Num) := ' ';
            else
               Space_String (Space_Num) := ' ';
               S := S + 1;
            end if;
         end loop;

         return Space_String (1 .. Space_Num) & Line_Wide_Img;

      end;
   exception
      when ASIS_Inappropriate_Line =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => "Asis.Text.Line_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Line_Image",
            Arg_Line    => The_Line,
            Ex          => Ex);
   end Line_Image;

   ---------------------
   -- Lines (Element) --
   ---------------------

   function Lines (Element : Asis.Element) return Asis.Text.Line_List is
   begin
      Check_Validity (Element, "Asis.Text.Lines (Element)");

      if not Is_Text_Available (Element) then
         return Nil_Line_List;
      end if;

      return Lines (Element, Element_Span (Element));

   exception
      when ASIS_Inappropriate_Element =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Lines (Element)",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Lines (Element)",
            Arg_Element => Element,
            Ex          => Ex);
   end Lines;

   ---------------------------
   -- Lines (Element, Span) --
   ---------------------------

   function Lines
     (Element  : Asis.Element;
      The_Span : Asis.Text.Span)
      return     Asis.Text.Line_List
   is
   begin
      Check_Validity (Element, "Asis.Text.Lines (Element, Span)");

      if not Is_Text_Available (Element) then
         return Nil_Line_List;
      end if;

      if Is_Nil (The_Span) or else
         The_Span.Last_Line > Line_Number (Number_Of_Lines (Element))
      then
         Raise_ASIS_Inappropriate_Line_Number ("Lines (Element, Span)");
      end if;

      declare
         LList : Asis.Text.Line_List :=
            Lines (Element, The_Span.First_Line, The_Span.Last_Line);
         --  this call to Lines is "tree-swapping-safe";
         --  note also, that this call to Lines should not raise
         --  any exception, because all the checks are already done
         First_Line : constant Line_Number := LList'First;
         Last_Line  : constant Line_Number  := LList'Last;

         First_Line_Sloc : Source_Ptr := Line_Location (LList (First_Line));
         --  Needs adjustment if The_Span does does not start from the first
         --  column.

         Count : Character_Position;

         Sindex : constant Source_File_Index :=
           Get_Source_File_Index (First_Line_Sloc);
         Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      begin
         --  and now we have to adjust the first and the last line in LList:
         --  for the first line both the line location and the line length
         --  should be adjusted, for the last line - the line length only;
         --  the case when there is only one line is special:

         if The_Span.First_Column > 1 then
            Count := The_Span.First_Column - 1;

            while Count > 0 loop

               if Is_Start_Of_Wide_Char_For_ASIS (Src, First_Line_Sloc) then
                  Skip_Wide_For_ASIS (Src, First_Line_Sloc);
               else
                  First_Line_Sloc := First_Line_Sloc + 1;
               end if;

               Count := Count - 1;
            end loop;

            Set_Line_Location
              (L => LList (First_Line),
               S => First_Line_Sloc);
         end if;

         if First_Line = Last_Line then
            --  Special case when there is only one line.
            Set_Line_Length
              (L => LList (First_Line),
               N => The_Span.Last_Column - The_Span.First_Column + 1);
         else
            Set_Line_Length
              (L => LList (First_Line),
               N => Line_Length (LList (First_Line)) -
                    The_Span.First_Column + 1);

            Set_Line_Length
              (L => LList (Last_Line),
               N => The_Span.Last_Column);
         end if;

         return LList;

      end;
   exception
      when ASIS_Inappropriate_Element | ASIS_Inappropriate_Line_Number =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => "Asis.Text.Lines (Element, Asis.Text.Span)",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Lines (Element, Asis.Text.Span)",
            Arg_Element => Element,
            Arg_Span    => The_Span,
            Ex          => Ex);
   end Lines;

   --------------------------------------------
   -- Lines (Element, First_Line, Last_Line) --
   --------------------------------------------

   function Lines
     (Element    : Asis.Element;
      First_Line : Line_Number_Positive;
      Last_Line  : Line_Number)
      return       Asis.Text.Line_List
   is
      Result_List : Line_List (First_Line .. Last_Line);
      --  there is no harm to define result list here, because the
      --  real work with it will be started when all the tests are passed.
   begin

      Check_Validity
        (Element, "Asis.Text.Lines (Element, First_Line, Last_Line)");

      if Is_Nil (Element) then
         return Nil_Line_List;
      end if;

      if First_Line > Last_Line or else  --  ???
         Last_Line > Line_Number (Number_Of_Lines (Element))
      then
         Raise_ASIS_Inappropriate_Line_Number
           ("Asis.Text.Lines (Element, First_Line, Last_Line)");
      end if;

      --  if we are here, we have Result_List consisting of Nil_Lines,
      --  and we know, that all the conditions for returning the
      --  proper Line_List are met. So we have to make proper settings
      --  for the fields of all the Lines from Result_List
      Set_Lines (Result_List, Element);
      --  this is "tree-swapping-safe"

      return Result_List;

   exception
      when ASIS_Inappropriate_Element | ASIS_Inappropriate_Line_Number =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call =>
               "Asis.Text.Lines (Asis.Element, First_Line, Last_Line)",
               Argument   => Element);
         end if;

         raise;
      when Ex : others =>
         --  We construct the fake Span value to pass the line numbers into
         --  the diagnosis
         Report_ASIS_Bug
           (Query_Name  =>
              "Asis.Text.Lines (Asis.Element, First_Line, Last_Line)",
            Arg_Element => Element,
            Arg_Span    => (First_Line   => First_Line,
                            First_Column => 1,
                            Last_Line    => Last_Line,
                            Last_Column  => Character_Position'Last),
            Ex          => Ex);
   end Lines;

   -----------------------
   -- Non_Comment_Image --
   -----------------------

   function Non_Comment_Image
     (The_Line : Asis.Text.Line)
      return     Wide_String
   is
   begin
      Check_Validity (The_Line, "Asis.Text.Non_Comment_Image");

      if The_Line.Length = 0 then
         --  just a small optimization
         return "";
      end if;

      declare
         The_Line_Image : constant Wide_String := Line_Image (The_Line);
         Comment_Pos    : constant Source_Ptr     := Comment_Beginning
           (Text_Buffer (To_String (The_Line_Image)));

      begin

         if Comment_Pos = No_Location then
            --  no comment in this Line
            return The_Line_Image;
         else
            return The_Line_Image
              (The_Line_Image'First .. Natural (Comment_Pos) - 1);
         end if;

      end;

   exception
      when ASIS_Inappropriate_Line =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => "Asis.Text.Non_Comment_Image");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => "Asis.Text.Non_Comment_Image",
            Arg_Line    => The_Line,
            Ex          => Ex);
   end Non_Comment_Image;

end Asis.Text;
