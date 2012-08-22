------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . A _ S I N P U T                          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with System.WCh_Con;          use System.WCh_Con;

with Asis.Set_Get;            use Asis.Set_Get;

with Atree;                   use Atree;
with Opt;                     use Opt;
with Sinfo;                   use Sinfo;
with Sinput;                  use Sinput;
with Widechar;                use Widechar;

package body A4G.A_Sinput is

   use ASCII;
   --  Make control characters visible

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Skip_Comment (P : in out Source_Ptr);
   --  When P is set on the first '-' of a comment, this procedure resets
   --  the value of P to the first character of the group of control
   --  characters signifying the end of line containing the comment
   --  initially indicated by P.
   --
   --  This procedure should not be used for the last comment in the
   --  group of comments following a compilation unit in a compilation.

   procedure Skip_String (P : in out Source_Ptr);
   --  When P set on the first quoter of a string literal (it may be '"' or
   --  '%', this procedure resets the value of P to the first character
   --  after the literal.

   -------------------------
   -- A_Get_Column_Number --
   -------------------------

   function A_Get_Column_Number (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S      : Source_Ptr;
      Result : Source_Ptr := 0;
   begin
      S := Line_Start (P);

      while S <= P loop

         if Is_Start_Of_Wide_Char_For_ASIS (Src, S) then
            Skip_Wide_For_ASIS (Src, S);
         else
            S := S + 1;
         end if;

         Result := Result + 1;
      end loop;

      return Result;
   end A_Get_Column_Number;

   -----------------------
   -- Comment_Beginning --
   -----------------------

   function Comment_Beginning (Line_Image : Text_Buffer) return Source_Ptr is
      Line_Image_Start : constant Source_Ptr := Line_Image'First;
      Line_Image_End   : constant Source_Ptr := Line_Image'Last;
      Scanner_Pos      : Source_Ptr;
      String_Delimiter : Standard.Character;
   begin
      Scanner_Pos := Line_Image_Start - 1;

      Scan_The_Line : while Scanner_Pos < Line_Image_End loop

         Scanner_Pos := Scanner_Pos + 1;

         case Line_Image (Scanner_Pos) is

            when '"' | '%' =>

               if not ((Scanner_Pos - 1) >= Line_Image_Start and then
                       Line_Image (Scanner_Pos - 1) = '''
                     and then
                       (Scanner_Pos + 1) <= Line_Image_End and then
                       Line_Image (Scanner_Pos + 1) = ''')
               then
                  --  We have to awoid considering character literals '"'
                  --  '%' as string brackets

                  String_Delimiter := Line_Image (Scanner_Pos);

                  Skip_String_Literal : loop
                     Scanner_Pos := Scanner_Pos + 1;

                     if Line_Image (Scanner_Pos) = String_Delimiter then

                        --  we are in a legal Ada source, therefore:
                        if Scanner_Pos < Line_Image'Last and then
                           Line_Image (Scanner_Pos + 1) = String_Delimiter
                        then
                           --  Delimiter as character inside the literal.
                           Scanner_Pos := Scanner_Pos + 1;
                        else
                           --  End of the literal.
                           exit Skip_String_Literal;
                        end if;

                     end if;

                  end loop Skip_String_Literal;

               end if;

            when '-' =>

               if    (Scanner_Pos < Line_Image'Last) and then
                     (Line_Image (Scanner_Pos + 1) = '-')
               then
                  return Scanner_Pos;
               end if;

            when others =>
               null;

         end case;

      end loop Scan_The_Line;

      --  There wasn't any comment if we reach this point.
      return No_Location;
   end Comment_Beginning;

   --------------------
   -- Exp_Name_Image --
   --------------------

   function Exp_Name_Image (Name : Node_Id) return String is
      Prefix_Node   : Node_Id;
      Selector_Node : Node_Id;
   begin
      if Nkind (Name) = N_Identifier or else
         Nkind (Name) = N_Defining_Identifier
      then
         --  ????? See E729-A04!
         return Identifier_Image (Name);
      end if;

      if Nkind (Name) = N_Defining_Program_Unit_Name then
         Prefix_Node   := Sinfo.Name (Name);
         Selector_Node := Defining_Identifier (Name);
      else
         --  Nkind (Name) = N_Expanded_Name
         Prefix_Node   := Prefix (Name);
         Selector_Node := Selector_Name (Name);
      end if;

      return   Exp_Name_Image (Prefix_Node)
             & '.'
             & Identifier_Image (Selector_Node);  --  ???
   end Exp_Name_Image;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character (P : Source_Ptr) return Character is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
   begin
      return Src (P);
   end Get_Character;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : Asis.Element) return Source_Ptr is
   begin
      return Sloc (Node (E));
   end Get_Location;

   -------------------------
   -- Get_Num_Literal_End --
   -------------------------

   function Get_Num_Literal_End (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S      : Source_Ptr;
      B_Char : Character;
   begin
      --  Src (P) is the leading digit of a numeric literal
      S := P + 1;
      loop
         if Is_Hexadecimal_Digit (Src (S)) or else Src (S) = '_' then
            S := S + 1;
         elsif Src (S) = '#' or else Src (S) = ':' then
            --  based literal: 16#E#E1 or 016#offf#
            --  J.2 (3): "The number sign characters (#) of a based_literal
            --  can be replaced by colons (:) provided that the replacement
            --  is done for both occurrences. But in case of a colon, we
            --  have to make sure that we indeed have a based literal, but not
            --  a decimal literal immediatelly followed by an assignment sign,
            --  see G119-012:
            --
            --      SPLIT_INDEX:INTEGER RANGE 1..80:=1;

            if Src (S) = ':' and then Src (S + 1) = '=' then
               S := S - 1;
               exit;
            end if;

            B_Char := Src (S);
            --  and now - looking for trailing '#' or ':':
            S := S + 1;

            while Src (S) /= B_Char loop
               S := S + 1;
            end loop;

            if Src (S + 1) = 'E' or else
               Src (S + 1) = 'e'
            then
               --  this means something like 5#1234.1234#E2
               S := S + 2;
            else
               exit;
            end if;

         elsif Src (S) = '+'
           or else
             Src (S) = '-'
         then   -- 12E+34 or 12+34?

            if Src (S - 1) = 'E'
              or else
                Src (S - 1) = 'e'
            then
               --  it is the sign of the exponent
               S := S + 1;
            else
               S := S - 1; -- to go back in the literal
               exit;
            end if;

         elsif  Src (S) = '.' then        -- 3.14 or 3..14?

            if Is_Hexadecimal_Digit (Src (S + 1)) then
               S := S + 1;
            else
               S := S - 1; -- to go back in the literal
               exit;
            end if;

         else -- for sure, we already are outside the literal
            S := S - 1; -- to go back in the literal
            exit;
         end if;
      end loop;

      return S;
   end Get_Num_Literal_End;

   --------------------
   -- Get_String_End --
   --------------------

   function Get_String_End (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S      : Source_Ptr;
      Quote  : Character;
   begin
      --  Src (P) is the leading quotation of the non-nul string constant
      --  which can be either '"' OR '%' (J.2 (2)).
      Quote := Src (P);
      S := P + 1;
      loop

         if Src (S) = Quote and then Src (S + 1) = Quote then
            S := S + 2;
         elsif Src (S) /= Quote then

            if Is_Start_Of_Wide_Char_For_ASIS (Src, S) then
               Skip_Wide_For_ASIS (Src, S);
            else
               S := S + 1;
            end if;

         else
            --  S points to the trailing quotation of the constant
            exit;
         end if;

      end loop;

      return S;
   end Get_String_End;

   -------------------
   -- Get_Wide_Word --
   -------------------

   function Get_Wide_Word
     (P_Start : Source_Ptr;
      P_End   : Source_Ptr)
      return Wide_String
   is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P_Start);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);

      Result   : Wide_String (1 .. Positive (P_End - P_Start + 1));
      Last_Idx : Natural := 0;
      Next_Ch  : Char_Code;
      S        : Source_Ptr;
      Success  : Boolean;
      pragma Unreferenced (Success);
   begin

      S := P_Start;

      while S <= P_End loop

         Last_Idx := Last_Idx + 1;

         if Is_Start_Of_Wide_Char_For_ASIS (Src, S) then
            Scan_Wide (Src, S, Next_Ch, Success);
            Result (Last_Idx) := Wide_Character'Val (Next_Ch);

         else
            Result (Last_Idx) := To_Wide_Character (Src (S));
            S := S + 1;
         end if;

      end loop;

      return Result (1 .. Last_Idx);
   end Get_Wide_Word;

   -----------------
   -- Get_Wide_Ch --
   -----------------

   function Get_Wide_Ch (S : Source_Ptr) return Wide_Character is
      Sindex  : constant Source_File_Index := Get_Source_File_Index (S);
      Src     : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S1      : Source_Ptr                  := S;
      Ch      : Char_Code;
      Result  : Wide_Character;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin

      if Is_Start_Of_Wide_Char_For_ASIS (Src, S1) then
         Scan_Wide (Src, S1, Ch, Success);

         Result := Wide_Character'Val (Ch);

      else
         Result := To_Wide_Character (Src (S1));
      end if;

      return Result;

   end Get_Wide_Ch;

   ------------------
   -- Get_Word_End --
   ------------------

   function Get_Word_End
     (P       : Source_Ptr;
      In_Word : In_Word_Condition)
      return Source_Ptr
   is
      S      : Source_Ptr;
   begin
      S := P;
      while In_Word (S + 1) loop
         S := S + 1;
      end loop;
      return S;
   end Get_Word_End;

   ----------------------
   -- Identifier_Image --
   ----------------------

   function Identifier_Image (Ident : Node_Id) return String is
      Image_Start : Source_Ptr;
      Image_End   : Source_Ptr;
   begin
      Image_Start := Sloc (Ident);
      Image_End   := Get_Word_End (P       => Image_Start,
                                    In_Word => In_Identifier'Access);
      --  See E729-A04!!!
      return To_String (Get_Wide_Word (Image_Start, Image_End));
   end Identifier_Image;

   -------------------
   -- In_Identifier --
   -------------------

   function In_Identifier (P : Source_Ptr) return Boolean is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      Char   : Character;
      Result : Boolean := True;
   begin
      Char := Src (P);

      if Char = ' '      or else
         Char = '&'      or else
         Char = '''      or else
         Char = '('      or else
         Char = ')'      or else
         Char = '*'      or else
         Char = '+'      or else
         Char = ','      or else
         Char = '-'      or else
         Char = '.'      or else
         Char = '/'      or else
         Char = ':'      or else
         Char = ';'      or else
         Char = '<'      or else
         Char = '='      or else
         Char = '>'      or else
         Char = '|'      or else
         Char = '!'      or else
         Char = ASCII.LF or else
         Char = ASCII.FF or else
         Char = ASCII.HT or else
         Char = ASCII.VT or else
         Char = ASCII.CR
      then
         Result := False;
      end if;

      return Result;

   end In_Identifier;

   -----------------
   -- Is_EOL_Char --
   -----------------

   function Is_EOL_Char (Ch : Character) return Boolean is
      Result : Boolean := False;
   begin

      Result :=
        Ch = ASCII.CR
       or else
        Ch = ASCII.LF
       or else
        Ch = ASCII.FF
       or else
        Ch = ASCII.VT;

      return Result;
   end Is_EOL_Char;

   ------------------------------------
   -- Is_Start_Of_Wide_Char_For_ASIS --
   ------------------------------------

   function Is_Start_Of_Wide_Char_For_ASIS
     (S    : Source_Buffer_Ptr;
      P    : Source_Ptr;
      C    : Source_Ptr := No_Location)
      return Boolean
   is
      Result : Boolean := False;
   begin

      if C /= No_Location and then P > C then

         --  We are in comment, so we can not have bracket encoding
         if Wide_Character_Encoding_Method /= WCEM_Brackets then
            Result := Is_Start_Of_Wide_Char (S, P);
         end if;

      else
         Result := Is_Start_Of_Wide_Char (S, P);

         if not Result then
            Result := P <= S'Last - 2
                 and then S (P) = '['
                 and then S (P + 1) = '"'
                 and then (S (P + 2) in '0' .. '9'
                               or else
                              S (P + 2) in 'a' .. 'f'
                               or else
                              S (P + 2) in 'A' .. 'F');
         end if;

      end if;

      return Result;
   end Is_Start_Of_Wide_Char_For_ASIS;

   ---------------------
   -- Next_Identifier --
   ---------------------

   function Next_Identifier (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S      : Source_Ptr;
   begin
      S := P + 1;
      while not Is_Letter (Src (S)) loop

         if Src (S) = '-' and then Src (S + 1) = '-' then
            Skip_Comment (S);
         else
            S := S + 1;
         end if;

      end loop;

      return S;
   end Next_Identifier;

   ---------------------
   -- Number_Of_Lines --
   ---------------------

   function Number_Of_Lines (E : Asis.Element) return Integer is
      SFI : constant Source_File_Index :=
        Get_Source_File_Index (Get_Location (E));
   begin
--      return Integer (Num_Source_Lines (SFI) + Line_Offset (SFI));
      return Integer (Num_Source_Lines (SFI));
   end Number_Of_Lines;

   --------------------
   -- Operator_Image --
   --------------------

   function Operator_Image (Node : Node_Id) return String is
      S_Start : constant Source_Ptr := Sloc (Node);
      --  S_Start points to the leading character of a given operator symbol.
      Sindex  : constant Source_File_Index :=
                   Get_Source_File_Index (S_Start);
      Src     : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S_End   : Source_Ptr := S_Start;
      --  should be set as pointing to the last character of a given
      --  operator symbol.
      Ch      : Character;
   begin
      Ch := Src (S_Start);

      if        Ch = 'A' or else Ch = 'a'    -- "abs" or "and"
        or else Ch = 'M' or else Ch = 'm'    -- "mod"
        or else Ch = 'N' or else Ch = 'n'    -- "not"
        or else Ch = 'R' or else Ch = 'r'    -- "rem"
        or else Ch = 'X' or else Ch = 'x'    -- "xor"
      then
         S_End := S_Start + 2;

      elsif Ch = 'O' or else Ch = 'o' then -- "or"
         S_End := S_Start + 1;

      elsif        Ch = '='             -- "="
           or else Ch = '+'             -- "+"
           or else Ch = '-'             -- "-"
           or else Ch = '&'             -- "&"
      then
         S_End := S_Start;

      elsif        Ch = '/'  -- "/=" or "/"?
           or else Ch = '<'  -- "<=" or "<"?
           or else Ch = '>'  -- ">=" or ">"?
           or else Ch = '*' -- "**" or "*"?
      then
         Ch := Src (S_Start + 1);
         if Ch = '=' or else     -- "/=", "<=" or ">="
             Ch = '*'            -- "**"
         then
            S_End := S_Start + 1;
         else
            S_End := S_Start;
            --  "<", ">", "*" or "/"
         end if;
      end if;

      return (1 => '"') & String (Src (S_Start .. S_End)) & (1 => '"');
   end Operator_Image;

   -------------------------
   -- Rightmost_Non_Blank --
   -------------------------

   function Rightmost_Non_Blank (P : Source_Ptr) return Source_Ptr is
      S : Source_Ptr := P;
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
   begin
      loop
         if Src (S) = '-' and then Src (S + 1) = '-' then
            Skip_Comment (S);
         elsif Is_Graphic (Src (S)) and then Src (S) /= ' ' then
            exit;
         else
            S := S + 1;
         end if;
      end loop;
      return S;
   end Rightmost_Non_Blank;

   ------------------------------
   -- Search_Beginning_Of_Word --
   ------------------------------

   function Search_Beginning_Of_Word (S : Source_Ptr) return Source_Ptr is
      SFI : constant Source_File_Index := Get_Source_File_Index (S);
      Src : constant Source_Buffer_Ptr := Source_Text (SFI);
      S_P : Source_Ptr;

   begin
      S_P := S;
      while S_P >= Source_First (SFI)
        and then (Src (S_P) in 'A' .. 'Z' or else
                  Src (S_P) in 'a' .. 'z' or else
                  Src (S_P) in '0' .. '9' or else
                  Src (S_P) = '_')
      loop
         S_P := S_P - 1;
      end loop;

      return S_P + 1;
   end Search_Beginning_Of_Word;

   ------------------------
   -- Search_End_Of_Word --
   ------------------------

   function Search_End_Of_Word (S : Source_Ptr) return Source_Ptr is
      S_P  : Source_Ptr                 := S;
      SFI  : constant Source_File_Index := Get_Source_File_Index (S);
      Src  : constant Source_Buffer_Ptr := Source_Text (SFI);
      Char : Character;
   begin

      Char := Src (S_P);

      while not (Char = ' '      or else
                 Char = '&'      or else
                 Char = '''      or else
                 Char = '('      or else
                 Char = ')'      or else
                 Char = '*'      or else
                 Char = '+'      or else
                 Char = ','      or else
                 Char = '-'      or else
                 Char = '.'      or else
                 Char = '/'      or else
                 Char = ':'      or else
                 Char = ';'      or else
                 Char = '<'      or else
                 Char = '='      or else
                 Char = '>'      or else
                 Char = '|'      or else
                 Char = '!'      or else
                 Char = ASCII.LF or else
                 Char = ASCII.FF or else
                 Char = ASCII.HT or else
                 Char = ASCII.VT or else
                 Char = ASCII.CR)
      loop
         S_P := S_P + 1;
         Char := Src (S_P);
      end loop;

      S_P := S_P - 1;

      return S_P;

   end Search_End_Of_Word;

   -----------------------------
   -- Search_Left_Parenthesis --
   -----------------------------

   function Search_Left_Parenthesis (S : Source_Ptr) return Source_Ptr is
      S_P : Source_Ptr                 := S - 1;
      SFI : constant Source_File_Index := Get_Source_File_Index (S);
      Src : constant Source_Buffer_Ptr := Source_Text (SFI);
   begin
      loop
         case Src (S_P) is
            when  '(' =>
               return  S_P;
            when  CR | LF   =>
               declare
                  TempS : Source_Ptr := Line_Start (S_P);
               begin
                  while (Src (TempS) /= '-' or else
                          Src (TempS + 1) /= '-')
                     and then
                          TempS < S_P
                  loop
                     TempS := TempS + 1;
                  end loop;
                  S_P := TempS - 1;
               end;

            when others =>
               S_P := S_P - 1;
         end case;
      end loop;
   end Search_Left_Parenthesis;

   ----------------------
   -- Search_Next_Word --
   ----------------------

   function Search_Next_Word (S : Source_Ptr) return Source_Ptr is
      S_P : Source_Ptr                 := S + 1;
      SFI : constant Source_File_Index := Get_Source_File_Index (S);
      Src : constant Source_Buffer_Ptr := Source_Text (SFI);
   begin
      loop
         case Src (S_P) is
            when  ' ' | HT | CR | LF =>
               S_P := S_P + 1;
            when  '-' =>
               if Src (S_P + 1) = '-' then
                  Skip_Comment (S_P);
               else
                  return S_P;
               end if;
            when others  =>
               return S_P;
         end case;
      end loop;
   end Search_Next_Word;

   ----------------------
   -- Search_Prev_Word --
   ----------------------

   function Search_Prev_Word (S : Source_Ptr) return Source_Ptr is
      S_P : Source_Ptr                 := S - 1;
      SFI : constant Source_File_Index := Get_Source_File_Index (S);
      Src : constant Source_Buffer_Ptr := Source_Text (SFI);
   begin
      loop
         case Src (S_P) is
            when  ' ' | HT  =>
               S_P := S_P - 1;
            when  CR | LF   =>
               declare
                  TempS : Source_Ptr := Line_Start (S_P);
               begin
                  while (Src (TempS) /= '-' or else
                          Src (TempS + 1) /= '-')
                       and then
                          TempS < S_P
                  loop
                     TempS := TempS + 1;
                  end loop;
                  S_P := TempS - 1;
               end;
            when others  =>
               return S_P;
         end case;
      end loop;
   end Search_Prev_Word;

   ----------------------------
   -- Search_Prev_Word_Start --
   ----------------------------

   function Search_Prev_Word_Start (S : Source_Ptr) return Source_Ptr is
   begin
      return Search_Beginning_Of_Word (Search_Prev_Word (S));
   end Search_Prev_Word_Start;

   -----------------------------
   -- Search_Rightmost_Symbol --
   -----------------------------

   function Search_Rightmost_Symbol
     (P    : Source_Ptr;
      Char : Character := ';')
      return Source_Ptr
   is
      S      : Source_Ptr := P;
      --  the location to be returned, the search is started from P
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
   begin
      while Src (S) /= Char loop

         if Src (S) = '-' and then Src (S + 1) = '-' then
            Skip_Comment (S);

         elsif (Src (S) = '"' or else Src (S) = '%')
             and then
               not (Src (S - 1) = ''' and then Src (S + 1) = ''')
         then
            Skip_String (S);

         else
            S := S + 1;
         end if;

      end loop;

      return S;
   end Search_Rightmost_Symbol;

   -----------------
   -- Skip_String --
   -----------------

   procedure Skip_String (P : in out Source_Ptr)  is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
      Quoter : constant Character         := Src (P);
   begin
      --  we are in the beginning of a legal string literal in a legal
      --  Ada program. So we do not have to be careful with all
      --  the checks:
      while not (Src (P) = Quoter and then Src (P + 1) /= Quoter) loop
         P := P + 1;
      end loop;
      P := P + 1;
   end Skip_String;

   ------------------
   -- Skip_Comment --
   ------------------

   procedure Skip_Comment (P : in out Source_Ptr) is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr := Source_Text (Sindex);
   begin
      if Src (P) = '-' and then Src (P + 1) = '-' then
         P := P + 2;
         while not (Src (P) = VT or else
                    Src (P) = CR or else
                    Src (P) = LF or else
                    Src (P) = FF)
         loop
            P := P + 1;
         end loop;
      end if;
   end Skip_Comment;

   ------------------------
   -- Skip_Wide_For_ASIS --
   ------------------------

   procedure Skip_Wide_For_ASIS
     (S : Source_Buffer_Ptr;
      P : in out Source_Ptr)
   is
      Old_P                              : constant Source_Ptr := P;
      Old_Wide_Character_Encoding_Method : WC_Encoding_Method;
   begin
      Skip_Wide (S, P);

      if P = Old_P + 1 then
         --  We have a bracket encoding, but the encoding method is different
         --  from WCEM_Brackets
         P := P - 1;
         Old_Wide_Character_Encoding_Method := Wide_Character_Encoding_Method;
         Wide_Character_Encoding_Method := WCEM_Brackets;
         Skip_Wide (S, P);
         Wide_Character_Encoding_Method := Old_Wide_Character_Encoding_Method;

      end if;

   end Skip_Wide_For_ASIS;

   ------------------------------
   -- Source_Locations_To_Span --
   ------------------------------

   function Source_Locations_To_Span
     (Span_Beg : Source_Ptr;
      Span_End : Source_Ptr)
      return Span
   is
      Sp : Span;
   begin
      Sp.First_Line   := Line_Number (Get_Physical_Line_Number   (Span_Beg));
      Sp.First_Column := Character_Position (A_Get_Column_Number (Span_Beg));
      Sp.Last_Line    := Line_Number (Get_Physical_Line_Number   (Span_End));
      Sp.Last_Column  := Character_Position (A_Get_Column_Number (Span_End));

      return Sp;
   end Source_Locations_To_Span;

   -----------------------
   -- Wide_String_Image --
   -----------------------

   function Wide_String_Image (Node : Node_Id) return Wide_String is
      S_Start : constant Source_Ptr := Sloc (Node);
      --  S_Start points to the leading quote of a given string literal.
      Sindex  : constant Source_File_Index :=
                   Get_Source_File_Index (S_Start);
      Src     : constant Source_Buffer_Ptr := Source_Text (Sindex);
      S_End   : Source_Ptr := S_Start + 1;
      --  should be set as pointing to the last character of a
      --  string literal; empty and  non-empty literals are processed
      --  in the same way - we simply take a literal as it is from the
      --  Source Buffer
      Quote  : constant Character := Src (S_Start);
      --  Quoter may be '"' or '%'!
   begin
      loop
         if Src (S_End)     = Quote and then
            Src (S_End + 1) = Quote
         then
            --  doubled string quote as an element of a given string
            S_End := S_End + 2;
         elsif Src (S_End)  /= Quote then
            --  "usial" string element
            S_End := S_End + 1;
         else
            --  S_End points to the trailing quote of a given string
            exit;
         end if;
      end loop;

      declare
         Result   : Wide_String (1 .. Positive (S_End - S_Start + 1));
         Last_Idx : Natural := 0;
         Next_Ch  : Char_Code;
         S        : Source_Ptr;
         Success  : Boolean;
         pragma Unreferenced (Success);
      begin
         S := S_Start;

         while S <= S_End loop

            Last_Idx := Last_Idx + 1;

            if Is_Start_Of_Wide_Char_For_ASIS (Src, S) then
               Scan_Wide (Src, S, Next_Ch, Success);
               Result (Last_Idx) := Wide_Character'Val (Next_Ch);

            else
               Result (Last_Idx) := To_Wide_Character (Src (S));
               S := S + 1;
            end if;

         end loop;

         return Result (1 .. Last_Idx);
      end;

   end Wide_String_Image;

end A4G.A_Sinput;
