------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . S I N P U T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2008, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package adds to the GNAT Sinput package some utility routines
--  used for obtaining and/or analyzing the pieces of the compilation
--  unit's source code from the source buffer.
--
--  Note, that up to the version 3.09, the Tree_Read procedure in the GNAT
--  Sinput package contains a bug - it does not reset to the initial values
--  the global variables used to implement caching for searching for
--  a source file index. The ASIS implementation includes the corrected
--  version of Sinput package
--
--  The routines defined in this package are intended to be used in the
--  implementation of the Asis.Text package and for implementing queries
--  from other ASIS packages having String or Asis_String as the returned
--  (sub)type.
--
--  All the routines defined in this package rely on the fact that all
--  the source buffers being accessed correspond to the compilable units,
--  so they do not care about error finding and recovery.

with Asis.Text; use Asis.Text;

with Types;     use Types;

package A4G.A_Sinput is

   function A_Get_Column_Number (P : Source_Ptr) return Source_Ptr;
   --  This function differs from the Sinput.Get_Column_Number function in two
   --  aspects. First, it does not make any transformations of Tab characters
   --  into equivalent sequences of blanks to represent the standard 1,9,17..
   --  spacing pattern, it returns the column number of the specified
   --  Source_Ptr value as the "distance" from the  beginning of the
   --  corresponding line in the source text. Second, this function considers a
   --  content of the source buffer as a possible encoding of wide character
   --  string and counts the column number in wide characters that make up
   --  the source code.

   function Wide_String_Image (Node : Node_Id) return Wide_String;
   --  for Node of N_String_Literal, N_Defining_Operator_Symbol or
   --  N_Operator_Symbol kind returns the string image of the corresponding
   --  represented string literal, including string quoters, as it is
   --  required by the ASIS queries Value_Image, Name_Image and
   --  Defining_Name_Image. It is an error to call this function for
   --  a node of some other node kind. This function transformes the internal
   --  representation of the argument construct taking into account the
   --  encoding method.

   function Operator_Image (Node : Node_Id) return String;
   --  this function returns the string imege of an operator_symbol
   --  from infix calls to operator functions. It works on nodes of
   --  N_Identifier and N_Op_Xxx kind. The result includes string quotes
   --  as for the prefix call to operator function.

   function Get_Character (P : Source_Ptr) return Character;
   --  Returns the character pointed by P.
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv and from Subservises

   function Get_Wide_Word
     (P_Start : Source_Ptr;
      P_End   : Source_Ptr)
   return Wide_String;
   --  Returns a part of the source text corresponding to the part of ints
   --  internal representation bounded by P_Start .. P_End. Takes into account
   --  the encoding of wide characters and makes the corresponding conversions.
   --  This function does not check, that P_Start and P_End both point into the
   --  same source.
   --  This function is not "tree-swapping-safe"

   function Source_Locations_To_Span
     (Span_Beg : Source_Ptr;
      Span_End : Source_Ptr)
      return Span;
   --  Transforms the pair of locations in the source buffer into an
   --  ASIS Span. Note, that ASIS Span counts the source positions in wide
   --  characters, whereas Span_Beg and Span_End are pointers to the internal
   --  string (but not wide string!) representation of the source text!
   --  This function is not "tree-swapping-safe"

   function Get_Location (E : Asis.Element) return Source_Ptr;
   --  Returns the value of the Sloc field of the (original) node
   --  on which E is based
   --  This function is "tree-swapping-safe"
   --  FROM Subservises

   function Number_Of_Lines (E : Asis.Element) return Integer;
   --  Returns the number of the last line in the source file accessable
   --  through this Element, taking into account Source_Reference pragma if
   --  it presents in the source file.
   --
   --  This function is "tree-swapping-safe"
   --  FROM Subservises

   function Identifier_Image (Ident : Node_Id) return String;
   --  For a node, which is of N_Identifier or N_Defining_Identifier kind,
   --  this function returns the string image of the corresponding
   --  (defining) identifier
   --  Note, that this function does not take into account the possible
   --  encoding of upper half wide characters. The results of this function are
   --  used in internal Compilation Unit table only, so this function does not
   --  make any problem for proper encoding processing in Asis.Text. But anyway
   --  this should be revised to completely conform to the source
   --  representation required by the Ada standard.

   function Exp_Name_Image (Name : Node_Id) return String;
   --  For a node, which is of N_Defining_Program_Unit_Name,
   --  N_Defining_Identifier, N_Expanded_Name or N_Identifier kind,
   --  this function returns the string image of the corresponding name

   function Comment_Beginning (Line_Image : Text_Buffer) return Source_Ptr;
   --  Returns position of the first _comment_ hyphen in the argument string.
   --  If there is no comment, then returns No_Location.
   --  The string has to correspond to a legal Ada program fragment,
   --  otherwise a constraint error may be raised.
   --
   --  Note, that this function can be used for detecting the comment beginning
   --  in the line buffer of the Standard String type, because the index range
   --  of Text_Buffer (and the range of Source_Ptr) includes the low bound of
   --  Positive.

   ------------------------------------------------------------------------
   --  Staring from this point, some mess exists, which originates from  --
   --  collecting all the text processing/source buffer-processing       --
   --  routines from Subservices and S_B_Serv                            --
   ------------------------------------------------------------------------

   function Next_Identifier (P : Source_Ptr) return Source_Ptr;
   --  Returns the location of the first charaster of the identifier which
   --  should follow the position indicated by P. Initially this
   --  function was intended to find the beginning of the pragma identifier,
   --  so two requirements should be met for its correct use: P points to
   --  some separator (as defined by RM 95 2.2 (3-6), and the next lexem
   --  should be either comment or identifier.
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function Get_String_End (P : Source_Ptr) return Source_Ptr;
   --  Supposing that P points to the leading quotation of the string
   --  literal, this function defines the location of the quotation
   --  ending the string constant.
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function Get_Num_Literal_End (P : Source_Ptr) return Source_Ptr;
   --  Supposing that P points to the first character of a numeric
   --  literal, this function defines the location of the last character
   --  of the literal.
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function Search_Rightmost_Symbol
     (P    : Source_Ptr;
      Char : Character := ';')
      return Source_Ptr;
   --  The function returns the location of the rightmost symbol equial
   --  to Char for the position indicated by P (including P itself).
   --  Comments are skipped during the search
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function Rightmost_Non_Blank (P : Source_Ptr) return Source_Ptr;
   --  returns the first non-blank symbol (excluding format effectors)
   --  following P (if P itself is a non-blank symbol, P is returned).
   --  Comments are skipped

   type In_Word_Condition is access function (P : Source_Ptr) return Boolean;
   --  I wish I had time to get rid of this awkward approach based on
   --  In_Word_Condition!  :((

   function Get_Word_End
     (P       : Source_Ptr;
      In_Word : In_Word_Condition)
      return Source_Ptr;
   --  The function returns the location of the firs/last character of the
   --  lexical element which contains character pointed by P. It is supposed
   --  that P does not point inside comment, separator or delimiter (RM95 2.2)
   --
   --  The first version of these function (with the second parameters of
   --  In_Word_Char_Condition type is used when it is enough to test only one
   --  character to get the answer. But if it is necessary to examine some
   --  characters before/after the given character, the second form should be
   --  used with the corresponding test function.
   --
   --  The initial idea is to use these functions to get the start/end of
   --  identifiers, numeric literals and string literals.
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function In_Identifier (P : Source_Ptr) return Boolean;
   --  Returns true if P points somewhere inside an identifier, and False
   --  otherwise
   --  This function is not "tree-swapping-safe"
   --  FROM S_B_Serv

   function Search_Prev_Word (S : Source_Ptr) return Source_Ptr;
   --  Returns the location of the previous word end.
   --  The comments are skipped.
   --  This function is not "tree-swapping-safe"
   --  FROM Subservises

   function Search_Beginning_Of_Word (S : Source_Ptr) return Source_Ptr;
   --  Returns the location of the beginning of the word to which S points.
   --  This function is not "tree-swapping-safe"
   --  FROM Subservises

   function Search_Prev_Word_Start (S : Source_Ptr) return Source_Ptr;
   --  Equivalent to Search_Beginning_Of_Word (Search_Prev_Word (S))

   function Search_End_Of_Word (S : Source_Ptr) return Source_Ptr;
   --  Returns the location of the end of the word to which S points.
   --  This function is not "tree-swapping-safe"
   --  FROM Subservises
   --  It's crazy to have it along with Get_Word_End!!!

   function Search_Next_Word (S : Source_Ptr) return Source_Ptr;
   --  Returns the location of the next word beginning. The comments
   --  are skipped.
   --  This function is not "tree-swapping-safe"
   --  FROM Subservises

   function Search_Left_Parenthesis (S : Source_Ptr) return Source_Ptr;
   --  Returns the location of the first inclusion of left parenthesis before
   --  the location in source file to which S points.
   --  This function is not "tree-swapping-safe"
   --  FROM Subservises

   function Is_EOL_Char (Ch : Character) return Boolean;
   --  Checks if Ch is a character defining an end of line. According to RM05
   --  2.2(2/2), "a sequence of one or more format_effectors other than the
   --  character whose code position is 16#09# (CHARACTER TABULATION) signifies
   --  at least one end of line."

   function Get_Wide_Ch (S : Source_Ptr) return Wide_Character;
   --  Provided that S points to the first character of the internal
   --  representation of some character from the original source, returns
   --  this riginal source character, taking into account the encoding method

   function Is_Start_Of_Wide_Char_For_ASIS
     (S    : Source_Buffer_Ptr;
      P    : Source_Ptr;
      C    : Source_Ptr := No_Location)
      return Boolean;
   --  Determines if S (P) is the start of a wide character sequence. This
   --  function differs from Widechar in two aspects: first, it assumes that
   --  the bracket encoding can not be used in a comment text, and if set, the
   --  actual for C should point to the beginning of the comment that in the
   --  source buffer, and second, in any non-comment text it assumes that a
   --  bracket encoding is always set ON (see the description of -gnatW option
   --  in GNAT UGN).

   procedure Skip_Wide_For_ASIS (S : Source_Buffer_Ptr; P : in out Source_Ptr);
   --  Similar to Widechar.Skip_Wide, but always skips bracked encoding
   --  sequense. Before calling this function, the caller should check thar
   --  Is_Start_Of_Wide_Char_For_ASIS (S, P) is True

end A4G.A_Sinput;
