------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                           A 4 G . S K I P _ T B                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2003, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  Ada Core Technologies Inc --
-- (http://www.gnat.com).                                                   --
--                                                                          --
------------------------------------------------------------------------------

with Asis;

with Types;  use Types;

package A4G.Skip_TB is

   --  This package encapsulates routines which is used in finding the end of
   --  an Element Span tp scip all the "syntax sugar", such as ";", ")",
   --  "end if", "end Unit_Name" etc.

   function Skip_Trailing_Brackets
     (E : Asis.Element;
      S : Source_Ptr)
      return Source_Ptr;

   --  This function encapsulates all the cases when different kinds of
   --  "syntax sugar" should be skiped, being implemented as a look-up
   --  table for Internal_Element_Kinds. It returns the location of the
   --  very last character of the image of E, provided that S is set to
   --  point to iys last component. If a given Element has several levels
   --  of subcomponents, this function is called each time when the
   --  next right-most component is traversed in the recursive processing
   --  defined by A4G.Span_End.Nonterminal_Component.

   function Needs_Extra_Parentheses (E : Asis.Element) return Boolean;
   --  This function detects if we need an extra parenthesis in the qualified
   --  expression in the situation like this
   --    Var : String := String'((1 => C));
   --  The problem is that from the point of view of ASIS Element hierarchy
   --  it there is no difference between this situation and
   --    Var : String := String'(1 => C);
   --  because we do not have A_Parenthesized_Expression in the first case,
   --  we just have to decompose the qualified expression according to the
   --  syntax rule: subtype_mark'(expression), and the outer pair of
   --  parenthesis belongs to a qualified expression, and the inner - to an
   --  enclosed aggregate.
   --  We need this function in the spec to use it in gnatpp.

end A4G.Skip_TB;
