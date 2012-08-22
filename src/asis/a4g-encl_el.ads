------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . E N C L _ E L                           --
--                                                                          --
--                                 S p e c                                  --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines for computing the enclosing element
--  for the Asis.Elements.Enclosing_Element function

with Asis;

package A4G.Encl_El is

   function Corresponding_Instantiation
     (Element : Asis.Element)
      return Asis.Element;
   --  This function accepts an Element representing an expanded generic
   --  declaration as an argument and returns the generic instantiation
   --  which was expanded in the argument declaration. According to subclause
   --  15.26, this instantiation should be returned as the Enclosing_Element
   --  for the expanded generic declaration.
   --
   --  Should we move this function in Asis.Extensions?

   function Enclosing_For_Explicit_Instance_Component
     (Element : Asis.Element)
      return Asis.Element;
   --  Computes the Enclosing Element for an explicit component of an
   --  expanded generic declaration. The problem in this case is, that if
   --  the result represents the whole expanded declaration, the
   --  Special_Case field of the result should be properly set

   function Enclosing_Element_For_Explicit
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for explicit
   --  elements

   function Enclosing_Element_For_Implicit
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for implicit
   --  elements. It's only partially implemented for now.

   function Enclosing_Element_For_Limited_View
     (Element : Asis.Element)
      return Asis.Element;
   --  This is the general constructor of enclosing element for elements from
   --  limited view.

end A4G.Encl_El;
