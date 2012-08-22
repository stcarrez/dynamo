------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . E X P R _ S E M                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2005, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
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

--  This package contains routines needed for semantic queries from
--  the Asis.Expressions package

with Asis; use Asis;

package A4G.Expr_Sem is

   --  All the routines defined in this package do not check their
   --  arguments - a caller is responsible for the proper use of these
   --  routines

   function Expr_Type (Expression : Asis.Expression) return Asis.Declaration;
   --  using the fact, that Expression is of An_Expression kind and that
   --  the Etype field is set for its Node, this function finds the
   --  type declaration which should be returned as the result of
   --  Corresponding_Expression_Type (Expression)

   function Identifier_Name_Definition
     (Reference_I : Element)
      return        Asis.Defining_Name;

   function Character_Literal_Name_Definition
     (Reference_Ch : Element)
      return         Asis.Defining_Name;
   --  Each of these two functions provides an Element representing the
   --  defining occurrence for its argument, provided that the argument is
   --  of appropriate kind and all the necessary checks have already been
   --  done

   function Is_Reference
     (Name : Asis.Element;
      Ref  : Asis.Element)
      return Boolean;
   --  Provided that Name is of A_Defining_Name kind, this function checks is
   --  Ref is a reference to this name. It is an error to call this function
   --  for any Element which is not of A_Defining_Name as an actual for Name.
   --  Any Element is acceptable as an actual for Ref, and this function does
   --  not raise any exception in any case.
   --
   --  ??? Should we move this function into Asis.Extensions?

   function Needs_List (Reference : Asis.Element) return Boolean;
   --  Supposed to be applied to the argument of the
   --  Corresponding_Name_Definition_List query. Checks if Reference
   --  is ambiguous and refers to more than one entity.

   procedure Collect_Overloaded_Entities (Reference : Asis.Element);
   --  Supposed to be called for the argument of the
   --  Corresponding_Name_Definition_List query in case if Needs_List is
   --  True for it. Collects in the Element Table all the defining
   --  names referred by Reference.
   --  This procedure supposes, that the Element Table is already initialized
   --  in the calling context

   procedure Correct_Result
     (Result    : in out Element;
      Reference :        Element);
   --  This procedure implements a most complicated case for the fix needed
   --  for BB10-002. It correct the result in the situation when we have nested
   --  generic instantiations, and the named number in question is declared in
   --  template. In this situation the approach based on Original_Entity field
   --  returns the defining name from the template, but we need the defining
   --  name from the outer instantiation (see the test for BB10-002 for more
   --  details)

   function Is_From_Dispatching_Call (Reference : Element) return Boolean;
   --  This function detects if its argument is a name from a dispatching call
   --  for that Corresponding_Name_Definition should return Nil_Element (that
   --  is, a name of a called subprogram or the name of a formal parameter from
   --  a named association)

   function Is_Implicit_Formal_Par (Result_El : Element) return Boolean;
   --  Is supposed to be applied to the result of Identifier_Name_Definition.
   --  Checks if this result corresponds to the situation when the argument of
   --  Identifier_Name_Definition is the reference to a formal parameter from
   --  implicit inherited subprogram

   procedure Correct_Impl_Form_Par
     (Result    : in out Element;
      Reference :        Element);
   --  This procedure is supposed to be called is Result is the result of the
   --  call to Identifier_Name_Definition, and Is_Implicit_Formal_Par yields
   --  True for Result. It creates the correct defining name for this iplicit
   --  inherited formal parameter.

end A4G.Expr_Sem;
