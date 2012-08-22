------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                   A S I S . L I M I T E D _ V I E W S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2010, Free Software Foundation, Inc.            --
--                                                                          --
-- This specification  is  added to be used together with the Ada  Semantic --
-- Interface Specification Standard (ISO/IEC 15291) for use with GNAT.      --                        --
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

package Asis.Limited_Views is

--  This package addresses the issue of limited view that is a consequence of
--  adding 'limited with' clause to Ada 2005. It tries to follow as close as
--  possible the notion of a limited view as it is defined in 10.1.1 (12).
--
--  Here is a summary of the problems an ASIS application may have with limited
--  views. Consider we have an ASIS Context that contains the following units:
--
--     package P is
--        type T is tagged record
--           ...
--        end record;
--     end P;

--     limited with P;
--     package Q is
--        type Access_T is access P.T;
--     end Q;
--
--  An application can apply Corresponding_Name_Declaration (or
--  Corresponding_Name_Definition) to An_Identifier Element denoting the
--  reference to T in Compilation Unit Q. According to Ada 2005 Standard, the
--  result should be a (tagged) limited view of the type P.T. An application
--  may also apply these Corresponding_Name_Declaration/
--  Corresponding_Name_Definition queries to An_Identifier Element that denotes
--  the reference to P in the Compilation Unit Q, and according to the Ada 2005
--  standard, the result should be the limited view of the package P. An
--  application should have the possibility to check if some
--  An_Incomplete_Type_Declaration or A_Tagged_Incomplete_Type_Declaration or
--  A_Package_Declaration Element is actually a limited view (and some
--  A_Defining_Identifier Element is a component of a type or a package
--  limited view). This may be important because of various reasons: according
--  to the Ada 2005 Standard, limited views do not have syntax representations
--  and they are implicit, so Asis.Text queries cannot be applied to them;
--  Traverse_Element cannot work on limited views (??? it just does not have
--  very much sense!) etc.
--
--  An application can start from compilation unit P. For the example given
--  above, P has only its limited view in the given Context, so the
--  possibilities of its structural decomposition are limited by the properties
--  that Ada Standard defines for limited views. So an application may want
--  to know if for a given compilation unit only its limited view is available
--  in the given Context.
--
--  Finally, if Corresponding_Name_Declaration/Corresponding_Name_Definition
--  returns a limited type/package view (or a defining identifier from it,
--  an application may want to go to the corresponding non-limited view (if
--  possible, that is, if for the corresponding enclosing compilation unit not
--  only its limited view is available).
--
--  The interface proposed below tries to address all these application needs.

   function Has_Limited_View_Only
     (Right : Asis.Compilation_Unit)
     return Boolean;
   --  Checks if the argument unit in a given Context has a limited view
   --  only. Returns False for any unexpected Unit.
   --
   --  For the moment, works correctly only for an ASIS Context based on a
   --  single tree.
   --
   --  Expected Compilation_Unit_Kinds:
   --     A_Package

   function Is_From_Limited_View (D : Asis.Element) return Boolean;
   --  Checks if the argument is a (part of a) limited view. Returns False for
   --  any unexpected Element.
   --
   --  Expected Defining_Names kinds:
   --     A_Defining_Identifier
   --
   --  Expected Declaration_Kinds:
   --     An_Incomplete_Type_Declaration
   --     A_Tagged_Incomplete_Type_Declaration
   --     A_Package_Declaration

   function Get_Nonlimited_View (D : Asis.Element) return Asis.Element;
   --  If Is_From_Limited_View (D), returns the corresponding full view of the
   --  argument in case if
   --  not Has_Limited_View_Only (Enclosing_Compilation_Unit (D)), otherwise
   --  returns Nil_Element. Raises Asis_Inappropriate_Element if
   --  not Is_From_Limited_View (D).

   --------------------------------------------
   -- Limited view and Standard ASIS Queries --
   --------------------------------------------

   --  If A_Declaration Element Is_From_Limited_View, and if it represents a
   --  limited view of a type, it can be of An_Incomplete_Type_Declaration or
   --  A_Tagged_Incomplete_Type_Declaration Kind only.

   ---------------------
   --  Asis.Elements: --
   ---------------------
   --  * if Has_Limited_View_Only (CU), then:
   --    - Unit_Declaration (CU) returns the result that Is_From_Limited_View;
   --
   --    - Context_Clause_Elements (CU) returns Nil_Element_LIst;
   --
   --    - Compilation_Pragmas (CU) returns Nil_Element_List;
   --
   --  * if Is_From_Limited_View (E) then:
   --    - Enclosing_Element is also Is_From_Limited_View;

   -----------------------
   -- Asis.Declarations --
   -----------------------
   --  * If A_Package_Declaration Element Is_From_Limited_View, then for this
   --    Element:
   --
   --    - Visible_Part_Declarative_Items returns only artificial type and
   --      local package declarations that all Is_From_Limited_View;
   --
   --    - Is_Private_Present always returns False;
   --
   --    - Private_Part_Declarative_Items always returns Nil_Element_List;
   --
   --  * if A_Declaration Element Is_From_Limited_View, then for this Element:
   --    - Names return a list containing a single Is_From_Limited_View
   --      Element
   --
   --  * if A_Defining_Expanded_Name Element Is_From_Limited_View, then for
   --    this Element:
   --    - Defining_Prefix returns Is_From_Limited_View result;
   --
   --    - Defining_Selector returns Is_From_Limited_View result;

   ----------------------
   -- Asis.Expressions --
   ----------------------
   --  * If An_Identifier Element Is_From_Limited_View, then for this element:
   --
   --    - Corresponding_Name_Definition returns Nil_Element;
   --
   --    - Corresponding_Name_Definition_List returns Nil_Element_List;
   --
   --    - Corresponding_Name_Declaration returns Nil_Element;

end Asis.Limited_Views;
