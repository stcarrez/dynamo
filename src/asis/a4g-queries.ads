------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . Q U E R I E S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
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
-- The original version of this component has been developed by Jean-Charles--
-- Marteau (Jean-Charles.Marteau@ensimag.imag.fr) and Serge Reboul          --
-- (Serge.Reboul@ensimag.imag.fr), ENSIMAG High School Graduates (Computer  --
-- sciences) Grenoble, France in Sema Group Grenoble, France. Now this      --
-- component is maintained by the ASIS team                                 --
--                                                                          --
------------------------------------------------------------------------------

with Asis; use Asis;

----------------------------------------------------------
-- The goal of this package is, when we have an element --
-- to let us have ALL the possible queries for that     --
-- element that return its children.                    --
----------------------------------------------------------

package A4G.Queries is

   --  There is 3 kinds of queries in Asis :
   type Query_Kinds is
     (Bug,
      --  just for the discriminant default expression
      Single_Element_Query,
      --  Queries taking an element and returning an element.
      Element_List_Query,
      --  Queries taking an element and returning a list of elements.
      Element_List_Query_With_Boolean
      --  Queries taking an element and a boolean and returning a list
      --  of elements.
    );

   type A_Single_Element_Query is access
      function (Elem : Asis.Element) return Asis.Element;

   type A_Element_List_Query is access
      function (Elem : Asis.Element) return Asis.Element_List;

   type A_Element_List_Query_With_Boolean is access
      function
        (Elem : Asis.Element;
         Bool : Boolean)
         return Asis.Element_List;

   --  Discriminant record that can access any type of query.
   type Func_Elem (Query_Kind : Query_Kinds := Bug) is record
      case Query_Kind is
         when Bug =>
            null;
         when Single_Element_Query =>
            Func_Simple : A_Single_Element_Query;
         when Element_List_Query =>
            Func_List : A_Element_List_Query;
         when Element_List_Query_With_Boolean =>
            Func_List_Boolean : A_Element_List_Query_With_Boolean;
            Bool : Boolean;
      end case;
   end record;

   type Query_Array is array (Positive range <>) of Func_Elem;

   --  an empty array, when the element is a terminal
   No_Query : Query_Array (1 .. 0);

   ----------------------------------------------------
   -- This function returns a Query_Array containing --
   -- all the existing queries for that element.     --
   -- If an element has no children, No_Query is     --
   -- returned.                                      --
   ----------------------------------------------------
   function Appropriate_Queries (Element : Asis.Element) return Query_Array;

end A4G.Queries;
