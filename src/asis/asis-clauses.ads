------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                          A S I S . C L A U S E S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2012, Free Software Foundation, Inc.       --
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes that you have made are clearly indicated.                        --
--                                                                          --
-- This  specification  also  contains  suggestions  and  discussion  items --
-- related to revising the  ASIS Standard according to the changes proposed --
-- for  the  new  revision of the Ada standard. The copyright notice above, --
-- and the license provisions that follow apply solely to these suggestions --
-- and  discussion  items  that  are separated by the corresponding comment --
-- sentinels                                                                --
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

------------------------------------------------------------------------------
--  19 package Asis.Clauses

--  Suggestions related to changing this specification to accept new Ada
--  features as defined in incoming revision of the Ada Standard (ISO 8652)
--  are marked by following comment sentinels:
--
--  --|A2005 start
--   ... the suggestion goes here ...
--  --|A2005 end
--
--  and the discussion items are marked by the comment sentinels of teh form:
--
--  --|D2005 start
--   ... the discussion item goes here ...
--  --|D2005 end

------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Clauses is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Clauses
--
--  This package encapsulates a set of queries that operate on A_Clause
--  elements.
--
--  --|ER---------------------------------------------------------------------
--  --|ER A_Use_Package_Clause  - 8.4
--  --|ER A_Use_Type_Clause     - 8.4
--  --|ER A_Use_All_Type_Clause - 8.4  --  Ada 2012
--  --|ER A_With_Clause         - 10.1.2
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Clause_Names
--  --|CR
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  19.1  function Clause_Names
------------------------------------------------------------------------------

   function Clause_Names (Clause : Asis.Element) return Asis.Name_List;

------------------------------------------------------------------------------
--  Clause  - Specifies the with_clause or use_clause to query
--
--  Returns a list of the names that appear in the given clause.
--  The names in the list should be in their order of appearance in the
--  original clauses from the compilation text.
--
--  Results of this query may vary across ASIS implementations.  Some
--  implementations normalize all clauses containing multiple names
--  into an equivalent sequence of corresponding single clauses.
--  Similarly, an implementation may keep a name only once even though that
--  name can appear more than once in a clause.
--
--  Appropriate Element_Kinds:
--       A_Use_Package_Clause
--       A_Use_Type_Clause
--       A_Use_All_Type_Clause     --  Ada 2012
--       A_With_Clause
--
--  Returns Expression_Kinds:
--       An_Identifier
--       A_Selected_Component
--       An_Attribute_Reference
--
--  --|ER---------------------------------------------------------------------
--  --|ER A_Representation_Clause - 13.1
--  --|ER---------------------------------------------------------------------
--  --|ER An_Attribute_Definition_Clause - 13.3
--  --|ER An_Enumeration_Representation_Clause - 13.4
--  --|ER An_At_Clause - J.7
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Representation_Clause_Name
--  --|CR    function Representation_Clause_Expression
--
------------------------------------------------------------------------------
--  19.2  function Representation_Clause_Name
------------------------------------------------------------------------------

   function Representation_Clause_Name
     (Clause : Asis.Clause)
      return   Asis.Name;

------------------------------------------------------------------------------
--  Clause  - Specifies the representation_clause to query
--
--  Returns the direct_name expression following the reserved word "for".
--
--  |D2005 start
--  But A_Component_Clause does not have the reserved word "for"! The wording
--  needs revising!
--  |D2005 end
--
--  Appropriate Clause_Kinds:
--       A_Representation_Clause
--       A_Component_Clause
--
--  Returns Expression_Kinds:
--       An_Identifier
--       An_Attribute_Reference
--
------------------------------------------------------------------------------
--  19.3  function Representation_Clause_Expression
------------------------------------------------------------------------------

   function Representation_Clause_Expression
     (Clause : Asis.Representation_Clause)
      return   Asis.Expression;

------------------------------------------------------------------------------
--  Clause  - Specifies the representation_clause to query
--
--  Returns the expression following the reserved word "use" or the reserved
--  words "use at".
--
--  Appropriate Representation_Clause_Kinds:
--       An_Attribute_Definition_Clause
--       An_Enumeration_Representation_Clause
--       An_At_Clause
--
--  Returns Element_Kinds:
--       An_Expression
--
--  --|ER---------------------------------------------------------------------
--  --|ER A_Record_Representation_Clause - 13.5.1
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Representation_Clause_Name
--  --|CR    function Mod_Clause_Expression
--  --|CR    function Component_Clauses
--
------------------------------------------------------------------------------
--  19.4  function Mod_Clause_Expression
------------------------------------------------------------------------------

   function Mod_Clause_Expression
     (Clause : Asis.Representation_Clause)
      return   Asis.Expression;

------------------------------------------------------------------------------
--  Clause  - Specifies the record representation clause to query
--
--  Returns the static_expression appearing after the reserved words "at mod".
--
--  Returns a Nil_Element if a mod_clause is not present.
--
--  Appropriate Representation_Clause_Kinds:
--       A_Record_Representation_Clause
--
--  Returns Element_Kinds:
--       Not_An_Element
--       An_Expression
--
------------------------------------------------------------------------------
--  19.5  function Component_Clauses
------------------------------------------------------------------------------

   function Component_Clauses
     (Clause          : Asis.Representation_Clause;
      Include_Pragmas : Boolean := False)
      return            Asis.Component_Clause_List;

------------------------------------------------------------------------------
--  Clause          - Specifies the record representation clause to query
--  Include_Pragmas - Specifies whether pragmas are to be returned
--
--  Returns the component_clause and pragma elements from the
--  record_representation_clause, in their order of appearance.
--
--  Returns a Nil_Element_List if the record_representation_clause has no
--  component_clause or pragma elements.
--
--  Appropriate Representation_Clause_Kinds:
--       A_Record_Representation_Clause
--
--  Returns Element_Kinds:
--       A_Clause
--       A_Pragma
--
--  Returns Clause_Kinds:
--       A_Component_Clause
--
--  --|ER---------------------------------------------------------------------
--  --|ER A_Component_Clause - 13.5.1
--  --|CR
--  --|CR Child elements returned by:
--  --|CR    function Representation_Clause_Name
--  --|CR    function Component_Clause_Position
--  --|CR    function Component_Clause_Range
--
------------------------------------------------------------------------------
--  19.6  function Component_Clause_Position
------------------------------------------------------------------------------

   function Component_Clause_Position
     (Clause : Asis.Component_Clause)
      return   Asis.Expression;

------------------------------------------------------------------------------
--  Clause  - Specifies the component_clause to query
--
--  Returns the position expression for the component_clause.
--
--  Appropriate Clause_Kinds:
--       A_Component_Clause
--
--  Returns Element_Kinds:
--       An_Expression
--
------------------------------------------------------------------------------
--  19.7  function Component_Clause_Range
------------------------------------------------------------------------------

   function Component_Clause_Range
     (Clause : Asis.Component_Clause)
      return   Asis.Discrete_Range;

------------------------------------------------------------------------------
--  Clause  - Specifies the component_clause to query
--
--  Returns the first_bit .. last_bit range for the component_clause.
--
--  Appropriate Clause_Kinds:
--       A_Component_Clause
--
--  Returns Discrete_Range_Kinds:
--       A_Discrete_Simple_Expression_Range
--
------------------------------------------------------------------------------

end Asis.Clauses;
