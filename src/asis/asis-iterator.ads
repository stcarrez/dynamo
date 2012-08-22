------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                          A S I S . I T E R A T O R                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (c) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes  that  you have made are clearly indicated. The copyright notice --
-- above,  and  the  license  provisions  that  follow  apply solely to the --
-- contents of the part following the private keyword.                      --
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

------------------------------------------------------------------------------
--  14 package Asis.Iterator
------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Iterator is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Iterator encapsulates the generic procedure Traverse_Element which
--  allows an ASIS application to perform an iterative traversal of a
--  logical syntax tree. It requires the use of two generic procedures,
--  Pre_Operation, which identifies processing for the traversal, and
--  Post_Operation, which identifies processing after the traversal.
--  The State_Information allows processing state to be passed during the
--  iteration of Traverse_Element.
--
--  Package Asis.Iterator is established as a child package to highlight the
--  iteration capability and to facilitate the translation of ASIS to IDL.
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  14.1  procedure Traverse_Element
------------------------------------------------------------------------------

   generic

      type State_Information is limited private;

      with procedure Pre_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Element :        Asis.Element;
                        Control : in out Traverse_Control;
                        State   : in out State_Information) is <>;

   procedure Traverse_Element
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out State_Information);

------------------------------------------------------------------------------
--  Element             - Specifies the initial element in the traversal
--  Control             - Specifies what next to do with the traversal
--  State_Information   - Specifies other information for the traversal
--
--  Traverses the element and all its component elements, if any.
--  Component elements are all elements that can be obtained by a combination
--  of the ASIS structural queries appropriate for the given element.
--
--  If an element has one or more component elements, each is called a child
--  element.  An element's parent element is its Enclosing_Element.  Children
--  with the same parent are sibling elements.  The type Traverse_Control uses
--  the terms children and siblings to control the traverse.
--
--  For each element, the formal procedure Pre_Operation is called when first
--  visiting the element.  Each of that element's children are then visited
--  and finally the formal procedure Post_Operation is called for the element.
--
--  The order of Element traversal is in terms of the textual representation of
--  the Elements.  Elements are traversed in left-to-right and top-to-bottom
--  order.
--
--  Traversal of Implicit Elements:
--
--  Implicit elements are not traversed by default.  However, they may be
--  explicitly queried and then passed to the traversal instance.  Implicit
--  elements include implicit predefined operator declarations, implicit
--  inherited subprogram declarations, implicit expanded generic specifications
--  and bodies, default expressions supplied to procedure, function, and entry
--  calls, etc.
--
--  Applications that wish to traverse these implicit Elements shall query for
--  them at the appropriate places in a traversal and then recursively call
--  their instantiation of the traversal generic.  (Implicit elements provided
--  by ASIS do not cover all possible Ada implicit constructs.  For example,
--  implicit initializations for variables of an access type are not provided
--  by ASIS.)
--
--  Traversal of Association lists:
--
--  Argument and association lists for procedure calls, function calls, entry
--  calls, generic instantiations, and aggregates are traversed in their
--  unnormalized forms, as if the Normalized parameter was False for those
--  queries.  Implementations that always normalize certain associations may
--  return Is_Normalized associations.  See the Implementation Permissions
--  for the queries Discriminant_Associations, Generic_Actual_Part,
--  Call_Statement_Parameters, Record_Component_Associations, or
--  Function_Call_Parameters.
--
--  Applications that wish to explicitly traverse normalized associations can
--  do so by querying the appropriate locations in order to obtain the
--  normalized list.  The list can then be traversed by recursively calling
--  the traverse instance.  Once that sub-traversal is finished, the Control
--  parameter can be set to Abandon_Children to skip processing of the
--  unnormalized argument list.
--
--  Traversal can be controlled with the Control parameter.
--
--  A call to an instance of Traverse_Element will not result in calls to
--  Pre_Operation or Post_Operation unless Control is set to Continue.
--
--  The subprograms matching Pre_Operation and Post_Operation can set
--  their Control parameter to affect the traverse:
--
--     Continue                -- Continues the normal depth-first traversal.
--
--     Abandon_Children        -- Prevents traversal of the current element's
--                             -- children.
--                             -- If set in a Pre_Operation, traversal picks up
--                             -- with the next sibling element of the current
--                             -- element.
--                             -- If set in a Post_Operation, this is the
--                             -- same as Continue, all children will already
--                             -- have been traversed.  Traversal picks up with
--                             -- the Post_Operation of the parent.
--
--     Abandon_Siblings        -- Prevents traversal of the current element's
--                             -- children and remaining siblings.
--                             -- If set in a Pre_Operation, this abandons the
--                             -- associated Post_Operation for the current
--                             -- element.  Traversal picks up with the
--                             -- Post_Operation of the parent.
--                             -- If set in a Post_Operation, traversal picks
--                             -- up with the Post_Operation of the parent.
--
--     Terminate_Immediately   -- Does exactly that.
--
--  Raises ASIS_Inappropriate_Element if the element is a Nil_Element
------------------------------------------------------------------------------

end Asis.Iterator;
