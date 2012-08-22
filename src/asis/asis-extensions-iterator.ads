------------------------------------------------------------------------------
--                                                                          --
--                        ASIS-for-GNAT COMPONENTS                          --
--                                                                          --
--             A S I S . E X T E N S I O N S . I T E R A T O R              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (c) 2003-2006, Free Software Foundation, Inc.          --
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

--  This package encapsulates the generic procedure Traverse_Unit which
--  is a generalization of the standard ASIS Asis.Iterator.Traverse_Element
--  procedure. Traverse_Unit traverses all the syntactical components of the
--  argument ASIS Compilation Unit.

package Asis.Extensions.Iterator is

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

   procedure Traverse_Unit
     (Unit    :        Asis.Compilation_Unit;
      Control : in out Traverse_Control;
      State   : in out State_Information);
   --  Traverses all the syntactical structure of the argument Compilation
   --  Unit. In ASIS, a Compilation Unit consists of context clause Elements
   --  and of the Element representing the program unit, and these syntax
   --  elements does not have a common root. Traverse_Unit instantiates
   --  Asis.Iterator.Traverse_Element passing its own formal parameters as
   --  actuals for Traverse_Element. Then it goes into all the
   --  first-depth-level structural components of the argument Compilation
   --  Unit by applying this instance of Traverse_Element to it.
   --
   --  If the value of traverse Control becomes Terminate_Immediately,
   --  traversing of all the unit components is terminated (that is, if it
   --  happens in some context clause Element, the Unit declaration Element
   --  will not be traversed.
   --
   --  Appropriate Unit_Kinds:
   --     A_Procedure
   --     A_Function
   --     A_Package
   --
   --     A_Generic_Procedure
   --     A_Generic_Function
   --     A_Generic_Package
   --
   --     A_Procedure_Instance
   --     A_Function_Instance
   --     A_Package_Instance
   --
   --     A_Procedure_Renaming
   --     A_Function_Renaming
   --     A_Package_Renaming
   --
   --     A_Generic_Procedure_Renaming
   --     A_Generic_Function_Renaming
   --     A_Generic_Package_Renaming
   --
   --     A_Procedure_Body
   --     A_Procedure_Body
   --     A_Function_Body
   --     A_Package_Body
   --
   --     A_Procedure_Body_Subunit
   --     A_Function_Body_Subunit
   --     A_Package_Body_Subunit
   --     A_Task_Body_Subunit
   --     A_Protected_Body_Subunit

end Asis.Extensions.Iterator;
