------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--     A S I S . A D A _ E N V I R O N M E N T S . C O N T A I N E R S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
--                                                                          --
-- This   specification  is  derived   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  The copyright --
-- notice above, and the license provisions that follow apply solely to the --
-- contents of the part following the private keyword.                      --
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
--  9  package Asis.Ada_Environments.Containers
------------------------------------------------------------------------------

------------------------------------------------------------------------------
package Asis.Ada_Environments.Containers is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Ada_Environments.Containers
--
--  If an Ada implementation supports the notion of a program library or
--  "library" as specified in Subclause 10(2) of the Ada Reference Manual,
--  then an ASIS Context value can be mapped onto one or more implementor
--  libraries represented by Containers.
--
------------------------------------------------------------------------------
--  9.1   type Container
------------------------------------------------------------------------------
--
--  The Container abstraction is a logical collection of compilation units.
--  For example, one container might hold compilation units which include Ada
--  predefined library units, another container might hold
--  implementation-defined packages. Alternatively, there might be 26
--  containers, each holding compilation units that begin with their respective
--  letter of the alphabet. The point is that no implementation-independent
--  semantics are associated with a container; it is simply a logical
--  collection.
--
--  ASIS implementations shall minimally map the Asis.Context to a list of
--  one ASIS Container whose Name is that of the Asis.Context Name.
------------------------------------------------------------------------------

   type Container is private;
   Nil_Container : constant Container;

   function "="
     (Left  : Container;
      Right : Container)
     return   Boolean is abstract;

------------------------------------------------------------------------------
--  9.2   type Container_List
------------------------------------------------------------------------------

   type Container_List is array (List_Index range <>) of Container;

------------------------------------------------------------------------------
--  9.3   function Defining_Containers
------------------------------------------------------------------------------

   function Defining_Containers
     (The_Context : Asis.Context)
      return        Container_List;

------------------------------------------------------------------------------
--  The_Context - Specifies the Context to define
--
--  Returns a Container_List value that defines the single environment Context.
--  Each Container will have an Enclosing_Context that Is_Identical to the
--  argument The_Context.  The order of Container values in the list is not
--  defined.
--
--  Returns a minimal list of length one if the ASIS Ada implementation does
--  not support the concept of a program library.  In this case, the Container
--  will have the same name as the given Context.
--
--  Raises ASIS_Inappropriate_Context if The_Context is not open.
--

------------------------------------------------------------------------------
--  9.4   function Enclosing_Context
------------------------------------------------------------------------------

   function Enclosing_Context
     (The_Container : Container)
      return          Asis.Context;

------------------------------------------------------------------------------
--  The_Container - Specifies the Container to query
--
--  Returns the Context value associated with the Container.
--
--  Returns the Context for which the Container value was originally obtained.
--  Container values obtained through the Defining_Containers query will always
--  remember the Context from which they were defined.
--
--  Because Context is limited private, this function is only intended to be
--  used to supply a Context parameter for other queries.
--
--  Raises ASIS_Inappropriate_Container if the Container is a Nil_Container.
--
------------------------------------------------------------------------------
--  9.5   function Library_Unit_Declaration
------------------------------------------------------------------------------

   function Library_Unit_Declarations
     (The_Container : Container)
      return          Asis.Compilation_Unit_List;

------------------------------------------------------------------------------
--  The_Container - Specifies the Container to query
--
--  Returns a list of all library_unit_declaration and
--  library_unit_renaming_declaration  elements contained in the Container.
--  Individual units will appear only once in an order that is not defined.
--
--  A Nil_Compilation_Unit_List is returned if there are no declarations of
--  library units within the Container.
--
--  This query will never return a unit with A_Configuration_Compilation or
--  a Nonexistent unit kind. It will never return a unit with A_Procedure_Body
--  or A_Function_Body unit kind even though the unit is interpreted as both
--  the declaration and body of a library procedure or library function.
--  (Reference Manual 10.1.4(4).
--
--  All units in the result will have an Enclosing_Container value that
--  Is_Identical to the Container.
--
--  Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
--  is not open.
--

------------------------------------------------------------------------------
--  9.6   function Compilation_Unit_Bodies
------------------------------------------------------------------------------

   function Compilation_Unit_Bodies
     (The_Container : Container)
      return          Asis.Compilation_Unit_List;

------------------------------------------------------------------------------
--  The_Container - Specifies the Container to query
--
--  Returns a list of all library_unit_body and subunit elements contained in
--  the Container. Individual units will appear only once in an order that is
--  not defined.
--
--  A Nil_Compilation_Unit_List is returned if there are no bodies within the
--  Container.
--
--  This query will never return a unit with A_Configuration_Compilation or
--  a nonexistent unit kind.
--
--  All units in the result will have an Enclosing_Container value that
--  Is_Identical to the Container.
--
--  Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
--  is not open.
--
------------------------------------------------------------------------------
--  9.7   function Compilation_Units
------------------------------------------------------------------------------

   function Compilation_Units
     (The_Container : Container)
      return          Asis.Compilation_Unit_List;

------------------------------------------------------------------------------
--  The_Container - Specifies the Container to query
--
--  Returns a list of all compilation units contained in the Container.
--  Individual units will appear only once in an order that is not defined.
--
--  A Nil_Compilation_Unit_List is returned if there are no units within the
--  Container.
--
--  This query will never return a unit with A_Configuration_Compilation or
--  a nonexistent unit kind.
--
--  All units in the result will have an Enclosing_Container value that
--  Is_Identical to the Container.
--
--  Raises ASIS_Inappropriate_Context if the Enclosing_Context(Container)
--  is not open.
--
------------------------------------------------------------------------------
--  9.8   function Is_Equal
------------------------------------------------------------------------------

   function Is_Equal
     (Left  : Container;
      Right : Container)
      return  Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the first Container
--  Right   - Specifies the second Container
--
--  Returns True if Left and Right designate Container values that contain the
--  same set of compilation units.  The Container values may have been defined
--  from different Context values.
--

------------------------------------------------------------------------------
--  9.9   function Is_Identical
------------------------------------------------------------------------------

   function Is_Identical
     (Left  : Container;
      Right : Container)
      return  Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the first Container
--  Right   - Specifies the second Container
--
--  Returns True if Is_Equal(Left, Right) and the Container values have been
--  defined from Is_Equal Context values.
--
------------------------------------------------------------------------------
--  9.10  function Name
------------------------------------------------------------------------------

   function Name (The_Container : Container) return Wide_String;

------------------------------------------------------------------------------
--  The_Container - Specifies the Container to name
--
--  Returns the Name value associated with the Container.
--
--  Returns a null string if the Container is a Nil_Container.

private

   type Container is record
      Id       : Container_Id := Nil_Container_Id;
      Cont_Id  : Context_Id   := Non_Associated;
      Obtained : ASIS_OS_Time := Nil_ASIS_OS_Time;
   end record;

   Nil_Container : constant Container :=
     (Id       => Nil_Container_Id,
      Cont_Id  => Non_Associated,
      Obtained => Nil_ASIS_OS_Time);

end Asis.Ada_Environments.Containers;
