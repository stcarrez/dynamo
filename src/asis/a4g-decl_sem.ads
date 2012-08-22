------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                        A 4 G . D E C L _ S E M                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
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

--  This package contains routines needed for semantic queries from
--  the Asis.Declarations package

with Asis;  use Asis;
with Types; use Types;

package A4G.Decl_Sem is

   --  All the routines defined in this package do not check their
   --  arguments - a caller is responcible for the proper use of these
   --  routines

   -------------------------------------------------
   --  Routines for Corresponding_Type_Definition --
   -------------------------------------------------

   function Serach_First_View (Type_Entity : Entity_Id) return Entity_Id;
   --  taking the node representing the type entity, this function looks
   --  for the first occurence of this name in the corresponding declarative
   --  region. The idea is to find the declaration of the private or incomplete
   --  type for which this type defines the full view. If there is no private
   --  or incomplete view, the function returns its argument as a result.
   --
   --  Note, that Type_Entity should not represent an implicit type created
   --  by the compiler.
   --
   --  The reason why we need this function is that some functions from Sinfo
   --  and Einfo needed for semantic queries from Asis.Declarations do not
   --  correspond to their documentation or/and have irregular behaviour. If
   --  and when the corresponding problems in Einfo and Sinfo are fixed, it
   --  would be very nice to get rid of this function, which in fact is no
   --  more than ad hoc solution.

   ---------------------------------------------
   --  Routines for Corresponding_Declaration --
   ---------------------------------------------

   function Get_Expanded_Spec (Instance_Node : Node_Id) return Node_Id;
   --  For Instance_Node, which should represent a generic instantiation,
   --  this function returns the node representing the expanded generic
   --  specification. This function never returns an Empty node.
   --
   --  Note, that in case of subprogram instantiation GNAT creates an
   --  artificial package enclosing the resulted subprogram declaration
   --
   --  This is an error to call this function for argument which does not
   --  represent an instantiation, or for a node representing a library
   --  unit declaration

   function Corresponding_Decl_Node (Body_Node : Node_Id) return Node_Id;
   --  For Body_Node representing a body, renaming-as-body or a body stub, this
   --  function returns the node representing the corresponding declaration.
   --
   --  It is an error to call this function in case when no explicit separate
   --  declaration exists (that is, in case of renaming-as-declaration or
   --  a subprogram body (stub) for which no explicit separate declaration is
   --  presented.
   --
   --  It is also an error to call this function for a node representing a
   --  library unit declaration or for a node representing generic
   --  instantiation.

   --------------------------------------
   --  Routines for Corresponding_Body --
   --------------------------------------

   function Corresponding_Body_Node (Decl_Node : Node_Id) return Node_Id;
   --  For Decl_Node representing a declaration of a program unit,
   --  this function returns the node representing the corresponding
   --  body or renaming-as-body. In is an error to call this function,
   --  if a completion of the declaration represented by the argument is
   --  located in another compilation unit, and the tree being accessed
   --  does not contain this unit (this is the case for subprograms declared
   --  immediately within a library package/library generic package).

   function Get_Renaming_As_Body
     (Node      : Node_Id;
      Spec_Only : Boolean := False)
      return Node_Id;
   --  This function tries to find for Node (which should be of
   --  N_Subprogram_Declaration kind, otherwise this is an error to call
   --  this function) the node representing renaming-as-body which is
   --  the completion of this subprogram declaration. The Spec_Only
   --  flag should be set to limit the search by a package spec only

   -------------------------------------------------
   --  Routines for Corresponding_Generic_Element --
   -------------------------------------------------

   function Get_Corresponding_Generic_Element
     (Gen_Unit : Asis.Declaration;
      Def_Name : Asis.Element)
      return Asis.Element;
   --  This function traverses the declaration of a generic package
   --  Gen_Unit (by applying an instance of Traverce_Element) in order
   --  to find A_Defining_Name Element which represents the corresponding
   --  generic element for Def_Name; Def_Name should represent
   --  A_Defining_Name element which Is_Part_Of_Instance

end A4G.Decl_Sem;
