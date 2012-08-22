------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . S T A N D                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 15121 $
--                                                                          --
--            Copyright (c) 1999-2002, Free Software Foundation, Inc.       --
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

--  This package provides routines for processing the code of the predefined
--  Standard package in ASIS queries. The problem with Standard is that the
--  tree for it does not created from the source, but it is created by the
--  front-end in some special way. As a result, some elements of the tree
--  structure and some semantic links are missed for Standard, and we have
--  to simulate these missed tree componennts for ASIS queries.

with Asis;  use Asis;
with Types; use Types;

package A4G.Stand is

   function Is_Standard_Char_Type (N : Node_Id) return Boolean;
   --  Returns True N is N_Enumeration_Type_Definition node representing the
   --  definition of Standard.Character or Standard.Wide_Character. Returns
   --  False otherwise

   function Standard_Char_Decls
     (Type_Definition : Asis.Type_Definition;
      Implicit        : Boolean := False)
      return Asis.Element_List;
   --  Provided that Type_Definition is An_Enumeration_Type_Definition Element
   --  representing the definition of a predefined character type or a type
   --  derived from it, directly or indirectly, this function returns the
   --  corresponding list of An_Enumeration_Literal_Specification Elements.
   --  Elements in the result list are based on argument's node (they are
   --  "artificial" Elements created for components of the predefined Standard
   --  package or their derivations which are not presented in the tree.

   function Stand_Char_Image (Code : Char_Code) return Wide_String;
   --  Provided that N is a N_Character_Literal Node from Standard.ASCII
   --  package, returns its string image

   function Get_Numeric_Error_Renaming return Asis.Element;
   --  Returns the artificial declaration for an obsolete renaming of
   --  Numeric_Error in Standard. This function assumes that the current
   --  context and current tree are properly set.

end A4G.Stand;
