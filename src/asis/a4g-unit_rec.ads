------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . U N I T _ R E C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS  FOR A PARTICULAR PURPOSE.  See the GNU General --
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
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the type Unit_Record used for storing the
--  information about ASIS Compilation Units. This type is used as an actual
--  parameter for instantiating the GNAT Table package and obtaining the type
--  for defining the individual Unit Table for each ASIS Context.

with Asis;

with Asis.Extensions; use Asis.Extensions;

with A4G.A_Types;     use A4G.A_Types;

with Types;           use Types;

package  A4G.Unit_Rec is

   --  See A4G.Contt (spec) and A4G.Contt.UT for the complete documentation of
   --  the Compilation Units processing in ASIS.

   --  !!! Documentation should be put in order later!!!

   ---------------------------------
   -- Unit_Record type definition --
   ---------------------------------

   type Unit_Record is record -- the field should be commented also here!!!

      --------------------------------
      -- Fields for Unit Name Table --
      --------------------------------

      Ada_Name_Chars_Index      : Int;
      Norm_Ada_Name_Chars_Index : Int;
      File_Name_Chars_Index     : Int;
      Ref_Name_Chars_Index      : Int;
      --  Starting locations of characters in the Name_Chars table minus
      --  one (i.e. pointer to character just before first character). The
      --  reason for the bias of one is that indexes in Name_Buffer are
      --  one's origin, so this avoids unnecessary adds and subtracts of 1.

      Ada_Name_Len      : Short;
      Norm_Ada_Name_Len : Short;
      File_Name_Len     : Short;
      Ref_Name_Len      : Short;
      --  Lengths of the names in characters

      --  We keep separate starting locations and separate lengths
      --  for each "column" of Unit Name Table, but all the actual
      --  strings are stored in the same Name_Chars table

      Hash_Link : Unit_Id;
      --  Link to next entry in names table for same hash code

      -----------------------------------------------
      -- Fields for Black-Box Unit Name Attributes --
      -----------------------------------------------

      Top               : Node_Id;
      --  ??? Do we really need it?
      --  This field is used only during the tree investigation,
      --  and it is used only for the Units contained in this tree,
      --  which have been known to ASIS before (the aim is to optimize
      --  the tree investigation by eliminating the need to compute
      --  the top node for the these Units). Tree swapping makes
      --  the values of these fields obsolete, and we do not want
      --  to keep them valid after investigating the tree. Instead,
      --  if we need the top node corresponding to some Unit during
      --  processing of some ASIS query, we compute it, and this
      --  computation includes, if the Unit may be processed on the
      --  base of the currently accessed tree, resetting this
      --  tree, if necessary, and finding out the corresponding
      --  N_Compuilation_Unit node in this tree.

      Kind              : Asis.Unit_Kinds;
      Class             : Asis.Unit_Classes;
      Origin            : Asis.Unit_Origins;
      Main_Unit         : Boolean;
      Is_Body_Required  : Boolean;
      Time_Stamp        : Time_Stamp_Type;

      Is_Consistent     : Boolean;

      Source_File_Status : Source_File_Statuses;

      Full_View_Trees    : Elist_Id;
      Limited_View_Trees : Elist_Id;
      --  The lists of the trees in which the subtree for a given Unit is
      --  contained, all these trees are consistent in the sense, that all of
      --  them correspond to the same, latest version of unit's source. The
      --  first list contains trees that contain full view of the unit, the
      --  second contains trees that contain only limited views of the unit.

      Main_Tree : Tree_Id;
      --  The tree for which the given unit is a main unit. If there is no
      --  tree for which the unit is the main unit, this field set to
      --  Nil_Tree_Id

      --------------------------------------
      -- Fields for Semantic Dependencies --
      --------------------------------------

      Ancestors           : Elist_Id;
      Descendants         : Elist_Id;
      Direct_Supporters   : Elist_Id;
      Supporters          : Elist_Id;
      Implicit_Supporters : Elist_Id;
      Direct_Dependents   : Elist_Id;
      Dependents          : Elist_Id;
      Subunits_Or_Childs  : Elist_Id;

      Compilation_Dependencies : Elist_Id;

      Subunits_Computed   : Boolean;

      --  The meaning of these lists completely corresponds to the values
      --  of the Asis.Unit_Kinds.Relation_Kinds type, represented by its
      --  literals with the same names.
      --  SHOULD BE REVISED! For example, for subunits it may make sense
      --  to use Ancestors for parent bodies, and it would make sense
      --  for bodies to use Descendants for subunits.
      --
      --  All these fields are either non-empty unit lists, or equal to
      --  No_List, the latter case correspond to the situation, when
      --  the corresponding dependency list is empty (???).

      --  it would be nice to get rid of Direct_Supporters and of
      --  Direct_Dependants as of ill-defined notions (they are not
      --  defined in RM95, opposite to Supporters and Dependents

      --  We may need Implicit_Supporters for
      --  Asis.Compilation_Units.Elaboration_Order query

      --  Subunits_Computed indicates if for a given parent body all its
      --  subunits have already been computed (as a result of a Compilation
      --  Unit semantic query). Computing subunits include allocating
      --  nonexistent units for missed subunits.

      --  Do we really need the list for Ancestors? We can easy compute
      --  ancestors by stripping out selectors in normalized unit names.

      --  OPEN PROBLEMS with ASIS 95 definition
      --  =====================================

      --  1. RM95 says 10.1.1(9):
      --       A library unit is a program unit that is declared by a
      --       library_item. When a program unit is a library unit, the
      --       prefix "library" is used to refer to it (or "generic
      --       library" if generic), as well as to its declaration and
      --       body, as in "library procedure", "library package_body",
      --       or "generic library package". The term compilation unit
      --       is used to refer to a compilation_unit.  When the meaning
      --       is clear from context, the term is also used to refer to
      --       the library_item of a compilation_unit or to the proper_body
      --       of a subunit (that is, the compilation_unit without the
      --       context_clause and the separate (parent_unit_name)).
      --
      --     And AARM adds in 10.1.1(9.d):
      --       We like to use the word "unit" for declaration-plus-body
      --       things, and "item" for declaration or body separately
      --       (as in declarative_item). The terms "compilation_unit",
      --       "compilation unit" and "subunit" are exceptions to this rule.
      --
      --     RM95 says 10.1.1(10):
      --       The parent declaration of a library_item (and of the library
      --       unit) is the declaration denoted by the parent_unit_name, if
      --       any, of the defining_program_unit_name of the library_item.
      --       If there is no parent_unit_name, the parent declaration is
      --       the declaration of Standard, the library_item is a root
      --       library_item, and the library unit (renaming) is a root
      --       library unit (renaming).  The declaration and body of
      --       Standard itself have no parent declaration.  The parent unit
      --       of a library_item or library unit is the library unit
      --       declared by its parent declaration.
      --
      --     And AARM adds in 10.1.1(10.d):
      --       Library units ... have "parent declarations" [which are
      --       *compilation* units] and "parent units" [spec + body].
      --       We didn't bother to define the other possibilities: parent
      --       body of a library unit, parent declaration of a subunit,
      --       parent unit of a subunit. These are not needed...
      --
      --     RM95 says 10.1.1(11):
      --       The children of a library unit occur immediately within the
      --       declarative region of the declaration of the library unit.
      --       The ancestors of a library unit are itself, its parent, its
      --       parent's parent, and so on.  (Standard is an ancestor of
      --       every library unit.)  The descendant relation is the inverse
      --       of the ancestor relation.
      --
      --     AARM adds (10.1.1(11.b):
      --       We use the unadorned term "ancestors" here to concisely
      --       define both "ancestor unit" and "ancestor declaration"
      --
      --     THE QUESTION IS: what do the *ASIS* notions of ancestors
      --       and descendants mean - do they mean "ancestor (descendant)
      --       unit" or "ancestor (descendant) declaration"?
      --
      --     THE "WORKING" SOLUTION: now *in ASIS* "ancestor" means
      --       "ancestor declaration", and "descendant" means "descendant
      --       unit". (Is it really a good decision, if the Descendants
      --       relation is defined by ASIS as the inverse of the ancestor
      --       relation? From the other side, this looks quite natural from
      --       the application viewpoint: if we are asking about ancestors,
      --       we are very likely interested in declarations, but not
      --       bodies,, and when we are asking about descendants, we are
      --       very likely interested in *all* (compilation units which are)
      --       the descendants.
      --
      --       (Moreover, the query
      --       Asis.Compilation_Units.Corresponding_Children has only
      --       *declarations* as appropriate argument unit kinds, and it
      --       returns declarations *and bodies (if any)*

   end record;

end A4G.Unit_Rec;
