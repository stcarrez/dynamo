------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                         A 4 G . C O N T T . U T                          --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore.                  --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines for each ASIS Context the corresponding Unit Table,
--  which contains all the information needed for the black-box ASIS queries
--  about Compilation Units. This table also provides the mechanism for
--  searching for a unit by its Ada name, this mechanism is some slight
--  modification of the GNAT Namet package.

with Asis;            use Asis;
with Asis.Extensions; use Asis.Extensions;

package A4G.Contt.UT is        --  Context_Table.Unit_Tables

   ---------------------
   -- ASIS Unit Table --
   ---------------------

   --  ASIS Unit Table is the main part of the implementation of ASIS Context
   --  and ASIS Compilation Unit abstractions. The table is organized in the
   --  following way:

   --  - the internal representation of an ASIS Compilation Unit is the
   --    value of the corresponding Unit Record which is kept in Unit Table
   --    and indicated by Unit Id;

   --  - each ASIS Context has its own Unit Table, so most the routines
   --    dealing with Unit Table contain the Id of an Enclosing Context
   --    as a Parameter;

   --  - each ASIS Compilation Units keeps the Id of its enclosing
   --    Context as a part of its value;

   --  - The fully expanded Ada name, together for the spec/body sign,
   --    uniquely identifies a given unit inside its enclosing
   --    Context/Context; so the triple - expanded Ada name, spec/body
   --    sign and some identification of the Unit's enclosing Context/Context
   --    uniquely identifies a given unit among all the Unit processed
   --    by ASIS;

   --  - The normalized Ada name, is obtained from the fully expanded
   --    Ada Unit name by folding all the upper case letters in the
   --    corresponding lower case letters, appending the spec/body sign
   --    (which has the form "%s" for a spec and "%b" for a body);

   --  The entries in the table are accessed using a Unit_Id that ranges
   --  from First_Unit_Id to Last_Unit_Id. The fields of each entry and
   --  the corresponding interfaces may be subdivided into four groups.

   --  The first group, called as Unit Name Table,  provides the modified
   --  version of the functionality of the GNAT Namet package, it is used
   --  for storing the names of the Units in two forms - in the normalized
   --  and in the form corresponding to the (defining) occurrence of a
   --  given name in a source text. Each unit can be effectively searched
   --  by its normalized name.

   --  The second group contains the black-box attributes of a Unit.

   --  The third group contains the information about relations (semantic
   --  dependencies) between the given unit and the other units in the
   --  enclosing Context/Context Note, that Ada Unit name,
   --  included in the first group, logically should also be considered
   --  as a black-box Unit attribute.

   --  And the fourth group contains the fields needed for organization of the
   --  tree swapping during the multiple Units processing.

   ---------------------
   -- Unit Name Table --
   ---------------------

   --  Each Unit entry contain the following fields:

   --  "Normalized" Ada name  "Normalized" Ada names of the ASIS Compilation
   --                          Units are their names with upper case letters
   --                          folded to lower case (by applying the
   --                          Ada.Character.Handling.To_Lower functions;
   --                          this lover-case-folding has no relation to GNAT
   --                          conventions described in Namet!), appended by
   --                          suffix %s or %b for spec or bodies/subunits, as
   --                          defined in Uname (spec), and prepended by
   --                          the string image of the Id value of the unit's
   --                          enclosing Context. Each of the names of this
   --                          kind may have only one entry in Unit Name Table.
   --
   --  Ada name                Ada names of the ASIS Compilation Units are
   --                          stored keeping the casing from the source text.
   --                          These entries are used to implement the ASIS
   --                          query (-ies?) returning the Ada name of the
   --                          Unit. Ada names may be included more than one
   --                          time in Unit Name Table as the parts of the
   --                          different table entries, as the name of a spec
   --                          and the name of a corresponding body.
   --
   --  Source File Name        The name of the Ada source file used to compile
   --                          the given compilation unit (on its own or as a
   --                          supporter of some other unit).
   --
   --  Reference File Name     The name of the source file which represents
   --                          the unit source from the user's viewpoint. It is
   --                          the same as the Source File name unless the
   --                          Source_Reference pragma presents for the given
   --                          unit.

   type Column is (Norm_Ada_Name, Ada_Name, Source_File_Name, Ref_File_Name);
   --  This enumeration type defines literals used to make the difference
   --  between different forms of names stored in the Unit Table

   --  Really every name is kept as the reference into the Char table,
   --  together with the length of its name.

   --  The normalized names are hashed, so that a given normalized name appears
   --  only once in the table.

   --  Opposite to the GNAT name table, this name table does not handle the
   --  one-character values in a special way (there is no need for it, because
   --  storing an one-character name does not seem to be a usual thing
   --  for this table.)

   --  ASIS "normalized" Unit names follow the convention which is
   --  very similar to the GNAT convention defined in Uname (spec), the
   --  only difference is that ASIS folds all the upper case
   --  letters to the corresponding lower case letters without any encoding.
   --  ASIS packages implementing the ASIS Context model for GNAT contain
   --  "ASIS-related counterparts" of some facilities provided by three
   --  GNAT packages - Namet, Uname and Fname.

   --  We keep storing the two values, one of type Int and one of type Byte,
   --  with each names table entry and subprograms are provided for setting
   --  and retrieving these associated values. But for now these values are
   --  of no use in ASIS - we simply are keeping this mechanism from the
   --  GNAT name table - just in case.

   --  Unit Name Table may be considered as having the external view
   --  of the two-column table - for each row indicated by Unit_Id the
   --  first column contains the Ada name of the corresponding Unit and
   --  the second column contains Unit's "normalized" name.

   --  In fact we do not use any encoding-decoding in Unit Name Table. ASIS
   --  supports only a standard mode of GNAT (that is, it relies on the fact
   --  that all the identifiers contain only Row 00 characters). ASIS also
   --  assumes that all the names of the source files are the values of
   --  the Ada predefined String type.

   --  All the Unit Tables shares the same Name Buffer, see the specification
   --  of the parent package for its definition.

   ---------------------------------
   -- Unit Name Table Subprograms --
   ---------------------------------

   procedure Get_Name_String (Id : Unit_Id; Col : Column);
   --  Get_Name_String is used to retrieve the one of the three strings
   --  associated with an entry in the names table. The Col parameter
   --  indicates which of the names should be retrieved (Ada name, normalized
   --  Ada name or source file name) by indicating the "column" in the table
   --  The resulting string is stored in Name_Buffer and Name_Len is set.

   function Length_Of_Name (Id : Unit_Id; Col : Column) return Nat;
   --  ??? pragma Inline (Length_Of_Name);
   --  Returns length of given name in characters, the result is equivalent to
   --  calling Get_Name_String and reading Name_Len, except that a call to
   --  Length_Of_Name does not affect the contents of Name_Len and Name_Buffer.

   function Name_Find (C : Context_Id) return Unit_Id;
   --  Name_Find is called with a string stored in Name_Buffer whose length
   --  is in Name_Len (i.e. the characters of the name are in subscript
   --  positions 1 to Name_Len in Name_Buffer). It searches the names
   --  table to see if the string has already been stored. If so the Id of
   --  the existing entry is returned. Otherwise (opposite to the GNAT name
   --  table, in which a new entry is created it this situation with its
   --  Name_Table_Info field set to zero) the Id value corresponding to the
   --  ASIS Nil_Compilation_Unit, that is Nil_Unit, is returned.
   --
   --  Only normalized Ada names are hashed, so this function is intended to
   --  be applied to the normalized names only (in is not an error to apply
   --  it to other forms of names stored in the table, but the result will
   --  always be Nil_Unit.

   function Allocate_Unit_Entry (C : Context_Id) return Unit_Id;
   --  Allocates the new entry in the Unit Name Table for the "normalized"
   --  Ada Unit name stored in the Name_Buffer (Name_Len should be set
   --  in a proper way). This routine should be called only if the
   --  immediately preceding call to an operation working with Unit Name
   --  Table is the call to Name_Find which has yielded Nil_Unit as a
   --  result. Note, that this function sets only the "normalized" unit name,
   --  it does not set the Ada name or the source file name. It also
   --  increases by one the counter of allocated bodies or specs, depending
   --  on the suffix in the normalized unit name.

   function Allocate_Nonexistent_Unit_Entry (C : Context_Id) return Unit_Id;
   --  Differs from the previous function in the following aspects:
   --  -  'n' is added to the name suffix to mark that this entry
   --     corresponds to the nonexistent unit;
   --  -  The body/spec counters are not increased
   --  -  all the attributes of the allocated nonexistent unit are set by
   --     this procedure.
   --
   --  Allocates the new entry in the Unit Name Table for the "normalized"
   --  Ada Unit name stored in the Name_Buffer (Name_Len should be set
   --  in a proper way). This routine should be called only if the
   --  immediately preceding call to an operation working with Unit Name
   --  Table is the call to Name_Find which has yielded Nil_Unit as a
   --  result. Note, that this function sets only the "normalized" unit name,
   --  it does not set the Ada name or the source file name.

   function Set_Unit (C : Context_Id; U : Unit_Number_Type) return Unit_Id;
   --  Creates the Unit table entry for the unit U and sets the normalized
   --  unit name (which is supposed to be stored in A_Name_Buffer when this
   --  procedure is called) and the time stamp for the unit. It also adds
   --  the (Id of the) currently accessed tree to the (empty) list
   --  of (consistent) trees for this unit. All the other unit attributes
   --  are set to nil values. The ID of the created entry is returned as a
   --  result

   procedure Set_Ada_Name (Id : Unit_Id);
   pragma Inline (Set_Ada_Name);
   --  Sets the string stored in Name_Buffer whose length is Name_Len as the
   --  value of the Ada name of the ASIS Unit indicated by Id value

   procedure Set_Norm_Ada_Name (Id : Unit_Id);
   pragma Inline (Set_Norm_Ada_Name);
   --  Sets the string stored in Name_Buffer whose length is Name_Len as the
   --  value of the "normalized" Ada name of the ASIS Unit indicated by Id
   --  value

   procedure Set_Ref_File_As_Source_File (U : Unit_Id);
   --  For a given unit in a given context, sets the reference file name equal
   --  to the source file name (by copying the corresponding references to
   --  the ASIS Chars table

   procedure Set_Source_File_Name (Id  : Unit_Id; Ref : Boolean := False);
   pragma Inline (Set_Source_File_Name);
   --  Sets the string stored in the A_Name_Buffer whose length is A_Name_Len
   --  as the value of the source or reference (depending on the actual set
   --  for the Ref parameter) file name of the ASIS Unit indicated by Id value

   procedure Set_Norm_Ada_Name_String;
   --  Sets the Normalized Ada Unit name as the value of Name_Buffer.
   --  This normalized version of the Ada Unit name is
   --  obtained by folding to lover cases of the GNAT unit name
   --  which should be previously get as the content of
   --  Namet.Name_Buffer (that means that every call to this procedure
   --  should be preceded by the appropriate call to
   --  Namet.Get_Unqualified_Decoded_Name_String (or
   --  Namet.Get_Decoded_Name_String if the caller is sure, that the name is
   --  not qualified)

   procedure Set_Norm_Ada_Name_String_With_Check
     (Unit    :     Unit_Number_Type;
      Success : out Boolean);
   --  This is the modified version of Set_Norm_Ada_Name_String: after setting
   --  the ASIS name buffer it checks if Unit should be considered as
   --  Compilation_Unit by ASIS. The need for this check caused by artificial
   --  compilation units created by the compiler for library-level generic
   --  instantiations. If the check is successful, Success is set True,
   --  otherwise it is set False.
   --
   --  In case of a tree created for library-level instantiation of a generic
   --  package (only package ???) GNAT sets the suffix of the name of the
   --  corresponding unit in its unit table as '%b', but ASIS has to see
   --  this unit as a spec, therefore in this case this procedure resets the
   --  suffix of the unit name to '%s'

   procedure Set_Ref_File_Name_String (U : Unit_Id);
   --  Is supposed to be called when GNAT Namet.Name_Buffer contains a full
   --  reference file name. It sets the Reference File name as the value of
   --  A_Name_Buffer. This name is composed from the reference file name
   --  obtained from the tree and from the source file name (in which the
   --  directory information is already adjusted , if needed, by the
   --  corresponding call to Set_S_File_Name_String) to contain the directory
   --  information needed to access this file from the current directory.

   -------------------------------
   -- Black-Box Unit Attributes --
   -------------------------------

   --  Each Unit entry contains the following fields, representing the Unit
   --  black-box attributes, which are for the direct interest for the ASIS
   --  queries from the Asis_Compilation_Unit package, the primary idea of
   --  implementing the Context/Compilation_Unit stuff in ASIS-for-GNAT is
   --  to compute each of these attribute only once, when the new tree is
   --  inputted by ASIS for the first time, and then store them in Unit
   --  Table, so then ASIS queries will be able to get the required
   --  answer without any new tree processing:

   --  Top : Node_Id;
   --    The top node of the unit subtree in the currently accessed full tree.
   --    From one side, this node should be reset every time the full tree
   --    is changed. From the other side, the corresponding actions may be
   --    considered as too time-consumed. This problem is postponed now as
   --    OPEN PROBLEM, it is not important till we are working under the
   --    limitation "only one tree can be accessed at a time"

   --  Enclosing_Context : Context_Id;
   --    The reference to the Context table which indicates the Enclosing
   --    Context for a Unit

   --  Kind : Unit_Kinds;
   --    The kind of a Compilation Unit, as defined by Asis.Unit_Kinds
   --    package

   --  Class : Unit_Classes;
   --    The class of a Compilation Unit, as defined by Asis.Unit_Kinds
   --    package

   --  Origin : Unit_Origins;
   --    The origin of a Compilation Unit, as defined by Asis.Unit_Kinds
   --    package

   --  Main_Unit : Boolean;
   --    The boolean flag indicating if a Compilation Unit may be treated
   --    as the main unit for a partition (See RM 10.2(7))
   --    GNAT-specific!!??

   --  Is_Body_Required : Boolean;
   --    The boolean flag indicating if a Compilation Unit requires a body
   --    as a completion

   -----------------------------------------------------------
   --  Black-Box Unit Attributes Access and Update Routines --
   -----------------------------------------------------------

   function Top (U : Unit_Id) return Node_Id;
   --  this function is not trivial, it can have tree swapping as its
   --  "side effect"

   function Kind (C : Context_Id; U : Unit_Id) return Unit_Kinds;
   function Class (C : Context_Id; U : Unit_Id) return Unit_Classes;
   function Origin (C : Context_Id; U : Unit_Id) return Unit_Origins;
   function Is_Main_Unit (C : Context_Id; U : Unit_Id) return Boolean;
   function Is_Body_Required (C : Context_Id; U : Unit_Id) return Boolean;
   --  This function does not reset Context, a Caller is responsible for this
   function Time_Stamp (C : Context_Id; U : Unit_Id) return Time_Stamp_Type;
   function Is_Consistent (C : Context_Id; U : Unit_Id) return Boolean;
   function Source_Status (C : Context_Id; U : Unit_Id)
      return Source_File_Statuses;
   function Main_Tree (C : Context_Id; U : Unit_Id) return Tree_Id;

   function Has_Limited_View_Only (C : Context_Id; U : Unit_Id) return Boolean;
   --  Checks if U has only limited view in C

   --------

   procedure Set_Top              (C : Context_Id; U : Unit_Id;
                                   N : Node_Id);
   procedure Set_Kind             (C : Context_Id; U : Unit_Id;
                                   K : Unit_Kinds);
   procedure Set_Class            (C : Context_Id; U : Unit_Id;
                                   Cl : Unit_Classes);
   procedure Set_Origin           (C : Context_Id; U : Unit_Id;
                                   O : Unit_Origins);
   procedure Set_Is_Main_Unit     (C : Context_Id; U : Unit_Id;
                                   M : Boolean);
   procedure Set_Is_Body_Required (C : Context_Id; U : Unit_Id;
                                   B : Boolean);
   procedure Set_Time_Stamp       (C : Context_Id; U : Unit_Id;
                                   T : Time_Stamp_Type);

   procedure Set_Is_Consistent    (C : Context_Id; U : Unit_Id;
                                   B : Boolean);

   procedure Set_Source_Status    (C : Context_Id; U : Unit_Id;

                                   S : Source_File_Statuses);
   -------------------------------------------------

   ---------------------------
   -- Semantic Dependencies --
   ---------------------------

   ----------------------------------------------------
   -- Subprograms for Semantic Dependencies Handling --
   ----------------------------------------------------

   function Not_Root return Boolean;
   --  Checks if U is not a root library unit (by checking if
   --  its name contains a dot). This function itself does not set the
   --  normalized name of U in A_Name_Buffer, it is supposed to be called
   --  when a proper name is already set.

   function Subunits (C : Context_Id;  U : Unit_Id) return Unit_Id_List;
   --  Returns the full list of Ids of subunits for U (if any). The full list
   --  contains nonexistent units for missed subunits
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function Get_Subunit
     (Parent_Body : Asis.Compilation_Unit;
      Stub_Node   : Node_Id)
      return Asis.Compilation_Unit;
   --  This function is intended to be used only when all the Unit attributes
   --  are already computed. It gets the Parent_Body, whose tree should
   --  contain Stub_Node as a node representing some body stub, and it
   --  returns the Compilation Unit containing the proper body for this stub.
   --  It returns a Nil_Compilation_Unit, if the Compilation Unit containing
   --  the proper body does not exist in the enclosing Context or if it is
   --  inconsistent with Parent_Body.

   function Children (U : Unit_Id) return Unit_Id_List;
   --  returns the list of Ids of children for U (if any)
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function GNAT_Compilation_Dependencies (U : Unit_Id) return Unit_Id_List;
   --  Returns the full list of GNAT compilation dependencies for U
   --  This list is empty if and only if U is not a main unit of some
   --  compilation which creates some tree for C.

   procedure Form_Parent_Name;
   --  supposing A_Name_Buffer containing a normalized unit name, this
   --  function forms the normalized name of its parent by stripping out
   --  the suffix in the Ada part of the name (that is, the part of the
   --  name between the rightmost '.' and '%") and changing the
   --  "normalized" suffix to "%s". A_Name_Len is set in accordance with
   --  this. If the Ada part of the name contains no suffix (that is, if
   --  it corresponds to a root library unit), A_Name_Len is set equal
   --  to 0.

   function Get_Parent_Unit (C : Context_Id; U : Unit_Id) return Unit_Id;
   --  returns the Id of the parent unit declaration for U. If U is
   --  First_Unit_Id, returns Nil_Unit.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function Get_Body (C : Context_Id; U : Unit_Id) return Unit_Id;
   --  returns the Id of the library_unit_body for the unit U.
   --  Nil_Unit is not a valid argument  for this function.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function Get_Declaration (C : Context_Id; U : Unit_Id) return Unit_Id;
   --  returns the Id of the library_unit_declaration for the unit U.
   --  Nil_Unit is not a valid argument  for this function.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function Get_Subunit_Parent_Body
     (C : Context_Id;
      U : Unit_Id)
      return Unit_Id;
   --  returns the Id of the library_unit_body or subunit being the parent
   --  body for subunit U (a caller is responsible for calling this function
   --  for subunits).

   function Get_Nonexistent_Unit (C : Context_Id) return Unit_Id;
   --  Is supposed to be called just after an attempt to get a unit which is
   --  supposed to be a needed declaration or a needed body (that is,
   --  A_Name_Buffer contains a normalized unit name ending with "%s" or "%b"
   --  respectively). Tries to find the unit of A_Nonexistent_Declaration
   --  or A_Nonexistent_Body kind with this name, if this attempt fails,
   --  allocates the new unit entry for the corresponding nonexistent unit.
   --  Returns the Id of found or allocated unit.

   function Get_Same_Unit
     (Arg_C  : Context_Id;
      Arg_U  : Unit_Id;
      Targ_C : Context_Id)
   return Unit_Id;
   --  Tries to find in Targ_C just the same unit as Arg_U is in Arg_C.
   --  Just the same means, that Arg_U and the result of this function
   --  should have just the same time stamps. If Arg_C = Targ_C, Arg_U
   --  is returned. If there is no "just the same" unit in Targ_C,
   --  Nil_Unit is returned.
   --
   --  If No (Arg_U), then the currently accessed Context is not reset (but
   --  this function is not supposed to be called for Arg_U equal to
   --  Nil_Unit_Id, although it is not an error). Otherwise Context is reset
   --  to Targ_C

   --------------------------------------
   -- General-Purpose Unit Subprograms --
   --------------------------------------

   procedure Finalize (C : Context_Id);
   --  Currently this routine is only used to generate debugging output
   --  for the Unit Table of a given Context.

   function Present (Unit : Unit_Id) return Boolean;
   --  Tests given Unit Id for equality with Nil_Unit. This allows
   --  notations like "if Present (Current_Supporter)" as opposed to
   --  "if Current_Supporter /= Nil_Unit

   function No (Unit : Unit_Id) return Boolean;
   --  Tests given Unit Id for equality with Nil_Unit. This allows
   --  notations like "if No (Current_Supporter)" as opposed to
   --  "if Current_Supporter = Nil_Unit

   function Last_Unit return Unit_Id;
   --  Returns the Unit_Id of the last unit which has been allocated in the
   --  Unit Name Table. Used to define that the Unit_Id value returned by
   --  Name_Find corresponds to the ASIS Compilation Unit which is not
   --  known to ASIS.

   function Lib_Unit_Decls (C : Context_Id) return Natural;
   --  returns the number of library_unit_declaratios allocated in the
   --  Context Unit table

   function Comp_Unit_Bodies (C : Context_Id) return Natural;
   --  returns the number of library_unit_bodies and subunits allocated
   --  in the Context Unit table

   function Next_Decl (D : Unit_Id) return Unit_Id;
   --  Returns the Unit_Id of the next unit (starting from, but not including
   --  D), which is a library_unit_declaration. Returns Nil_Unit, if there
   --  is no such a unit in C.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function First_Body return Unit_Id;
   --  Returns the Unit_Id of the first unit which is a
   --  compilation_unit_body or a subunit. Returns Nil_Unit, if there is
   --  no such a unit in a current Context.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   function Next_Body (B : Unit_Id) return Unit_Id;
   --  Returns the Unit_Id of the next unit (starting from, but not including
   --  B)  which is a compilation_unit_body or a subunit. Returns Nil_Unit,
   --  if there is no such a unit in C.
   --
   --  Note, that this function does not reset Context, it should be done in
   --  the caller!

   procedure Output_Unit (C : Context_Id; Unit : Unit_Id);
   --  Produces the debug output of the Unit Table entry corresponding
   --  to Unit
   --  DO WE NEED THIS PROCEDURE IN THE SPECIFICATION????

   procedure Print_Units (C : Context_Id);
   --  Produces the debug output from the Unit table for the Context C.

   function Enclosing_Unit
     (Cont : Context_Id;
      Node : Node_Id)
      return Asis.Compilation_Unit;
   --  This function is intended to be used to define the enclosing
   --  unit for an Element obtained as a result of some ASIS semantic query.
   --  It finds the N_Compilation_Unit node for the subtree enclosing
   --  the Node given as its argument, and then defines the corresponding
   --  Unit Id, which is supposed to be the Id of Enclosing Unit for an
   --  Element built up on the base of Node. It does not change the tree
   --  being currently accessed. All these computations are supposed
   --  to be performed for a Context Cont.
   --  Node should not be a result of Atree.Original_Node, because
   --  it is used as an argument for Atree.Parent function
   --
   --  Note, that this function does no consistency check, that is, the
   --  currently accessed tree may be not from the list of consistent trees
   --  for the resulted Unit.

   ---------------
   -- NEW STUFF --
   ---------------

   procedure Register_Units (Set_First_New_Unit : Boolean := False);
   --  When a new tree file is read in during Opening a Context, this procedure
   --  goes through all the units represented by this tree and checks if these
   --  units are already known to ASIS. If some unit is unknown, this
   --  procedure "register" it - it creates the corresponding entry in the
   --  unit table, and it sets the normalized unit name. It does not set any
   --  other field of unit record except Kind. It sets Kind as Not_A_Unit
   --  to indicate, that this unit is only registered, but not processed.
   --
   --  We need this (pre-)registration to be made before starting unit
   --  processing performed by Process_Unit_New, because we need all the units
   --  presenting in the tree to be presented also in the Context unit table
   --  when storing the dependency information.
   --
   --  Note, that all the consistency checks are made by Process_Unit_New,
   --  even though we can make them here. The reason is to separate this
   --  (pre-)registration (which is an auxiliary technical action) from
   --  unit-by-unit processing to facilitate the maintainability of the code.
   --
   --  If Set_First_New_Unit is set ON, stores in A4G.Contt.First_New_Unit
   --  the first new unit being registered. If Set_First_New_Unit is set OFF
   --  or if no new units has been registered, First_New_Unit is set to
   --  Nil_Unit
   --
   --  ??? The  current implementation uses Set_Unit, which also sets time
   --  ???  stamp for a unit being registered. It looks like we do not need
   --  ???  this, so we can get rid of this.

   function Already_Processed (C : Context_Id; U : Unit_Id) return Boolean;
   --  Checks if U has already been processed when scanning previous trees
   --  during opening C

   procedure Check_Source_Consistency
     (C : Context_Id;
      U_Id : Unit_Id);
   --  Is called when a Unit is being investigated as encountered for the first
   --  time during opening the Context C. It checks the existence of the source
   --  file for this unit, and if the source file exists, it checks that the
   --  units as represented by the tree is consistent with the source (if this
   --  is required by the options associated with the Context).
   --  This procedure should be called after extracting the source file name
   --  from the tree and putting this into the Context unit table.

   procedure Check_Consistency
     (C : Context_Id;
      U_Id : Unit_Id;
      U_Num : Unit_Number_Type);
   --  Is called when a unit is encountered again when opening C. Checks if in
   --  the currently accessed tree this unit has the same time stamp as it had
   --  in all the previously processed trees. In case if this check fails, it
   --  raises ASIS_Failed and forms the diagnosis on behalf of
   --  Asis.Ada_Environments.Open. (This procedure does not check the source
   --  file for the unit - this should be done by Check_Source_Consistency
   --  when the unit was processed for the first time)

   function TS_From_OS_Time (T : OS_Time) return Time_Stamp_Type;
   --  Converts OS_Time into Time_Stamp_Type. Is this the right place for
   --  this function???

   procedure Reset_Cache;
   --  Resents to the empty state the cache data structure used to speed up the
   --  Top function. Should be called as a part of closing a Context.

end A4G.Contt.UT;
