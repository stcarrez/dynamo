------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ E L I S T S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2007, Free Software Foundation, Inc.       --
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
------------------------------------------------------------------------------

--  This package is a modification of the GNAT Elists package, which
--  provides facilities for manipulating lists of AST nodes (see the GNAT
--  package Atree for format and implementation of tree nodes).
--
--  The following modifications of the GNAT Elists package (revision 1.13-spec
--  and 1.19 body) are made here:
--
--  1. List type: for multiple Context processing, ASIS needs a list type to
--     represent unit lists specific for each Context. To make it possible to
--     simulate the element list type by the Saved_Table type provided by the
--     GNAT Table package, the instantiations of the GNAT Table which
--     implements element lists in the ASIS lists package are moved from
--     the body into the spec.
--
--  2. List element type: ASIS needs lists of ASIS Compilation Units to be
--     processed as a part of the ASIS Context implementation. Therefore the
--     GNAT Node_Id type is systematically replaced by the ASIS Unit_Id type,
--     and the Node function is renamed into the Unit function
--
--  3. Removing non-needed subprograms - the following subprograms defined in
--    the GNAT Elists package are of no need for ASIS lists. They are removed
--    from the ASIS list package:
--        procedure Lock
--        procedure Tree_Read
--        procedure Tree_Write
--        function Elists_Address
--        function Elmts_Address
--
--  4. Adding subprograms for ASIS needs - they are grouped in the end of the
--     package spec after the corresponding comment separator
--
--  5. Removing Inline pragmas: in the current version of the ASIS lists
--     package all the Inline pragnas are removed
--
--  6. Adjusting documentation: some minor and natural adjustments in the
--     documentation has been done

with Table;
with Alloc;
with Types;       use Types;

with A4G.A_Types; use A4G.A_Types;

package A4G.A_Elists is

   --  An element list is represented by a header that is allocated in the
   --  Elist header table. This header contains pointers to the first and
   --  last elements in the list, or to No_Elmt if the list is empty.

   -----------------------------------------------------
   -- Subprograms coming from the GNAT Elists package --
   -----------------------------------------------------

   procedure Initialize;
   --  Initialize allocation of element list tables. Called at the start of
   --  compiling each new main source file. Note that Initialize must not be
   --  called if Tree_Read is used.

   function Last_Elist_Id return Elist_Id;
   --  Returns Id of last allocated element list header

   function Num_Elists return Nat;
   --  Number of currently allocated element lists

   function Last_Elmt_Id return Elmt_Id;
   --  Returns Id of last allocated list element

   function Unit (Elmt : Elmt_Id) return Unit_Id;
   --  Returns the value of a given list element. Returns Empty if Elmt
   --  is set to No_Elmt.

   function New_Elmt_List return Elist_Id;
   --  Creates a new empty element list. Typically this is used to initialize
   --  a field in some other node which points to an element list where the
   --  list is then subsequently filled in using Append calls.

   function First_Elmt (List : Elist_Id) return Elmt_Id;
   --  Obtains the first element of the given element list or, if the
   --  list has no items, then No_Elmt is returned.

   function Last_Elmt (List : Elist_Id) return Elmt_Id;
   --  Obtains the last element of the given element list or, if the
   --  list has no items, then No_Elmt is returned.

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id;
   --  This function returns the next element on an element list. The argument
   --  must be a list element other than No_Elmt. Returns No_Elmt if the given
   --  element is the last element of the list.

   function Is_Empty_Elmt_List (List : Elist_Id) return Boolean;
   --  This function determines if a given tree id references an element list
   --  that contains no items.

   procedure Append_Elmt (Unit : Unit_Id; To : Elist_Id);
   --  Appends Unit at the end of To, allocating a new element.

   procedure Prepend_Elmt (Unit : Unit_Id; To : Elist_Id);
   --  Appends Unit at the beginning of To, allocating a new element.

   procedure Insert_Elmt_After (Unit : Unit_Id; Elmt : Elmt_Id);
   --  Add a new element (Unit) right after the pre-existing element Elmt
   --  It is invalid to call this subprogram with Elmt = No_Elmt.

   procedure Replace_Elmt (Elmt : Elmt_Id; New_Unit : Unit_Id);
   --  Causes the given element of the list to refer to New_Unit, the node
   --  which was previously referred to by Elmt is effectively removed from
   --  the list and replaced by New_Unit.

   procedure Remove_Elmt (List : Elist_Id; Elmt : Elmt_Id);
   --  Removes Elmt from the given list. The node itself is not affected,
   --  but the space used by the list element may be (but is not required
   --  to be) freed for reuse in a subsequent Append_Elmt call.

   procedure Remove_Last_Elmt (List : Elist_Id);
   --  Removes the last element of the given list. The node itself is not
   --  affected, but the space used by the list element may be (but is not
   --  required to be) freed for reuse in a subsequent Append_Elmt call.

   function No (List : Elist_Id) return Boolean;
   --  Tests given Id for equality with No_Elist. This allows notations like
   --  "if No (Statements)" as opposed to "if Statements = No_Elist".

   function Present (List : Elist_Id) return Boolean;
   --  Tests given Id for inequality with No_Elist. This allows notations like
   --  "if Present (Statements)" as opposed to "if Statements /= No_Elist".

   function No (Elmt : Elmt_Id) return Boolean;
   --  Tests given Id for equality with No_Elmt. This allows notations like
   --  "if No (Operation)" as opposed to "if Operation = No_Elmt".

   function Present (Elmt : Elmt_Id) return Boolean;
   --  Tests given Id for inequality with No_Elmt. This allows notations like
   --  "if Present (Operation)" as opposed to "if Operation /= No_Elmt".

   --------------------------------------
   -- Subprograms added for ASIS needs --
   --------------------------------------

   procedure Add_To_Elmt_List (Unit : Unit_Id; List : in out Elist_Id);
   --  If List is equial to No_Lists, creates the new (empty) list, assigns
   --  it to List and appens Unit to this list. Otherwise, checks, if Unit
   --  already is in List, and if the check fails, appends Unit to List.
   --
   --  This procedure is intended to be used during creating the dependency
   --  lists for a Unit.

   function In_Elmt_List (U : Unit_Id; List : Elist_Id) return Boolean;
   --  Checks if Unit is included in the given List. Returns False for
   --  No_List and for empty list.

   procedure Print_List (List : Elist_Id);
   --  Currently this procedure only produces the debug output for List

   function List_Length (List : Elist_Id) return Natural;
   --  Returns the number of items in the given list. It is an error to call
   --  this function with No_Elist.

   function Intersect (List1 : Elist_Id; List2 : Elist_Id) return Boolean;
   --  Checks if List1 and List2 have a common data element (that is, if
   --  one of them is No_List or empty element list, False is returned).

   function Belongs (List1 : Elist_Id; List2 : Elist_Id) return Boolean;
   --  Checks if all the elements of List1 belongs to List2. If List1 is
   --  equial to No_List, returns True

   procedure Move_List
     (List_From :        Elist_Id;
      List_To   : in out Elist_Id);
   --  Moves (prepends) the content of List_From to List_To. If List_To is
   --  equial to No_Elist, it is created. For now, this procedure does not
   --  check if the elements from List_From are already in List_To, therefore
   --  as a result of a call to this procedure, List_To can contain
   --  duplicated elements
   --
   --  If before the call List_From was equal to No_List, it will be No_List
   --  after the call. In any other case List_From will be an empty list after
   --  the call

   -------------------------------------
   -- Implementation of Element Lists --
   -------------------------------------

   --  Element lists are composed of three types of entities. The element
   --  list header, which references the first and last elements of the
   --  list, the elements themselves which are singly linked and also
   --  reference the nodes on the list, and finally the nodes themselves.
   --  The following diagram shows how an element list is represented:

   --       +----------------------------------------------------+
   --       |  +------------------------------------------+      |
   --       |  |                                          |      |
   --       V  |                                          V      |
   --    +-----|--+    +-------+    +-------+         +-------+  |
   --    |  Elmt  |    |  1st  |    |  2nd  |         |  Last |  |
   --    |  List  |--->|  Elmt |--->|  Elmt  ---...-->|  Elmt ---+
   --    | Header |    |   |   |    |   |   |         |   |   |
   --    +--------+    +---|---+    +---|---+         +---|---+
   --                      |            |                 |
   --                      V            V                 V
   --                  +-------+    +-------+         +-------+
   --                  |       |    |       |         |       |
   --                  | Unit1 |    | Unit2 |         | Unit3 |
   --                  |       |    |       |         |       |
   --                  +-------+    +-------+         +-------+

   --  The list header is an entry in the Elists table. The values used for
   --  the type Elist_Id are subscripts into this table. The First_Elmt field
   --  (Lfield1) points to the first element on the list, or to No_Elmt in the
   --  case of an empty list. Similarly the Last_Elmt field (Lfield2) points to
   --  the last element on the list or to No_Elmt in the case of an empty list.

   --  The elements themselves are entries in the Elmts table. The Next field
   --  of each entry points to the next element, or to the Elist header if this
   --  is the last item in the list. The Unit field points to the node which
   --  is referenced by the corresponding list entry.

   --------------------------
   --  Element List Tables --
   --------------------------

   type Elist_Header is record
      First : Elmt_Id;
      Last  : Elmt_Id;
   end record;

   package Elists is new Table.Table (
     Table_Component_Type => Elist_Header,
     Table_Index_Type     => Elist_Id,
     Table_Low_Bound      => First_Elist_Id,
     Table_Initial        => Alloc.Elists_Initial,
     Table_Increment      => Alloc.Elists_Increment,
     Table_Name           => "Elists");

   type Elmt_Item is record
      Unit : Unit_Id;
      Next : Union_Id;
   end record;

   package Elmts is new Table.Table (
     Table_Component_Type => Elmt_Item,
     Table_Index_Type     => Elmt_Id,
     Table_Low_Bound      => First_Elmt_Id,
     Table_Initial        => Alloc.Elmts_Initial,
     Table_Increment      => Alloc.Elmts_Increment,
     Table_Name           => "Elmts");

   type Saved_Lists is record
      Saved_Elmts  : Elmts.Saved_Table;
      Saved_Elists : Elists.Saved_Table;
   end record;

end A4G.A_Elists;
