------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . C O N T T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2006, Free Software Foundation, Inc.       --
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

--  This package defines the Context (Context) Table - the top-level ASIS data
--  structure for ASIS Context/Compilation_Unit processing.

with A4G.A_Alloc;  use A4G.A_Alloc;
with A4G.A_Types;  use A4G.A_Types;
with A4G.Unit_Rec;
with A4G.Tree_Rec;
with A4G.A_Elists; use A4G.A_Elists;
with A4G.A_Opt;    use A4G.A_Opt;

with Table;
with Alloc;

with Types;        use Types;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Hostparm;

package A4G.Contt is

   ------------------------------------------------
   -- Subprograms for General Context Processing --
   ------------------------------------------------

   procedure Verify_Context_Name (Name : String; Cont : Context_Id);
   --  Verifies the string passed as the Name parameter for
   --  Asis.Ada_Environments.Associate. If the string can be used as a
   --  Context name, it is stored in a Context Table for a further use,
   --  and if the verification is failed, ASIS_Failed is raised and a Status
   --  is set as Parameter_Error.

   procedure Process_Context_Parameters
     (Parameters : String;
      Cont       : Context_Id := Non_Associated);
   --  Processes a Parameters string passed parameter to the
   --  Asis.Ada_Environments.Associate query. If there are any errors contained
   --  in the Context association parameters, ASIS_Failed is raised and
   --  a Status is set as Parameter_Error only in case of a fatal error,
   --  that is, when a given set of parameters does not allow to define a legal
   --  ASIS Context in case of ASIS-for-GNAT. For a non-fatal error detected
   --  for some parameter, ASIS warning is generated.
   --
   --  If the Parameters string contains tree file names, these names are
   --  stored in the Context Tree Table for Cont.

   function I_Options (C : Context_Id) return Argument_List;
   --  Returns the list of "-I" GNAT options according to the definition of
   --  the Context C.

   procedure Initialize;
   --  Should be called by Asis.Implementation.Initialize. Initializes the
   --  Context Table. Sets Current_Context and Current_Tree to nil values.

   procedure Finalize;
   --  Should be called by Asis.Implementation.Finalize.
   --  Finalizes all the Contexts being processed by ASIS and then finalizes
   --  the general Context Table. Produces the debug output, if the
   --  corresponding debug flags are set ON.
   --  ??? Requires revising

   procedure Pre_Initialize (C : Context_Id);
   --  Should be called by Asis.Ada_Environments.Associate. It initializes
   --  the unit and tree tables for C, but it does not put any information
   --  in these tables. Before doing this, it backups the current context,
   --  and after initializing Context tables it sets Current_Context to C and
   --  Current_Tree to Nil_Tree.

   procedure Initialize (C : Context_Id);
   --  Should be called by Asis.Ada_Environments.Open.
   --  Initializes the internal structures and Tables for the Context C.

   procedure Finalize (C : Context_Id);
   --  Should be called by Asis.Ada_Environments.Close.
   --  Finalizes the internal structures and Tables for the Context C.
   --  Produces the debug output, if the corresponding debug flags are
   --  set ON.

   function Allocate_New_Context return Context_Id;
   --  Allocates a new entry to an ASIS Context Table and returns the
   --  corresponding Id as a result

   function Context_Info (C : Context_Id) return String;
   --  returns the string, which content uniquely identifies the ASIS Context
   --  printed by C in user-understandable form. Initially is supposed to
   --  be called in the implementation of Asis_Compilation_Units.Unique_Name.
   --  May be used for producing some debug output.

   procedure Erase_Old (C : Context_Id);
   --  Erases all the settings for the given context, which have been
   --  made by previous calls to Asis.Ada_Environments.Associate
   --  procedure. (All the dynamically allocated memory is reclaimed)

   procedure Set_Context_Name (C : Context_Id; Name : String);
   --  Stores Name as the context name for context C

   procedure Set_Context_Parameters (C : Context_Id; Parameters : String);
   --  Stores Parameters as the context parameters for context C

   function Get_Context_Name (C : Context_Id) return String;
   --  returns a name string associated with a context

   function Get_Context_Parameters (C : Context_Id) return String;
   --  returns a parameters string associated with a context

   procedure Print_Context_Info;
   --  produces the general debug output for ASIS contexts;
   --  is intended to be used during ASIS implementation finalization

   procedure Print_Context_Info (C : Context_Id);
   --  produces the detailed debug output for the ASIS context C
   --  is intended to be used during ASIS implementation finalization

   procedure Print_Context_Parameters (C : Context_Id);
   --  prints strings which were used when the Context C was associated
   --  for the last time, as well as the corresponding settings made
   --  as the result of this association

   procedure Scan_Trees_New (C : Context_Id);
   --  This procedure does the main job when opening the Context C in case if
   --  tree processing mode for this context is set to Pre_Created or Mixed.
   --  It scans the set of tree files making up the Context and collects some
   --  block-box information about Compilation Units belonging to this Context.
   --  In case if any error is detected (including error when reading a tree
   --  file in -C1 or -CN Context mode or any inconsistency), ASIS_Failed is
   --  raised as a result of opening the Context

   function Get_Current_Tree return Tree_Id;
   --  Returns the Id of the tree currently accessed by ASIS.

   procedure Set_Current_Tree (Tree : Tree_Id);
   --  Sets the currently accessed tree

   function Get_Current_Cont return Context_Id;
   --  Returns the Id of the ASIS Context to which the currently accessed
   --  tree belongs

   procedure Set_Current_Cont (L : Context_Id);
   --  Sets the Id of the Context to which the currently accessed tree
   --  belongs

   ---------------------------------------------------
   -- Context Attributes Access and Update Routines --
   ---------------------------------------------------

   function Is_Associated (C : Context_Id) return Boolean;
   function Is_Opened     (C : Context_Id) return Boolean;
   function Opened_At     (C : Context_Id) return ASIS_OS_Time;

   function Context_Processing_Mode (C : Context_Id) return Context_Mode;
   function Tree_Processing_Mode    (C : Context_Id) return Tree_Mode;
   function Source_Processing_Mode  (C : Context_Id) return Source_Mode;
   function Use_Default_Trees       (C : Context_Id) return Boolean;
   function Gcc_To_Call             (C : Context_Id) return String_Access;

   --------

   procedure Set_Is_Associated (C : Context_Id; Ass : Boolean);
   procedure Set_Is_Opened     (C : Context_Id; Op  : Boolean);

   procedure Set_Context_Processing_Mode (C : Context_Id; M : Context_Mode);
   procedure Set_Tree_Processing_Mode    (C : Context_Id; M : Tree_Mode);
   procedure Set_Source_Processing_Mode  (C : Context_Id; M : Source_Mode);
   procedure Set_Use_Default_Trees       (C : Context_Id; B : Boolean);

   procedure Set_Default_Context_Processing_Mode (C : Context_Id);
   procedure Set_Default_Tree_Processing_Mode    (C : Context_Id);
   procedure Set_Default_Source_Processing_Mode  (C : Context_Id);
   -------------------------------------------------

   -----------------
   -- Name Buffer --
   -----------------

   --  All the Name Tables from the ASIS Context implementation
   --  shares the same Name Buffer.

   A_Name_Buffer : String (1 .. Hostparm.Max_Name_Length);
   --  This buffer is used to set the name to be stored in the table for the
   --  Name_Find call, and to retrieve the name for the Get_Name_String call.

   A_Name_Len : Natural;
   --  Length of name stored in Name_Buffer. Used as an input parameter for
   --  Name_Find, and as an output value by Get_Name_String.

   procedure Set_Name_String (S : String);
   --  Sets A_Name_Len as S'Length and after that sets
   --  A_Name_Buffer (1 .. A_Name_Len) as S. We do not need any encoding,
   --  and we usually operate with strings which should be stored as they
   --  came from the clients, so we simply can set the string to be
   --  stored or looked for in the name buffer as it is.

   procedure NB_Save;
   --  Saves the current state (the value of A_Name_Len and the characters
   --  in A_Name_Buffer (1 .. A_Name_Len) of the A_Name Buffer. This state may
   --  be restored by NB_Restore

   procedure NB_Restore;
   --  Restores the state of the A_Name Buffer, which has been saved by the
   --  NB_Save procedure

   ------------------
   -- Search Paths --
   ------------------

   procedure Set_Search_Paths (C : Context_Id);
   --  Stores the previously verified and stored in temporary data structures
   --  directory names as search paths for a given contexts. Also sets the
   --  list of the "-I" options for calling the compiler from inside ASIS.
   --  The temporary structures are cleaned, and the dynamically allocated
   --  storage used by them are reclaimed.

   function Locate_In_Search_Path
     (C         : Context_Id;
      File_Name : String;
      Dir_Kind  : Search_Dir_Kinds)
      return String_Access;
   --  This function tries to locate the given file (having File_Name as its
   --  name) in the search path associated with context C. If the file
   --  cannot be located, the null access value is returned

   -----------------
   --  NEW STUFF  --
   -----------------

   procedure Save_Context (C : Context_Id);
   --  Saves the tables for C. Does nothing, if the currently accessed Context
   --  is Non_Associated

   procedure Restore_Context (C : Context_Id);
   --  restored tables for C taking them from the internal C structure

   procedure Reset_Context (C : Context_Id);
   --  If C is not Nil_Context_Id, resets the currently accessed Context to be
   --  C, including restoring all the tables. If C is Nil_Context_Id, does
   --  nothing (we need this check for Nil_Context_Id, because C may come from
   --  Nil_Compilation_Unit

   procedure Backup_Current_Context;
   --  Saves tables for the currently accessed Context. Does nothing, if the
   --  currently accessed Context is Non_Associated.

private

   ------------------------
   -- ASIS Context Table --
   ------------------------

   --  The entries in the table are accessed using a Context_Id that ranges
   --  from Context_Low_Bound to Context_High_Bound. Context_Low_Bound is
   --  reserved for a Context which has never been  associated.
   --
   --  The following diagram shows the general idea of the multiple
   --  Context processing in ASIS:

   --  Asis.Compilation_Unit value:
   --  +-----------------------+
   --  | Id : Unit_Id;   ------+---------
   --  |                       |         |
   --  | Cont_Id : Context_Id;-+-        |
   --  +-----------------------+ |       |
   --                            |       |
   --                            |       |
   --  +-------------------------        |
   --  |                                 |
   --  |   Context Table:                |
   --  |   =============                 |
   --  |   +--------------+              |
   --  |   |              |              |
   --  |   |              |              |
   --  |   |              |              |
   --  |   |              |              |
   --  |   +--------------+              |    Unit_Reciord value
   --  +-->|              |              |   /
   --      |   ...        |              |  /
   --      |              |              V /                  Unit Table for
   --      |              |      +-----+-----+----------... / a given
   --      |   Units -----+----->|     |     |             /  Context
   --      |              |      +-----+-----+----------...
   --      |              |              ^  ^
   --      |              |              |  |------------------+
   --      |              |              |                     |
   --      |              |              |                     |
   --      |              |              V                     |
   --      |              |      +-----------------...         |
   --      | Name_Chars --+----> |                             |
   --      |              |      +-----------------...         |
   --      |              |                                    |
   --      |              |             +-----------------------
   --      |              |             |
   --      |              |             V
   --      |              |      +----------------...
   --      |  Hash_Table -+----> |
   --      |              |      +----------------...
   --      |              |
   --      |              |
   --      | ...          |
   --      |              |
   --      +--------------+
   --      |              |
   --      |              |
   --      | ...          |
   --      +--------------+
   --      |              |
   --      .              .
   --      .              .
   --      .              .

   ---------------------------
   -- Types for hash tables --
   ---------------------------

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   type Hash_Array      is array (Hash_Index_Type) of Unit_Id;
   --  Each kind of tables in the implementation of an ASIS Context uses
   --  its own type of hash table
   --
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   function Hash return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)
   --  In Unit Name Table it can really be applied only to the "normalized"
   --  unit names.

   ---------------
   -- NEW STUFF --
   ---------------

   package A_Name_Chars is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Name_Chars_Initial,
     Table_Increment      => Alloc.Name_Chars_Increment,
     Table_Name           => "A_Name_Chars");

   package Unit_Table is new Table.Table (
     Table_Component_Type => A4G.Unit_Rec.Unit_Record,
     Table_Index_Type     => A4G.A_Types.Unit_Id,
     Table_Low_Bound      => A4G.A_Types.First_Unit_Id,
     Table_Initial        => A4G.A_Alloc.Alloc_ASIS_Units_Initial,
     Table_Increment      => A4G.A_Alloc.Alloc_ASIS_Units_Increment,
     Table_Name           => "ASIS_Compilation_Units");

   package Tree_Table is new Table.Table (
     Table_Component_Type => A4G.Tree_Rec.Tree_Record,
     Table_Index_Type     => A4G.A_Types.Tree_Id,
     Table_Low_Bound      => A4G.A_Types.First_Tree_Id,
     Table_Initial        => A4G.A_Alloc.Alloc_ASIS_Trees_Initial,
     Table_Increment      => A4G.A_Alloc.Alloc_ASIS_Trees_Increment,
     Table_Name           => "ASIS_Trees");

   subtype Directory_List_Ptr is Argument_List_Access;
   subtype Tree_File_List_Ptr is Argument_List_Access;

   type Saved_Context is record
      Context_Name_Chars : A_Name_Chars.Saved_Table;
      Context_Unit_Lists : A4G.A_Elists.Saved_Lists;
      Units              : Unit_Table.Saved_Table;
      Trees              : Tree_Table.Saved_Table;
   end record;

   --------------------
   -- Context Record --
   --------------------

   type Context_Record is record  -- the field should be commented also here!!!

      ---------------------------------------------------
      -- General Context/Context Attributes and Fields --
      ---------------------------------------------------

      Name       : String_Access;
      Parameters : String_Access;
      --  to keep the parameters set by the ASIS Associate routine

      GCC : String_Access;
      --  If non-null, contains the full path to the compiler to be used when
      --  creating trees on the fly. (If null, the standard gcc/GNAT
      --  installation is used)

      Is_Associated : Boolean := False;
      Is_Opened     : Boolean := False;

      Opened_At     : ASIS_OS_Time := Last_ASIS_OS_Time;
      --  when an application opens a Context, we store the time of opening;
      --  we need it to check whether an Element or a Compilation_Unit in
      --  use has been obtained after the last opening of this Context

      Specs  : Natural;
      Bodies : Natural;
      --  counters for library_units_declarations and library_unit_bodies/
      --  subunits (respectively) contained in a Context. We need them to
      --  optimize processing of the queries Compilation_Units,
      --  Libary_Unit_Declarations and Compilation_Unit_Bodies from
      --  Asis.Compilation_Units and to make the difference between "regular"
      --  and nonexistent units. Last for Context's Unit table gives us the
      --  whole number of all the units, including nonexistent ones.

      -------------------------------------
      -- Fields for Context's Unit Table --
      -------------------------------------

      Hash_Table : Hash_Array; -- hash table for Unit Table

      Current_Main_Unit : Unit_Id;
      --  The variable to store the Id of the Unit corresponding to the
      --  main unit of the currently accessed tree

      --  ----------------------------------------------...
      --  | Nil  |   |...|XXX|   |   |   |   |
      --  | Unit |   |...|XXX|   |   |   |   |              <- Unit Table
      --  ----------------------------------------------...
      --                   ^   ^   ^   ^   ^
      --                   |   |   |   |   |
      --                   |    ----------------|
      --           Current_Main_Unit            |
      --                                        |
      --                                for all of these Units
      --                                Is_New (C, Unit) = True

      ------------------
      -- Search Paths --
      ------------------

      --  we do not know the number of the directories in a path, so we have
      --  to use pointers to the arrays of the pointers to strings

      Source_Path : Directory_List_Ptr;
      --  The search path for the source files
      Object_Path : Directory_List_Ptr;
      --  The search path for library (that is, object + ALI) files
      Tree_Path    : Directory_List_Ptr;
      --  The search path for the tree output files

      Context_I_Options : Directory_List_Ptr;
      --  Source search path for GNAT or another tree builder, when it is
      --  called from inside ASIS to create a tree output file "on the fly"
      --  ("I" comes after "-I" gcc/GNAT option). The corresponding search
      --  path is obtained form the value of the Source_Path field by
      --  prepending "-I" to each directory name kept in Source_Path and
      --  by appending "-I-" element to this path

      Context_Tree_Files : Tree_File_List_Ptr;

      Back_Up : Saved_Context;

      Mode              : Context_Mode := All_Trees;
      Tree_Processing   : Tree_Mode    := Pre_Created;
      Source_Processing : Source_Mode  := All_Sources;

      Use_Default_Trees : Boolean      := False;
      --  If set On, the value of the GNAT environment variable
      --  ADA_OBJECTS_PATH is appended to Object_Path

   end record;

   -------------------
   -- Context Table --
   -------------------

   package Contexts is new Table.Table (
     Table_Component_Type => Context_Record,
     Table_Index_Type     => Context_Id,
     Table_Low_Bound      => First_Context_Id,
     Table_Initial        => Alloc_Contexts_Initial,
     Table_Increment      => Alloc_Contexts_Increment,
     Table_Name           => "ASIS_Contexts");

   ------------------------------------------------------
   -- "Back-Up" Name Buffer for NB_Save and NB_Restore --
   ------------------------------------------------------

   Backup_Name_Buffer : String (1 .. Hostparm.Max_Name_Length);
   Backup_Name_Len    : Natural := 0;
   --  ??? is it the right place for these declarations???

   Current_Tree : Tree_Id := Nil_Tree;
   --  This is the tree, which is being currently accessed by ASIS.
   --  The Initialize procedure sets Current_Tree equal to Nil_Tree.

   Current_Context : Context_Id := Non_Associated;
   --  This is the Context to which the currently accessed tree belongs.
   --  The Initialize procedure sets Current_Context equal to Non_Associated.

   First_New_Unit : Unit_Id;
   --  In the Incremental Context mode stores the first unit registered
   --  from the newly created tree. Then used by Set_All_Dependencies routine
   --  to collect full dependencies only for the units added to the Context

end A4G.Contt;
