------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S N A M E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;

package Snames is

--  This package contains definitions of standard names (i.e. entries in the
--  Names table) that are used throughout the GNAT compiler. It also contains
--  the definitions of some enumeration types whose definitions are tied to the
--  order of these preset names.

   ------------------
   -- Preset Names --
   ------------------

   --  The following are preset entries in the names table, which are entered
   --  at the start of every compilation for easy access. Note that the order
   --  of initialization of these names in the body must be coordinated with
   --  the order of names in this table.

   --  Note: a name may not appear more than once in the following list. If
   --  additional pragmas or attributes are introduced which might otherwise
   --  cause a duplicate, then list it only once in this table, and adjust the
   --  definition of the functions for testing for pragma names and attribute
   --  names, and returning their ID values. Of course everything is simpler if
   --  no such duplications occur.

   --  First we have the one character names used to optimize the lookup
   --  process for one character identifiers (to avoid the hashing in this
   --  case) There are a full 256 of these, but only the entries for lower
   --  case and upper case letters have identifiers

   --  The lower case letter entries are used for one character identifiers
   --  appearing in the source, for example in pragma Interface (C).

   Name_A : constant Name_Id := First_Name_Id + Character'Pos ('a');
   Name_B : constant Name_Id := First_Name_Id + Character'Pos ('b');
   Name_C : constant Name_Id := First_Name_Id + Character'Pos ('c');
   Name_D : constant Name_Id := First_Name_Id + Character'Pos ('d');
   Name_E : constant Name_Id := First_Name_Id + Character'Pos ('e');
   Name_F : constant Name_Id := First_Name_Id + Character'Pos ('f');
   Name_G : constant Name_Id := First_Name_Id + Character'Pos ('g');
   Name_H : constant Name_Id := First_Name_Id + Character'Pos ('h');
   Name_I : constant Name_Id := First_Name_Id + Character'Pos ('i');
   Name_J : constant Name_Id := First_Name_Id + Character'Pos ('j');
   Name_K : constant Name_Id := First_Name_Id + Character'Pos ('k');
   Name_L : constant Name_Id := First_Name_Id + Character'Pos ('l');
   Name_M : constant Name_Id := First_Name_Id + Character'Pos ('m');
   Name_N : constant Name_Id := First_Name_Id + Character'Pos ('n');
   Name_O : constant Name_Id := First_Name_Id + Character'Pos ('o');
   Name_P : constant Name_Id := First_Name_Id + Character'Pos ('p');
   Name_Q : constant Name_Id := First_Name_Id + Character'Pos ('q');
   Name_R : constant Name_Id := First_Name_Id + Character'Pos ('r');
   Name_S : constant Name_Id := First_Name_Id + Character'Pos ('s');
   Name_T : constant Name_Id := First_Name_Id + Character'Pos ('t');
   Name_U : constant Name_Id := First_Name_Id + Character'Pos ('u');
   Name_V : constant Name_Id := First_Name_Id + Character'Pos ('v');
   Name_W : constant Name_Id := First_Name_Id + Character'Pos ('w');
   Name_X : constant Name_Id := First_Name_Id + Character'Pos ('x');
   Name_Y : constant Name_Id := First_Name_Id + Character'Pos ('y');
   Name_Z : constant Name_Id := First_Name_Id + Character'Pos ('z');

   --  The upper case letter entries are used by expander code for local
   --  variables that do not require unique names (e.g. formal parameter names
   --  in constructed procedures).

   Name_uA : constant Name_Id := First_Name_Id + Character'Pos ('A');
   Name_uB : constant Name_Id := First_Name_Id + Character'Pos ('B');
   Name_uC : constant Name_Id := First_Name_Id + Character'Pos ('C');
   Name_uD : constant Name_Id := First_Name_Id + Character'Pos ('D');
   Name_uE : constant Name_Id := First_Name_Id + Character'Pos ('E');
   Name_uF : constant Name_Id := First_Name_Id + Character'Pos ('F');
   Name_uG : constant Name_Id := First_Name_Id + Character'Pos ('G');
   Name_uH : constant Name_Id := First_Name_Id + Character'Pos ('H');
   Name_uI : constant Name_Id := First_Name_Id + Character'Pos ('I');
   Name_uJ : constant Name_Id := First_Name_Id + Character'Pos ('J');
   Name_uK : constant Name_Id := First_Name_Id + Character'Pos ('K');
   Name_uL : constant Name_Id := First_Name_Id + Character'Pos ('L');
   Name_uM : constant Name_Id := First_Name_Id + Character'Pos ('M');
   Name_uN : constant Name_Id := First_Name_Id + Character'Pos ('N');
   Name_uO : constant Name_Id := First_Name_Id + Character'Pos ('O');
   Name_uP : constant Name_Id := First_Name_Id + Character'Pos ('P');
   Name_uQ : constant Name_Id := First_Name_Id + Character'Pos ('Q');
   Name_uR : constant Name_Id := First_Name_Id + Character'Pos ('R');
   Name_uS : constant Name_Id := First_Name_Id + Character'Pos ('S');
   Name_uT : constant Name_Id := First_Name_Id + Character'Pos ('T');
   Name_uU : constant Name_Id := First_Name_Id + Character'Pos ('U');
   Name_uV : constant Name_Id := First_Name_Id + Character'Pos ('V');
   Name_uW : constant Name_Id := First_Name_Id + Character'Pos ('W');
   Name_uX : constant Name_Id := First_Name_Id + Character'Pos ('X');
   Name_uY : constant Name_Id := First_Name_Id + Character'Pos ('Y');
   Name_uZ : constant Name_Id := First_Name_Id + Character'Pos ('Z');

   --  Note: the following table is read by the utility program XSNAMES, and
   --  its format should not be changed without coordinating with this program.

   N : constant Name_Id := First_Name_Id + 256;
   --  Synonym used in standard name definitions

   --  Names referenced in snames.h

   Name_uParent                        : constant Name_Id := N + 000;
   Name_uTag                           : constant Name_Id := N + 001;
   Name_Off                            : constant Name_Id := N + 002;
   Name_Space                          : constant Name_Id := N + 003;
   Name_Time                           : constant Name_Id := N + 004;

   --  Names of aspects for which there are no matching pragmas or attributes
   --  so that they need to be included for aspect specification use.

   Name_Default_Value                  : constant Name_Id := N + 005;
   Name_Default_Component_Value        : constant Name_Id := N + 006;
   Name_Dimension                      : constant Name_Id := N + 007;
   Name_Dimension_System               : constant Name_Id := N + 008;
   Name_Dynamic_Predicate              : constant Name_Id := N + 009;
   Name_Static_Predicate               : constant Name_Id := N + 010;
   Name_Synchronization                : constant Name_Id := N + 011;
   Name_Unimplemented                  : constant Name_Id := N + 012;

   --  Some special names used by the expander. Note that the lower case u's
   --  at the start of these names get translated to extra underscores. These
   --  names are only referenced internally by expander generated code.

   Name_uAbort_Signal                  : constant Name_Id := N + 013;
   Name_uAlignment                     : constant Name_Id := N + 014;
   Name_uAssign                        : constant Name_Id := N + 015;
   Name_uATCB                          : constant Name_Id := N + 016;
   Name_uChain                         : constant Name_Id := N + 017;
   Name_uController                    : constant Name_Id := N + 018;
   Name_uCPU                           : constant Name_Id := N + 019;
   Name_uDispatching_Domain            : constant Name_Id := N + 020;
   Name_uEntry_Bodies                  : constant Name_Id := N + 021;
   Name_uExpunge                       : constant Name_Id := N + 022;
   Name_uFinalizer                     : constant Name_Id := N + 023;
   Name_uIdepth                        : constant Name_Id := N + 024;
   Name_uInit                          : constant Name_Id := N + 025;
   Name_uInvariant                     : constant Name_Id := N + 026;
   Name_uMaster                        : constant Name_Id := N + 027;
   Name_uObject                        : constant Name_Id := N + 028;
   Name_uPost                          : constant Name_Id := N + 029;
   Name_uPostconditions                : constant Name_Id := N + 030;
   Name_uPre                           : constant Name_Id := N + 031;
   Name_uPriority                      : constant Name_Id := N + 032;
   Name_uProcess_ATSD                  : constant Name_Id := N + 033;
   Name_uRelative_Deadline             : constant Name_Id := N + 034;
   Name_uResult                        : constant Name_Id := N + 035;
   Name_uSecondary_Stack               : constant Name_Id := N + 036;
   Name_uService                       : constant Name_Id := N + 037;
   Name_uSize                          : constant Name_Id := N + 038;
   Name_uStack                         : constant Name_Id := N + 039;
   Name_uTags                          : constant Name_Id := N + 040;
   Name_uTask                          : constant Name_Id := N + 041;
   Name_uTask_Id                       : constant Name_Id := N + 042;
   Name_uTask_Info                     : constant Name_Id := N + 043;
   Name_uTask_Name                     : constant Name_Id := N + 044;
   Name_uTrace_Sp                      : constant Name_Id := N + 045;
   Name_uType_Invariant                : constant Name_Id := N + 046;

   --  Names of predefined primitives used in the expansion of dispatching
   --  requeue and select statements, Abort, 'Callable and 'Terminated.

   Name_uDisp_Asynchronous_Select      : constant Name_Id := N + 047;
   Name_uDisp_Conditional_Select       : constant Name_Id := N + 048;
   Name_uDisp_Get_Prim_Op_Kind         : constant Name_Id := N + 049;
   Name_uDisp_Get_Task_Id              : constant Name_Id := N + 050;
   Name_uDisp_Requeue                  : constant Name_Id := N + 051;
   Name_uDisp_Timed_Select             : constant Name_Id := N + 052;

   --  Names of routines and fields in Ada.Finalization, needed by expander

   Name_Initialize                     : constant Name_Id := N + 053;
   Name_Adjust                         : constant Name_Id := N + 054;
   Name_Finalize                       : constant Name_Id := N + 055;
   Name_Finalize_Address               : constant Name_Id := N + 056;
   Name_Next                           : constant Name_Id := N + 057;
   Name_Prev                           : constant Name_Id := N + 058;

   --  Names of allocation routines, also needed by expander

   Name_Allocate                       : constant Name_Id := N + 059;
   Name_Deallocate                     : constant Name_Id := N + 060;
   Name_Dereference                    : constant Name_Id := N + 061;

   --  Text_IO generic subpackages (see Rtsfind.Check_Text_IO_Special_Unit)

   First_Text_IO_Package               : constant Name_Id := N + 062;
   Name_Decimal_IO                     : constant Name_Id := N + 062;
   Name_Enumeration_IO                 : constant Name_Id := N + 063;
   Name_Fixed_IO                       : constant Name_Id := N + 064;
   Name_Float_IO                       : constant Name_Id := N + 065;
   Name_Integer_IO                     : constant Name_Id := N + 066;
   Name_Modular_IO                     : constant Name_Id := N + 067;
   Last_Text_IO_Package                : constant Name_Id := N + 067;

   subtype Text_IO_Package_Name is Name_Id
     range First_Text_IO_Package .. Last_Text_IO_Package;

   --  Names used by the analyzer and expander for aspect Dimension and
   --  Dimension_System to deal with Sqrt and IO routines.

   Name_Dim_Symbol                     : constant Name_Id := N + 068; -- Ada 12
   Name_Item                           : constant Name_Id := N + 069; -- Ada 12
   Name_Put_Dim_Of                     : constant Name_Id := N + 070; -- Ada 12
   Name_Sqrt                           : constant Name_Id := N + 071; -- Ada 12
   Name_Symbol                         : constant Name_Id := N + 072; -- Ada 12
   Name_Unit_Symbol                    : constant Name_Id := N + 073; -- Ada 12

   --  Some miscellaneous names used for error detection/recovery

   Name_Const                          : constant Name_Id := N + 074;
   Name_Error                          : constant Name_Id := N + 075;
   Name_False                          : constant Name_Id := N + 076;
   Name_Go                             : constant Name_Id := N + 077;
   Name_Put                            : constant Name_Id := N + 078;
   Name_Put_Line                       : constant Name_Id := N + 079;
   Name_To                             : constant Name_Id := N + 080;

   --  Name used by the integrated preprocessor

   Name_Defined                        : constant Name_Id := N + 081;

   --  Names for packages that are treated specially by the compiler

   Name_Exception_Traces               : constant Name_Id := N + 082;
   Name_Finalization                   : constant Name_Id := N + 083;
   Name_Interfaces                     : constant Name_Id := N + 084;
   Name_Most_Recent_Exception          : constant Name_Id := N + 085;
   Name_Standard                       : constant Name_Id := N + 086;
   Name_System                         : constant Name_Id := N + 087;
   Name_Text_IO                        : constant Name_Id := N + 088;
   Name_Wide_Text_IO                   : constant Name_Id := N + 089;
   Name_Wide_Wide_Text_IO              : constant Name_Id := N + 090;

   --  Names of implementations of the distributed systems annex

   First_PCS_Name                      : constant Name_Id := N + 091;
   Name_No_DSA                         : constant Name_Id := N + 091;
   Name_GARLIC_DSA                     : constant Name_Id := N + 092;
   Name_PolyORB_DSA                    : constant Name_Id := N + 093;
   Last_PCS_Name                       : constant Name_Id := N + 093;

   subtype PCS_Names is Name_Id
     range First_PCS_Name .. Last_PCS_Name;

   --  Names of identifiers used in expanding distribution stubs

   Name_Addr                           : constant Name_Id := N + 094;
   Name_Async                          : constant Name_Id := N + 095;
   Name_Get_Active_Partition_ID        : constant Name_Id := N + 096;
   Name_Get_RCI_Package_Receiver       : constant Name_Id := N + 097;
   Name_Get_RCI_Package_Ref            : constant Name_Id := N + 098;
   Name_Origin                         : constant Name_Id := N + 099;
   Name_Params                         : constant Name_Id := N + 100;
   Name_Partition                      : constant Name_Id := N + 101;
   Name_Partition_Interface            : constant Name_Id := N + 102;
   Name_Ras                            : constant Name_Id := N + 103;
   Name_uCall                          : constant Name_Id := N + 104;
   Name_RCI_Name                       : constant Name_Id := N + 105;
   Name_Receiver                       : constant Name_Id := N + 106;
   Name_Rpc                            : constant Name_Id := N + 107;
   Name_Subp_Id                        : constant Name_Id := N + 108;
   Name_Operation                      : constant Name_Id := N + 109;
   Name_Argument                       : constant Name_Id := N + 110;
   Name_Arg_Modes                      : constant Name_Id := N + 111;
   Name_Handler                        : constant Name_Id := N + 112;
   Name_Target                         : constant Name_Id := N + 113;
   Name_Req                            : constant Name_Id := N + 114;
   Name_Obj_TypeCode                   : constant Name_Id := N + 115;
   Name_Stub                           : constant Name_Id := N + 116;

   --  Operator Symbol entries. The actual names have an upper case O at the
   --  start in place of the Op_ prefix (e.g. the actual name that corresponds
   --  to Name_Op_Abs is "Oabs".

   First_Operator_Name                 : constant Name_Id := N + 117;
   Name_Op_Abs                         : constant Name_Id := N + 117; -- "abs"
   Name_Op_And                         : constant Name_Id := N + 118; -- "and"
   Name_Op_Mod                         : constant Name_Id := N + 119; -- "mod"
   Name_Op_Not                         : constant Name_Id := N + 120; -- "not"
   Name_Op_Or                          : constant Name_Id := N + 121; -- "or"
   Name_Op_Rem                         : constant Name_Id := N + 122; -- "rem"
   Name_Op_Xor                         : constant Name_Id := N + 123; -- "xor"
   Name_Op_Eq                          : constant Name_Id := N + 124; -- "="
   Name_Op_Ne                          : constant Name_Id := N + 125; -- "/="
   Name_Op_Lt                          : constant Name_Id := N + 126; -- "<"
   Name_Op_Le                          : constant Name_Id := N + 127; -- "<="
   Name_Op_Gt                          : constant Name_Id := N + 128; -- ">"
   Name_Op_Ge                          : constant Name_Id := N + 129; -- ">="
   Name_Op_Add                         : constant Name_Id := N + 130; -- "+"
   Name_Op_Subtract                    : constant Name_Id := N + 131; -- "-"
   Name_Op_Concat                      : constant Name_Id := N + 132; -- "&"
   Name_Op_Multiply                    : constant Name_Id := N + 133; -- "*"
   Name_Op_Divide                      : constant Name_Id := N + 134; -- "/"
   Name_Op_Expon                       : constant Name_Id := N + 135; -- "**"
   Last_Operator_Name                  : constant Name_Id := N + 135;

   --  Names for all pragmas recognized by GNAT. The entries with the comment
   --  "Ada 83" are pragmas that are defined in Ada 83, but not in Ada 95.
   --  These pragmas are fully implemented in all modes (Ada 83, Ada 95, and
   --  Ada 2005). In Ada 95 and Ada 2005 modes, they are technically considered
   --  to be implementation dependent pragmas.

   --  The entries marked GNAT are pragmas that are defined by GNAT and that
   --  are implemented in all modes (Ada 83, Ada 95, and Ada 2005). Complete
   --  descriptions of the syntax of these implementation dependent pragmas may
   --  be found in the appropriate section in unit Sem_Prag in file
   --  sem-prag.adb, and they are documented in the GNAT reference manual.

   --  The entries marked Ada 05 are Ada 2005 pragmas. They are implemented in
   --  Ada 83 and Ada 95 mode as well, where they are technically considered to
   --  be implementation dependent pragmas.

   --  The entries marked Ada 12 are Ada 2012 pragmas. They are implemented in
   --  Ada 83, Ada 95, and Ada 2005 mode as well, where they are technically
   --  considered to be implementation dependent pragmas.

   --  The entries marked AAMP are AAMP specific pragmas that are recognized
   --  only in GNAT for the AAMP. They are ignored in other versions with
   --  appropriate warnings.

   First_Pragma_Name                   : constant Name_Id := N + 136;

   --  Configuration pragmas are grouped at start. Note that there is a list
   --  of these names in the GNAT Users guide, be sure to update this list if
   --  a new configuration pragma is added.

   Name_Ada_83                         : constant Name_Id := N + 136; -- GNAT
   Name_Ada_95                         : constant Name_Id := N + 137; -- GNAT
   Name_Ada_05                         : constant Name_Id := N + 138; -- GNAT
   Name_Ada_2005                       : constant Name_Id := N + 139; -- GNAT
   Name_Ada_12                         : constant Name_Id := N + 140; -- GNAT
   Name_Ada_2012                       : constant Name_Id := N + 141; -- GNAT
   Name_Allow_Integer_Address          : constant Name_Id := N + 142; -- GNAT
   Name_Annotate                       : constant Name_Id := N + 143; -- GNAT
   Name_Assertion_Policy               : constant Name_Id := N + 144; -- Ada 05
   Name_Assume                         : constant Name_Id := N + 145; -- GNAT
   Name_Assume_No_Invalid_Values       : constant Name_Id := N + 146; -- GNAT
   Name_Attribute_Definition           : constant Name_Id := N + 147; -- GNAT
   Name_C_Pass_By_Copy                 : constant Name_Id := N + 148; -- GNAT
   Name_Check_Float_Overflow           : constant Name_Id := N + 149; -- GNAT
   Name_Check_Name                     : constant Name_Id := N + 150; -- GNAT
   Name_Check_Policy                   : constant Name_Id := N + 151; -- GNAT
   Name_Compile_Time_Error             : constant Name_Id := N + 152; -- GNAT
   Name_Compile_Time_Warning           : constant Name_Id := N + 153; -- GNAT
   Name_Compiler_Unit                  : constant Name_Id := N + 154; -- GNAT
   Name_Compiler_Unit_Warning          : constant Name_Id := N + 155; -- GNAT
   Name_Component_Alignment            : constant Name_Id := N + 156; -- GNAT
   Name_Convention_Identifier          : constant Name_Id := N + 157; -- GNAT
   Name_Debug_Policy                   : constant Name_Id := N + 158; -- GNAT
   Name_Detect_Blocking                : constant Name_Id := N + 159; -- Ada 05
   Name_Default_Storage_Pool           : constant Name_Id := N + 160; -- Ada 12
   Name_Disable_Atomic_Synchronization : constant Name_Id := N + 161; -- GNAT
   Name_Discard_Names                  : constant Name_Id := N + 162;

   --  Note: Dispatching_Domain is not in this list because its name matches
   --  the name of the corresponding attribute. However, it is included in the
   --  definition of the type Pragma_Id, and the functions Get_Pragma_Id and
   --  Is_Pragma_Id correctly recognize and process Dispatching_Domain.
   --  Dispatching_Domain is a standard Ada 2012 pragma.

   Name_Elaboration_Checks             : constant Name_Id := N + 163; -- GNAT
   Name_Eliminate                      : constant Name_Id := N + 164; -- GNAT
   Name_Enable_Atomic_Synchronization  : constant Name_Id := N + 165; -- GNAT
   Name_Extend_System                  : constant Name_Id := N + 166; -- GNAT
   Name_Extensions_Allowed             : constant Name_Id := N + 167; -- GNAT
   Name_External_Name_Casing           : constant Name_Id := N + 168; -- GNAT

   --  Note: Fast_Math is not in this list because its name matches the name of
   --  the corresponding attribute. However, it is included in the definition
   --  of the type Pragma_Id, and the functions Get_Pragma_Id,
   --  Is_[Configuration_]Pragma_Id, and correctly recognize and process
   --  Fast_Math.

   Name_Favor_Top_Level                : constant Name_Id := N + 169; -- GNAT
   Name_Implicit_Packing               : constant Name_Id := N + 170; -- GNAT
   Name_Initialize_Scalars             : constant Name_Id := N + 171; -- GNAT
   Name_Interrupt_State                : constant Name_Id := N + 172; -- GNAT
   Name_License                        : constant Name_Id := N + 173; -- GNAT
   Name_Locking_Policy                 : constant Name_Id := N + 174;
   Name_Loop_Optimize                  : constant Name_Id := N + 175; -- GNAT
   Name_No_Run_Time                    : constant Name_Id := N + 176; -- GNAT
   Name_No_Strict_Aliasing             : constant Name_Id := N + 177; -- GNAT
   Name_No_Tagged_Streams              : constant Name_Id := N + 178; -- GNAT
   Name_Normalize_Scalars              : constant Name_Id := N + 179;
   Name_Optimize_Alignment             : constant Name_Id := N + 180; -- GNAT
   Name_Overflow_Mode                  : constant Name_Id := N + 181; -- GNAT
   Name_Overriding_Renamings           : constant Name_Id := N + 182; -- GNAT
   Name_Partition_Elaboration_Policy   : constant Name_Id := N + 183; -- Ada 05
   Name_Persistent_BSS                 : constant Name_Id := N + 184; -- GNAT
   Name_Polling                        : constant Name_Id := N + 185; -- GNAT
   Name_Prefix_Exception_Messages      : constant Name_Id := N + 186; -- GNAT
   Name_Priority_Specific_Dispatching  : constant Name_Id := N + 187; -- Ada 05
   Name_Profile                        : constant Name_Id := N + 188; -- Ada 05
   Name_Profile_Warnings               : constant Name_Id := N + 189; -- GNAT
   Name_Propagate_Exceptions           : constant Name_Id := N + 190; -- GNAT
   Name_Queuing_Policy                 : constant Name_Id := N + 191;
   Name_Rational                       : constant Name_Id := N + 192; -- GNAT
   Name_Ravenscar                      : constant Name_Id := N + 193; -- GNAT
   Name_Restricted_Run_Time            : constant Name_Id := N + 194; -- GNAT
   Name_Restrictions                   : constant Name_Id := N + 195;
   Name_Restriction_Warnings           : constant Name_Id := N + 196; -- GNAT
   Name_Reviewable                     : constant Name_Id := N + 197;
   Name_Short_Circuit_And_Or           : constant Name_Id := N + 198; -- GNAT
   Name_Short_Descriptors              : constant Name_Id := N + 199; -- GNAT
   Name_Source_File_Name               : constant Name_Id := N + 200; -- GNAT
   Name_Source_File_Name_Project       : constant Name_Id := N + 201; -- GNAT
   Name_SPARK_Mode                     : constant Name_Id := N + 202; -- GNAT
   Name_Style_Checks                   : constant Name_Id := N + 203; -- GNAT
   Name_Suppress                       : constant Name_Id := N + 204;
   Name_Suppress_Exception_Locations   : constant Name_Id := N + 205; -- GNAT
   Name_Task_Dispatching_Policy        : constant Name_Id := N + 206;
   Name_Unevaluated_Use_Of_Old         : constant Name_Id := N + 207; -- GNAT
   Name_Universal_Data                 : constant Name_Id := N + 208; -- AAMP
   Name_Unsuppress                     : constant Name_Id := N + 209; -- Ada 05
   Name_Use_VADS_Size                  : constant Name_Id := N + 210; -- GNAT
   Name_Validity_Checks                : constant Name_Id := N + 211; -- GNAT
   Name_Warning_As_Error               : constant Name_Id := N + 212; -- GNAT
   Name_Warnings                       : constant Name_Id := N + 213; -- GNAT
   Name_Wide_Character_Encoding        : constant Name_Id := N + 214; -- GNAT
   Last_Configuration_Pragma_Name      : constant Name_Id := N + 214;

   --  Remaining pragma names (non-configuration pragmas)

   Name_Abort_Defer                    : constant Name_Id := N + 215; -- GNAT
   Name_Abstract_State                 : constant Name_Id := N + 216; -- GNAT
   Name_All_Calls_Remote               : constant Name_Id := N + 217;
   Name_Assert                         : constant Name_Id := N + 218; -- Ada 05
   Name_Assert_And_Cut                 : constant Name_Id := N + 219; -- GNAT
   Name_Async_Readers                  : constant Name_Id := N + 220; -- GNAT
   Name_Async_Writers                  : constant Name_Id := N + 221; -- GNAT
   Name_Asynchronous                   : constant Name_Id := N + 222;
   Name_Atomic                         : constant Name_Id := N + 223;
   Name_Atomic_Components              : constant Name_Id := N + 224;
   Name_Attach_Handler                 : constant Name_Id := N + 225;
   Name_Check                          : constant Name_Id := N + 226; -- GNAT
   Name_CIL_Constructor                : constant Name_Id := N + 227; -- GNAT
   Name_Comment                        : constant Name_Id := N + 228; -- GNAT
   Name_Common_Object                  : constant Name_Id := N + 229; -- GNAT
   Name_Complete_Representation        : constant Name_Id := N + 230; -- GNAT
   Name_Complex_Representation         : constant Name_Id := N + 231; -- GNAT
   Name_Contract_Cases                 : constant Name_Id := N + 232; -- GNAT
   Name_Controlled                     : constant Name_Id := N + 233;
   Name_Convention                     : constant Name_Id := N + 234;
   Name_CPP_Class                      : constant Name_Id := N + 235; -- GNAT
   Name_CPP_Constructor                : constant Name_Id := N + 236; -- GNAT
   Name_CPP_Virtual                    : constant Name_Id := N + 237; -- GNAT
   Name_CPP_Vtable                     : constant Name_Id := N + 238; -- GNAT

   --  Note: CPU is not in this list because its name matches the name of
   --  the corresponding attribute. However, it is included in the definition
   --  of the type Pragma_Id, and the functions Get_Pragma_Id and Is_Pragma_Id
   --  correctly recognize and process CPU. CPU is a standard Ada 2012
   --  pragma.

   Name_Debug                          : constant Name_Id := N + 239; -- GNAT
   Name_Default_Initial_Condition      : constant Name_Id := N + 240; -- GNAT
   Name_Depends                        : constant Name_Id := N + 241; -- GNAT
   Name_Effective_Reads                : constant Name_Id := N + 242; -- GNAT
   Name_Effective_Writes               : constant Name_Id := N + 243; -- GNAT
   Name_Elaborate                      : constant Name_Id := N + 244; -- Ada 83
   Name_Elaborate_All                  : constant Name_Id := N + 245;
   Name_Elaborate_Body                 : constant Name_Id := N + 246;
   Name_Export                         : constant Name_Id := N + 247;
   Name_Export_Function                : constant Name_Id := N + 248; -- GNAT
   Name_Export_Object                  : constant Name_Id := N + 249; -- GNAT
   Name_Export_Procedure               : constant Name_Id := N + 250; -- GNAT
   Name_Export_Value                   : constant Name_Id := N + 251; -- GNAT
   Name_Export_Valued_Procedure        : constant Name_Id := N + 252; -- GNAT
   Name_Extensions_Visible             : constant Name_Id := N + 253; -- GNAT
   Name_External                       : constant Name_Id := N + 254; -- GNAT
   Name_Finalize_Storage_Only          : constant Name_Id := N + 255; -- GNAT
   Name_Ghost                          : constant Name_Id := N + 256; -- GNAT
   Name_Global                         : constant Name_Id := N + 257; -- GNAT
   Name_Ident                          : constant Name_Id := N + 258; -- GNAT
   Name_Implementation_Defined         : constant Name_Id := N + 259; -- GNAT
   Name_Implemented                    : constant Name_Id := N + 260; -- Ada 12
   Name_Import                         : constant Name_Id := N + 261;
   Name_Import_Function                : constant Name_Id := N + 262; -- GNAT
   Name_Import_Object                  : constant Name_Id := N + 263; -- GNAT
   Name_Import_Procedure               : constant Name_Id := N + 264; -- GNAT
   Name_Import_Valued_Procedure        : constant Name_Id := N + 265; -- GNAT
   Name_Independent                    : constant Name_Id := N + 266; -- Ada 12
   Name_Independent_Components         : constant Name_Id := N + 267; -- Ada 12
   Name_Initial_Condition              : constant Name_Id := N + 268; -- GNAT
   Name_Initializes                    : constant Name_Id := N + 269; -- GNAT
   Name_Inline                         : constant Name_Id := N + 270;
   Name_Inline_Always                  : constant Name_Id := N + 271; -- GNAT
   Name_Inline_Generic                 : constant Name_Id := N + 272; -- GNAT
   Name_Inspection_Point               : constant Name_Id := N + 273;

   --  Note: Interface is not in this list because its name matches an Ada 05
   --  keyword. However it is included in the definition of the type
   --  Attribute_Id, and the functions Get_Pragma_Id and Is_Pragma_Id correctly
   --  recognize and process Name_Interface.

   Name_Interface_Name                 : constant Name_Id := N + 274; -- GNAT
   Name_Interrupt_Handler              : constant Name_Id := N + 275;

   --  Note: Interrupt_Priority is not in this list because its name matches
   --  the name of the corresponding attribute. However, it is included in the
   --  definition of the type Pragma_Id, and the functions Get_Pragma_Id and
   --  Is_Pragma_Id correctly recognize and process Interrupt_Priority.

   Name_Invariant                      : constant Name_Id := N + 276; -- GNAT
   Name_Java_Constructor               : constant Name_Id := N + 277; -- GNAT
   Name_Java_Interface                 : constant Name_Id := N + 278; -- GNAT
   Name_Keep_Names                     : constant Name_Id := N + 279; -- GNAT
   Name_Link_With                      : constant Name_Id := N + 280; -- GNAT
   Name_Linker_Alias                   : constant Name_Id := N + 281; -- GNAT
   Name_Linker_Constructor             : constant Name_Id := N + 282; -- GNAT
   Name_Linker_Destructor              : constant Name_Id := N + 283; -- GNAT
   Name_Linker_Options                 : constant Name_Id := N + 284;
   Name_Linker_Section                 : constant Name_Id := N + 285; -- GNAT
   Name_List                           : constant Name_Id := N + 286;

   --  Note: Lock_Free is not in this list because its name matches the name of
   --  the corresponding attribute. However, it is included in the definition
   --  of the type Pragma_Id, and the functions Get_Pragma_Id and Is_Pragma_Id
   --  correctly recognize and process Lock_Free. Lock_Free is a GNAT pragma.

   Name_Loop_Invariant                 : constant Name_Id := N + 287; -- GNAT
   Name_Loop_Variant                   : constant Name_Id := N + 288; -- GNAT
   Name_Machine_Attribute              : constant Name_Id := N + 289; -- GNAT
   Name_Main                           : constant Name_Id := N + 290; -- GNAT
   Name_Main_Storage                   : constant Name_Id := N + 291; -- GNAT
   Name_Memory_Size                    : constant Name_Id := N + 292; -- Ada 83
   Name_No_Body                        : constant Name_Id := N + 293; -- GNAT
   Name_No_Elaboration_Code_All        : constant Name_Id := N + 294; -- GNAT
   Name_No_Inline                      : constant Name_Id := N + 295; -- GNAT
   Name_No_Return                      : constant Name_Id := N + 296; -- Ada 05
   Name_Obsolescent                    : constant Name_Id := N + 297; -- GNAT
   Name_Optimize                       : constant Name_Id := N + 298;
   Name_Ordered                        : constant Name_Id := N + 299; -- GNAT
   Name_Pack                           : constant Name_Id := N + 300;
   Name_Page                           : constant Name_Id := N + 301;
   Name_Part_Of                        : constant Name_Id := N + 302; -- GNAT
   Name_Passive                        : constant Name_Id := N + 303; -- GNAT
   Name_Post                           : constant Name_Id := N + 304; -- GNAT
   Name_Postcondition                  : constant Name_Id := N + 305; -- GNAT
   Name_Post_Class                     : constant Name_Id := N + 306; -- GNAT
   Name_Pre                            : constant Name_Id := N + 307; -- GNAT
   Name_Precondition                   : constant Name_Id := N + 308; -- GNAT
   Name_Predicate                      : constant Name_Id := N + 309; -- GNAT
   Name_Preelaborable_Initialization   : constant Name_Id := N + 310; -- Ada 05
   Name_Preelaborate                   : constant Name_Id := N + 311;
   Name_Pre_Class                      : constant Name_Id := N + 312; -- GNAT

   --  Note: Priority is not in this list because its name matches the name of
   --  the corresponding attribute. However, it is included in the definition
   --  of the type Pragma_Id, and the functions Get_Pragma_Id and Is_Pragma_Id
   --  correctly recognize and process Priority. Priority is a standard Ada 95
   --  pragma.

   Name_Provide_Shift_Operators        : constant Name_Id := N + 313; -- GNAT
   Name_Psect_Object                   : constant Name_Id := N + 314; -- GNAT
   Name_Pure                           : constant Name_Id := N + 315;
   Name_Pure_Function                  : constant Name_Id := N + 316; -- GNAT
   Name_Refined_Depends                : constant Name_Id := N + 317; -- GNAT
   Name_Refined_Global                 : constant Name_Id := N + 318; -- GNAT
   Name_Refined_Post                   : constant Name_Id := N + 319; -- GNAT
   Name_Refined_State                  : constant Name_Id := N + 320; -- GNAT
   Name_Relative_Deadline              : constant Name_Id := N + 321; -- Ada 05
   Name_Remote_Access_Type             : constant Name_Id := N + 322; -- GNAT
   Name_Remote_Call_Interface          : constant Name_Id := N + 323;
   Name_Remote_Types                   : constant Name_Id := N + 324;
   Name_Share_Generic                  : constant Name_Id := N + 325; -- GNAT
   Name_Shared                         : constant Name_Id := N + 326; -- Ada 83
   Name_Shared_Passive                 : constant Name_Id := N + 327;
   Name_Simple_Storage_Pool_Type       : constant Name_Id := N + 328; -- GNAT

   --  Note: Storage_Size is not in this list because its name matches the name
   --  of the corresponding attribute. However, it is included in the
   --  definition of the type Attribute_Id, and the functions Get_Pragma_Id and
   --  Is_Pragma_Id correctly recognize and process Name_Storage_Size.

   --  Note: Storage_Unit is also omitted from the list because of a clash with
   --  an attribute name, and is treated similarly.

   Name_Source_Reference               : constant Name_Id := N + 329; -- GNAT
   Name_Static_Elaboration_Desired     : constant Name_Id := N + 330; -- GNAT
   Name_Stream_Convert                 : constant Name_Id := N + 331; -- GNAT
   Name_Subtitle                       : constant Name_Id := N + 332; -- GNAT
   Name_Suppress_All                   : constant Name_Id := N + 333; -- GNAT
   Name_Suppress_Debug_Info            : constant Name_Id := N + 334; -- GNAT
   Name_Suppress_Initialization        : constant Name_Id := N + 335; -- GNAT
   Name_System_Name                    : constant Name_Id := N + 336; -- Ada 83
   Name_Test_Case                      : constant Name_Id := N + 337; -- GNAT
   Name_Task_Info                      : constant Name_Id := N + 338; -- GNAT
   Name_Task_Name                      : constant Name_Id := N + 339; -- GNAT
   Name_Task_Storage                   : constant Name_Id := N + 340; -- GNAT
   Name_Thread_Local_Storage           : constant Name_Id := N + 341; -- GNAT
   Name_Time_Slice                     : constant Name_Id := N + 342; -- GNAT
   Name_Title                          : constant Name_Id := N + 343; -- GNAT
   Name_Type_Invariant                 : constant Name_Id := N + 344; -- GNAT
   Name_Type_Invariant_Class           : constant Name_Id := N + 345; -- GNAT
   Name_Unchecked_Union                : constant Name_Id := N + 346; -- Ada 05
   Name_Unimplemented_Unit             : constant Name_Id := N + 347; -- GNAT
   Name_Universal_Aliasing             : constant Name_Id := N + 348; -- GNAT
   Name_Unmodified                     : constant Name_Id := N + 349; -- GNAT
   Name_Unreferenced                   : constant Name_Id := N + 350; -- GNAT
   Name_Unreferenced_Objects           : constant Name_Id := N + 351; -- GNAT
   Name_Unreserve_All_Interrupts       : constant Name_Id := N + 352; -- GNAT
   Name_Volatile                       : constant Name_Id := N + 353;
   Name_Volatile_Components            : constant Name_Id := N + 354;
   Name_Weak_External                  : constant Name_Id := N + 355; -- GNAT
   Last_Pragma_Name                    : constant Name_Id := N + 355;

   --  Language convention names for pragma Convention/Export/Import/Interface
   --  Note that Name_C is not included in this list, since it was already
   --  declared earlier in the context of one-character identifier names (where
   --  the order is critical to the fast look up process).

   --  Note: there are no convention names corresponding to the conventions
   --  Entry and Protected, this is because these conventions cannot be
   --  specified by a pragma.

   First_Convention_Name               : constant Name_Id := N + 356;
   Name_Ada                            : constant Name_Id := N + 356;
   Name_Ada_Pass_By_Copy               : constant Name_Id := N + 357;
   Name_Ada_Pass_By_Reference          : constant Name_Id := N + 358;
   Name_Assembler                      : constant Name_Id := N + 359;
   Name_CIL                            : constant Name_Id := N + 360;
   Name_COBOL                          : constant Name_Id := N + 361;
   Name_CPP                            : constant Name_Id := N + 362;
   Name_Fortran                        : constant Name_Id := N + 363;
   Name_Intrinsic                      : constant Name_Id := N + 364;
   Name_Java                           : constant Name_Id := N + 365;
   Name_Stdcall                        : constant Name_Id := N + 366;
   Name_Stubbed                        : constant Name_Id := N + 367;
   Last_Convention_Name                : constant Name_Id := N + 367;

   --  The following names are preset as synonyms for Assembler

   Name_Asm                            : constant Name_Id := N + 368;
   Name_Assembly                       : constant Name_Id := N + 369;

   --  The following names are preset as synonyms for C

   Name_Default                        : constant Name_Id := N + 370;
   --  Name_External (previously defined as pragma)

   --  The following names are preset as synonyms for CPP

   Name_C_Plus_Plus                    : constant Name_Id := N + 371;

   --  The following names are present as synonyms for Stdcall

   Name_DLL                            : constant Name_Id := N + 372;
   Name_Win32                          : constant Name_Id := N + 373;

   --  Other special names used in processing attributes and pragmas

   Name_Allow                          : constant Name_Id := N + 374;
   Name_Amount                         : constant Name_Id := N + 375;
   Name_As_Is                          : constant Name_Id := N + 376;
   Name_Attr_Long_Float                : constant Name_Id := N + 377;
   Name_Assertion                      : constant Name_Id := N + 378;
   Name_Assertions                     : constant Name_Id := N + 379;
   Name_Attribute_Name                 : constant Name_Id := N + 380;
   Name_Body_File_Name                 : constant Name_Id := N + 381;
   Name_Boolean_Entry_Barriers         : constant Name_Id := N + 382;
   Name_By_Any                         : constant Name_Id := N + 383;
   Name_By_Entry                       : constant Name_Id := N + 384;
   Name_By_Protected_Procedure         : constant Name_Id := N + 385;
   Name_Casing                         : constant Name_Id := N + 386;
   Name_Check_All                      : constant Name_Id := N + 387;
   Name_Code                           : constant Name_Id := N + 388;
   Name_Component                      : constant Name_Id := N + 389;
   Name_Component_Size_4               : constant Name_Id := N + 390;
   Name_Copy                           : constant Name_Id := N + 391;
   Name_D_Float                        : constant Name_Id := N + 392;
   Name_Decreases                      : constant Name_Id := N + 393;
   Name_Disable                        : constant Name_Id := N + 394;
   Name_Dot_Replacement                : constant Name_Id := N + 395;
   Name_Dynamic                        : constant Name_Id := N + 396;
   Name_Eliminated                     : constant Name_Id := N + 397;
   Name_Ensures                        : constant Name_Id := N + 398;
   Name_Entity                         : constant Name_Id := N + 399;
   Name_Entry_Count                    : constant Name_Id := N + 400;
   Name_External_Name                  : constant Name_Id := N + 401;
   Name_First_Optional_Parameter       : constant Name_Id := N + 402;
   Name_Force                          : constant Name_Id := N + 403;
   Name_Form                           : constant Name_Id := N + 404;
   Name_G_Float                        : constant Name_Id := N + 405;
   Name_Gcc                            : constant Name_Id := N + 406;
   Name_General                        : constant Name_Id := N + 407;
   Name_Gnat                           : constant Name_Id := N + 408;
   Name_Gnatprove                      : constant Name_Id := N + 409;
   Name_GPL                            : constant Name_Id := N + 410;
   Name_High_Order_First               : constant Name_Id := N + 411;
   Name_IEEE_Float                     : constant Name_Id := N + 412;
   Name_Ignore                         : constant Name_Id := N + 413;
   Name_In_Out                         : constant Name_Id := N + 414;
   Name_Increases                      : constant Name_Id := N + 415;
   Name_Info                           : constant Name_Id := N + 416;
   Name_Internal                       : constant Name_Id := N + 417;
   Name_Ivdep                          : constant Name_Id := N + 418;
   Name_Link_Name                      : constant Name_Id := N + 419;
   Name_Low_Order_First                : constant Name_Id := N + 420;
   Name_Lowercase                      : constant Name_Id := N + 421;
   Name_Max_Entry_Queue_Depth          : constant Name_Id := N + 422;
   Name_Max_Entry_Queue_Length         : constant Name_Id := N + 423;
   Name_Max_Size                       : constant Name_Id := N + 424;
   Name_Mechanism                      : constant Name_Id := N + 425;
   Name_Message                        : constant Name_Id := N + 426;
   Name_Minimized                      : constant Name_Id := N + 427;
   Name_Mixedcase                      : constant Name_Id := N + 428;
   Name_Mode                           : constant Name_Id := N + 429;
   Name_Modified_GPL                   : constant Name_Id := N + 430;
   Name_Name                           : constant Name_Id := N + 431;
   Name_NCA                            : constant Name_Id := N + 432;
   Name_No                             : constant Name_Id := N + 433;
   Name_No_Access_Parameter_Allocators : constant Name_Id := N + 434;
   Name_No_Coextensions                : constant Name_Id := N + 435;
   Name_No_Dependence                  : constant Name_Id := N + 436;
   Name_No_Dynamic_Attachment          : constant Name_Id := N + 437;
   Name_No_Dynamic_Interrupts          : constant Name_Id := N + 438;
   Name_No_Elaboration_Code            : constant Name_Id := N + 439;
   Name_No_Implementation_Extensions   : constant Name_Id := N + 440;
   Name_No_Obsolescent_Features        : constant Name_Id := N + 441;
   Name_No_Requeue                     : constant Name_Id := N + 442;
   Name_No_Requeue_Statements          : constant Name_Id := N + 443;
   Name_No_Specification_Of_Aspect     : constant Name_Id := N + 444;
   Name_No_Standard_Allocators_After_Elaboration : constant Name_Id := N + 445;
   Name_No_Task_Attributes             : constant Name_Id := N + 446;
   Name_No_Task_Attributes_Package     : constant Name_Id := N + 447;
   Name_No_Use_Of_Attribute            : constant Name_Id := N + 448;
   Name_No_Use_Of_Entity               : constant Name_Id := N + 449;
   Name_No_Use_Of_Pragma               : constant Name_Id := N + 450;
   Name_No_Unroll                      : constant Name_Id := N + 451;
   Name_No_Vector                      : constant Name_Id := N + 452;
   Name_Nominal                        : constant Name_Id := N + 453;
   Name_Non_Volatile                   : constant Name_Id := N + 454;
   Name_On                             : constant Name_Id := N + 455;
   Name_Optional                       : constant Name_Id := N + 456;
   Name_Policy                         : constant Name_Id := N + 457;
   Name_Parameter_Types                : constant Name_Id := N + 458;
   Name_Proof_In                       : constant Name_Id := N + 459;
   Name_Reason                         : constant Name_Id := N + 460;
   Name_Reference                      : constant Name_Id := N + 461;
   Name_Requires                       : constant Name_Id := N + 462;
   Name_Restricted                     : constant Name_Id := N + 463;
   Name_Result_Mechanism               : constant Name_Id := N + 464;
   Name_Result_Type                    : constant Name_Id := N + 465;
   Name_Robustness                     : constant Name_Id := N + 466;
   Name_Runtime                        : constant Name_Id := N + 467;
   Name_SB                             : constant Name_Id := N + 468;
   Name_Secondary_Stack_Size           : constant Name_Id := N + 469;
   Name_Section                        : constant Name_Id := N + 470;
   Name_Semaphore                      : constant Name_Id := N + 471;
   Name_Simple_Barriers                : constant Name_Id := N + 472;
   Name_SPARK                          : constant Name_Id := N + 473;
   Name_SPARK_05                       : constant Name_Id := N + 474;
   Name_Spec_File_Name                 : constant Name_Id := N + 475;
   Name_State                          : constant Name_Id := N + 476;
   Name_Statement_Assertions           : constant Name_Id := N + 477;
   Name_Static                         : constant Name_Id := N + 478;
   Name_Stack_Size                     : constant Name_Id := N + 479;
   Name_Strict                         : constant Name_Id := N + 480;
   Name_Subunit_File_Name              : constant Name_Id := N + 481;
   Name_Suppressed                     : constant Name_Id := N + 482;
   Name_Task_Stack_Size_Default        : constant Name_Id := N + 483;
   Name_Task_Type                      : constant Name_Id := N + 484;
   Name_Time_Slicing_Enabled           : constant Name_Id := N + 485;
   Name_Top_Guard                      : constant Name_Id := N + 486;
   Name_UBA                            : constant Name_Id := N + 487;
   Name_UBS                            : constant Name_Id := N + 488;
   Name_UBSB                           : constant Name_Id := N + 489;
   Name_Unit_Name                      : constant Name_Id := N + 490;
   Name_Unknown                        : constant Name_Id := N + 491;
   Name_Unrestricted                   : constant Name_Id := N + 492;
   Name_Unroll                         : constant Name_Id := N + 493;
   Name_Uppercase                      : constant Name_Id := N + 494;
   Name_User                           : constant Name_Id := N + 495;
   Name_Variant                        : constant Name_Id := N + 496;
   Name_VAX_Float                      : constant Name_Id := N + 497;
   Name_Vector                         : constant Name_Id := N + 498;
   Name_Vtable_Ptr                     : constant Name_Id := N + 499;
   Name_Warn                           : constant Name_Id := N + 500;
   Name_Working_Storage                : constant Name_Id := N + 501;

   --  Names of recognized attributes. The entries with the comment "Ada 83"
   --  are attributes that are defined in Ada 83, but not in Ada 95. These
   --  attributes are implemented in all Ada modes in GNAT.

   --  The entries marked GNAT are attributes that are defined by GNAT and
   --  implemented in all Ada modes. Full descriptions of these implementation
   --  dependent attributes may be found in the appropriate Sem_Attr section.

   First_Attribute_Name                : constant Name_Id := N + 502;
   Name_Abort_Signal                   : constant Name_Id := N + 502; -- GNAT
   Name_Access                         : constant Name_Id := N + 503;
   Name_Address                        : constant Name_Id := N + 504;
   Name_Address_Size                   : constant Name_Id := N + 505; -- GNAT
   Name_Aft                            : constant Name_Id := N + 506;
   Name_Alignment                      : constant Name_Id := N + 507;
   Name_Asm_Input                      : constant Name_Id := N + 508; -- GNAT
   Name_Asm_Output                     : constant Name_Id := N + 509; -- GNAT
   Name_Atomic_Always_Lock_Free        : constant Name_Id := N + 510; -- GNAT
   Name_Bit                            : constant Name_Id := N + 511; -- GNAT
   Name_Bit_Order                      : constant Name_Id := N + 512;
   Name_Bit_Position                   : constant Name_Id := N + 513; -- GNAT
   Name_Body_Version                   : constant Name_Id := N + 514;
   Name_Callable                       : constant Name_Id := N + 515;
   Name_Caller                         : constant Name_Id := N + 516;
   Name_Code_Address                   : constant Name_Id := N + 517; -- GNAT
   Name_Compiler_Version               : constant Name_Id := N + 518; -- GNAT
   Name_Component_Size                 : constant Name_Id := N + 519;
   Name_Compose                        : constant Name_Id := N + 520;
   Name_Constant_Indexing              : constant Name_Id := N + 521; -- GNAT
   Name_Constrained                    : constant Name_Id := N + 522;
   Name_Count                          : constant Name_Id := N + 523;
   Name_Default_Bit_Order              : constant Name_Id := N + 524; -- GNAT
   Name_Default_Scalar_Storage_Order   : constant Name_Id := N + 525; -- GNAT
   Name_Default_Iterator               : constant Name_Id := N + 526; -- GNAT
   Name_Definite                       : constant Name_Id := N + 527;
   Name_Delta                          : constant Name_Id := N + 528;
   Name_Denorm                         : constant Name_Id := N + 529;
   Name_Deref                          : constant Name_Id := N + 530; -- GNAT
   Name_Descriptor_Size                : constant Name_Id := N + 531;
   Name_Digits                         : constant Name_Id := N + 532;
   Name_Elaborated                     : constant Name_Id := N + 533; -- GNAT
   Name_Emax                           : constant Name_Id := N + 534; -- Ada 83
   Name_Enabled                        : constant Name_Id := N + 535; -- GNAT
   Name_Enum_Rep                       : constant Name_Id := N + 536; -- GNAT
   Name_Enum_Val                       : constant Name_Id := N + 537; -- GNAT
   Name_Epsilon                        : constant Name_Id := N + 538; -- Ada 83
   Name_Exponent                       : constant Name_Id := N + 539;
   Name_External_Tag                   : constant Name_Id := N + 540;
   Name_Fast_Math                      : constant Name_Id := N + 541; -- GNAT
   Name_First                          : constant Name_Id := N + 542;
   Name_First_Bit                      : constant Name_Id := N + 543;
   Name_First_Valid                    : constant Name_Id := N + 544; -- Ada 12
   Name_Fixed_Value                    : constant Name_Id := N + 545; -- GNAT
   Name_Fore                           : constant Name_Id := N + 546;
   Name_Has_Access_Values              : constant Name_Id := N + 547; -- GNAT
   Name_Has_Discriminants              : constant Name_Id := N + 548; -- GNAT
   Name_Has_Same_Storage               : constant Name_Id := N + 549; -- Ada 12
   Name_Has_Tagged_Values              : constant Name_Id := N + 550; -- GNAT
   Name_Identity                       : constant Name_Id := N + 551;
   Name_Img                            : constant Name_Id := N + 552; -- GNAT
   Name_Implicit_Dereference           : constant Name_Id := N + 553; -- GNAT
   Name_Integer_Value                  : constant Name_Id := N + 554; -- GNAT
   Name_Invalid_Value                  : constant Name_Id := N + 555; -- GNAT
   Name_Iterator_Element               : constant Name_Id := N + 556; -- GNAT
   Name_Iterable                       : constant Name_Id := N + 557; -- GNAT
   Name_Large                          : constant Name_Id := N + 558; -- Ada 83
   Name_Last                           : constant Name_Id := N + 559;
   Name_Last_Bit                       : constant Name_Id := N + 560;
   Name_Last_Valid                     : constant Name_Id := N + 561; -- Ada 12
   Name_Leading_Part                   : constant Name_Id := N + 562;
   Name_Length                         : constant Name_Id := N + 563;
   Name_Library_Level                  : constant Name_Id := N + 564; -- GNAT
   Name_Lock_Free                      : constant Name_Id := N + 565; -- GNAT
   Name_Loop_Entry                     : constant Name_Id := N + 566; -- GNAT
   Name_Machine_Emax                   : constant Name_Id := N + 567;
   Name_Machine_Emin                   : constant Name_Id := N + 568;
   Name_Machine_Mantissa               : constant Name_Id := N + 569;
   Name_Machine_Overflows              : constant Name_Id := N + 570;
   Name_Machine_Radix                  : constant Name_Id := N + 571;
   Name_Machine_Rounding               : constant Name_Id := N + 572; -- Ada 05
   Name_Machine_Rounds                 : constant Name_Id := N + 573;
   Name_Machine_Size                   : constant Name_Id := N + 574; -- GNAT
   Name_Mantissa                       : constant Name_Id := N + 575; -- Ada 83
   Name_Max_Alignment_For_Allocation   : constant Name_Id := N + 576; -- Ada 12
   Name_Max_Size_In_Storage_Elements   : constant Name_Id := N + 577;
   Name_Maximum_Alignment              : constant Name_Id := N + 578; -- GNAT
   Name_Mechanism_Code                 : constant Name_Id := N + 579; -- GNAT
   Name_Mod                            : constant Name_Id := N + 580; -- Ada 05
   Name_Model_Emin                     : constant Name_Id := N + 581;
   Name_Model_Epsilon                  : constant Name_Id := N + 582;
   Name_Model_Mantissa                 : constant Name_Id := N + 583;
   Name_Model_Small                    : constant Name_Id := N + 584;
   Name_Modulus                        : constant Name_Id := N + 585;
   Name_Null_Parameter                 : constant Name_Id := N + 586; -- GNAT
   Name_Object_Size                    : constant Name_Id := N + 587; -- GNAT
   Name_Old                            : constant Name_Id := N + 588; -- GNAT
   Name_Overlaps_Storage               : constant Name_Id := N + 589; -- GNAT
   Name_Partition_ID                   : constant Name_Id := N + 590;
   Name_Passed_By_Reference            : constant Name_Id := N + 591; -- GNAT
   Name_Pool_Address                   : constant Name_Id := N + 592; -- GNAT
   Name_Pos                            : constant Name_Id := N + 593;
   Name_Position                       : constant Name_Id := N + 594;
   Name_Priority                       : constant Name_Id := N + 595; -- Ada 05
   Name_Range                          : constant Name_Id := N + 596;
   Name_Range_Length                   : constant Name_Id := N + 597; -- GNAT
   Name_Ref                            : constant Name_Id := N + 598; -- GNAT
   Name_Restriction_Set                : constant Name_Id := N + 599; -- GNAT
   Name_Result                         : constant Name_Id := N + 600; -- GNAT
   Name_Round                          : constant Name_Id := N + 601;
   Name_Safe_Emax                      : constant Name_Id := N + 602; -- Ada 83
   Name_Safe_First                     : constant Name_Id := N + 603;
   Name_Safe_Large                     : constant Name_Id := N + 604; -- Ada 83
   Name_Safe_Last                      : constant Name_Id := N + 605;
   Name_Safe_Small                     : constant Name_Id := N + 606; -- Ada 83
   Name_Scalar_Storage_Order           : constant Name_Id := N + 607; -- GNAT
   Name_Scale                          : constant Name_Id := N + 608;
   Name_Scaling                        : constant Name_Id := N + 609;
   Name_Signed_Zeros                   : constant Name_Id := N + 610;
   Name_Size                           : constant Name_Id := N + 611;
   Name_Small                          : constant Name_Id := N + 612; -- Ada 83
   Name_Storage_Size                   : constant Name_Id := N + 613;
   Name_Storage_Unit                   : constant Name_Id := N + 614; -- GNAT
   Name_Stream_Size                    : constant Name_Id := N + 615; -- Ada 05
   Name_System_Allocator_Alignment     : constant Name_Id := N + 616; -- GNAT
   Name_Tag                            : constant Name_Id := N + 617;
   Name_Target_Name                    : constant Name_Id := N + 618; -- GNAT
   Name_Terminated                     : constant Name_Id := N + 619;
   Name_To_Address                     : constant Name_Id := N + 620; -- GNAT
   Name_Type_Class                     : constant Name_Id := N + 621; -- GNAT
   Name_Type_Key                       : constant Name_Id := N + 622; -- GNAT
   Name_UET_Address                    : constant Name_Id := N + 623; -- GNAT
   Name_Unbiased_Rounding              : constant Name_Id := N + 624;
   Name_Unchecked_Access               : constant Name_Id := N + 625;
   Name_Unconstrained_Array            : constant Name_Id := N + 626; -- GNAT
   Name_Universal_Literal_String       : constant Name_Id := N + 627; -- GNAT
   Name_Unrestricted_Access            : constant Name_Id := N + 628; -- GNAT
   Name_Update                         : constant Name_Id := N + 629; -- GNAT
   Name_VADS_Size                      : constant Name_Id := N + 630; -- GNAT
   Name_Val                            : constant Name_Id := N + 631;
   Name_Valid                          : constant Name_Id := N + 632;
   Name_Valid_Scalars                  : constant Name_Id := N + 633; -- GNAT
   Name_Value_Size                     : constant Name_Id := N + 634; -- GNAT
   Name_Variable_Indexing              : constant Name_Id := N + 635; -- GNAT
   Name_Version                        : constant Name_Id := N + 636;
   Name_Wchar_T_Size                   : constant Name_Id := N + 637; -- GNAT
   Name_Wide_Wide_Width                : constant Name_Id := N + 638; -- Ada 05
   Name_Wide_Width                     : constant Name_Id := N + 639;
   Name_Width                          : constant Name_Id := N + 640;
   Name_Word_Size                      : constant Name_Id := N + 641; -- GNAT

   --  Attributes that designate attributes returning renamable functions,
   --  i.e. functions that return other than a universal value and that
   --  have non-universal arguments.

   First_Renamable_Function_Attribute  : constant Name_Id := N + 642;
   Name_Adjacent                       : constant Name_Id := N + 642;
   Name_Ceiling                        : constant Name_Id := N + 643;
   Name_Copy_Sign                      : constant Name_Id := N + 644;
   Name_Floor                          : constant Name_Id := N + 645;
   Name_Fraction                       : constant Name_Id := N + 646;
   Name_From_Any                       : constant Name_Id := N + 647; -- GNAT
   Name_Image                          : constant Name_Id := N + 648;
   Name_Input                          : constant Name_Id := N + 649;
   Name_Machine                        : constant Name_Id := N + 650;
   Name_Max                            : constant Name_Id := N + 651;
   Name_Min                            : constant Name_Id := N + 652;
   Name_Model                          : constant Name_Id := N + 653;
   Name_Pred                           : constant Name_Id := N + 654;
   Name_Remainder                      : constant Name_Id := N + 655;
   Name_Rounding                       : constant Name_Id := N + 656;
   Name_Succ                           : constant Name_Id := N + 657;
   Name_To_Any                         : constant Name_Id := N + 658; -- GNAT
   Name_Truncation                     : constant Name_Id := N + 659;
   Name_TypeCode                       : constant Name_Id := N + 660; -- GNAT
   Name_Value                          : constant Name_Id := N + 661;
   Name_Wide_Image                     : constant Name_Id := N + 662;
   Name_Wide_Wide_Image                : constant Name_Id := N + 663;
   Name_Wide_Value                     : constant Name_Id := N + 664;
   Name_Wide_Wide_Value                : constant Name_Id := N + 665;
   Last_Renamable_Function_Attribute   : constant Name_Id := N + 665;

   --  Attributes that designate procedures

   First_Procedure_Attribute           : constant Name_Id := N + 666;
   Name_Output                         : constant Name_Id := N + 666;
   Name_Read                           : constant Name_Id := N + 667;
   Name_Write                          : constant Name_Id := N + 668;
   Last_Procedure_Attribute            : constant Name_Id := N + 668;

   --  Remaining attributes are ones that return entities

   --  Note that Elab_Subp_Body is not considered to be a valid attribute name
   --  unless we are operating in CodePeer mode.

   First_Entity_Attribute_Name         : constant Name_Id := N + 669;
   Name_Elab_Body                      : constant Name_Id := N + 669; -- GNAT
   Name_Elab_Spec                      : constant Name_Id := N + 670; -- GNAT
   Name_Elab_Subp_Body                 : constant Name_Id := N + 671; -- GNAT
   Name_Simple_Storage_Pool            : constant Name_Id := N + 672; -- GNAT
   Name_Storage_Pool                   : constant Name_Id := N + 673;

   --  These attributes are the ones that return types

   First_Type_Attribute_Name           : constant Name_Id := N + 674;
   Name_Base                           : constant Name_Id := N + 674;
   Name_Class                          : constant Name_Id := N + 675;
   Name_Stub_Type                      : constant Name_Id := N + 676; -- GNAT
   Last_Type_Attribute_Name            : constant Name_Id := N + 676;
   Last_Entity_Attribute_Name          : constant Name_Id := N + 676;
   Last_Attribute_Name                 : constant Name_Id := N + 676;

   --  Names of internal attributes. They are not real attributes but special
   --  names used internally by GNAT in order to deal with delayed aspects
   --  (Aspect_CPU, Aspect_Dispatching_Domain, Aspect_Interrupt_Priority) that
   --  don't have corresponding pragmas or user-referencable attributes.

   --  It is convenient to have these internal attributes available for
   --  processing the aspects, since the normal approach is to convert an
   --  aspect into its corresponding pragma or attribute specification.

   --  These attributes do have Attribute_Id values so that case statements
   --  on Attribute_Id include these cases, but they are NOT included in the
   --  Attribute_Name subtype defined above, which is typically used in the
   --  front end for checking syntax of submitted programs (where the use of
   --  internal attributes is not permitted).

   First_Internal_Attribute_Name       : constant Name_Id := N + 677;
   Name_CPU                            : constant Name_Id := N + 677;
   Name_Dispatching_Domain             : constant Name_Id := N + 678;
   Name_Interrupt_Priority             : constant Name_Id := N + 679;
   Last_Internal_Attribute_Name        : constant Name_Id := N + 679;

   --  Names of recognized locking policy identifiers

   First_Locking_Policy_Name           : constant Name_Id := N + 680;
   Name_Ceiling_Locking                : constant Name_Id := N + 680;
   Name_Inheritance_Locking            : constant Name_Id := N + 681;
   Name_Concurrent_Readers_Locking     : constant Name_Id := N + 682; -- GNAT
   Last_Locking_Policy_Name            : constant Name_Id := N + 682;

   --  Names of recognized queuing policy identifiers

   --  Note: policies are identified by the first character of the name (e.g. F
   --  for FIFO_Queuing). If new policy names are added, the first character
   --  must be distinct.

   First_Queuing_Policy_Name           : constant Name_Id := N + 683;
   Name_FIFO_Queuing                   : constant Name_Id := N + 683;
   Name_Priority_Queuing               : constant Name_Id := N + 684;
   Last_Queuing_Policy_Name            : constant Name_Id := N + 684;

   --  Names of recognized task dispatching policy identifiers

   --  Note: policies are identified by the first character of the name (e.g. F
   --  for FIFO_Within_Priorities). If new policy names are added, the first
   --  character must be distinct.

   First_Task_Dispatching_Policy_Name         : constant Name_Id := N + 685;
   Name_EDF_Across_Priorities                 : constant Name_Id := N + 685;
   Name_FIFO_Within_Priorities                : constant Name_Id := N + 686;
   Name_Non_Preemptive_FIFO_Within_Priorities : constant Name_Id := N + 687;
   Name_Round_Robin_Within_Priorities         : constant Name_Id := N + 688;
   Last_Task_Dispatching_Policy_Name          : constant Name_Id := N + 688;

   --  Names of recognized partition elaboration policy identifiers

   --  Note: policies are identified by the first character of the name (e.g. S
   --  for Sequential). If new policy names are added, the first character must
   --  be distinct.

   First_Partition_Elaboration_Policy_Name : constant Name_Id := N + 689;
   Name_Concurrent                         : constant Name_Id := N + 689;
   Name_Sequential                         : constant Name_Id := N + 690;
   Last_Partition_Elaboration_Policy_Name  : constant Name_Id := N + 690;

   --  Names of recognized checks for pragma Suppress

   --  Note: the name Atomic_Synchronization can only be specified internally
   --  as a result of using pragma Enable/Disable_Atomic_Synchronization.

   First_Check_Name                    : constant Name_Id := N + 691;
   Name_Access_Check                   : constant Name_Id := N + 691;
   Name_Accessibility_Check            : constant Name_Id := N + 692;
   Name_Alignment_Check                : constant Name_Id := N + 693; -- GNAT
   Name_Allocation_Check               : constant Name_Id := N + 694;
   Name_Atomic_Synchronization         : constant Name_Id := N + 695; -- GNAT
   Name_Discriminant_Check             : constant Name_Id := N + 696;
   Name_Division_Check                 : constant Name_Id := N + 697;
   Name_Duplicated_Tag_Check           : constant Name_Id := N + 698; -- GNAT
   Name_Elaboration_Check              : constant Name_Id := N + 699;
   Name_Index_Check                    : constant Name_Id := N + 700;
   Name_Length_Check                   : constant Name_Id := N + 701;
   Name_Overflow_Check                 : constant Name_Id := N + 702;
   Name_Predicate_Check                : constant Name_Id := N + 703; -- GNAT
   Name_Range_Check                    : constant Name_Id := N + 704;
   Name_Storage_Check                  : constant Name_Id := N + 705;
   Name_Tag_Check                      : constant Name_Id := N + 706;
   Name_Validity_Check                 : constant Name_Id := N + 707; -- GNAT
   Name_All_Checks                     : constant Name_Id := N + 708;
   Last_Check_Name                     : constant Name_Id := N + 708;

   --  Ada 83 reserved words, excluding those already declared in the attribute
   --  list (Access, Delta, Digits, Mod, Range).

   Name_Abort                            : constant Name_Id := N + 709;
   Name_Abs                              : constant Name_Id := N + 710;
   Name_Accept                           : constant Name_Id := N + 711;
   Name_And                              : constant Name_Id := N + 712;
   Name_All                              : constant Name_Id := N + 713;
   Name_Array                            : constant Name_Id := N + 714;
   Name_At                               : constant Name_Id := N + 715;
   Name_Begin                            : constant Name_Id := N + 716;
   Name_Body                             : constant Name_Id := N + 717;
   Name_Case                             : constant Name_Id := N + 718;
   Name_Constant                         : constant Name_Id := N + 719;
   Name_Declare                          : constant Name_Id := N + 720;
   Name_Delay                            : constant Name_Id := N + 721;
   Name_Do                               : constant Name_Id := N + 722;
   Name_Else                             : constant Name_Id := N + 723;
   Name_Elsif                            : constant Name_Id := N + 724;
   Name_End                              : constant Name_Id := N + 725;
   Name_Entry                            : constant Name_Id := N + 726;
   Name_Exception                        : constant Name_Id := N + 727;
   Name_Exit                             : constant Name_Id := N + 728;
   Name_For                              : constant Name_Id := N + 729;
   Name_Function                         : constant Name_Id := N + 730;
   Name_Generic                          : constant Name_Id := N + 731;
   Name_Goto                             : constant Name_Id := N + 732;
   Name_If                               : constant Name_Id := N + 733;
   Name_In                               : constant Name_Id := N + 734;
   Name_Is                               : constant Name_Id := N + 735;
   Name_Limited                          : constant Name_Id := N + 736;
   Name_Loop                             : constant Name_Id := N + 737;
   Name_New                              : constant Name_Id := N + 738;
   Name_Not                              : constant Name_Id := N + 739;
   Name_Null                             : constant Name_Id := N + 740;
   Name_Of                               : constant Name_Id := N + 741;
   Name_Or                               : constant Name_Id := N + 742;
   Name_Others                           : constant Name_Id := N + 743;
   Name_Out                              : constant Name_Id := N + 744;
   Name_Package                          : constant Name_Id := N + 745;
   Name_Pragma                           : constant Name_Id := N + 746;
   Name_Private                          : constant Name_Id := N + 747;
   Name_Procedure                        : constant Name_Id := N + 748;
   Name_Raise                            : constant Name_Id := N + 749;
   Name_Record                           : constant Name_Id := N + 750;
   Name_Rem                              : constant Name_Id := N + 751;
   Name_Renames                          : constant Name_Id := N + 752;
   Name_Return                           : constant Name_Id := N + 753;
   Name_Reverse                          : constant Name_Id := N + 754;
   Name_Select                           : constant Name_Id := N + 755;
   Name_Separate                         : constant Name_Id := N + 756;
   Name_Subtype                          : constant Name_Id := N + 757;
   Name_Task                             : constant Name_Id := N + 758;
   Name_Terminate                        : constant Name_Id := N + 759;
   Name_Then                             : constant Name_Id := N + 760;
   Name_Type                             : constant Name_Id := N + 761;
   Name_Use                              : constant Name_Id := N + 762;
   Name_When                             : constant Name_Id := N + 763;
   Name_While                            : constant Name_Id := N + 764;
   Name_With                             : constant Name_Id := N + 765;
   Name_Xor                              : constant Name_Id := N + 766;

   --  Names of intrinsic subprograms

   --  Note: Asm is missing from this list, since Asm is a legitimate
   --  convention name. So is To_Address, which is a GNAT attribute.

   First_Intrinsic_Name                  : constant Name_Id := N + 767;
   Name_Compilation_Date                 : constant Name_Id := N + 767;
   Name_Compilation_Time                 : constant Name_Id := N + 768;
   Name_Divide                           : constant Name_Id := N + 769;
   Name_Enclosing_Entity                 : constant Name_Id := N + 770;
   Name_Exception_Information            : constant Name_Id := N + 771;
   Name_Exception_Message                : constant Name_Id := N + 772;
   Name_Exception_Name                   : constant Name_Id := N + 773;
   Name_File                             : constant Name_Id := N + 774;
   Name_Generic_Dispatching_Constructor  : constant Name_Id := N + 775;
   Name_Import_Address                   : constant Name_Id := N + 776;
   Name_Import_Largest_Value             : constant Name_Id := N + 777;
   Name_Import_Value                     : constant Name_Id := N + 778;
   Name_Is_Negative                      : constant Name_Id := N + 779;
   Name_Line                             : constant Name_Id := N + 780;
   Name_Rotate_Left                      : constant Name_Id := N + 781;
   Name_Rotate_Right                     : constant Name_Id := N + 782;
   Name_Shift_Left                       : constant Name_Id := N + 783;
   Name_Shift_Right                      : constant Name_Id := N + 784;
   Name_Shift_Right_Arithmetic           : constant Name_Id := N + 785;
   Name_Source_Location                  : constant Name_Id := N + 786;
   Name_Unchecked_Conversion             : constant Name_Id := N + 787;
   Name_Unchecked_Deallocation           : constant Name_Id := N + 788;
   Name_To_Pointer                       : constant Name_Id := N + 789;
   Last_Intrinsic_Name                   : constant Name_Id := N + 789;

   --  Names used in processing intrinsic calls

   Name_Free                             : constant Name_Id := N + 790;

   --  Ada 95 reserved words

   First_95_Reserved_Word                : constant Name_Id := N + 791;
   Name_Abstract                         : constant Name_Id := N + 791;
   Name_Aliased                          : constant Name_Id := N + 792;
   Name_Protected                        : constant Name_Id := N + 793;
   Name_Until                            : constant Name_Id := N + 794;
   Name_Requeue                          : constant Name_Id := N + 795;
   Name_Tagged                           : constant Name_Id := N + 796;
   Last_95_Reserved_Word                 : constant Name_Id := N + 796;

   subtype Ada_95_Reserved_Words is
     Name_Id range First_95_Reserved_Word .. Last_95_Reserved_Word;

   --  Miscellaneous names used in semantic checking

   Name_Raise_Exception                  : constant Name_Id := N + 797;

   --  Additional reserved words and identifiers used in GNAT Project Files
   --  Note that Name_External is already previously declared.

   --  Names with a -- GB annotation are only used in gprbuild or gprclean

   Name_Active                             : constant Name_Id := N + 798;
   Name_Aggregate                          : constant Name_Id := N + 799;
   Name_Archive_Builder                    : constant Name_Id := N + 800;
   Name_Archive_Builder_Append_Option      : constant Name_Id := N + 801;
   Name_Archive_Indexer                    : constant Name_Id := N + 802;
   Name_Archive_Suffix                     : constant Name_Id := N + 803;
   Name_Artifacts                          : constant Name_Id := N + 804;
   Name_Artifacts_In_Exec_Dir              : constant Name_Id := N + 805; -- GB
   Name_Artifacts_In_Object_Dir            : constant Name_Id := N + 806; -- GB
   Name_Binder                             : constant Name_Id := N + 807;
   Name_Body_Suffix                        : constant Name_Id := N + 808;
   Name_Builder                            : constant Name_Id := N + 809;
   Name_Clean                              : constant Name_Id := N + 810;
   Name_Compiler                           : constant Name_Id := N + 811;
   Name_Compiler_Command                   : constant Name_Id := N + 812; -- GB
   Name_Config_Body_File_Name              : constant Name_Id := N + 813;
   Name_Config_Body_File_Name_Index        : constant Name_Id := N + 814;
   Name_Config_Body_File_Name_Pattern      : constant Name_Id := N + 815;
   Name_Config_File_Switches               : constant Name_Id := N + 816;
   Name_Config_File_Unique                 : constant Name_Id := N + 817;
   Name_Config_Spec_File_Name              : constant Name_Id := N + 818;
   Name_Config_Spec_File_Name_Index        : constant Name_Id := N + 819;
   Name_Config_Spec_File_Name_Pattern      : constant Name_Id := N + 820;
   Name_Configuration                      : constant Name_Id := N + 821;
   Name_Cross_Reference                    : constant Name_Id := N + 822;
   Name_Default_Language                   : constant Name_Id := N + 823;
   Name_Default_Switches                   : constant Name_Id := N + 824;
   Name_Dependency_Driver                  : constant Name_Id := N + 825;
   Name_Dependency_Kind                    : constant Name_Id := N + 826;
   Name_Dependency_Switches                : constant Name_Id := N + 827;
   Name_Driver                             : constant Name_Id := N + 828;
   Name_Excluded_Source_Dirs               : constant Name_Id := N + 829;
   Name_Excluded_Source_Files              : constant Name_Id := N + 830;
   Name_Excluded_Source_List_File          : constant Name_Id := N + 831;
   Name_Exec_Dir                           : constant Name_Id := N + 832;
   Name_Exec_Subdir                        : constant Name_Id := N + 833;
   Name_Excluded_Patterns                  : constant Name_Id := N + 834;
   Name_Executable                         : constant Name_Id := N + 835;
   Name_Executable_Suffix                  : constant Name_Id := N + 836;
   Name_Extends                            : constant Name_Id := N + 837;
   Name_External_As_List                   : constant Name_Id := N + 838;
   Name_Externally_Built                   : constant Name_Id := N + 839;
   Name_Finder                             : constant Name_Id := N + 840;
   Name_Global_Compilation_Switches        : constant Name_Id := N + 841;
   Name_Global_Configuration_Pragmas       : constant Name_Id := N + 842;
   Name_Global_Config_File                 : constant Name_Id := N + 843; -- GB
   Name_Gnatls                             : constant Name_Id := N + 844;
   Name_Gnatstub                           : constant Name_Id := N + 845;
   Name_Gnu                                : constant Name_Id := N + 846;
   Name_Ide                                : constant Name_Id := N + 847;
   Name_Ignore_Source_Sub_Dirs             : constant Name_Id := N + 848;
   Name_Implementation                     : constant Name_Id := N + 849;
   Name_Implementation_Exceptions          : constant Name_Id := N + 850;
   Name_Implementation_Suffix              : constant Name_Id := N + 851;
   Name_Included_Artifact_Patterns         : constant Name_Id := N + 852;
   Name_Included_Patterns                  : constant Name_Id := N + 853;
   Name_Include_Switches                   : constant Name_Id := N + 854;
   Name_Include_Path                       : constant Name_Id := N + 855;
   Name_Include_Path_File                  : constant Name_Id := N + 856;
   Name_Inherit_Source_Path                : constant Name_Id := N + 857;
   Name_Install                            : constant Name_Id := N + 858;
   Name_Install_Name                       : constant Name_Id := N + 859;
   Name_Languages                          : constant Name_Id := N + 860;
   Name_Language_Kind                      : constant Name_Id := N + 861;
   Name_Leading_Library_Options            : constant Name_Id := N + 862;
   Name_Leading_Required_Switches          : constant Name_Id := N + 863;
   Name_Leading_Switches                   : constant Name_Id := N + 864;
   Name_Lib_Subdir                         : constant Name_Id := N + 865;
   Name_Link_Lib_Subdir                    : constant Name_Id := N + 866;
   Name_Library                            : constant Name_Id := N + 867;
   Name_Library_Ali_Dir                    : constant Name_Id := N + 868;
   Name_Library_Auto_Init                  : constant Name_Id := N + 869;
   Name_Library_Auto_Init_Supported        : constant Name_Id := N + 870;
   Name_Library_Builder                    : constant Name_Id := N + 871;
   Name_Library_Dir                        : constant Name_Id := N + 872;
   Name_Library_GCC                        : constant Name_Id := N + 873;
   Name_Library_Install_Name_Option        : constant Name_Id := N + 874;
   Name_Library_Interface                  : constant Name_Id := N + 875;
   Name_Library_Kind                       : constant Name_Id := N + 876;
   Name_Library_Name                       : constant Name_Id := N + 877;
   Name_Library_Major_Minor_Id_Supported   : constant Name_Id := N + 878;
   Name_Library_Options                    : constant Name_Id := N + 879;
   Name_Library_Partial_Linker             : constant Name_Id := N + 880;
   Name_Library_Reference_Symbol_File      : constant Name_Id := N + 881;
   Name_Library_Rpath_Options              : constant Name_Id := N + 882; -- GB
   Name_Library_Standalone                 : constant Name_Id := N + 883;
   Name_Library_Encapsulated_Options       : constant Name_Id := N + 884; -- GB
   Name_Library_Encapsulated_Supported     : constant Name_Id := N + 885; -- GB
   Name_Library_Src_Dir                    : constant Name_Id := N + 886;
   Name_Library_Support                    : constant Name_Id := N + 887;
   Name_Library_Symbol_File                : constant Name_Id := N + 888;
   Name_Library_Symbol_Policy              : constant Name_Id := N + 889;
   Name_Library_Version                    : constant Name_Id := N + 890;
   Name_Library_Version_Switches           : constant Name_Id := N + 891;
   Name_Linker                             : constant Name_Id := N + 892;
   Name_Linker_Executable_Option           : constant Name_Id := N + 893;
   Name_Linker_Lib_Dir_Option              : constant Name_Id := N + 894;
   Name_Linker_Lib_Name_Option             : constant Name_Id := N + 895;
   Name_Local_Config_File                  : constant Name_Id := N + 896; -- GB
   Name_Local_Configuration_Pragmas        : constant Name_Id := N + 897;
   Name_Locally_Removed_Files              : constant Name_Id := N + 898;
   Name_Map_File_Option                    : constant Name_Id := N + 899;
   Name_Mapping_File_Switches              : constant Name_Id := N + 900;
   Name_Mapping_Spec_Suffix                : constant Name_Id := N + 901;
   Name_Mapping_Body_Suffix                : constant Name_Id := N + 902;
   Name_Max_Command_Line_Length            : constant Name_Id := N + 903;
   Name_Metrics                            : constant Name_Id := N + 904;
   Name_Multi_Unit_Object_Separator        : constant Name_Id := N + 905;
   Name_Multi_Unit_Switches                : constant Name_Id := N + 906;
   Name_Naming                             : constant Name_Id := N + 907;
   Name_None                               : constant Name_Id := N + 908;
   Name_Object_Artifact_Extensions         : constant Name_Id := N + 909;
   Name_Object_File_Suffix                 : constant Name_Id := N + 910;
   Name_Object_File_Switches               : constant Name_Id := N + 911;
   Name_Object_Generated                   : constant Name_Id := N + 912;
   Name_Object_List                        : constant Name_Id := N + 913;
   Name_Object_Path_Switches               : constant Name_Id := N + 914;
   Name_Objects_Linked                     : constant Name_Id := N + 915;
   Name_Objects_Path                       : constant Name_Id := N + 916;
   Name_Objects_Path_File                  : constant Name_Id := N + 917;
   Name_Object_Dir                         : constant Name_Id := N + 918;
   Name_Option_List                        : constant Name_Id := N + 919;
   Name_Path_Syntax                        : constant Name_Id := N + 920;
   Name_Pic_Option                         : constant Name_Id := N + 921;
   Name_Pretty_Printer                     : constant Name_Id := N + 922;
   Name_Prefix                             : constant Name_Id := N + 923;
   Name_Project                            : constant Name_Id := N + 924;
   Name_Project_Dir                        : constant Name_Id := N + 925;
   Name_Project_Files                      : constant Name_Id := N + 926;
   Name_Project_Path                       : constant Name_Id := N + 927;
   Name_Project_Subdir                     : constant Name_Id := N + 928;
   Name_Remote                             : constant Name_Id := N + 929;
   Name_Response_File_Format               : constant Name_Id := N + 930;
   Name_Response_File_Switches             : constant Name_Id := N + 931;
   Name_Root_Dir                           : constant Name_Id := N + 932;
   Name_Roots                              : constant Name_Id := N + 933; -- GB
   Name_Required_Switches                  : constant Name_Id := N + 934;
   Name_Run_Path_Option                    : constant Name_Id := N + 935;
   Name_Run_Path_Origin                    : constant Name_Id := N + 936;
   Name_Separate_Run_Path_Options          : constant Name_Id := N + 937;
   Name_Shared_Library_Minimum_Switches    : constant Name_Id := N + 938;
   Name_Shared_Library_Prefix              : constant Name_Id := N + 939;
   Name_Shared_Library_Suffix              : constant Name_Id := N + 940;
   Name_Separate_Suffix                    : constant Name_Id := N + 941;
   Name_Source_Artifact_Extensions         : constant Name_Id := N + 942;
   Name_Source_Dirs                        : constant Name_Id := N + 943;
   Name_Source_File_Switches               : constant Name_Id := N + 944;
   Name_Source_Files                       : constant Name_Id := N + 945;
   Name_Source_List_File                   : constant Name_Id := N + 946;
   Name_Sources_Subdir                     : constant Name_Id := N + 947;
   Name_Spec                               : constant Name_Id := N + 948;
   Name_Spec_Suffix                        : constant Name_Id := N + 949;
   Name_Specification                      : constant Name_Id := N + 950;
   Name_Specification_Exceptions           : constant Name_Id := N + 951;
   Name_Specification_Suffix               : constant Name_Id := N + 952;
   Name_Stack                              : constant Name_Id := N + 953;
   Name_Switches                           : constant Name_Id := N + 954;
   Name_Symbolic_Link_Supported            : constant Name_Id := N + 955;
   Name_Synchronize                        : constant Name_Id := N + 956;
   Name_Toolchain_Description              : constant Name_Id := N + 957;
   Name_Toolchain_Version                  : constant Name_Id := N + 958;
   Name_Trailing_Required_Switches         : constant Name_Id := N + 959;
   Name_Trailing_Switches                  : constant Name_Id := N + 960;
   Name_Runtime_Library_Dir                : constant Name_Id := N + 961;
   Name_Runtime_Source_Dir                 : constant Name_Id := N + 962;

   --  Other miscellaneous names used in front end

   Name_Unaligned_Valid                  : constant Name_Id := N + 963;

   --  Names used to implement iterators over predefined containers

   Name_Cursor                           : constant Name_Id := N + 964;
   Name_Element                          : constant Name_Id := N + 965;
   Name_Element_Type                     : constant Name_Id := N + 966;
   Name_Has_Element                      : constant Name_Id := N + 967;
   Name_No_Element                       : constant Name_Id := N + 968;
   Name_Forward_Iterator                 : constant Name_Id := N + 969;
   Name_Reversible_Iterator              : constant Name_Id := N + 970;
   Name_Previous                         : constant Name_Id := N + 971;

   --  Ada 2005 reserved words

   First_2005_Reserved_Word              : constant Name_Id := N + 972;
   Name_Interface                        : constant Name_Id := N + 972;
   Name_Overriding                       : constant Name_Id := N + 973;
   Name_Synchronized                     : constant Name_Id := N + 974;
   Last_2005_Reserved_Word               : constant Name_Id := N + 974;

   subtype Ada_2005_Reserved_Words is
     Name_Id range First_2005_Reserved_Word .. Last_2005_Reserved_Word;

   --  Ada 2012 reserved words

   First_2012_Reserved_Word              : constant Name_Id := N + 975;
   Name_Some                             : constant Name_Id := N + 975;
   Last_2012_Reserved_Word               : constant Name_Id := N + 975;

   subtype Ada_2012_Reserved_Words is
     Name_Id range First_2012_Reserved_Word .. Last_2012_Reserved_Word;

   --  Mark last defined name for consistency check in Snames body

   Last_Predefined_Name                  : constant Name_Id := N + 975;

   ---------------------------------------
   -- Subtypes Defining Name Categories --
   ---------------------------------------

   subtype Any_Operator_Name is Name_Id range
     First_Operator_Name .. Last_Operator_Name;

   subtype Configuration_Pragma_Names is Name_Id range
     First_Pragma_Name .. Last_Configuration_Pragma_Name;

   ------------------------------
   -- Attribute ID Definitions --
   ------------------------------

   type Attribute_Id is (
      Attribute_Abort_Signal,
      Attribute_Access,
      Attribute_Address,
      Attribute_Address_Size,
      Attribute_Aft,
      Attribute_Alignment,
      Attribute_Asm_Input,
      Attribute_Asm_Output,
      Attribute_Atomic_Always_Lock_Free,
      Attribute_Bit,
      Attribute_Bit_Order,
      Attribute_Bit_Position,
      Attribute_Body_Version,
      Attribute_Callable,
      Attribute_Caller,
      Attribute_Code_Address,
      Attribute_Compiler_Version,
      Attribute_Component_Size,
      Attribute_Compose,
      Attribute_Constant_Indexing,
      Attribute_Constrained,
      Attribute_Count,
      Attribute_Default_Bit_Order,
      Attribute_Default_Scalar_Storage_Order,
      Attribute_Default_Iterator,
      Attribute_Definite,
      Attribute_Delta,
      Attribute_Denorm,
      Attribute_Deref,
      Attribute_Descriptor_Size,
      Attribute_Digits,
      Attribute_Elaborated,
      Attribute_Emax,
      Attribute_Enabled,
      Attribute_Enum_Rep,
      Attribute_Enum_Val,
      Attribute_Epsilon,
      Attribute_Exponent,
      Attribute_External_Tag,
      Attribute_Fast_Math,
      Attribute_First,
      Attribute_First_Bit,
      Attribute_First_Valid,
      Attribute_Fixed_Value,
      Attribute_Fore,
      Attribute_Has_Access_Values,
      Attribute_Has_Discriminants,
      Attribute_Has_Same_Storage,
      Attribute_Has_Tagged_Values,
      Attribute_Identity,
      Attribute_Img,
      Attribute_Implicit_Dereference,
      Attribute_Integer_Value,
      Attribute_Invalid_Value,
      Attribute_Iterator_Element,
      Attribute_Iterable,
      Attribute_Large,
      Attribute_Last,
      Attribute_Last_Bit,
      Attribute_Last_Valid,
      Attribute_Leading_Part,
      Attribute_Length,
      Attribute_Library_Level,
      Attribute_Lock_Free,
      Attribute_Loop_Entry,
      Attribute_Machine_Emax,
      Attribute_Machine_Emin,
      Attribute_Machine_Mantissa,
      Attribute_Machine_Overflows,
      Attribute_Machine_Radix,
      Attribute_Machine_Rounding,
      Attribute_Machine_Rounds,
      Attribute_Machine_Size,
      Attribute_Mantissa,
      Attribute_Max_Alignment_For_Allocation,
      Attribute_Max_Size_In_Storage_Elements,
      Attribute_Maximum_Alignment,
      Attribute_Mechanism_Code,
      Attribute_Mod,
      Attribute_Model_Emin,
      Attribute_Model_Epsilon,
      Attribute_Model_Mantissa,
      Attribute_Model_Small,
      Attribute_Modulus,
      Attribute_Null_Parameter,
      Attribute_Object_Size,
      Attribute_Old,
      Attribute_Overlaps_Storage,
      Attribute_Partition_ID,
      Attribute_Passed_By_Reference,
      Attribute_Pool_Address,
      Attribute_Pos,
      Attribute_Position,
      Attribute_Priority,
      Attribute_Range,
      Attribute_Range_Length,
      Attribute_Ref,
      Attribute_Restriction_Set,
      Attribute_Result,
      Attribute_Round,
      Attribute_Safe_Emax,
      Attribute_Safe_First,
      Attribute_Safe_Large,
      Attribute_Safe_Last,
      Attribute_Safe_Small,
      Attribute_Scalar_Storage_Order,
      Attribute_Scale,
      Attribute_Scaling,
      Attribute_Signed_Zeros,
      Attribute_Size,
      Attribute_Small,
      Attribute_Storage_Size,
      Attribute_Storage_Unit,
      Attribute_Stream_Size,
      Attribute_System_Allocator_Alignment,
      Attribute_Tag,
      Attribute_Target_Name,
      Attribute_Terminated,
      Attribute_To_Address,
      Attribute_Type_Class,
      Attribute_Type_Key,
      Attribute_UET_Address,
      Attribute_Unbiased_Rounding,
      Attribute_Unchecked_Access,
      Attribute_Unconstrained_Array,
      Attribute_Universal_Literal_String,
      Attribute_Unrestricted_Access,
      Attribute_Update,
      Attribute_VADS_Size,
      Attribute_Val,
      Attribute_Valid,
      Attribute_Valid_Scalars,
      Attribute_Value_Size,
      Attribute_Variable_Indexing,
      Attribute_Version,
      Attribute_Wchar_T_Size,
      Attribute_Wide_Wide_Width,
      Attribute_Wide_Width,
      Attribute_Width,
      Attribute_Word_Size,

      --  Attributes designating renamable functions

      Attribute_Adjacent,
      Attribute_Ceiling,
      Attribute_Copy_Sign,
      Attribute_Floor,
      Attribute_Fraction,
      Attribute_From_Any,
      Attribute_Image,
      Attribute_Input,
      Attribute_Machine,
      Attribute_Max,
      Attribute_Min,
      Attribute_Model,
      Attribute_Pred,
      Attribute_Remainder,
      Attribute_Rounding,
      Attribute_Succ,
      Attribute_To_Any,
      Attribute_Truncation,
      Attribute_TypeCode,
      Attribute_Value,
      Attribute_Wide_Image,
      Attribute_Wide_Wide_Image,
      Attribute_Wide_Value,
      Attribute_Wide_Wide_Value,

      --  Attributes designating procedures

      Attribute_Output,
      Attribute_Read,
      Attribute_Write,

      --  Entity attributes (includes type attributes)

      Attribute_Elab_Body,
      Attribute_Elab_Spec,
      Attribute_Elab_Subp_Body,
      Attribute_Simple_Storage_Pool,
      Attribute_Storage_Pool,

      --  Type attributes

      Attribute_Base,
      Attribute_Class,
      Attribute_Stub_Type,

      --  The internal attributes are on their own, out of order, because of
      --  the special processing required to deal with the fact that their
      --  names are not attribute names.

      Attribute_CPU,
      Attribute_Dispatching_Domain,
      Attribute_Interrupt_Priority);

      subtype Internal_Attribute_Id is Attribute_Id range
        Attribute_CPU .. Attribute_Interrupt_Priority;

      type Attribute_Class_Array is array (Attribute_Id) of Boolean;
      --  Type used to build attribute classification flag arrays

   ------------------------------------
   -- Convention Name ID Definitions --
   ------------------------------------

   type Convention_Id is (

      --  The native-to-Ada (non-foreign) conventions come first. These include
      --  the ones defined in the RM, plus Stubbed.

      Convention_Ada,
      Convention_Intrinsic,
      Convention_Entry,
      Convention_Protected,
      Convention_Stubbed,

      --  The following conventions are equivalent to Ada for all purposes
      --  except controlling the way parameters are passed.

      Convention_Ada_Pass_By_Copy,
      Convention_Ada_Pass_By_Reference,

      --  The remaining conventions are foreign language conventions

      Convention_Assembler,  --  also Asm, Assembly
      Convention_C,          --  also Default, External
      Convention_CIL,
      Convention_COBOL,
      Convention_CPP,
      Convention_Fortran,
      Convention_Java,
      Convention_Stdcall);   --  also DLL, Win32

      --  Note: Convention C_Pass_By_Copy is allowed only for record types
      --  (where it is treated like C except that the appropriate flag is set
      --  in the record type). Recognizing this convention is specially handled
      --  in Sem_Prag.

   for Convention_Id'Size use 8;
   --  Plenty of space for expansion

   subtype Foreign_Convention is
     Convention_Id range Convention_Assembler .. Convention_Id'Last;

   -----------------------------------
   -- Locking Policy ID Definitions --
   -----------------------------------

   type Locking_Policy_Id is (
      Locking_Policy_Inheritance_Locking,
      Locking_Policy_Ceiling_Locking,
      Locking_Policy_Concurrent_Readers_Locking);

   ---------------------------
   -- Pragma ID Definitions --
   ---------------------------

   type Pragma_Id is (

      --  Configuration pragmas

      --  Note: This list is in the GNAT users guide, so be sure that if any
      --  additions or deletions are made to the following list, they are
      --  properly reflected in the users guide.

      Pragma_Ada_83,
      Pragma_Ada_95,
      Pragma_Ada_05,
      Pragma_Ada_2005,
      Pragma_Ada_12,
      Pragma_Ada_2012,
      Pragma_Allow_Integer_Address,
      Pragma_Annotate,
      Pragma_Assertion_Policy,
      Pragma_Assume,
      Pragma_Assume_No_Invalid_Values,
      Pragma_Attribute_Definition,
      Pragma_C_Pass_By_Copy,
      Pragma_Check_Float_Overflow,
      Pragma_Check_Name,
      Pragma_Check_Policy,
      Pragma_Compile_Time_Error,
      Pragma_Compile_Time_Warning,
      Pragma_Compiler_Unit,
      Pragma_Compiler_Unit_Warning,
      Pragma_Component_Alignment,
      Pragma_Convention_Identifier,
      Pragma_Debug_Policy,
      Pragma_Detect_Blocking,
      Pragma_Default_Storage_Pool,
      Pragma_Disable_Atomic_Synchronization,
      Pragma_Discard_Names,
      Pragma_Elaboration_Checks,
      Pragma_Eliminate,
      Pragma_Enable_Atomic_Synchronization,
      Pragma_Extend_System,
      Pragma_Extensions_Allowed,
      Pragma_External_Name_Casing,
      Pragma_Favor_Top_Level,
      Pragma_Implicit_Packing,
      Pragma_Initialize_Scalars,
      Pragma_Interrupt_State,
      Pragma_License,
      Pragma_Locking_Policy,
      Pragma_Loop_Optimize,
      Pragma_No_Run_Time,
      Pragma_No_Strict_Aliasing,
      Pragma_No_Tagged_Streams,
      Pragma_Normalize_Scalars,
      Pragma_Optimize_Alignment,
      Pragma_Overflow_Mode,
      Pragma_Overriding_Renamings,
      Pragma_Partition_Elaboration_Policy,
      Pragma_Persistent_BSS,
      Pragma_Polling,
      Pragma_Prefix_Exception_Messages,
      Pragma_Priority_Specific_Dispatching,
      Pragma_Profile,
      Pragma_Profile_Warnings,
      Pragma_Propagate_Exceptions,
      Pragma_Queuing_Policy,
      Pragma_Rational,
      Pragma_Ravenscar,
      Pragma_Restricted_Run_Time,
      Pragma_Restrictions,
      Pragma_Restriction_Warnings,
      Pragma_Reviewable,
      Pragma_Short_Circuit_And_Or,
      Pragma_Short_Descriptors,
      Pragma_Source_File_Name,
      Pragma_Source_File_Name_Project,
      Pragma_SPARK_Mode,
      Pragma_Style_Checks,
      Pragma_Suppress,
      Pragma_Suppress_Exception_Locations,
      Pragma_Task_Dispatching_Policy,
      Pragma_Unevaluated_Use_Of_Old,
      Pragma_Universal_Data,
      Pragma_Unsuppress,
      Pragma_Use_VADS_Size,
      Pragma_Validity_Checks,
      Pragma_Warning_As_Error,
      Pragma_Warnings,
      Pragma_Wide_Character_Encoding,

      --  Remaining (non-configuration) pragmas

      Pragma_Abort_Defer,
      Pragma_Abstract_State,
      Pragma_All_Calls_Remote,
      Pragma_Assert,
      Pragma_Assert_And_Cut,
      Pragma_Async_Readers,
      Pragma_Async_Writers,
      Pragma_Asynchronous,
      Pragma_Atomic,
      Pragma_Atomic_Components,
      Pragma_Attach_Handler,
      Pragma_Check,
      Pragma_CIL_Constructor,
      Pragma_Comment,
      Pragma_Common_Object,
      Pragma_Complete_Representation,
      Pragma_Complex_Representation,
      Pragma_Contract_Cases,
      Pragma_Controlled,
      Pragma_Convention,
      Pragma_CPP_Class,
      Pragma_CPP_Constructor,
      Pragma_CPP_Virtual,
      Pragma_CPP_Vtable,
      Pragma_Debug,
      Pragma_Default_Initial_Condition,
      Pragma_Depends,
      Pragma_Effective_Reads,
      Pragma_Effective_Writes,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Pragma_Elaborate_Body,
      Pragma_Export,
      Pragma_Export_Function,
      Pragma_Export_Object,
      Pragma_Export_Procedure,
      Pragma_Export_Value,
      Pragma_Export_Valued_Procedure,
      Pragma_Extensions_Visible,
      Pragma_External,
      Pragma_Finalize_Storage_Only,
      Pragma_Ghost,
      Pragma_Global,
      Pragma_Ident,
      Pragma_Implementation_Defined,
      Pragma_Implemented,
      Pragma_Import,
      Pragma_Import_Function,
      Pragma_Import_Object,
      Pragma_Import_Procedure,
      Pragma_Import_Valued_Procedure,
      Pragma_Independent,
      Pragma_Independent_Components,
      Pragma_Initial_Condition,
      Pragma_Initializes,
      Pragma_Inline,
      Pragma_Inline_Always,
      Pragma_Inline_Generic,
      Pragma_Inspection_Point,
      Pragma_Interface_Name,
      Pragma_Interrupt_Handler,
      Pragma_Invariant,
      Pragma_Java_Constructor,
      Pragma_Java_Interface,
      Pragma_Keep_Names,
      Pragma_Link_With,
      Pragma_Linker_Alias,
      Pragma_Linker_Constructor,
      Pragma_Linker_Destructor,
      Pragma_Linker_Options,
      Pragma_Linker_Section,
      Pragma_List,
      Pragma_Loop_Invariant,
      Pragma_Loop_Variant,
      Pragma_Machine_Attribute,
      Pragma_Main,
      Pragma_Main_Storage,
      Pragma_Memory_Size,
      Pragma_No_Body,
      Pragma_No_Elaboration_Code_All,
      Pragma_No_Inline,
      Pragma_No_Return,
      Pragma_Obsolescent,
      Pragma_Optimize,
      Pragma_Ordered,
      Pragma_Pack,
      Pragma_Page,
      Pragma_Part_Of,
      Pragma_Passive,
      Pragma_Post,
      Pragma_Postcondition,
      Pragma_Post_Class,
      Pragma_Pre,
      Pragma_Precondition,
      Pragma_Predicate,
      Pragma_Preelaborable_Initialization,
      Pragma_Preelaborate,
      Pragma_Pre_Class,
      Pragma_Provide_Shift_Operators,
      Pragma_Psect_Object,
      Pragma_Pure,
      Pragma_Pure_Function,
      Pragma_Refined_Depends,
      Pragma_Refined_Global,
      Pragma_Refined_Post,
      Pragma_Refined_State,
      Pragma_Relative_Deadline,
      Pragma_Remote_Access_Type,
      Pragma_Remote_Call_Interface,
      Pragma_Remote_Types,
      Pragma_Share_Generic,
      Pragma_Shared,
      Pragma_Shared_Passive,
      Pragma_Simple_Storage_Pool_Type,
      Pragma_Source_Reference,
      Pragma_Static_Elaboration_Desired,
      Pragma_Stream_Convert,
      Pragma_Subtitle,
      Pragma_Suppress_All,
      Pragma_Suppress_Debug_Info,
      Pragma_Suppress_Initialization,
      Pragma_System_Name,
      Pragma_Test_Case,
      Pragma_Task_Info,
      Pragma_Task_Name,
      Pragma_Task_Storage,
      Pragma_Thread_Local_Storage,
      Pragma_Time_Slice,
      Pragma_Title,
      Pragma_Type_Invariant,
      Pragma_Type_Invariant_Class,
      Pragma_Unchecked_Union,
      Pragma_Unimplemented_Unit,
      Pragma_Universal_Aliasing,
      Pragma_Unmodified,
      Pragma_Unreferenced,
      Pragma_Unreferenced_Objects,
      Pragma_Unreserve_All_Interrupts,
      Pragma_Volatile,
      Pragma_Volatile_Components,
      Pragma_Weak_External,

      --  The following pragmas are on their own, out of order, because of the
      --  special processing required to deal with the fact that their names
      --  match existing attribute names.

      Pragma_CPU,
      Pragma_Default_Scalar_Storage_Order,
      Pragma_Dispatching_Domain,
      Pragma_Fast_Math,
      Pragma_Interface,
      Pragma_Interrupt_Priority,
      Pragma_Lock_Free,
      Pragma_Priority,
      Pragma_Storage_Size,
      Pragma_Storage_Unit,

      --  The value to represent an unknown or unrecognized pragma

      Unknown_Pragma);

   -----------------------------------
   -- Queuing Policy ID definitions --
   -----------------------------------

   type Queuing_Policy_Id is (
      Queuing_Policy_FIFO_Queuing,
      Queuing_Policy_Priority_Queuing);

   --------------------------------------------
   -- Task Dispatching Policy ID definitions --
   --------------------------------------------

   type Task_Dispatching_Policy_Id is (
      Task_Dispatching_FIFO_Within_Priorities);
   --  Id values used to identify task dispatching policies

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Called to initialize the preset names in the names table

   function Is_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute. Note
   --  that Name_Elab_Subp_Body returns False if not operating in CodePeer
   --  mode. This is the mechanism for considering this pragma illegal in
   --  normal GNAT programs.

   function Is_Entity_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized entity attribute,
   --  i.e. an attribute reference that returns an entity.

   function Is_Internal_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of an INT attribute (Name_CPU,
   --  Name_Dispatching_Domain, Name_Interrupt_Priority).

   function Is_Procedure_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute that
   --  designates a procedure (and can therefore appear as a statement).

   function Is_Function_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized attribute
   --  that designates a renameable function, and can therefore appear in
   --  a renaming statement. Note that not all attributes designating
   --  functions are renamable, in particular, those returning a universal
   --  value cannot be renamed.

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized type attribute,
   --  i.e. an attribute reference that returns a type

   function Is_Convention_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of one of the recognized
   --  language conventions, as required by pragma Convention, Import,
   --  Export, Interface. Returns True if so. Also returns True for a
   --  name that has been specified by a Convention_Identifier pragma.
   --  If neither case holds, returns False.

   function Is_Keyword_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is one of the (reserved) keyword names. This
   --  includes all the keywords defined in the Ada standard (taking into
   --  effect the Ada version). It also includes additional keywords in
   --  contexts where additional keywords have been added. For example, in the
   --  context of parsing project files, keywords such as PROJECT are included.

   function Is_Locking_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized locking policy

   function Is_Partition_Elaboration_Policy_Name
     (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized partition
   --  elaboration policy.

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of an operator symbol

   function Is_Pragma_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized pragma. Note
   --  that pragmas CPU, Dispatching_Domain, Fast_Math, Interrupt_Priority,
   --  Lock_Free, Priority, Storage_Size, and Storage_Unit are recognized
   --  as pragmas by this function even though their names are separate from
   --  the other pragma names. For this reason, clients should always use
   --  this function, rather than do range tests on Name_Id values.

   function Is_Configuration_Pragma_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized configuration
   --  pragma. Note that pragma Fast_Math is recognized as a configuration
   --  pragma by this function even though its name is separate from other
   --  configuration pragma names. For this reason, clients should always
   --  use this function, rather than do range tests on Name_Id values.

   function Is_Queuing_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized queuing policy

   function Is_Task_Dispatching_Policy_Name (N : Name_Id) return Boolean;
   --  Test to see if the name N is the name of a recognized task
   --  dispatching policy.

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id;
   --  Returns Id of attribute corresponding to given name. It is an error to
   --  call this function with a name that is not the name of a attribute. Note
   --  that the function also works correctly for internal attribute names even
   --  though there are not included in the main list of attribute Names.

   function Get_Convention_Id (N : Name_Id) return Convention_Id;
   --  Returns Id of language convention corresponding to given name. It is
   --  an error to call this function with a name that is not the name of a
   --  convention, or one that has been previously recorded using a call to
   --  Record_Convention_Identifier.

   function Get_Convention_Name (C : Convention_Id) return Name_Id;
   --  Returns the name of language convention corresponding to given
   --  convention id.

   function Get_Locking_Policy_Id (N : Name_Id) return Locking_Policy_Id;
   --  Returns Id of locking policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id;
   --  Returns Id of pragma corresponding to given name. Returns Unknown_Pragma
   --  if N is not a name of a known (Ada defined or GNAT-specific) pragma.
   --  Note that the function also works correctly for names of pragmas that
   --  are not included in the main list of pragma Names (e.g. Name_CPU returns
   --  Pragma_CPU).

   function Get_Queuing_Policy_Id (N : Name_Id) return Queuing_Policy_Id;
   --  Returns Id of queuing policy corresponding to given name. It is an error
   --  to call this function with a name that is not the name of a check.

   function Get_Task_Dispatching_Policy_Id
     (N : Name_Id) return Task_Dispatching_Policy_Id;
   --  Returns Id of task dispatching policy corresponding to given name. It
   --  is an error to call this function with a name that is not the name of
   --  a defined check.

   procedure Record_Convention_Identifier
     (Id         : Name_Id;
      Convention : Convention_Id);
   --  A call to this procedure, resulting from an occurrence of a pragma
   --  Convention_Identifier, records that from now on an occurrence of Id
   --  will be recognized as a name for the specified convention.

private
   pragma Inline (Is_Attribute_Name);
   pragma Inline (Is_Entity_Attribute_Name);
   pragma Inline (Is_Type_Attribute_Name);
   pragma Inline (Is_Locking_Policy_Name);
   pragma Inline (Is_Partition_Elaboration_Policy_Name);
   pragma Inline (Is_Operator_Symbol_Name);
   pragma Inline (Is_Queuing_Policy_Name);
   pragma Inline (Is_Pragma_Name);
   pragma Inline (Is_Task_Dispatching_Policy_Name);

end Snames;
