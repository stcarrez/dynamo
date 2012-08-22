------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                            A 4 G . V C H E C K                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT  is  now  maintained  by  AdaCore               --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

with Asis;           use Asis;
with Asis.Text;      use Asis.Text;
with Asis.Errors;    use Asis.Errors;

with A4G.A_Types;    use A4G.A_Types;
with A4G.Int_Knds;   use A4G.Int_Knds;

--  This package contains the routines for checking the validity of the
--  arguments of the ASIS queries and for generating the diagnostic info.
--  This package in its current state originated from the very beginning of
--  the ASIS project and it definitely needs revising.

package A4G.Vcheck is

--  GNAT-style reformatting required!! Only formal compatibility with -gnatg
--  is achieved by now.

--  This package contains procedures for checking the validity of the
--  Context, Compilation_Unit and Element values, raising the
--  Asis-defined exceptions (with previous setting of the status
--  value and forming the Diagnosis string) in some standard
--  situations and utility subprograms to be used in the keeping to
--  the "catch-all" policy defined in the subsection 3.6 of the
--  ASIS: Detailed Semantics and Implementation Document (v. 1.1.1)
--
--  The first prototype implementation uses rather roof approach
--  to keep to the "catch-all" policy in respect to the
--  non-Asis-defined exceptions and to handle the status and Diagnosis
--  information. The main principles of this approach are:
--
--  (a) all non-Asis exceptions are caught by
--
--        when others =>
--          Raise_ASIS_Failed (Diagnosis => "Name of the routine in which"
--                             &" this handler is" );
--
--       exception handler, and as a rule no attempt is undertaken
--       to recover from the error situation (Status is set as
--       "Value_Error" and ASIS_Failed is raised);
--
--  (b) Diagnosis string contains only general description of the failure
--      and the indication of the Asis query or low-level implementation
--      utility subprogram in which the Asis-defined exception was initially
--      raised;
--
--  (c) if Asis-defined exception propagates through the Asis implementation,
--      then the information related to the dynamic context of the exception
--      propagation is added to the Diagnosis (and separated by the
--      Asis_Types_And_Limits.ASIS_Line_Terminator constant).
--
--  (d) most of the routines in the Asis implementation should contain the
--      exception handler with the "others" choice (as described in the
--      paragraph (a) above. If it is known that the Asis_defined exception
--      could be raised in the procedure or function body, then the body
--      should contain two following exception handlers in the order given:
--
--      when Asis_ASIS_Inappropriate_Context    |
--          ASIS_Inappropriate_Compilation_Unit |
--          ASIS_Inappropriate_Element            =>
--
--          raise; -- nothing should be done additionally;
--                 -- exception has been raised in the argument(s)
--                 -- validity/appropriation checks in the same
--                 -- frame
--
--      when ASIS_Failed -- | other possible Asis-defined exceptions
--                           =>
--        Add_Call_Information (Outer_Call => "Name of the routine in which"
--                           &" this handler is" );
--        raise; ---????????
--
--      when others =>
--        Raise_ASIS_Failed (Diagnosis => "Name of the routine in which"
--                           &" this handler is" );
--

   procedure Add (Phrase : String);
   --  Adds Phrase to Diagnosis_Buffer and resets Diagnosis_Len. Exits with no
   --  (further) change in Diagnosis_Buffer as soon as Diagnosis_Len attains
   --  Max_Diagnosis_Length.

   ------------------------------------------------------------
   --  Checking the validity of Context, Compilation_Unit and
   --  Element
   ------------------------------------------------------------

------------------------------------------------------------
   procedure Check_Validity
     (Compilation_Unit : Asis.Compilation_Unit;
      Query            : String);
------------------------------------------------------------
--   Compilation_Unit - Specifies the unit to check
--   Query            - Specifies the name of query in which the check
--                      is performed. The parameter value is used to
--                      form the Diagnosis string
--
--  Performs the check if the unit does not belong to the closed library
--  Sets the corresponding Status and forms the corresponding Diagnosis
--  if the check fails
--
--  BETTER DOCS NEEDED!!
---------------------------------------------------------------
   procedure Check_Validity
     (Element : Asis.Element;
      Query   : String);
------------------------------------------------------------
--   Element - Specifies the element to check
--   Query   - Specifies the name of query in which the check is performed.
--             The parameter value is used to form the Diagnosis string
--
--  Performs the check if the element does not belong to the invalid
--  element
--  Sets the corresponding Status and forms the corresponding Diagnosis
--  if the check fails
--
--  BETTER DOCS NEEDED!!
---------------------------------------------------------------
   procedure Check_Validity (Line  : Asis.Text.Line;
                             Query : String);
------------------------------------------------------------
--   Line    - Specifies the Line to check
--   Query   - Specifies the name of query in which the check is performed.
--             The parameter value is used to form the Diagnosis string
--
--  Performs the check if the line does not belong to the invalid
--   Context
--  Sets the corresponding Status and forms the corresponding Diagnosis
--  if the check fails
--
--  BETTER DOCS NEEDED!!
------------------------------------------------------------
   procedure Check_Validity (Context : Asis.Context;
                             Query   : String);
------------------------------------------------------------
--   Context - Specifies the ASIS Context to check
--   Query   - Specifies the name of query in which the check is performed.
--             The parameter value is used to form the Diagnosis string
--
--  Performs the check if the Context is not in inassosiated or inopened
--  state
--  Sets the corresponding Status and forms the corresponding Diagnosis
--  if the check fails
--
--  BETTER DOCS NEEDED!!

   -------------------------------------
   -- Raising Asis-defined exceptions --
   -------------------------------------

   procedure Raise_ASIS_Failed
     (Diagnosis    : String;
      Argument     : Asis.Element            := Nil_Element;
      Stat         : Asis.Errors.Error_Kinds := Internal_Error;
      Bool_Par     : Boolean                 := False;
      Internal_Bug : Boolean := True);
   --  Raises ASIS_Failed with Stat as the value of ASIS Error Status.
   --  Usually expects the query name as Diagnosis. If the corresponding ASIS
   --  standard query has an optional boolean parameter, and if this parameter
   --  is set on for the given call to this query, then Bool_Par is expected to
   --  be set True. Internal_Bug specifies if the procedure is called for an
   --  internal implementation bug.
   --  This routine may be used to raise ASIS_Failed not only for the cases of
   --  internal implementation errors. In this case Internal_Bug should be set
   --  OFF, and Stat should specify the error kind.
   --  If Argument is not IS_Nil, adds the debug image of the argument to the
   --  diagnosis string.

   procedure Raise_ASIS_Failed_In_Traversing
     (Start_Element  : Asis.Element;
      Failure_At     : Asis.Element;
      Pre_Op         : Boolean;
      Exception_Info : String);
   --  Raises ASIS_Failed with Stat as Unhandled_Exception_Error for the
   --  situation  when some non-ASIS exception is raised in actual Pre-
   --  (Pre-Op is set ON) or Post-Operation (Pre_Op is set False). If forms the
   --  diagnostic message indicating the starting Element of the traversal
   --  (should be provided as the actual for Start_Element), the Element for
   --  which the failure took place (actual for Failure_At) and the information
   --  about the exception raised (passed as Exception_Info)

--------------------------------------------------------------------
   procedure Raise_ASIS_Inappropriate_Compilation_Unit
                         (Diagnosis : String);
--------------------------------------------------------------------
--  Diagnosis - Specifies the query to which the Compilation Unit
--              with inappropriate kind was passed
--
--  Raises ASIS_Inappropriate_Compilation_Unit with Value_Error status
--
--  BETTER DOCS NEEDED!!
--------------------------------------------------------------------
   procedure Raise_ASIS_Inappropriate_Element
     (Diagnosis  : String;
      Wrong_Kind : Internal_Element_Kinds;
      Status     : Error_Kinds      := Value_Error);
--------------------------------------------------------------------
--  Diagnosis - Specifies the query to which the Element with
--              inappropriate kind was passed
--
--  Raises ASIS_Inappropriate_Element with Status error status
--
--  BETTER DOCS NEEDED!!
--
--------------------------------------------------------------------
   procedure Raise_ASIS_Inappropriate_Line_Number
     (Diagnosis : String;
      Status    : Error_Kinds      := Value_Error);
--------------------------------------------------------------------
   procedure Not_Implemented_Yet (Diagnosis : String);
--------------------------------------------------------------------
--  Diagnosis - Specifies the query which has not been implemented
--              properly yet and has a placeholder as its body
--
--  Raises ASIS_Failed with Not_Implemented_Error status
--
--  This procedure is used in the placeholder bodies of the non-implemented
--  queries
--
--  BETTER DOCS NEEDED!!
--------------------------------------------------------------------

   ----------------------------------------------
   --  Setting the Status and Diagnosis values --
   ----------------------------------------------

--------------------------------------------------------------------
   procedure Set_Error_Status
     (Status    : Error_Kinds      := Not_An_Error;
      Diagnosis : String := Nil_Asis_String);
--------------------------------------------------------------------
--
--  This procedure is the full analog of the A4G.Set_Status
--  procedure, it is intended to be used as the implementation of the
--  A4G.Set_Status (by means of the renaming_as_a_body
--  in the body of Asis_Environment)
--------------------------------------------------------------------

   -----------------------------
   -- Adding Call Information --
   -----------------------------

   procedure Add_Call_Information
     (Outer_Call : String;
      Argument   : Asis.Element := Nil_Element;
      Bool_Par   : Boolean := False);
   --  Adds in the ASIS diagnosis the information about dynamically enclosing
   --  calls when an ASIS exception is propagated from some dynamically
   --  enclosed routine. If Argument is not Nil, adds its debug image in the
   --  Diagnosis, if needed (that is, if Argument_Postponed flag is set on,
   --  and resets this flag OFF after that)

   pragma No_Return (Raise_ASIS_Failed);
   pragma No_Return (Raise_ASIS_Inappropriate_Compilation_Unit);
   pragma No_Return (Raise_ASIS_Inappropriate_Element);
   pragma No_Return (Not_Implemented_Yet);

   -------------------------------------------
   -- The revised code is below this header --
   -------------------------------------------

   ---------------------------------------------------------------------
   -- Data Objects for Handling the Diagnosis string and Error Status --
   ---------------------------------------------------------------------

   Status_Indicator : Error_Kinds := Not_An_Error;

   Diagnosis_Buffer : String (1 .. Max_Diagnosis_Length);
   Diagnosis_Len    : Natural range 0 .. Max_Diagnosis_Length := 0;
   --  The string buffer to form ASIS Diagnosis

   ---------------------------
   --  ASIS Error handling  --
   ---------------------------

   --  The following documentation item should be revised when the revising of
   --  the ASIS exception handling is completed (BB14-010)

   --  According to the ASIS Standard, only ASIS-defined exceptions are allowed
   --  to be raised by ASIS queries.
   --
   --  The semantics of ASIS_Inappropriate_*** expectations well-defined and is
   --  supposed to be implemented in full conformance with ASIS Standard
   --  requirements.
   --
   --  According to  Asis.Exceptions, ASIS_Failed is "is a catch-all exception
   --  that may be raised for different reasons in different ASIS
   --  implementations".
   --
   --  We are using the following approach to handing and reporting the
   --  internal implementation errors and to raising ASIS_Failed.
   --
   --  1. According to the (rather ill-defined) requirement of the ASIS
   --     Standard and other documents generated during the ASIS
   --     standardization process (in particular, "ASIS: Detailed Semantics and
   --     Implementation. Ada Semantic Interface Specification. ASIS version
   --     1.1.1. April 26, 1994 by Gary E. Barnes), the body of every ASIS
   --     query contains 'when OTHERS' exception handler which does not allow
   --     any non-ASIS exception to propagate out of any ASIS query. (The
   --     exception is a set of rather trivial bodies of ASIS queries for which
   --     raising of non-ASIS exceptions is practically impossible).
   --
   --  2. By default any unexpected exception raise (that is, raising of any
   --     non-ASIS exception because of any reason) is treated as ASIS
   --     implementation error which should be immediately reported and which
   --     does not allow to continue the execution of any ASIS application.
   --     That is, detection of such an error should result in immediate exit
   --     to OS. This is implemented as the default behavior of all the ASIS
   --     and ASIS extensions queries.
   --
   --  3. An application may want to continue its execution even in case when
   --     an internal ASIS implementation error is detected. (The reason could
   --     be to try to use some workarounds for known ASIS implementation
   --     problems and to get the required information by some other ways).
   --     For this the application may set the Keep_Going flag ON by setting
   --     '-k' parameter of Asis.Implementation.Initialize. As a result,
   --     instead of generating the exit to OS, ASIS_Failed is raised with
   --     Unhandled_Exception_Error status.
   --
   --  4. We have some other reasons for raising ASIS_Failed. It can be raised
   --     in case when ASIS is initialized with "treat ASIS warnings as errors"
   --     mode as a part of generating of the ASIS warning. We have to use
   --     ASIS_Failed here, because the ASIS Standard does not give us any
   --     other choice. ASIS_Failed may also be raised by queries from
   --     Asis.Compilation_Units which are not supposed to be used for a
   --     dynamic Context. The important thing is that when ASIS_Failed is
   --     raised as a result of ASIS warning, the Error Status can never be
   --     Unhandled_Exception_Error
   --
   --  5. The general approach to exception handling in our ASIS implementation
   --     is:
   --
   --     - For every standard ASIS query and for every ASIS Extensions query
   --       which is not trivial enough to conclude that raising of any
   --       non-ASIS exception is practically impossible, the body of this
   --       query should contain "when EX : OTHERS' handler, and the only
   --       statement in this handler should be the call to Report_ASIS_Bug
   --       procedure (see the documentation of this procedure below)
   --
   --     - For every standard ASIS query and for every ASIS Extensions query
   --       which is not trivial enough to conclude that generating of the ASIS
   --       failure is practically impossible or for which we can not be sure
   --       that its implementation does not use any other ASIS queries, the
   --       body of this query should contain the handler for ASIS_Failed with
   --       the following code
   --
   --          when ASIS_Failed =>
   --             is Status_Indicator = Unhandled_Exception_Error then
   --                Add_Call_Information (...)
   --             end if;
   --
   --             raise;
   --
   --     The idea in behind is: if ASIS_Failed is the result of the internal
   --     ASIS implementation bug (that is, unexpected exception suppressed),
   --     we are collecting the information of enclosing calls, this may be
   --     useful if the call to one ASIS query is used in the implementation
   --     of some other ASIS query, the full "trace" of calls of the ASIS
   --     queries may be useful for an ASIS application programmer if (s)he
   --     would like to provide protections for ASIS bugs and to use some
   --     workarounds. And if ASIS_Failed is a result of ASIS warning, it
   --     should just propagated out of ASIS with the Diagnosis string formed
   --     as a part of error generation. ASIS_Failed raised when calling
   --     Asis.Compilation_Units queries which can not be used for a dynamic
   --     context could never be raised inside the call to an ASIS query
   --     enclosed into a call to some other ASIS query.

   procedure Report_ASIS_Bug
     (Query_Name    : String;
      Ex            : Exception_Occurrence;
      Arg_Element   : Asis.Element          := Nil_Element;
      Arg_Element_2 : Asis.Element          := Nil_Element;
      Arg_CU        : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_CU_2      : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Arg_Line      : Asis.Text.Line        := Nil_Line;
      Arg_Span      : Asis.Text.Span        := Nil_Span;
      Bool_Par_ON   : Boolean               := False;
      Context_Par   : Boolean               := False
      --  What else???
      );
   pragma No_Return (Report_ASIS_Bug);
   --  This procedure is supposed to be called in "when OTHERS" exception
   --  handlers of an ASIS queries only. If we are in such an exception
   --  handler, then for sure we have detected some ASIS implementation bug.
   --  Depending on the ASIS initialization options, this procedure may
   --  perform the following options:
   --
   --  - generates the ASIS bug box similar to the GNAT bug box generated by
   --    the GNAT Comperr.Compiler_Abort procedure.
   --
   --  - raises ASIS_Failed with the summary of the information which should
   --    go into the ASIS bug box as the ASIS Diagnosis string
   --
   --  - causes the exit to OS.
   --
   --  Either exit to OS or raising ASIS_Failed should take place in any case.
   --  In case of exit to OS, the ASIS bug box should be generated.
   --
   --  The call to this procedure should never result in exception raise (other
   --  then raising ASIS_Failed by purpose), the worst case is that the
   --  diagnostic information formed by this procedure is incomplete.
   --
   --  Query_Name should be set to the full expanded Ada name of the query
   --  where this procedure is called
   --
   --  Ex should be the exception name form enclosing "when Ex : others"
   --  handlers.
   --
   --  All the other parameters are used to compose some debug information
   --  about the parameters of the call which fails. Some ASIS queries have
   --  two Element or Compilation Unit parameters, that's why we need
   --  Arg_Element_2 and Arg_CU_2. For queries having only one Element or CU
   --  parameter, Arg_Element or Arg_CU should be used to pass the argument of
   --  the call to form the diagnostic info.

end A4G.Vcheck;
