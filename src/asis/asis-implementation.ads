------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                  A S I S . I M P L E M E N T A T I O N                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (c) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes  that  you have made are clearly indicated. The copyright notice --
-- above,  and  the  license  provisions  that  follow  apply solely to the --
-- contents of the part following the private keyword.                      --
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

------------------------------------------------------------------------------
--  6  package Asis.Implementation
------------------------------------------------------------------------------
------------------------------------------------------------------------------
with Asis.Errors;
package Asis.Implementation is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Implementation provides queries to initialize, finalize, and query the
--  error status of the ASIS Implementation.
------------------------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--  6.1   function ASIS_Version
------------------------------------------------------------------------------

   function ASIS_Version return Wide_String;

------------------------------------------------------------------------------
--  6.2   function ASIS_Implementor
------------------------------------------------------------------------------

   function ASIS_Implementor return Wide_String;

------------------------------------------------------------------------------
--  6.3   function ASIS_Implementor_Version
------------------------------------------------------------------------------

   function ASIS_Implementor_Version return Wide_String;

------------------------------------------------------------------------------
--  6.4   function ASIS_Implementor_Information
------------------------------------------------------------------------------

   function ASIS_Implementor_Information return Wide_String;

------------------------------------------------------------------------------
--  Returns values which identify:
--
--   ASIS_Version                 - the version of the ASIS interface,
--                                  e.g., "2.1"
--   ASIS_Implementor             - the name of the implementor,
--                                  e.g., "Ada Inc."
--   ASIS_Implementor_Version     - the implementation's version,
--                                  e.g., "5.2a"
--   ASIS_Implementor_Information - implementation information,
--                                  e.g., "Copyright ..."
--
------------------------------------------------------------------------------
--  6.5   function Is_Initialized
------------------------------------------------------------------------------

   function Is_Initialized return Boolean;

------------------------------------------------------------------------------
--  Returns True if ASIS is currently initialized.
--
------------------------------------------------------------------------------
--  6.6   procedure Initialize
------------------------------------------------------------------------------

   procedure Initialize (Parameters : Wide_String := "");

------------------------------------------------------------------------------
--  Parameters  - Specifies implementation specific parameters.
--
--  Performs any necessary initialization activities.  This shall be invoked
--  at least once before any other ASIS services are used.  Parameter values
--  are implementation dependent.  The call is ignored if ASIS is already
--  initialized. All ASIS queries and services are ready for use once this
--  call completes.
--
--  Raises ASIS_Failed if ASIS failed to initialize or if the Parameters
--  argument is invalid.  Status is Environment_Error or Parameter_Error.
--
--  --|AN Application Note:
--  --|AN
--  --|AN The ASIS implementation may be Initialized and Finalized any number
--  --|AN of times during the operation of an ASIS program. However, all
--  --|AN existing Context, Compilation_Unit and Element values become invalid
--  --|AN when ASIS Is_Finalized. Subsequent calls to ASIS queries or services
--  --|AN using such invalid Compilation_Unit or Element values will cause
--  --|AN ASIS_Inappropriate_Context to be raised.
--
------------------------------------------------------------------------------
--  6.7   function Is_Finalized
------------------------------------------------------------------------------

   function Is_Finalized return Boolean;

------------------------------------------------------------------------------
--  Returns True if ASIS is currently finalized or if ASIS has never been
--  initialized.
--
------------------------------------------------------------------------------
--  6.8   procedure Finalize
------------------------------------------------------------------------------

   procedure Finalize (Parameters : Wide_String := "");

------------------------------------------------------------------------------
--  Parameters  - Specifies any implementation required parameter values.
--
--  Performs any necessary ASIS termination activities.  This should be invoked
--  once following the last use of other ASIS queries.  Parameter values are
--  implementation dependent. The call is ignored if ASIS is already finalized.
--  Subsequent calls to ASIS Environment, Compilation_Unit, and Element
--  queries, are erroneous while the environment Is_Finalized.
--
--  Raises ASIS_Failed if the ASIS implementation failed to finalize.  Status
--  is likely to be Internal_Error and will not be Not_An_Error.
--
-------------------------------------------------------------------------------
--  Whenever an error condition is detected, and any ASIS exception is raised,
--  an Asis.Errors.Error_Kinds value and a Diagnosis string is stored.  These
--  values can be retrieved by the Status and Diagnosis functions.  The
--  Diagnosis function will retrieve the diagnostic message describing the
--  error.
--
--  Error information always refers to the most recently recorded error.
--
--  Note that Diagnosis values are implementation dependent and may vary
--  greatly among ASIS implementations.
--
------------------------------------------------------------------------------
--  6.9   function Status
------------------------------------------------------------------------------

   function Status return Asis.Errors.Error_Kinds;

------------------------------------------------------------------------------
--  Returns the Error_Kinds value for the most recent error.
--
------------------------------------------------------------------------------
--  6.10  function Diagnosis
------------------------------------------------------------------------------

   function Diagnosis return Wide_String;

------------------------------------------------------------------------------
--  Returns a string value describing the most recent error.
--
--  Will typically return a null string if Status = Not_An_Error.
--
------------------------------------------------------------------------------
--  6.11  procedure Set_Status
------------------------------------------------------------------------------

   procedure Set_Status
     (Status    : Asis.Errors.Error_Kinds := Asis.Errors.Not_An_Error;
      Diagnosis : Wide_String             := "");

------------------------------------------------------------------------------
--  Status      - Specifies the new status to be recorded
--  Diagnosis   - Specifies the new diagnosis to be recorded
--
--  Sets (clears, if the defaults are used) the Status and Diagnosis
--  information.  Future calls to Status will return this Status (Not_An_Error)
--  and this Diagnosis (a null string).
--
--  Raises ASIS_Failed, with a Status of Internal_Error and a Diagnosis of
--  a null string, if the Status parameter is Not_An_Error and the Diagnosis
--  parameter is not a null string.
--
------------------------------------------------------------------------------
--
end Asis.Implementation;
