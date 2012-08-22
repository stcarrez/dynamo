------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                          A S I S . E R R O R S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 14416 $
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes that you have made are clearly indicated.                        --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--  4  package Asis.Errors
------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Errors is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
--  ASIS reports all operational errors by raising an exception.  Whenever an
--  ASIS implementation raises one of the exceptions declared in package
--  Asis.Exceptions, it will previously have set the values returned by the
--  Status and Diagnosis queries to indicate the cause of the error.  The
--  possible values for Status are indicated in the definition of Error_Kinds
--  below, with suggestions for the associated contents of the Diagnosis
--  string as a comment.
--
--  The Diagnosis and Status queries are provided in the Asis.Implementation
--  package to supply more information about the reasons for raising any
--  exception.
--
--  ASIS applications are encouraged to follow this same convention whenever
--  they explicitly raise any ASIS exception--always record a Status and
--  Diagnosis prior to raising the exception.
------------------------------------------------------------------------------
--  4.1   type Error_Kinds
------------------------------------------------------------------------------
--  This enumeration type describes the various kinds of errors.
--

   type Error_Kinds is (

      Not_An_Error,               -- No error is presently recorded
      Value_Error,                -- Routine argument value invalid
      Initialization_Error,       -- ASIS is uninitialized
      Environment_Error,          -- ASIS could not initialize
      Parameter_Error,            -- Bad Parameter given to Initialize
      Capacity_Error,             -- Implementation overloaded
      Name_Error,                 -- Context/unit not found
      Use_Error,                  -- Context/unit not use/open-able
      Data_Error,                 -- Context/unit bad/invalid/corrupt
      Text_Error,                 -- The program text cannot be located
      Storage_Error,              -- Storage_Error suppressed
      Obsolete_Reference_Error,   -- Argument or result is invalid due to
      --                             and inconsistent compilation unit
      Unhandled_Exception_Error,  -- Unexpected exception suppressed
      Not_Implemented_Error,      -- Functionality not implemented
      Internal_Error);            -- Implementation internal failure

end Asis.Errors;
