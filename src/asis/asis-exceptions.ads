------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                       A S I S . E X C E P T I O N S                      --
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
--  5  package Asis.Exceptions
------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Exceptions is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  ASIS exceptions are:

   ASIS_Inappropriate_Context : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is passed a Context value that is not appropriate for the
--  operation.  This exception will typically indicate that a user error
--  has occurred within the application.
------------------------------------------------------------------------------

   ASIS_Inappropriate_Container : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is passed a Container value that is not appropriate for
--  the operation.  This exception will typically indicate that a user error
--  has occurred within the application.
------------------------------------------------------------------------------

   ASIS_Inappropriate_Compilation_Unit : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is passed a Compilation_Unit value that is not
--  appropriate.  This exception will typically indicate that a user
--  error has occurred within the application.
------------------------------------------------------------------------------

   ASIS_Inappropriate_Element : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is given an Element value that is not appropriate.  This
--  exception will typically indicate that a user error has occurred within
--  the application.
------------------------------------------------------------------------------

   ASIS_Inappropriate_Line : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is given a Line value that is not appropriate.
------------------------------------------------------------------------------

   ASIS_Inappropriate_Line_Number : exception;

------------------------------------------------------------------------------
--  Raised when ASIS is given a Line_Number value that is not appropriate.
--  This exception will typically indicate that a user error has occurred
--  within the application.
------------------------------------------------------------------------------

   ASIS_Failed : exception;

------------------------------------------------------------------------------
--  This is a catch-all exception that may be raised for different reasons
--  in different ASIS implementations.  All ASIS routines may raise ASIS_Failed
--  whenever they cannot normally complete their operation.  This exception
--  will typically indicate a failure of the underlying ASIS implementation.
------------------------------------------------------------------------------
end Asis.Exceptions;
