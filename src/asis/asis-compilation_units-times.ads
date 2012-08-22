------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--          A S I S . C O M P I L A T I O N _ U N I T S . T I M E S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This   specification  is  adapted   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  In accordance --
-- with the copyright of that document, you can freely copy and modify this --
-- specification, provided that if you redistribute a modified version, any --
-- changes that you have made are clearly indicated.                        --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--  11 package Asis.Compilation_Units.Times
------------------------------------------------------------------------------
------------------------------------------------------------------------------
with Ada.Calendar;
package Asis.Compilation_Units.Times is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Compilation_Units.Times encapsulates the time related functions used
--  within ASIS.
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  11.1  type Time
------------------------------------------------------------------------------
--  ASIS uses the predefined Ada.Calendar.Time.
--  ASIS uses the predefined Standard.Duration.
--  The constant Nil_ASIS_Time is defined to support time queries where a
--  time is unavailable/unknown.
------------------------------------------------------------------------------

   Nil_ASIS_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Year    => 1901,
      Month   => 1,
      Day     => 1,
      Seconds => 0.0);

------------------------------------------------------------------------------
--  11.2  function Time_Of_Last_Update
------------------------------------------------------------------------------

   function Time_Of_Last_Update
     (Compilation_Unit : Asis.Compilation_Unit)
      return Ada.Calendar.Time;

------------------------------------------------------------------------------
--  Compilation_Unit    - Specifies the unit to query
--
--  Returns the time that this physical compilation unit was most recently
--  updated in its implementor's Ada Environment.  This will often be the
--  time of its last compilation.  The exact significance of the result is
--  implementation specific.
--  Returns Nil_ASIS_Time if the unit has a Nil or nonexistent unit kind, or if
--  the time of last update is not available, or not meaningful, for any
--  reason.
--
--  All Unit Kinds are appropriate.
--
------------------------------------------------------------------------------
--  11.3  function Compilation_CPU_Duration
------------------------------------------------------------------------------

   function Compilation_CPU_Duration
     (Compilation_Unit : Asis.Compilation_Unit)
      return Standard.Duration;

------------------------------------------------------------------------------
--  Compilation_Unit  - Specifies the unit to query
--
--  Returns the Central Processing Unit (CPU) duration used to compile the
--  physical compilation unit associated with the Compilation_Unit argument.
--  The exact significance, or accuracy, of the result is implementation
--  specific.  Returns a duration of 0.0 if the unit has a Nil or nonexistent
--  unit kind, or if the CPU duration for the last compilation is not available
--  for any reason. Returns a duration of 86_400.0 if the CPU duration for the
--  last compilation is greater than 1 day.
--
--  All Unit Kinds are appropriate.
--

------------------------------------------------------------------------------
--  11.4  function Attribute_Time
------------------------------------------------------------------------------

   function Attribute_Time
     (Compilation_Unit : Asis.Compilation_Unit;
      Attribute        : Wide_String)
      return Ada.Calendar.Time;

------------------------------------------------------------------------------
--  Compilation_Unit    - Specifies the unit to query
--  Attribute           - Specifies the name of the attribute to query
--
--  Returns the Time value associated with the given attribute.  Returns
--  Nil_ASIS_Time if the argument is a Nil_Compilation_Unit, the unit does
--  not have the given Attribute, or the implementation does not record times
--  for attributes.
--
--  All Unit Kinds are appropriate.
--
--  Results of this query may vary across ASIS implementations.
--
------------------------------------------------------------------------------

end Asis.Compilation_Units.Times;
