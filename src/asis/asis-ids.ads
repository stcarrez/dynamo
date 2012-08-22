------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                             A S I S . I D S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2008, Free Software Foundation, Inc.       --
--                                                                          --
-- This   specification  is  derived   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  The copyright --
-- notice above, and the license provisions that follow apply solely to the --
--  contents of the part following the private keyword.                     --
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

------------------------------------------------------------------------------
--  21 package Asis.Ids
------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Ids is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Ids provides support for permanent unique Element "Identifiers" (Ids).
--  An Id is an efficient way for a tool to reference an ASIS element.  The Id
--  is permanent from session to session provided that the Ada compilation
--  environment is unchanged.
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  This package encapsulates a set of operations and queries that implement
--  the ASIS Id abstraction.  An Id is a way of identifying a particular
--  Element, from a particular Compilation_Unit, from a particular Context.
--  Ids can be written to files.  Ids can be read from files and converted into
--  an Element value with the use of a suitable open Context.
--
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  21.1  type Id
------------------------------------------------------------------------------
--  The Ada element Id abstraction (a private type).
--
--  The Id type is a distinct abstract type representing permanent "names"
--  that correspond to specific Element values.
--
--  These names can be written to files, retrieved at a later time, and
--  converted to Element values.
------------------------------------------------------------------------------
--  ASIS Ids are a means of identifying particular Element values obtained from
--  a particular physical compilation unit.  Ids are "relative names".  Each Id
--  value is valid (is usable, makes sense, can be interpreted) only in the
--  context of an appropriate open ASIS Context.
--
--  Id shall be an undiscriminated private type, or, shall be derived from an
--  undiscriminated private type.  It shall be declared as a new type or as a
--  subtype of an existing type.
------------------------------------------------------------------------------

   type Id is private;
   Nil_Id : constant Id;

   function "=" (Left : Id; Right : Id) return Boolean is abstract;

------------------------------------------------------------------------------
--  21.2  function Hash
------------------------------------------------------------------------------

   function Hash (The_Id : Id) return Asis.ASIS_Integer;

------------------------------------------------------------------------------
--  21.3  function "<"
------------------------------------------------------------------------------

   function "<" (Left : Id; Right : Id) return Boolean;

------------------------------------------------------------------------------
--  21.4  function ">"
------------------------------------------------------------------------------

   function ">" (Left : Id; Right : Id) return Boolean;

------------------------------------------------------------------------------
--  21.5  function Is_Nil
------------------------------------------------------------------------------

   function Is_Nil (Right : Id) return Boolean;

------------------------------------------------------------------------------
--  Right   - Specifies the Id to check
--
--  Returns True if the Id is the Nil_Id.
--
------------------------------------------------------------------------------
--  21.6  function Is_Equal
------------------------------------------------------------------------------

   function Is_Equal (Left : Id; Right : Id) return Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the left Id to compare
--  Right   - Specifies the right Id to compare
--
--  Returns True if Left and Right represent the same physical Id, from the
--  same physical compilation unit.  The two Ids convert
--  to Is_Identical Elements when converted with the same open ASIS Context.
--
------------------------------------------------------------------------------
--  21.7  function Create_Id
------------------------------------------------------------------------------

   function Create_Id (Element : Asis.Element) return Id;

------------------------------------------------------------------------------
--  Element - Specifies any Element value whose Id is desired
--
--  Returns a unique Id value corresponding to this Element, from the
--  corresponding Enclosing_Compilation_Unit and the corresponding
--  Enclosing_Context.  The Id value will not be equal ("=") to the Id value
--  for any other Element value unless the two Elements are Is_Identical.
--
--  Nil_Id is returned for a Nil_Element.
--
--  All Element_Kinds are appropriate.
--
------------------------------------------------------------------------------
--  21.8  function Create_Element
------------------------------------------------------------------------------

   function Create_Element
     (The_Id      : Id;
      The_Context : Asis.Context)
      return Asis.Element;

------------------------------------------------------------------------------
--  The_Id      - Specifies the Id to be converted to an Element
--  The_Context - Specifies the Context containing the Element with this Id
--
--  Returns the Element value corresponding to The_Id.  The_Id shall
--  correspond to an Element available from a Compilation_Unit contained by
--  (referencible through) The_Context.
--
--  Raises ASIS_Inappropriate_Element if the Element value is not available
--  though The_Context.  The Status is Value_Error and the Diagnosis
--  string will attempt to indicate the reason for the failure. (e.g., "Unit is
--  inconsistent", "No such unit", "Element is inconsistent (Unit
--  inconsistent)", etc.)
--

------------------------------------------------------------------------------
--  21.9  function Debug_Image
------------------------------------------------------------------------------

   function Debug_Image (The_Id : Id) return Wide_String;

------------------------------------------------------------------------------
--  The_Id  - Specifies an Id to convert
--
--  Returns a string value containing implementation-defined debug
--  information associated with the Id.
--
--  The return value uses Asis.Text.Delimiter_Image to separate the lines
--  of multi-line results.  The return value does not end with
--  Asis.Text.Delimiter_Image.
--
--  These values are intended for two purposes.  They are suitable for
--  inclusion in problem reports sent to the ASIS implementor.  They can
--  be presumed to contain information useful when debugging the
--  implementation itself. They are also suitable for use by the ASIS
--  application when printing simple application debugging messages during
--  application development.  They are intended to be, to some worthwhile
--  degree, intelligible to the user.
--
------------------------------------------------------------------------------
private

   --  The content of this private part is specific for the ASIS
   --  implementation for GNAT

   type Id is access String;
   Nil_Id : constant Id := null;

------------------------------------------------------------------------------

end Asis.Ids;
