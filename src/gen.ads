-----------------------------------------------------------------------
--  gen -- Code Generator
--  Copyright (C) 2009, 2010, 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
package Gen is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function To_UString (Value : in String) return UString
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function To_String (Value : in UString) return String
     renames Ada.Strings.Unbounded.To_String;

   function Length (Value : in UString) return Natural
     renames Ada.Strings.Unbounded.Length;

   function "=" (Left, Right : in UString) return Boolean
     renames Ada.Strings.Unbounded."=";
   function "=" (Left : in UString; Right : in String) return Boolean
     renames Ada.Strings.Unbounded."=";

end Gen;
