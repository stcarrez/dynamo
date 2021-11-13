-----------------------------------------------------------------------
--  gen -- Code Generator
--  Copyright (C) 2009, 2010, 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
