-----------------------------------------------------------------------
--  gen-model-xmi -- UML-XMI model
--  Copyright (C) 2012 Stephane Carrez
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;

with Util.Beans.Objects;

with Gen.Model.List;
with Gen.Model.Packages;
with Gen.Model.Mappings;
package Gen.Model.XMI is

   use Ada.Strings.Unbounded;

   type Model_Element;
   type Model_Element_Access is access all Model_Element'Class;

   package Model_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Model_Element_Access);

   subtype Model_Vector is Model_Vectors.Vector;

   --  ------------------------------
   --  Model Element
   --  ------------------------------
   type Model_Element is new Definition with record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      XMI_Id        : Ada.Strings.Unbounded.Unbounded_String;
      Tagged_Values : Model_Vector;
   end record;

   type Operation is new Model_Element with record
      Visibility : Natural;
   end record;

   type Attribute is new Model_Element with record
      Visibility : Natural;
   end record;

   type Tagged_Value is new Model_Element with record
      Value      : Ada.Strings.Unbounded.Unbounded_String;
      Value_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Tag_Definition is new Model_Element with record
      Multiplicity_Lower : Natural := 0;
      Multiplicity_Upper : Natural := 0;
   end record;

   type Class is new Model_Element with record
      Operations   : Model_Vector;
      Attributes   : Model_Vector;
      Associations : Model_Vector;
   end record;

   package Table_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                     Element_Type    => Model_Element_Access,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");

   subtype Table_Cursor is Table_Map.Cursor;

   --  Returns true if the table cursor contains a valid table
   function Has_Element (Position : Table_Cursor) return Boolean
                         renames Table_Map.Has_Element;

   --  Returns the table definition.
   function Element (Position : Table_Cursor) return Model_Element_Access
                     renames Table_Map.Element;

   --  Move the iterator to the next table definition.
   procedure Next (Position : in out Table_Cursor)
                   renames Table_Map.Next;

private


end Gen.Model.XMI;
