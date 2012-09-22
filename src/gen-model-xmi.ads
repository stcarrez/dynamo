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

   type Element_Type is (XMI_STEREOTYPE,
                         XMI_PACKAGE,
                         XMI_CLASS,
                         XMI_ATTRIBUTE,
                         XMI_OPERATION,
                         XMI_TAG_VALUE,
                         XMI_TAG_DEFINITION,
                         XMI_COMMENT);

   --  ------------------------------
   --  Model Element
   --  ------------------------------
   type Model_Element is new Definition with record
      --  Element name.
      Name          : Ada.Strings.Unbounded.Unbounded_String;

      --  Element XMI id.
      XMI_Id        : Ada.Strings.Unbounded.Unbounded_String;

      --  List of tagged values for the element.
      Tagged_Values : Model_Vector;

      --  Elements contained.
      Elements      : Model_Vector;
   end record;

   --  Get the element type.
   function Get_Type (Node : in Model_Element) return Element_Type;

   --  ------------------------------
   --  Data type
   --  ------------------------------
   type Data_Type_Element is new Model_Element with null record;
   type Data_Type_Element_Access is access all Data_Type_Element'Class;

   --  Get the element type.
   overriding
   function Get_Type (Node : in Data_Type_Element) return Element_Type;

   --  ------------------------------
   --  Enum
   --  ------------------------------
   type Enum_Element is new Model_Element with null record;
   type Enum_Element_Access is access all Enum_Element'Class;

   --  Get the element type.
   overriding
   function Get_Type (Node : in Enum_Element) return Element_Type;

   procedure Add_Literal (Node : in out Enum_Element;
                          Id   : in Util.Beans.Objects.Object;
                          Name : in Util.Beans.Objects.Object);

   --  ------------------------------
   --  Literal
   --  ------------------------------
   --  The literal describes a possible value for an enum.
   type Literal_Element is new Model_Element with null record;
   type Literal_Element_Access is access all Literal_Element'Class;

   --  Get the element type.
   overriding
   function Get_Type (Node : in Literal_Element) return Element_Type;

   type Operation_Element is new Model_Element with record
      Visibility : Natural;
   end record;
   type Operation_Element_Access is access all Operation_Element'Class;

   type Attribute_Element is new Model_Element with record
      Visibility : Natural;
   end record;
   type Attribute_Element_Access is access all Attribute_Element'Class;

   type Tagged_Value is new Model_Element with record
      Value      : Ada.Strings.Unbounded.Unbounded_String;
      Value_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Tagged_Value_Access is access all Tagged_Value'Class;

   type Tag_Definition_Element is new Model_Element with record
      Multiplicity_Lower : Natural := 0;
      Multiplicity_Upper : Natural := 0;
   end record;
   type Tag_Definition_Element_Access is access all Tag_Definition_Element'Class;

   type Class_Element is new Model_Element with record
      Operations   : Model_Vector;
      Attributes   : Model_Vector;
      Associations : Model_Vector;
   end record;
   type Class_Element_Access is access all Class_Element'Class;

   type Package_Element is new Model_Element with record
      Classes      : Model_Vector;
   end record;
   type Package_Element_Access is access all Package_Element'Class;

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
