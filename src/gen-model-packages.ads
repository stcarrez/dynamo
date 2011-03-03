-----------------------------------------------------------------------
--  gen-model-packages -- Packages holding model, query representation
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;

with Gen.Model.List;
with Gen.Model.Mappings;
limited with Gen.Model.Tables;
package Gen.Model.Packages is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Package Definition
   --  ------------------------------
   --  The <b>Package_Definition</b> holds the tables, queries and other information
   --  that must be generated for a given Ada package.
   type Package_Definition is new Definition with private;
   type Package_Definition_Access is access all Package_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Package_Definition;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   overriding
   procedure Prepare (O : in out Package_Definition);

   --  Initialize the package instance
   overriding
   procedure Initialize (O : in out Package_Definition);

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   --  The <b>Model_Definition</b> contains the complete model from one or
   --  several files.  It maintains a list of Ada packages that must be generated.
   type Model_Definition is new Definition with private;
   type Model_Definition_Access is access all Model_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Model_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Initialize the model definition instance.
   overriding
   procedure Initialize (O : in out Model_Definition);

   --  Register or find the package knowing its name
   procedure Register_Package (O      : in out Model_Definition;
                               Name   : in Unbounded_String;
                               Result : out Package_Definition_Access);

   --  Register the declaration of the given table in the model.
   procedure Register_Table (O     : in out Model_Definition;
                             Table : access Gen.Model.Tables.Table_Definition'Class);

   --  Register the declaration of the given query in the model.
   procedure Register_Query (O     : in out Model_Definition;
                             Table : access Gen.Model.Tables.Table_Definition'Class);

   --  Register a type mapping.  The <b>From</b> type describes a type in the XML
   --  configuration files (hibernate, query, ...) and the <b>To</b> represents the
   --  corresponding Ada type.
   procedure Register_Type (O    : in out Model_Definition;
                            From : in String;
                            To   : in String);

   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   overriding
   procedure Prepare (O : in out Model_Definition);

   package Package_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                     Element_Type    => Package_Definition_Access,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");

   subtype Package_Cursor is Package_Map.Cursor;

   --  Get the first package of the model definition.
   function First (From : Model_Definition) return Package_Cursor;

   --  Returns true if the package cursor contains a valid package
   function Has_Element (Position : Package_Cursor) return Boolean
                         renames Package_Map.Has_Element;

   --  Returns the package definition.
   function Element (Position : Package_Cursor) return Package_Definition_Access
                     renames Package_Map.Element;

   --  Move the iterator to the next package definition.
   procedure Next (Position : in out Package_Cursor)
                   renames Package_Map.Next;

private

   package Table_List is new Gen.Model.List (T        => Definition,
                                             T_Access => Definition_Access);

   package String_Set is
     new Ada.Containers.Hashed_Sets (Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Elements => Ada.Strings.Unbounded."=");


   type List_Object is new Util.Beans.Basic.List_Bean with record
      Values     : Util.Beans.Objects.Vectors.Vector;
      Row        : Natural;
      Value_Bean : Util.Beans.Objects.Object;
   end record;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in List_Object) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out List_Object;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in List_Object) return Util.Beans.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in List_Object;
                       Name : in String) return Util.Beans.Objects.Object;

   type Package_Definition is new Definition with record
      --  Hibernate tables
      Tables       : aliased Table_List.List_Definition;
      Tables_Bean  : Util.Beans.Objects.Object;

      --  Custom queries
      Queries      : aliased Table_List.List_Definition;
      Queries_Bean : Util.Beans.Objects.Object;

      --  A list of external packages which are used (used for with clause generation).
      Used_Types   : aliased List_Object;
      Used         : Util.Beans.Objects.Object;

      --  The package Ada name
      Pkg_Name     : Unbounded_String;

      Name         : Unbounded_String;

      --  The base name for the package (ex: gen-model-users)
      Base_Name    : Unbounded_String;

      --  True if the package uses Ada.Calendar.Time
      Uses_Calendar_Time : Boolean := False;
   end record;

   type Model_Definition is new Definition with record
      --  List of all tables.
      Tables      : aliased Table_List.List_Definition;
      Tables_Bean : Util.Beans.Objects.Object;

      --  List of all queries.
      Queries      : aliased Table_List.List_Definition;
      Queries_Bean : Util.Beans.Objects.Object;

      --  Map of all packages.
      Packages    : Package_Map.Map;
   end record;

end Gen.Model.Packages;
