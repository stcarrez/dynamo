-----------------------------------------------------------------------
--  gen-model-packages -- Packages holding model, query representation
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with Ada.Strings.Unbounded.Hash;

with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Strings.Sets;

with Gen.Model.List;
with Gen.Model.Mappings;
limited with Gen.Model.Enums;
limited with Gen.Model.Tables;
limited with Gen.Model.Queries;
limited with Gen.Model.Beans;
limited with Gen.Model.Stypes;
package Gen.Model.Packages is

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   --  The <b>Model_Definition</b> contains the complete model from one or
   --  several files.  It maintains a list of Ada packages that must be generated.
   type Model_Definition is new Definition with private;
   type Model_Definition_Access is access all Model_Definition'Class;

   --  Validate the definition by checking and reporting problems to the logger interface.
   overriding
   procedure Validate (Def : in out Model_Definition;
                       Log : in out Util.Log.Logging'Class);

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
                       Name : in String) return UBO.Object;

   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   overriding
   procedure Prepare (O : in out Package_Definition);

   --  Validate the definition by checking and reporting problems to the logger interface.
   overriding
   procedure Validate (Def : in out Package_Definition;
                       Log : in out Util.Log.Logging'Class);

   --  Initialize the package instance
   overriding
   procedure Initialize (O : in out Package_Definition);

   --  Find the type identified by the name.
   function Find_Type (From : in Package_Definition;
                       Name : in UString)
                       return Gen.Model.Mappings.Mapping_Definition_Access;

   --  Get the model which contains all the package definitions.
   function Get_Model (From : in Package_Definition)
                       return Model_Definition_Access;

   --  Returns True if the package is a pre-defined package and must not be generated.
   function Is_Predefined (From : in Package_Definition) return Boolean;

   --  Set the package as a pre-defined package.
   procedure Set_Predefined (From : in out Package_Definition);

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Model_Definition;
                       Name : String) return UBO.Object;

   --  Initialize the model definition instance.
   overriding
   procedure Initialize (O : in out Model_Definition);

   --  Returns True if the model contains at least one package.
   function Has_Packages (O : in Model_Definition) return Boolean;

   --  Register or find the package knowing its name
   procedure Register_Package (O      : in out Model_Definition;
                               Name   : in UString;
                               Result : out Package_Definition_Access);

   --  Register the declaration of the given enum in the model.
   procedure Register_Enum (O    : in out Model_Definition;
                            Enum : access Gen.Model.Enums.Enum_Definition'Class);

   --  Register the declaration of the given data type in the model.
   procedure Register_Stype (O     : in out Model_Definition;
                             Stype : access Gen.Model.Stypes.Stype_Definition'Class);

   --  Register the declaration of the given table in the model.
   procedure Register_Table (O     : in out Model_Definition;
                             Table : access Gen.Model.Tables.Table_Definition'Class);

   --  Register the declaration of the given query in the model.
   procedure Register_Query (O     : in out Model_Definition;
                             Table : access Gen.Model.Queries.Query_File_Definition'Class);

   --  Register the declaration of the given bean in the model.
   procedure Register_Bean (O     : in out Model_Definition;
                            Bean  : access Gen.Model.Beans.Bean_Definition'Class);

   --  Register a type mapping.  The <b>From</b> type describes a type in the XML
   --  configuration files (hibernate, query, ...) and the <b>To</b> represents the
   --  corresponding Ada type.
   procedure Register_Type (O    : in out Model_Definition;
                            From : in String;
                            To   : in String);

   --  Find the type identified by the name.
   function Find_Type (From : in Model_Definition;
                       Name : in UString)
                       return Gen.Model.Mappings.Mapping_Definition_Access;

   --  Set the directory name associated with the model. This directory name allows to
   --  save and build a model in separate directories for the application, the unit tests
   --  and others.
   procedure Set_Dirname (O : in out Model_Definition;
                          Target_Dir : in String;
                          Model_Dir  : in String);

   --  Get the directory name associated with the model.
   function Get_Dirname (O : in Model_Definition) return String;

   --  Get the directory name which contains the model.
   function Get_Model_Directory (O : in Model_Definition) return String;

   --  Enable the generation of the Ada package given by the name.  By default all the Ada
   --  packages found in the model are generated.  When called, this enables the generation
   --  only for the Ada packages registered here.
   procedure Enable_Package_Generation (Model : in out Model_Definition;
                                        Name  : in String);

   --  Returns True if the generation is enabled for the given package name.
   function Is_Generation_Enabled (Model : in Model_Definition;
                                   Name  : in String) return Boolean;

   --  Iterate over the model tables.
   procedure Iterate_Tables (Model   : in Model_Definition;
                             Process : not null access
                               procedure (Item : in out Tables.Table_Definition));

   --  Iterate over the model enums.
   procedure Iterate_Enums (Model   : in Model_Definition;
                             Process : not null access
                               procedure (Item : in out Enums.Enum_Definition));

   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   overriding
   procedure Prepare (O : in out Model_Definition);

   package Package_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => UString,
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

   --  Returns False if the <tt>Left</tt> table does not depend on <tt>Right</tt>.
   --  Returns True if the <tt>Left</tt> table depends on the <tt>Right</tt> table.
   function Dependency_Compare (Left, Right : in Definition_Access) return Boolean;

   --  Sort the tables on their dependency.
   procedure Dependency_Sort is new Table_List.Sort_On ("<" => Dependency_Compare);

   subtype Table_List_Definition is Table_List.List_Definition;
   subtype Enum_List_Definition is Table_List.List_Definition;
   subtype Types_List_Definition is Table_List.List_Definition;
   subtype Stype_List_Definition is Table_List.List_Definition;

   type List_Object is new Util.Beans.Basic.List_Bean with record
      Values     : UBO.Vectors.Vector;
      Row        : Natural;
      Value_Bean : UBO.Object;
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
   function Get_Row (From  : in List_Object) return UBO.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in List_Object;
                       Name : in String) return UBO.Object;

   type Package_Definition is new Definition with record
      --  Enums defined in the package.
      Enums        : aliased Enum_List_Definition;
      Enums_Bean   : UBO.Object;

      --  Simple data types defined in the package.
      Stypes       : aliased Stype_List_Definition;
      Stypes_Bean  : UBO.Object;

      --  Hibernate tables
      Tables       : aliased Table_List_Definition;
      Tables_Bean  : UBO.Object;

      --  Custom queries
      Queries      : aliased Table_List_Definition;
      Queries_Bean : UBO.Object;

      --  Ada Beans
      Beans        : aliased Table_List_Definition;
      Beans_Bean   : UBO.Object;

      --  A list of external packages which are used (used for with clause generation).
      Used_Spec_Types    : aliased List_Object;
      Used_Spec          : UBO.Object;

      --  A list of external packages which are used (used for with clause generation).
      Used_Body_Types    : aliased List_Object;
      Used_Body          : UBO.Object;

      --  A map of all types defined in this package.
      Types        : Gen.Model.Mappings.Mapping_Maps.Map;

      --  The base name for the package (ex: gen-model-users)
      Base_Name    : UString;

      --  The global model (used to resolve types from other packages).
      Model              : Model_Definition_Access;

      --  True if the package uses Ada.Calendar.Time
      Uses_Calendar_Time : Boolean := False;

      --  True if the package is a pre-defined package (ie, defined by a UML profile).
      Is_Predefined      : Boolean := False;
   end record;

   type Model_Definition is new Definition with record
      --  List of all enums.
      Enums        : aliased Enum_List_Definition;
      Enums_Bean   : UBO.Object;

      --  Simple data types defined in the package.
      Stypes       : aliased Stype_List_Definition;
      Stypes_Bean  : UBO.Object;

      --  List of all tables.
      Tables       : aliased Table_List_Definition;
      Tables_Bean  : UBO.Object;

      --  List of all queries.
      Queries      : aliased Table_List_Definition;
      Queries_Bean : UBO.Object;

      --  Ada Beans
      Beans        : aliased Table_List_Definition;
      Beans_Bean   : UBO.Object;

      --  Map of all packages.
      Packages     : Package_Map.Map;

      --  Directory associated with the model ('src', 'samples', 'regtests', ...).
      Dir_Name     : UString;

      --  Directory that contains the SQL and model files.
      DB_Name      : UString;

      --  When not empty, a list of packages that must be taken into account for the generation.
      --  By default all packages and tables defined in the model are generated.
      Gen_Packages : Util.Strings.Sets.Set;
   end record;

end Gen.Model.Packages;
