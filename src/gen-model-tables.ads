-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;

with Gen.Model.List;
with Gen.Model.Packages;
with Gen.Model.Mappings;
with Gen.Model.Operations;
package Gen.Model.Tables is

   type Table_Definition is tagged;
   type Table_Definition_Access is access all Table_Definition'Class;

   --  ------------------------------
   --  Column Definition
   --  ------------------------------
   type Column_Definition is new Definition with record
      Number   : Natural := 0;
      Table    : Table_Definition_Access;
      Bean     : UBO.Object;

      --  The column type name.
      Type_Name : UString;

      --  The SQL type associated with the column.
      Sql_Type  : UString;

      --  The SQL name associated with the column.
      Sql_Name   : UString;

      --  The SQL length for strings.
      Sql_Length : Positive := 255;

      --  Whether the column must not be null in the database
      Not_Null : Boolean := False;

      --  Whether the column must be unique
      Unique    : Boolean := False;

      --  True if this column is the optimistic locking version column.
      Is_Version : Boolean := False;

      --  True if this column is the primary key column.
      Is_Key : Boolean := False;

      --  True if the column can be read by the application.
      Is_Readable  : Boolean := True;

      --  True if the column is included in the insert statement
      Is_Inserted  : Boolean := True;

      --  True if the column is included in the update statement
      Is_Updated   : Boolean := True;

      --  True if the column is auditable (generate code to track changes).
      Is_Auditable : Boolean := False;

      --  True if the Ada mapping must use the foreign key type.
      Use_Foreign_Key_Type : Boolean := False;

      --  The class generator to use for this column.
      Generator    : UBO.Object;

      --  The type mapping of the column.
      Type_Mapping : Gen.Model.Mappings.Mapping_Definition_Access;
   end record;
   type Column_Definition_Access is access all Column_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Column_Definition);

   --  Validate the definition by checking and reporting problems to the logger interface.
   overriding
   procedure Validate (Def : in out Column_Definition;
                       Log : in out Util.Log.Logging'Class);

   --  Returns true if the column type is a basic type.
   function Is_Basic_Type (From : Column_Definition) return Boolean;

   --  Returns true if the column is using a variable length (ex: a string).
   function Is_Variable_Length (From : Column_Definition) return Boolean;

   --  Returns the column type.
   function Get_Type (From : Column_Definition) return String;

   --  Set the column type.
   procedure Set_Type (Into : in out Column_Definition;
                       Name : in String);

   --  Set the SQL length of the column.
   procedure Set_Sql_Length (Into  : in out Column_Definition;
                             Value : in String;
                             Log   : in out Util.Log.Logging'Class);

   --  Returns the column type mapping.
   function Get_Type_Mapping (From : in Column_Definition)
                              return Gen.Model.Mappings.Mapping_Definition_Access;

   package Column_List is new Gen.Model.List (T         => Column_Definition,
                                              T_Access  => Column_Definition_Access);

   --  ------------------------------
   --  Association Definition
   --  ------------------------------
   type Association_Definition is new Column_Definition with private;
   type Association_Definition_Access is access all Association_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Association_Definition;
                       Name : String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Association_Definition);

   package Operation_List is
     new Gen.Model.List (T         => Gen.Model.Operations.Operation_Definition,
                         T_Access  => Gen.Model.Operations.Operation_Definition_Access);

   package Table_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Table_Definition_Access);
   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Table_Definition is new Mappings.Mapping_Definition with record
      Members          : aliased Column_List.List_Definition;
      Members_Bean     : UBO.Object;
      Auditables       : aliased Column_List.List_Definition;
      Auditables_Bean  : UBO.Object;
      Operations       : aliased Operation_List.List_Definition;
      Operations_Bean  : UBO.Object;
      Parent           : Table_Definition_Access;
      Parent_Name      : UString;
      Package_Def      : Gen.Model.Packages.Package_Definition_Access;
      Type_Name        : UString;
      Pkg_Name         : UString;
      Table_Name       : UString;
      Version_Column   : Column_Definition_Access;
      Id_Column        : Column_Definition_Access;

      --  The number of <<PK>> columns found.
      Key_Count        : Natural := 0;
      Has_Associations : Boolean := False;

      --  The list of tables that this table depends on.
      Dependencies     : Table_Vectors.Vector;

      --  Controls whether the <tt>Vector</tt> type and the <tt>List</tt> procedure must
      --  be generated.
      Has_List         : Boolean := True;

      --  Mark flag used by the dependency calculation.
      Has_Mark         : Boolean := False;

      --  Whether the bean type is a limited type or not.
      Is_Limited       : Boolean := False;

      --  Whether the serialization operation have to be generated.
      Is_Serializable  : Boolean := False;

      --  Whether the table contains auditable fields.
      Is_Auditable     : Boolean := False;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Table_Definition;
                       Name : String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Table_Definition);

   --  Validate the definition by checking and reporting problems to the logger interface.
   overriding
   procedure Validate (Def : in out Table_Definition;
                       Log : in out Util.Log.Logging'Class);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Table_Definition);

   --  Collect the dependencies to other tables.
   procedure Collect_Dependencies (O : in out Table_Definition);

   type Dependency_Type is (NONE, FORWARD, BACKWARD);  -- CIRCULAR is not yet managed.

   --  Get the dependency between the two tables.
   --  Returns NONE if both table don't depend on each other.
   --  Returns FORWARD if the <tt>Left</tt> table depends on <tt>Right</tt>.
   --  Returns BACKWARD if the <tt>Right</tt> table depends on <tt>Left</tt>.
   function Depends_On (Left, Right : in Table_Definition_Access) return Dependency_Type;

   --  Create a table with the given name.
   function Create_Table (Name : in UString) return Table_Definition_Access;

   --  Create a table column with the given name and add it to the table.
   procedure Add_Column (Table  : in out Table_Definition;
                         Name   : in UString;
                         Column : out Column_Definition_Access);

   --  Create a table association with the given name and add it to the table.
   procedure Add_Association (Table  : in out Table_Definition;
                              Name   : in UString;
                              Assoc  : out Association_Definition_Access);

   --  Create an operation with the given name and add it to the table.
   procedure Add_Operation (Table     : in out Table_Definition;
                            Name      : in UString;
                            Operation : out Model.Operations.Operation_Definition_Access);

   --  Set the table name and determines the package name.
   procedure Set_Table_Name (Table : in out Table_Definition;
                             Name  : in String);

   package Table_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => UString,
                                     Element_Type    => Table_Definition_Access,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => "=");

   subtype Table_Cursor is Table_Map.Cursor;

   --  Returns true if the table cursor contains a valid table
   function Has_Element (Position : Table_Cursor) return Boolean
     renames Table_Map.Has_Element;

   --  Returns the table definition.
   function Element (Position : Table_Cursor) return Table_Definition_Access
     renames Table_Map.Element;

   --  Move the iterator to the next table definition.
   procedure Next (Position : in out Table_Cursor)
     renames Table_Map.Next;

   package Table_List is new Gen.Model.List (T        => Definition,
                                             T_Access => Definition_Access);

private

   type Association_Definition is new Column_Definition with null record;

end Gen.Model.Tables;
