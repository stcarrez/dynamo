-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with Util.Beans.Objects;

with Gen.Model.List;
with Gen.Model.Packages;
with Gen.Model.Mappings;
package Gen.Model.Tables is

   use Ada.Strings.Unbounded;

   type Table_Definition;
   type Table_Definition_Access is access all Table_Definition'Class;

   --  ------------------------------
   --  Column Definition
   --  ------------------------------
   type Column_Definition is new Definition with record
      Number : Natural := 0;
      Table  : Table_Definition_Access;

      --  Whether the column must not be null in the database
      Not_Null : Boolean := False;

      --  Whether the column must be unique
      Unique   : Boolean := False;

      --  The column type name.
      Type_Name : Unbounded_String;

      --  The SQL type associated with the column.
      Sql_Type : Unbounded_String;

      --  The SQL name associated with the column.
      Sql_Name : Unbounded_String;

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

      Type_Mapping : Gen.Model.Mappings.Mapping_Definition_Access;

      --  The class generator to use for this column.
      Generator    : Util.Beans.Objects.Object;
   end record;
   type Column_Definition_Access is access all Column_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Column_Definition);

   --  Returns true if the column type is a basic type.
   function Is_Basic_Type (From : Column_Definition) return Boolean;

   --  Returns the column type.
   function Get_Type (From : Column_Definition) return String;

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
                       Name : String) return Util.Beans.Objects.Object;

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Table_Definition is new Definition with record
      Members        : aliased Column_List.List_Definition;
      Members_Bean   : Util.Beans.Objects.Object;
      Parent         : Table_Definition_Access;
      Package_Def    : Gen.Model.Packages.Package_Definition_Access;
      Type_Name      : Unbounded_String;
      Pkg_Name       : Unbounded_String;
      Version_Column : Column_Definition_Access;
      Id_Column      : Column_Definition_Access;
      Has_Associations : Boolean := False;
   end record;

   --  Get the table unique name.
   overriding
   function Get_Name (From : in Table_Definition) return String;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Table_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Table_Definition);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Table_Definition);

   --  Set the table name and determines the package name.
   procedure Set_Table_Name (Table : in out Table_Definition;
                             Name  : in String);

   package Table_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
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

private

   type Association_Definition is new Column_Definition with null record;


end Gen.Model.Tables;
