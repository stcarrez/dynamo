-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

with Ada.Strings;
with Util.Strings;
with Util.Log.Loggers;

with EL.Contexts.Default;
with EL.Utils;
with EL.Variables.Default;
with Gen.Model.Enums;
package body Gen.Model.Tables is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.Tables");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return Util.Beans.Objects.Object is
      use type Gen.Model.Mappings.Mapping_Definition_Access;
   begin

      if Name = "type" then
         declare
            T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
         begin
            if T = null then
               return Util.Beans.Objects.Null_Object;
            end if;
            return Util.Beans.Objects.To_Object (T.all'Access, Util.Beans.Objects.STATIC);
         end;

      elsif Name = "index" then
         return Util.Beans.Objects.To_Object (From.Number);

      elsif Name = "isUnique" then
         return Util.Beans.Objects.To_Object (From.Unique);

      elsif Name = "isNull" then
         return Util.Beans.Objects.To_Object (not From.Not_Null);

      elsif Name = "isInserted" then
         return Util.Beans.Objects.To_Object (From.Is_Inserted);

      elsif Name = "isUpdated" then
         return Util.Beans.Objects.To_Object (From.Is_Updated);

      elsif Name = "sqlType" then
         if Length (From.Sql_Type) > 0 then
            return Util.Beans.Objects.To_Object (From.Sql_Type);
         end if;
         declare
            T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
         begin
            if T = null then
               return Util.Beans.Objects.Null_Object;
               --  If this is an association to another table, use the primary key of that table.
            elsif T.all in Table_Definition'Class
              and then Table_Definition'Class (T.all).Id_Column /= null  then
               return Table_Definition'Class (T.all).Id_Column.Get_Value (Name);
            elsif T.all in Enums.Enum_Definition'Class then
               return T.Get_Value ("sqlType");
            else
               declare
                  Ctx       : EL.Contexts.Default.Default_Context;
                  Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
               begin
                  Variables.Bind ("column", From.Bean);
                  Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
                  return EL.Utils.Eval (To_String (T.Target), Ctx);
               end;
            end if;
         end;

      elsif Name = "sqlName" then
         if Length (From.Sql_Name) > 0 then
            return Util.Beans.Objects.To_Object (From.Sql_Name);
         end if;
         declare
            T     : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
            Table : Table_Definition_Access;
         begin
            if T.all in Table_Definition'Class then
               Table := Table_Definition'Class (T.all)'Access;
            end if;
            if Table /= null and then Table.Id_Column /= null then
               return Util.Beans.Objects.To_Object (From.Name & "_" & Table.Id_Column.Name);
            else
               return Util.Beans.Objects.To_Object (From.Name);
            end if;
         end;

      elsif Name = "sqlLength" then
         return Util.Beans.Objects.To_Object (From.Sql_Length);

      elsif Name = "isVersion" then
         return Util.Beans.Objects.To_Object (From.Is_Version);

      elsif Name = "isReadable" then
         return Util.Beans.Objects.To_Object (From.Is_Readable);

      elsif Name = "isPrimaryKey" then
         return Util.Beans.Objects.To_Object (From.Is_Key);

      elsif Name = "isPrimitiveType" then
         return Util.Beans.Objects.To_Object (From.Is_Basic_Type);

      elsif Name = "generator" then
         return From.Generator;

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Returns true if the column type is a basic type.
   --  ------------------------------
   function Is_Basic_Type (From : in Column_Definition) return Boolean is
      use type Gen.Model.Mappings.Mapping_Definition_Access;
      use type Gen.Model.Mappings.Basic_Type;

      T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
      Name : constant String := To_String (From.Type_Name);
   begin
      if T /= null then
         return T.Kind /= Gen.Model.Mappings.T_BLOB and T.Kind /= Gen.Model.Mappings.T_TABLE
         and T.Kind /= Gen.Model.Mappings.T_BEAN;
      end if;
      return Name = "int" or Name = "String"
        or Name = "ADO.Identifier" or Name = "Timestamp"
        or Name = "Integer"
        or Name = "long" or Name = "Long" or Name = "Date" or Name = "Time";
   end Is_Basic_Type;

   --  ------------------------------
   --  Returns the column type.
   --  ------------------------------
   function Get_Type (From : in Column_Definition) return String is
      use type Gen.Model.Mappings.Mapping_Definition_Access;
      T : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
   begin
      if T /= null then
         return To_String (T.Target);
      else
         return To_String (From.Type_Name);
      end if;
   end Get_Type;

   --  ------------------------------
   --  Returns the column type mapping.
   --  ------------------------------
   function Get_Type_Mapping (From : in Column_Definition)
                              return Gen.Model.Mappings.Mapping_Definition_Access is
      use type Mappings.Mapping_Definition_Access;
      use type Gen.Model.Packages.Package_Definition_Access;

      Result : Gen.Model.Mappings.Mapping_Definition_Access := null;
   begin
      if From.Type_Mapping /= null then
         return From.Type_Mapping;
      end if;
      if From.Table /= null and then From.Table.Package_Def /= null then
         Result := From.Table.Package_Def.Find_Type (From.Type_Name);
      end if;
      if Result = null then
         Result := Gen.Model.Mappings.Find_Type (From.Type_Name);
      end if;
      if Result /= null and then From.Use_Foreign_Key_Type
        and then Result.all in Table_Definition'Class
        and then Table_Definition'Class (Result.all).Id_Column /= null then
         return Table_Definition'Class (Result.all).Id_Column.Get_Type_Mapping;
      end if;
      return Result;
   end Get_Type_Mapping;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Column_Definition) is
      use type Mappings.Mapping_Definition_Access;

   begin
      O.Bean := Util.Beans.Objects.To_Object (O'Unchecked_Access,
                                              Util.Beans.Objects.STATIC);
   end Prepare;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Association_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      return Column_Definition (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Association_Definition) is
      use type Gen.Model.Mappings.Mapping_Definition_Access;

      T     : constant Gen.Model.Mappings.Mapping_Definition_Access := O.Get_Type_Mapping;
      Table : Table_Definition_Access;
   begin
      if T = null then
         Table := Create_Table (O.Type_Name);
         O.Type_Mapping := Table.all'Access;
      end if;
   end Prepare;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Table_Definition) is
   begin
      O.Members_Bean := Util.Beans.Objects.To_Object (O.Members'Unchecked_Access,
                                                      Util.Beans.Objects.STATIC);
      O.Operations_Bean := Util.Beans.Objects.To_Object (O.Operations'Unchecked_Access,
                                                         Util.Beans.Objects.STATIC);
   end Initialize;

   --  ------------------------------
   --  Create a table with the given name.
   --  ------------------------------
   function Create_Table (Name : in Unbounded_String) return Table_Definition_Access is
      Table : constant Table_Definition_Access := new Table_Definition;
   begin
      Table.Name   := Name;
      Table.Kind   := Gen.Model.Mappings.T_TABLE;
      Table.Target := Name;
      declare
         Pos : constant Natural := Index (Table.Name, ".", Ada.Strings.Backward);
      begin
         if Pos > 0 then
            Table.Pkg_Name := Unbounded_Slice (Table.Name, 1, Pos - 1);
            Table.Type_Name := Unbounded_Slice (Table.Name, Pos + 1, Length (Table.Name));
         else
            Table.Pkg_Name := To_Unbounded_String ("ADO");
            Table.Type_Name := Table.Name;
         end if;
         Table.Table_Name := Table.Type_Name;
      end;
      return Table;
   end Create_Table;

   --  ------------------------------
   --  Create a table column with the given name and add it to the table.
   --  ------------------------------
   procedure Add_Column (Table  : in out Table_Definition;
                         Name   : in Unbounded_String;
                         Column : out Column_Definition_Access) is
   begin
      Column := new Column_Definition;
      Column.Name     := Name;
      Column.Sql_Name := Name;
      Column.Number   := Table.Members.Get_Count;
      Column.Table    := Table'Unchecked_Access;
      Table.Members.Append (Column);
   end Add_Column;

   --  ------------------------------
   --  Create a table association with the given name and add it to the table.
   --  ------------------------------
   procedure Add_Association (Table  : in out Table_Definition;
                              Name   : in Unbounded_String;
                              Assoc  : out Association_Definition_Access) is
   begin
      Assoc := new Association_Definition;
      Assoc.Name   := Name;
      Assoc.Number := Table.Members.Get_Count;
      Assoc.Table  := Table'Unchecked_Access;
      Table.Members.Append (Assoc.all'Access);
      Table.Has_Associations := True;
   end Add_Association;

   --  ------------------------------
   --  Create an operation with the given name and add it to the table.
   --  ------------------------------
   procedure Add_Operation (Table     : in out Table_Definition;
                            Name      : in Unbounded_String;
                            Operation : out Model.Operations.Operation_Definition_Access) is
   begin
      Operation := new Model.Operations.Operation_Definition;
      Operation.Name := Name;
      Table.Operations.Append (Operation.all'Access);
   end Add_Operation;

   --  ------------------------------
   --  Get the dependency between the two tables.
   --  Returns NONE if both table don't depend on each other.
   --  Returns FORWARD if the <tt>Left</tt> table depends on <tt>Right</tt>.
   --  Returns BACKWARD if the <tt>Right</tt> table depends on <tt>Left</tt>.
   --  ------------------------------
   function Depends_On (Left, Right : in Table_Definition_Access) return Dependency_Type is
   begin
      if Left.Dependencies.Contains (Right) then
         Log.Info ("Table {0} depends on {1}",
                   To_String (Left.Name), To_String (Right.Name));
         return FORWARD;
      elsif Right.Dependencies.Contains (Left) then
         Log.Info ("Table {1} depends on {0}",
                   To_String (Left.Name), To_String (Right.Name));
         return BACKWARD;
      else
         return NONE;
      end if;
   end Depends_On;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Table_Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "members" or Name = "columns" then
         return From.Members_Bean;

      elsif Name = "operations" then
         return From.Operations_Bean;

      elsif Name = "id" and From.Id_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Id_Column.all'Access;
         begin
            return Util.Beans.Objects.To_Object (Bean, Util.Beans.Objects.STATIC);
         end;

      elsif Name = "version" and From.Version_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access
              := From.Version_Column.all'Unchecked_Access;
         begin
            return Util.Beans.Objects.To_Object (Bean, Util.Beans.Objects.STATIC);
         end;

      elsif Name = "hasAssociations" then
         return Util.Beans.Objects.To_Object (From.Has_Associations);

      elsif Name = "hasList" then
         return Util.Beans.Objects.To_Object (From.Has_List);

      elsif Name = "type" then
         return Util.Beans.Objects.To_Object (From.Type_Name);

      elsif Name = "table" then
         return Util.Beans.Objects.To_Object (From.Table_Name);

      elsif Name = "isVersion" or Name = "isPrimaryKey" or Name = "isBean"
        or Name = "isPrimitiveType" or Name = "isEnum" or Name = "isIdentifier"
        or Name = "isBoolean" or Name = "isBlob" or Name = "isDate" or Name = "isString" then
         return Util.Beans.Objects.To_Object (False);

      elsif Name = "isReadable" or Name = "isObject" then
         return Util.Beans.Objects.To_Object (True);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Table_Definition) is
      use type Gen.Model.Mappings.Mapping_Definition_Access;

      Iter : Column_List.Cursor := O.Members.First;
      C    : Column_Definition_Access;
      T    : Gen.Model.Mappings.Mapping_Definition_Access;
   begin
      Log.Info ("Prepare table {0}", O.Name);

      while Column_List.Has_Element (Iter) loop
         C := Column_List.Element (Iter);
         C.Prepare;
         if C.Is_Key then
            Log.Info ("Found key {0}", C.Name);
            O.Id_Column := C;
         end if;
         if C.Is_Version then
            Log.Info ("Found version column {0}", C.Name);
            O.Version_Column := C;

            --  For the <<Version>> columns, do not allow users to modify them.
            C.Is_Updated  := False;
            C.Is_Inserted := False;
         end if;

         --  Collect in the dependencies vectors the tables that we are using.
         if not C.Use_Foreign_Key_Type and then C.all in Association_Definition'Class then
            T := C.Get_Type_Mapping;
            if T /= null then
               O.Dependencies.Append (Table_Definition'Class (T.all)'Access);
            end if;
         end if;
         Column_List.Next (Iter);
      end loop;
      if O.Id_Column = null and Length (O.Table_Name) > 0 then
         Log.Error ("Table {0} does not have any primary key", To_String (O.Name));
      end if;
   end Prepare;

   --  ------------------------------
   --  Collect the dependencies to other tables.
   --  ------------------------------
   procedure Collect_Dependencies (O : in out Table_Definition) is
      Iter : Table_Vectors.Cursor := O.Dependencies.First;
      List : Table_Vectors.Vector;
      D    : Table_Definition_Access;
   begin
      Log.Info ("Collect dependencies of {0}", O.Name);

      if O.Has_Mark then
         return;
      end if;

      O.Has_Mark := True;
      while Table_Vectors.Has_Element (Iter) loop
         D := Table_Vectors.Element (Iter);
         D.Collect_Dependencies;
         List.Append (D.Dependencies);
         Table_Vectors.Next (Iter);
      end loop;
      O.Has_Mark := False;

      Iter := List.First;
      while Table_Vectors.Has_Element (Iter) loop
         D := Table_Vectors.Element (Iter);
         if not O.Dependencies.Contains (D) then
            Log.Info ("Adding dependency for {0} on {1}", To_String (O.Name), To_String (D.Name));
            O.Dependencies.Append (D);
         end if;
         Table_Vectors.Next (Iter);
      end loop;
   end Collect_Dependencies;

   --  ------------------------------
   --  Set the table name and determines the package name.
   --  ------------------------------
   procedure Set_Table_Name (Table : in out Table_Definition;
                             Name  : in String) is
      Pos : constant Natural := Util.Strings.Rindex (Name, '.');
   begin
      Table.Name := To_Unbounded_String (Name);
      if Pos > 0 then
         Table.Pkg_Name := To_Unbounded_String (Name (Name'First .. Pos - 1));
         Table.Type_Name := To_Unbounded_String (Name (Pos + 1 .. Name'Last));
      else
         Table.Pkg_Name := To_Unbounded_String ("ADO");
         Table.Type_Name := Table.Name;
      end if;
   end Set_Table_Name;

end Gen.Model.Tables;
