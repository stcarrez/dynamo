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

with Ada.Strings;
with Util.Strings;
with Util.Log.Loggers;

with EL.Contexts.Default;
with EL.Utils;
with EL.Variables.Default;
with Gen.Model.Enums;
with Gen.Model.Stypes;
package body Gen.Model.Tables is

   use Ada.Strings.Unbounded;
   use type Gen.Model.Mappings.Basic_Type;
   use type Gen.Model.Mappings.Mapping_Definition_Access;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.Tables");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return UBO.Object is
   begin

      if Name = "type" then
         declare
            T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
         begin
            if T = null then
               return UBO.Null_Object;
            end if;
            return UBO.To_Object (T.all'Access, UBO.STATIC);
         end;

      elsif Name = "index" then
         return UBO.To_Object (From.Number);

      elsif Name = "isUnique" then
         return UBO.To_Object (From.Unique);

      elsif Name = "isNull" then
         return UBO.To_Object (not From.Not_Null);

      elsif Name = "isInserted" then
         return UBO.To_Object (From.Is_Inserted);

      elsif Name = "isUpdated" then
         return UBO.To_Object (From.Is_Updated);

      elsif Name = "sqlType" then
         if Length (From.Sql_Type) > 0 then
            return UBO.To_Object (Mappings.Get_Type_Name (From.Sql_Type));
         end if;
         declare
            T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
         begin
            if T = null then
               return UBO.Null_Object;
               --  If this is an association to another table, use the primary key of that table.
            elsif T.all in Table_Definition'Class
              and then Table_Definition'Class (T.all).Id_Column /= null
            then
               return Table_Definition'Class (T.all).Id_Column.Get_Value (Name);
            elsif T.all in Enums.Enum_Definition'Class then
               return T.Get_Value ("sqlType");
            elsif T.all in Stypes.Stype_Definition'Class then
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
            return UBO.To_Object (From.Sql_Name);
         end if;
         declare
            T     : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
            Table : Table_Definition_Access;
         begin
            if T /= null and then T.all in Table_Definition'Class then
               Table := Table_Definition'Class (T.all)'Access;
            end if;
            if Table /= null and then Table.Id_Column /= null then
               return UBO.To_Object (From.Name & "_" & Table.Id_Column.Name);
            else
               return UBO.To_Object (From.Name);
            end if;
         end;

      elsif Name = "sqlLength" then
         return UBO.To_Object (From.Sql_Length);

      elsif Name = "isVersion" then
         return UBO.To_Object (From.Is_Version);

      elsif Name = "isReadable" then
         return UBO.To_Object (From.Is_Readable);

      elsif Name = "isPrimaryKey" then
         return UBO.To_Object (From.Is_Key);

      elsif Name = "isPrimitiveType" then
         return UBO.To_Object (From.Is_Basic_Type);

      elsif Name = "isAuditable" then
         return UBO.To_Object (From.Is_Auditable);

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

      T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
      Name : constant String := To_String (From.Type_Name);
   begin
      if T /= null then
         return not (T.Kind in Gen.Model.Mappings.T_BLOB | Gen.Model.Mappings.T_TABLE
                       | Gen.Model.Mappings.T_BEAN);
      end if;
      return Name in "int" | "String"
        | "ADO.Identifier" | "Timestamp"
        | "Integer"
        | "long" | "Long" | "Date" | "Time";
   end Is_Basic_Type;

   --  ------------------------------
   --  Returns true if the column is using a variable length (ex: a string).
   --  ------------------------------
   function Is_Variable_Length (From : Column_Definition) return Boolean is
      T    : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
   begin
      if T /= null then
         return T.Kind = Gen.Model.Mappings.T_STRING;
      else
         return False;
      end if;
   end Is_Variable_Length;

   --  ------------------------------
   --  Returns the column type.
   --  ------------------------------
   function Get_Type (From : in Column_Definition) return String is
      T : constant Gen.Model.Mappings.Mapping_Definition_Access := From.Get_Type_Mapping;
   begin
      if T /= null then
         return To_String (T.Target);
      else
         return To_String (From.Type_Name);
      end if;
   end Get_Type;

   --  ------------------------------
   --  Set the column type.
   --  ------------------------------
   procedure Set_Type (Into : in out Column_Definition;
                       Name : in String) is
   begin
      Into.Type_Name := To_UString (Name);
   end Set_Type;

   --  ------------------------------
   --  Set the SQL length of the column.
   --  ------------------------------
   procedure Set_Sql_Length (Into  : in out Column_Definition;
                             Value : in String;
                             Log   : in out Util.Log.Logging'Class) is
   begin
      Into.Sql_Length := Positive'Value (Value);

   exception
      when others =>
         Log.Error (Into.Get_Location & ": SQL length '" & Value
                    & "' for column '" & Into.Get_Name & "' must be a number");
   end Set_Sql_Length;

   --  ------------------------------
   --  Returns the column type mapping.
   --  ------------------------------
   function Get_Type_Mapping (From : in Column_Definition)
                              return Gen.Model.Mappings.Mapping_Definition_Access is
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
         Result := Gen.Model.Mappings.Find_Type (From.Type_Name, not From.Not_Null);
      end if;
      if Result /= null and then From.Use_Foreign_Key_Type
        and then Result.all in Table_Definition'Class
        and then Table_Definition'Class (Result.all).Id_Column /= null
      then
         Result := Table_Definition'Class (Result.all).Id_Column.Get_Type_Mapping;
         if Result /= null and then not From.Not_Null and then Result.Allow_Null /= null then
            Result := Result.Allow_Null;
         end if;
      end if;
      return Result;
   end Get_Type_Mapping;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Column_Definition) is
   begin
      O.Bean := UBO.To_Object (O'Unchecked_Access,
                                              UBO.STATIC);
   end Prepare;

   --  ------------------------------
   --  Validate the definition by checking and reporting problems to the logger interface.
   --  ------------------------------
   overriding
   procedure Validate (Def : in out Column_Definition;
                       Log : in out Util.Log.Logging'Class) is
      T : constant Gen.Model.Mappings.Mapping_Definition_Access := Def.Get_Type_Mapping;
   begin
      Definition (Def).Validate (Log);
      if not Def.Not_Null and then Def.Is_Basic_Type
        and then T /= null and then not T.Nullable
      then
         Log.Error (Def.Get_Location &
                      ": In table " & To_String (Def.Table.Name) &
                      ", column '" & To_String (Def.Name) &
                      "' uses not nullable type '" & To_String (Def.Type_Name) & "'");
      end if;
      if T = null then
         Log.Error (Def.Get_Location &
                      ": In table " & To_String (Def.Table.Name) &
                      ", column '" & To_String (Def.Name) &
                      "' uses unknown type '" & To_String (Def.Type_Name) & "'");
      end if;
      if T /= null and then T.Nullable and then Def.Not_Null then
         Log.Error (Def.Get_Location &
                      ": In table " & To_String (Def.Table.Name) &
                      ", column '" & To_String (Def.Name) &
                      "' is using a nullable type without not-null");
      end if;
   end Validate;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Association_Definition;
                       Name : String) return UBO.Object is
   begin
      return Column_Definition (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Association_Definition) is
      T     : constant Gen.Model.Mappings.Mapping_Definition_Access := O.Get_Type_Mapping;
      Table : Table_Definition_Access;
   begin
      Column_Definition (O).Prepare;
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
      O.Members_Bean := UBO.To_Object (O.Members'Unchecked_Access,
                                                      UBO.STATIC);
      O.Auditables_Bean := UBO.To_Object (O.Auditables'Unchecked_Access,
                                                         UBO.STATIC);
      O.Operations_Bean := UBO.To_Object (O.Operations'Unchecked_Access,
                                                         UBO.STATIC);
   end Initialize;

   --  ------------------------------
   --  Create a table with the given name.
   --  ------------------------------
   function Create_Table (Name : in UString) return Table_Definition_Access is
      Table : constant Table_Definition_Access := new Table_Definition;
   begin
      Log.Debug ("Create table {0}", Name);

      Table.Set_Name (Name);
      Table.Kind   := Gen.Model.Mappings.T_TABLE;
      Table.Target := Name;
      declare
         Pos : constant Natural := Index (Table.Name, ".", Ada.Strings.Backward);
      begin
         if Pos > 0 then
            Table.Pkg_Name := Unbounded_Slice (Table.Name, 1, Pos - 1);
            Table.Type_Name := Unbounded_Slice (Table.Name, Pos + 1, Length (Table.Name));
         else
            Table.Pkg_Name := To_UString ("ADO");
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
                         Name   : in UString;
                         Column : out Column_Definition_Access) is
   begin
      Column := new Column_Definition;
      Column.Set_Name (Name);
      Column.Sql_Name := Name;
      Column.Number   := Table.Members.Get_Count;
      Column.Table    := Table'Unchecked_Access;
      Table.Members.Append (Column);
   end Add_Column;

   --  ------------------------------
   --  Create a table association with the given name and add it to the table.
   --  ------------------------------
   procedure Add_Association (Table  : in out Table_Definition;
                              Name   : in UString;
                              Assoc  : out Association_Definition_Access) is
   begin
      Assoc := new Association_Definition;
      Assoc.Set_Name (Name);
      Assoc.Number := Table.Members.Get_Count;
      Assoc.Table  := Table'Unchecked_Access;
      Table.Members.Append (Assoc.all'Access);
      Table.Has_Associations := True;
   end Add_Association;

   --  ------------------------------
   --  Create an operation with the given name and add it to the table.
   --  ------------------------------
   procedure Add_Operation (Table     : in out Table_Definition;
                            Name      : in UString;
                            Operation : out Model.Operations.Operation_Definition_Access) is
   begin
      Operation := new Model.Operations.Operation_Definition;
      Operation.Set_Name (Name);
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
                       Name : in String) return UBO.Object is
   begin
      if Name in "members" | "columns" then
         return From.Members_Bean;

      elsif Name = "operations" then
         return From.Operations_Bean;

      elsif Name = "auditables" then
         return From.Auditables_Bean;

      elsif Name = "id" and then From.Id_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Id_Column.all'Access;
         begin
            return UBO.To_Object (Bean, UBO.STATIC);
         end;

      elsif Name = "version" and then From.Version_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access
              := From.Version_Column.all'Unchecked_Access;
         begin
            return UBO.To_Object (Bean, UBO.STATIC);
         end;

      elsif Name = "hasAssociations" then
         return UBO.To_Object (From.Has_Associations);

      elsif Name = "hasList" then
         return UBO.To_Object (From.Has_List);

      elsif Name = "type" then
         return UBO.To_Object (From.Type_Name);

      elsif Name in "table" | "sqlName" then
         return UBO.To_Object (From.Table_Name);

      elsif Name = "keyCount" then
         return UBO.To_Object (From.Key_Count);

      elsif Name = "parent" then
         if From.Parent /= null then
            declare
               Bean : constant Util.Beans.Basic.Readonly_Bean_Access
                 := From.Parent.all'Unchecked_Access;
            begin
               return UBO.To_Object (Bean, UBO.STATIC);
            end;
         else
            return UBO.Null_Object;
         end if;

      elsif Name in "isVersion" | "isPrimaryKey" | "isBean"
        | "isPrimitiveType" | "isEnum" | "isIdentifier"
        | "isDiscrete" | "isNewDiscrete"
        | "isBoolean" | "isBlob" | "isDate" | "isString"
      then
         return UBO.To_Object (False);

      elsif Name in "isReadable" | "isObject" then
         return UBO.To_Object (True);

      elsif Name = "isLimited" then
         return UBO.To_Object (From.Is_Limited);

      elsif Name = "isSerializable" then
         return UBO.To_Object (From.Is_Serializable);

      elsif Name = "isAuditable" then
         return UBO.To_Object (From.Is_Auditable);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Table_Definition) is
      Iter : Column_List.Cursor := O.Members.First;
      C    : Column_Definition_Access;
      T    : Gen.Model.Mappings.Mapping_Definition_Access;
   begin
      Log.Info ("Prepare table {0}", O.Get_Name);

      O.Key_Count := 0;
      while Column_List.Has_Element (Iter) loop
         C := Column_List.Element (Iter);
         C.Prepare;
         if C.Is_Key then
            Log.Info ("Found key {0}", C.Get_Name);
            O.Key_Count := O.Key_Count + 1;
            O.Id_Column := C;
         end if;
         if C.Is_Version then
            Log.Info ("Found version column {0}", C.Get_Name);
            O.Version_Column := C;

            --  For the <<Version>> columns, do not allow users to modify them.
            C.Is_Updated  := False;
            C.Is_Inserted := False;
         end if;
         if C.Is_Auditable then
            O.Is_Auditable := True;
            O.Auditables.Append (C);
         end if;

         --  Collect in the dependencies vectors the tables that we are using.
         if not C.Use_Foreign_Key_Type and then C.all in Association_Definition'Class then
            T := C.Get_Type_Mapping;
            if T /= null and then T.all in Table_Definition'Class then
               O.Dependencies.Append (Table_Definition'Class (T.all)'Access);
            end if;
         end if;
         Column_List.Next (Iter);
      end loop;
      if O.Id_Column = null and then Length (O.Table_Name) > 0 then
         Log.Error ("Table {0} does not have any primary key", To_String (O.Name));
      end if;
      if Length (O.Parent_Name) > 0 then
         declare
            Result : constant Mappings.Mapping_Definition_Access
              := O.Package_Def.Find_Type (O.Parent_Name);
         begin
            if Result /= null and then Result.all in Table_Definition'Class then
               O.Parent := Table_Definition'Class (Result.all)'Access;
            end if;
         end;
      end if;
   end Prepare;

   --  ------------------------------
   --  Validate the definition by checking and reporting problems to the logger interface.
   --  ------------------------------
   overriding
   procedure Validate (Def : in out Table_Definition;
                       Log : in out Util.Log.Logging'Class) is
   begin
      for Col of Def.Members loop
         Col.Validate (Log);
      end loop;
   end Validate;

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
      Table.Set_Name (Name);
      if Pos > 0 then
         Table.Pkg_Name := To_UString (Name (Name'First .. Pos - 1));
         Table.Type_Name := To_UString (Name (Pos + 1 .. Name'Last));
      else
         Table.Pkg_Name := To_UString ("ADO");
         Table.Type_Name := Table.Name;
      end if;
   end Set_Table_Name;

end Gen.Model.Tables;
