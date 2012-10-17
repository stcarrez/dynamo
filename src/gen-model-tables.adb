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

with Ada.Strings;
with Util.Strings;
with Util.Log.Loggers;
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

      elsif Name = "type" then
         return Util.Beans.Objects.To_Object (From.Type_Name);

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
            end if;
            return T.Get_Value ("name");
         end;

      elsif Name = "sqlName" then
         return Util.Beans.Objects.To_Object (From.Sql_Name);

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
         return T.Kind /= Gen.Model.Mappings.T_BLOB;
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

      Result : Gen.Model.Mappings.Mapping_Definition_Access := null;
      Pos    : constant Natural := Ada.Strings.Unbounded.Index (From.Type_Name, ".");
   begin
      if Pos = 0 then
         Result := Gen.Model.Mappings.Find_Type (From.Type_Name);
      end if;
      if From.Type_Name = "Gen.Tests.Tables.Test_Enum" then
         Log.Debug ("Found enum");
      end if;
      if Result = null then
         Result := From.Table.Package_Def.Find_Type (From.Type_Name);
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
--        O.Type_Mapping := Gen.Model.Mappings.Find_Type (O.Type_Name);
--        if O.Type_Mapping = null then
--           O.Type_Mapping := O.Table.Package_Def.Find_Type (O.Type_Name);
--        end if;
      null;
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
      Table.Name := Name;
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
      if Name = "version" then
         Table.Version_Column := Column;
         Column.Is_Version  := True;
         Column.Is_Updated  := False;
         Column.Is_Inserted := False;

      elsif Name = "id" then
         Table.Id_Column := Column;
         Column.Is_Key := True;

      end if;
   end Add_Column;

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

      elsif Name = "type" then
         return Util.Beans.Objects.To_Object (From.Type_Name);

      elsif Name = "table" then
         return Util.Beans.Objects.To_Object (From.Table_Name);

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
   begin
      while Column_List.Has_Element (Iter) loop
         Column_List.Element (Iter).Prepare;
         Column_List.Next (Iter);
      end loop;
   end Prepare;

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
