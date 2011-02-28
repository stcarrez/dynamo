-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
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

with Ada.Strings;
with Ada.Strings.Maps;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Gen.Utils;
with Util.Strings;
with Util.Strings.Transforms;
with Util.Log.Loggers;
package body Gen.Model.Tables is

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Model.Tables");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Column_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "type" then
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
         return Util.Beans.Objects.To_Object (From.Sql_Type);

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
         declare
            Node : constant DOM.Core.Node := Get_Child (From.Node, "generator");
         begin
            if Node /= null then
               return Get_Attribute (Node, "class");
            else
               return Util.Beans.Objects.Null_Object;
            end if;
         end;

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Returns true if the column type is a basic type.
   --  ------------------------------
   function Is_Basic_Type (From : Column_Definition) return Boolean is
      Name : constant String := To_String (From.Type_Name);
   begin
      return Name = "int" or Name = "String" or Name = "java.lang.String"
        or Name = "ADO.Identifier" or Name = "java.sql.Timestamp"
        or Name = "java.lang.Integer" or Name = "Integer"
        or Name = "long" or Name = "Long" or Name = "Date";
   end Is_Basic_Type;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Association_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      return Column_Definition (From).Get_Value (Name);
   end Get_Value;

   procedure Initialize (O : in out Table_Definition) is
   begin
      O.Members_Bean := EL.Objects.To_Object (O.Members'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Table_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "members" then
         return From.Members_Bean;

      elsif Name ="id" then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Id_Column.all'Access;
         begin
            return Util.Beans.Objects.To_Object (Bean);
         end;

      elsif Name ="version" then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Version_Column.all'Unchecked_Access;
         begin
            return Util.Beans.Objects.To_Object (Bean);
         end;

      elsif Name = "type" then
         return EL.Objects.To_Object (From.Type_Name);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Package_Definition;
                       Name : String) return EL.Objects.Object is
   begin
     if Name = "name" then
        return EL.Objects.To_Object (From.Pkg_Name);

     elsif Name = "package" then
        return EL.Objects.To_Object (From.Base_Name);

     elsif Name = "tables" then
        return From.Tables_Bean;

      elsif Name = "usedTypes" then
         return From.Used;

      else
        return Definition (From).Get_Value (Name);
     end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   function Get_Count (From : List_Object) return Natural is
   begin
      Log.Info ("Length {0}", Natural'Image (Natural (From.Values.Length)));
      return Natural (From.Values.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   procedure Set_Row_Index (From  : in out List_Object;
                            Index : in Natural) is
   begin
      Log.Info ("Setting row {0}", Natural'Image (Index));
      From.Row := Index;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   function Get_Row (From  : List_Object) return Util.Beans.Objects.Object is
   begin
      Log.Info ("Getting row {0}", Natural'Image (From.Row));
      return From.Values.Element (From.Row - 1);
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : List_Object;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Model_Definition;
                       Name : String) return EL.Objects.Object is
   begin
      if Name = "tables" then
         return From.Tables_Bean;
      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Register the column definition in the table
   --  ------------------------------
   procedure Register_Column (Table  : in out Table_Definition;
                              Column : in DOM.Core.Node) is
      Name : constant DOM.Core.DOM_String      := DOM.Core.Nodes.Node_Name (Column);
      C    : constant Column_Definition_Access := new Column_Definition;

   begin
      C.Node := Column;
      C.Number := Table.Members.Get_Count;
      Table.Members.Append (C);

      C.Is_Inserted := Get_Attribute (Column, "insert", True);
      C.Is_Updated  := Get_Attribute (Column, "update", True);

      --  Get the SQL mapping from an optional <column> element.
      declare
         N : DOM.Core.Node := Get_Child (Column, "column");
         T : constant DOM.Core.Node := Get_Child (Column, "type");
      begin
         if T /= null then
            C.Type_Name := To_Unbounded_String (Get_Normalized_Type (T, "name"));
         else
            C.Type_Name := To_Unbounded_String (Get_Normalized_Type (Column, "type"));
         end if;
         if N /= null then
            C.Sql_Name := Get_Attribute (N, "name");
            C.Sql_Type := Get_Attribute (N, "sql-type");
         else
            N := Column;
            C.Sql_Name := Get_Attribute (N, "column");
            C.Sql_Type := C.Type_Name;
         end if;
         C.Not_Null := Get_Attribute (N, "not-null");
         C.Unique   := Get_Attribute (N, "unique");
      end;
      if Name = "version" then
         Table.Version_Column := C;
         C.Is_Version  := True;
         C.Is_Updated  := False;
         C.Is_Inserted := False;

      elsif Name = "id" then
         Table.Id_Column := C;
         C.Is_Key := True;

      end if;
   end Register_Column;

   --  ------------------------------
   --  Register the association definition in the table
   --  ------------------------------
   procedure Register_Association (Table  : in out Table_Definition;
                                   Column : in DOM.Core.Node) is
      Name : constant DOM.Core.DOM_String      := DOM.Core.Nodes.Node_Name (Column);
      C    : constant Association_Definition_Access := new Association_Definition;
   begin
      C.Node := Column;
      C.Number := Table.Members.Get_Count;
      Table.Members.Append (C.all'Access);

      --  Get the SQL mapping from an optional <column> element.
      declare
         N : DOM.Core.Node := Get_Child (Column, "column");
      begin
         C.Type_Name := Get_Attribute (Column, "class");
         if N /= null then
            C.Sql_Name := Get_Attribute (N, "name");
            C.Sql_Type := Get_Attribute (N, "sql-type");
         else
            N := Column;
            C.Sql_Name := Get_Attribute (N, "column");
            C.Sql_Type := C.Type_Name;
         end if;
         C.Not_Null := Get_Attribute (N, "not-null");
         C.Unique   := Get_Attribute (N, "unique");
      end;
   end Register_Association;

   --  ------------------------------
   --  Register all the columns defined in the table
   --  ------------------------------
   procedure Register_Columns (Table : in out Table_Definition) is
      procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                                        Process => Register_Column);
      procedure Iterate_Association is new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                                                    Process => Register_Association);
   begin
      Iterate (Table, Table.Node, "id");
      Iterate (Table, Table.Node, "version");
      Iterate (Table, Table.Node, "property");
      Iterate_Association (Table, Table.Node, "many-to-one");
   end Register_Columns;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Package_Definition) is
      Used_Types  : String_Set.Set;
      Table_Iter  : Table_List.Cursor := O.Tables.First;
      T : constant Util.Beans.Basic.Readonly_Bean_Access := O.Used_Types'Unchecked_Access;

      function Get_Package_Name (Name : in String) return String is
         Pos : Natural := Util.Strings.Rindex (Name, '.');
      begin
         if Pos > Name'First then
            return Name (Name'First .. Pos - 1);
         else
            return "";
         end if;
      end Get_Package_Name;

   begin
      O.Used := EL.Objects.To_Object (T);
      O.Used_Types.Row := 0;
      O.Used_Types.Values.Clear;
      while Table_List.Has_Element (Table_Iter) loop
         declare
            Table : constant Table_Definition_Access := Table_List.Element (Table_Iter);
            C     : Column_List.Cursor := Table.Members.First;
         begin
            while Column_List.Has_Element (C) loop
               declare
                  Col : constant Column_Definition_Access := Column_List.Element (C);
                  Name : constant String := Get_Package_Name (To_String (Col.Type_Name));
               begin
                  if not Col.Is_Basic_Type and Name'Length > 0 then
                     Used_Types.Include (To_Unbounded_String (Name));
                  end if;
               end;
               Column_List.Next (C);
            end loop;
         end;
         Table_List.Next (Table_Iter);
      end loop;
      declare
         P : String_Set.Cursor := Used_Types.First;
      begin
         while String_Set.Has_Element (P) loop
            declare
               Name : constant Unbounded_String := String_Set.Element (P);
            begin
               Log.Info ("with {0}", Name);
               O.Used_Types.Values.Append (Util.Beans.Objects.To_Object (Name));
            end;
            String_Set.Next (P);
         end loop;
      end;
   end Prepare;

   --  ------------------------------
   --  Register or find the package knowing its name
   --  ------------------------------
   procedure Register_Package (O      : in out Model_Definition;
                               Name   : in Unbounded_String;
                               Result : out Package_Definition_Access) is
      Pos : constant Package_Map.Cursor := O.Packages.Find (Name);
   begin
      if not Package_Map.Has_Element (Pos) then
         declare
            Map : Ada.Strings.Maps.Character_Mapping;
            Base_Name : Unbounded_String;
         begin
            Map := Ada.Strings.Maps.To_Mapping (From => ".", To => "-");
            Base_Name := Translate (Name, Map);

            Result := new Package_Definition;
            Result.Pkg_Name := Name;
            Result.Tables_Bean := EL.Objects.To_Object (Result.Tables'Access);
            Util.Strings.Transforms.To_Lower_Case (To_String (Base_Name),
                                                   Result.Base_Name);
            O.Packages.Insert (Name, Result);
         end;
      else
         Result := Package_Map.Element (Pos);
      end if;
   end Register_Package;

   --  ------------------------------
   --  Register a new class definition in the model.
   --  ------------------------------
   procedure Register_Class (O    : in out Model_Definition;
                             Node : in DOM.Core.Node) is
      Table : constant Table_Definition_Access := new Table_Definition;
   begin
      Table.Node := Node;
      Table.Name := Table.Get_Attribute ("name");
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
      end;

      O.Register_Package (Table.Pkg_Name, Table.Package_Def);
      Table.Package_Def.Tables.Append (Table);
      Table.Register_Columns;

      O.Tables.Append (Table);
   end Register_Class;

   procedure Initialize (O : in out Model_Definition;
                         N : in DOM.Core.Node) is

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Model_Definition,
                                                        Process => Register_Class);

      T : constant Util.Beans.Basic.Readonly_Bean_Access := O.Tables'Unchecked_Access;
   begin
      O.Tables_Bean := EL.Objects.To_Object (T);
      Iterate (O, N, "class");
      Iterate (O, N, "subclass");
   end Initialize;

   --  ------------------------------
   --  Get the first package of the model definition.
   --  ------------------------------
   function First (From : Model_Definition) return Package_Cursor is
   begin
      return From.Packages.First;
   end First;

end Gen.Model.Tables;
