-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
--  Copyright (C) 2009, 2010 Stephane Carrez
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
package body Gen.Model.Tables is

   use type DOM.Core.Node;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Column_Definition;
                       Name : String) return EL.Objects.Object is
      Type_Name : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (From.Node, "type");
   begin
      if Name = "type" then
         return EL.Objects.To_Object (Type_Name);

      elsif Name = "index" then
         return EL.Objects.To_Object (From.Number);

      elsif Name = "isUnique" then
         return EL.Objects.To_Object (From.Unique);

      elsif Name = "isNull" then
         return EL.Objects.To_Object (not From.Not_Null);

      elsif Name = "sqlType" then
         return EL.Objects.To_Object (From.Sql_Type);

      elsif Name = "sqlName" then
         return EL.Objects.To_Object (From.Sql_Name);

      elsif Name = "isVersion" then
         return EL.Objects.To_Object (From.Is_Version);

      elsif Name = "isPrimaryKey" then
         return EL.Objects.To_Object (From.Is_Key);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   procedure Initialize (O : in out Table_Definition) is
   begin
      O.Members_Bean := EL.Objects.To_Object (O.Members'Unchecked_Access);
   end Initialize;

--     function Get_Member_Sql_Mode (Member : in DOM.Core.Node) return String is
--        Type_Name : constant DOM_String := Get_Attribute (Member, "type");
--     begin
--        if Type_Name = "String" then
--           return "10";
--        elsif Type_Name = "Integer" then
--           return "0";
--        elsif Type_Name = "Identifier" then
--           return "1";
--        elsif Type_Name = "Time" then
--           return "3";
--        else
--           return "11";
--        end if;
--     end Get_Member_Sql_Mode;

--     function Get_Member_Value (Member : in Node) return String is
--        Type_Name : constant DOM_String := Get_Attribute (Member, "type");
--     begin
--        if Type_Name = "String" then
--           return "Null_Unbounded_String";
--        elsif Type_Name = "Integer" then
--           return "0";
--        else
--           return "0";
--        end if;
--     end Get_Member_Value;


   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Table_Definition;
                       Name : String) return EL.Objects.Object is
   begin
      if Name = "members" then
         return From.Members_Bean;

      elsif Name ="id" then
         declare
            Bean : constant EL.Beans.Readonly_Bean_Access := From.Id_Column.all'Access;
         begin
            return EL.Objects.To_Object (Bean);
         end;

      elsif Name ="version" then
         declare
            Bean : constant EL.Beans.Readonly_Bean_Access := From.Version_Column.all'Access;
         begin
            return EL.Objects.To_Object (Bean);
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

     else
        return Definition (From).Get_Value (Name);
     end if;
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

      --  Get the SQL mapping from an optional <column> element.
      declare
         N : DOM.Core.Node := Get_Child (Column, "column");
      begin
         if N /= null then
            C.Sql_Name := Get_Attribute (N, "name");
            C.Sql_Type := Get_Attribute (N, "sql-type");
         else
            N := Column;
            C.Sql_Name := Get_Attribute (N, "column");
            C.Sql_Type := Get_Attribute (N, "type");
         end if;
         C.Not_Null := Get_Attribute (N, "not-null");
         C.Unique   := Get_Attribute (N, "unique");
      end;
      if Name = "version" then
         Table.Version_Column := C;
         C.Is_Version := True;

      elsif Name = "id" then
         Table.Id_Column := C;
         C.Is_Key := True;

      end if;
   end Register_Column;

   --  ------------------------------
   --  Register all the columns defined in the table
   --  ------------------------------
   procedure Register_Columns (Table : in out Table_Definition) is
      procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                                        Process => Register_Column);
   begin
      Iterate (Table, Table.Node, "id");
      Iterate (Table, Table.Node, "version");
      Iterate (Table, Table.Node, "property");
   end Register_Columns;

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

      T : constant EL.Beans.Readonly_Bean_Access := O.Tables'Unchecked_Access;
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
