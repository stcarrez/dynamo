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
with Util.Strings;
package body Gen.Model.Tables is

   use type DOM.Core.Node;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return Util.Beans.Objects.Object is
      use type Gen.Model.Mappings.Mapping_Definition_Access;
   begin

      if Name = "type" and From.Type_Mapping /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Type_Mapping.all'Access;
         begin
            return Util.Beans.Objects.To_Object (From.Type_Name);
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
      return Name = "int" or Name = "String"
        or Name = "ADO.Identifier" or Name = "Timestamp"
        or Name = "Integer"
        or Name = "long" or Name = "Long" or Name = "Date" or Name = "Time";
   end Is_Basic_Type;

   --  ------------------------------
   --  Returns the column type.
   --  ------------------------------
   function Get_Type (From : Column_Definition) return String is
      use type Gen.Model.Mappings.Mapping_Definition_Access;
   begin
      if From.Type_Mapping /= null then
         return To_String (From.Type_Mapping.Target);
      else
         return To_String (From.Type_Name);
      end if;
   end Get_Type;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Column_Definition) is
   begin
      O.Type_Mapping := Gen.Model.Mappings.Find_Type (O.Type_Name);
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
      O.Members_Bean := Util.Beans.Objects.To_Object (O.Members'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Table_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "members" or Name = "columns" then
         return From.Members_Bean;

      elsif Name ="id" and From.Id_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Id_Column.all'Access;
         begin
            return Util.Beans.Objects.To_Object (Bean);
         end;

      elsif Name ="version" and From.Version_Column /= null then
         declare
            Bean : constant Util.Beans.Basic.Readonly_Bean_Access := From.Version_Column.all'Unchecked_Access;
         begin
            return Util.Beans.Objects.To_Object (Bean);
         end;

      elsif Name = "hasAssociations" then
         return Util.Beans.Objects.To_Object (From.Has_Associations);

      elsif Name = "type" then
         return Util.Beans.Objects.To_Object (From.Type_Name);

      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);

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
      Pos : constant Natural := Util.Strings.RIndex (Name, '.');
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
