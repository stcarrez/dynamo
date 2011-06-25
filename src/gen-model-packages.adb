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

with Ada.Strings;
with Ada.Strings.Maps;

with Gen.Utils;
with Gen.Model.Tables;
with Gen.Model.Queries;

with Util.Strings;
with Util.Strings.Transforms;
with Util.Log.Loggers;
package body Gen.Model.Packages is

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Model.Packages");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Package_Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "name" then
         return Util.Beans.Objects.To_Object (From.Pkg_Name);

      elsif Name = "package" then
         return Util.Beans.Objects.To_Object (From.Base_Name);

      elsif Name = "tables" then
         return From.Tables_Bean;

      elsif Name = "queries" then
         return From.Queries_Bean;

      elsif Name = "usedTypes" then
         return From.Used;

      elsif Name = "useCalendarTime" then
         return Util.Beans.Objects.To_Object (From.Uses_Calendar_Time);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Register the declaration of the given table in the model.
   --  ------------------------------
   procedure Register_Table (O     : in out Model_Definition;
                             Table : access Gen.Model.Tables.Table_Definition'Class) is
   begin
      O.Register_Package (Table.Pkg_Name, Table.Package_Def);
      Table.Package_Def.Tables.Append (Table.all'Access);
      O.Tables.Append (Table.all'Access);
   end Register_Table;

   --  ------------------------------
   --  Register the declaration of the given query in the model.
   --  ------------------------------
   procedure Register_Query (O     : in out Model_Definition;
                             Table : access Gen.Model.Queries.Query_Definition'Class) is
   begin
      O.Register_Package (Table.Pkg_Name, Table.Package_Def);
      Table.Package_Def.Queries.Append (Table.all'Access);
      O.Queries.Append (Table.all'Access);
   end Register_Query;

   --  ------------------------------
   --  Register or find the package knowing its name
   --  ------------------------------
   procedure Register_Package (O      : in out Model_Definition;
                               Name   : in Unbounded_String;
                               Result : out Package_Definition_Access) is
      Pkg : constant String := Util.Strings.Transforms.To_Upper_Case (To_String (Name));
      Key : constant Unbounded_String := To_Unbounded_String (Pkg);
      Pos : constant Package_Map.Cursor := O.Packages.Find (Key);
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
            Result.Tables_Bean := Util.Beans.Objects.To_Object (Result.Tables'Access,
                                                                Util.Beans.Objects.STATIC);
            Util.Strings.Transforms.To_Lower_Case (To_String (Base_Name),
                                                   Result.Base_Name);
            O.Packages.Insert (Key, Result);
            Log.Debug ("Ada package '{0}' registered", Name);
         end;
      else
         Result := Package_Map.Element (Pos);
      end if;
   end Register_Package;

   --  ------------------------------
   --  Returns True if the model contains at least one package.
   --  ------------------------------
   function Has_Packages (O : in Model_Definition) return Boolean is
   begin
      return not O.Packages.Is_Empty;
   end Has_Packages;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Package_Definition) is
      use Gen.Model.Tables;

      procedure Prepare_Table (Table : in Table_Definition_Access);
      procedure Prepare_Tables (Tables : in Table_List.List_Definition);

      Used_Types  : Gen.Utils.String_Set.Set;
      T : constant Util.Beans.Basic.Readonly_Bean_Access := O.Used_Types'Unchecked_Access;

      procedure Prepare_Table (Table : in Table_Definition_Access) is
         C     : Column_List.Cursor := Table.Members.First;
      begin
         Table.Prepare;

         --  Walk the columns to get their type.
         while Column_List.Has_Element (C) loop
            declare
               Col  : constant Column_Definition_Access := Column_List.Element (C);
               T    : constant String := To_String (Col.Type_Name);
               Name : constant String := Gen.Utils.Get_Package_Name (T);
            begin
               if not Col.Is_Basic_Type and Name'Length > 0 then
                  Used_Types.Include (To_Unbounded_String (Name));

               elsif T = "Time" or T = "Date" or T = "Timestamp" or T = "Nullable_Time" then
                  O.Uses_Calendar_Time := True;
               end if;
            end;
            Column_List.Next (C);
         end loop;
      end Prepare_Table;

      procedure Prepare_Tables (Tables : in Table_List.List_Definition) is
         Table_Iter  : Table_List.Cursor := Tables.First;
      begin
         while Table_List.Has_Element (Table_Iter) loop
            declare
               Table : constant Table_Definition_Access
                 := Table_Definition_Access (Table_List.Element (Table_Iter));
            begin
               Prepare_Table (Table);
            end;
            Table_List.Next (Table_Iter);
         end loop;
      end Prepare_Tables;

   begin
      O.Used := Util.Beans.Objects.To_Object (T, Util.Beans.Objects.STATIC);
      O.Used_Types.Row := 0;
      O.Used_Types.Values.Clear;
      O.Uses_Calendar_Time := False;

      Prepare_Tables (O.Tables);
      Prepare_Tables (O.Queries);
      declare
         P : Gen.Utils.String_Set.Cursor := Used_Types.First;
      begin
         while Gen.Utils.String_Set.Has_Element (P) loop
            declare
               Name : constant Unbounded_String := Gen.Utils.String_Set.Element (P);
            begin
               Log.Info ("with {0}", Name);
               O.Used_Types.Values.Append (Util.Beans.Objects.To_Object (Name));
            end;
            Gen.Utils.String_Set.Next (P);
         end loop;
      end;
   end Prepare;

   --  ------------------------------
   --  Initialize the package instance
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Package_Definition) is
      use Util.Beans.Objects;
   begin
      O.Tables_Bean  := Util.Beans.Objects.To_Object (O.Tables'Unchecked_Access, STATIC);
      O.Queries_Bean := Util.Beans.Objects.To_Object (O.Queries'Unchecked_Access, STATIC);
      O.Used         := Util.Beans.Objects.To_Object (O.Used_Types'Unchecked_Access, STATIC);
   end Initialize;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : List_Object) return Natural is
   begin
      Log.Debug ("Length {0}", Natural'Image (Natural (From.Values.Length)));
      return Natural (From.Values.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out List_Object;
                            Index : in Natural) is
   begin
      Log.Debug ("Setting row {0}", Natural'Image (Index));
      From.Row := Index;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : List_Object) return Util.Beans.Objects.Object is
   begin
      Log.Debug ("Getting row {0}", Natural'Image (From.Row));
      return From.Values.Element (From.Row - 1);
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in List_Object;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From);
      pragma Unreferenced (Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Model_Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "tables" then
         return From.Tables_Bean;

      elsif Name = "dirname" then
         return Util.Beans.Objects.To_Object (From.Dir_Name);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the directory name associated with the model. This directory name allows to
   --  save and build a model in separate directories for the application, the unit tests
   --  and others.
   --  ------------------------------
   procedure Set_Dirname (O : in out Model_Definition;
                          Name : in String) is
   begin
      O.Dir_Name := To_Unbounded_String (Name);
   end Set_Dirname;

   --  ------------------------------
   --  Initialize the model definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Model_Definition) is
      T : constant Util.Beans.Basic.Readonly_Bean_Access := O.Tables'Unchecked_Access;
   begin
      O.Tables_Bean := Util.Beans.Objects.To_Object (T, Util.Beans.Objects.STATIC);
      O.Dir_Name    := To_Unbounded_String ("src");
   end Initialize;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Model_Definition) is
      Iter : Package_Cursor := O.Packages.First;
   begin
      while Has_Element (Iter) loop
         Element (Iter).Prepare;
         Next (Iter);
      end loop;
   end Prepare;

   --  ------------------------------
   --  Get the first package of the model definition.
   --  ------------------------------
   function First (From : Model_Definition) return Package_Cursor is
   begin
      return From.Packages.First;
   end First;

   --  ------------------------------
   --  Register a type mapping.  The <b>From</b> type describes a type in the XML
   --  configuration files (hibernate, query, ...) and the <b>To</b> represents the
   --  corresponding Ada type.
   --  ------------------------------
   procedure Register_Type (O    : in out Model_Definition;
                            From : in String;
                            To   : in String) is
   begin
      null;
   end Register_Type;

end Gen.Model.Packages;
