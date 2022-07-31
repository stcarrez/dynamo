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

with Ada.Strings;
with Ada.Strings.Maps;

with Gen.Utils;
with Gen.Model.Enums;
with Gen.Model.Stypes;
with Gen.Model.Tables;
with Gen.Model.Queries;
with Gen.Model.Beans;
with Gen.Model.Operations;

with Util.Strings;
with Util.Strings.Transforms;
with Util.Log.Loggers;
package body Gen.Model.Packages is

   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.Packages");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Package_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "name" then
         return UBO.To_Object (From.Name);

      elsif Name = "package" then
         return UBO.To_Object (From.Base_Name);

      elsif Name = "tables" then
         return From.Tables_Bean;

      elsif Name = "enums" then
         return From.Enums_Bean;

      elsif Name = "types" then
         return From.Stypes_Bean;

      elsif Name = "queries" then
         return From.Queries_Bean;

      elsif Name = "beans" then
         return From.Beans_Bean;

      elsif Name = "usedSpecTypes" then
         return From.Used_Spec;

      elsif Name = "usedBodyTypes" then
         return From.Used_Body;

      elsif Name = "useCalendarTime" then
         return UBO.To_Object (From.Uses_Calendar_Time);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Find the type identified by the name.
   --  ------------------------------
   function Find_Type (From : in Package_Definition;
                       Name : in UString)
                       return Gen.Model.Mappings.Mapping_Definition_Access is
      Pos : Mappings.Cursor;
   begin
      if Index (Name, ".") > 0 then
         Pos := From.Types.Find (Name);
         if not Mappings.Mapping_Maps.Has_Element (Pos) then
            return From.Model.Find_Type (Name);
         end if;
      else
         Pos := From.Types.Find (From.Name & "." & Name);
      end if;
      if Mappings.Mapping_Maps.Has_Element (Pos) then
         return Mappings.Mapping_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find_Type;

   --  ------------------------------
   --  Get the model which contains all the package definitions.
   --  ------------------------------
   function Get_Model (From : in Package_Definition)
                       return Model_Definition_Access is
   begin
      return From.Model;
   end Get_Model;

   --  ------------------------------
   --  Returns True if the package is a pre-defined package and must not be generated.
   --  ------------------------------
   function Is_Predefined (From : in Package_Definition) return Boolean is
   begin
      return From.Is_Predefined;
   end Is_Predefined;

   --  ------------------------------
   --  Set the package as a pre-defined package.
   --  ------------------------------
   procedure Set_Predefined (From : in out Package_Definition) is
   begin
      From.Is_Predefined := True;
   end Set_Predefined;

   --  ------------------------------
   --  Register the declaration of the given enum in the model.
   --  ------------------------------
   procedure Register_Enum (O      : in out Model_Definition;
                            Enum   : access Gen.Model.Enums.Enum_Definition'Class) is
      Name : constant String := Enum.Get_Name;
   begin
      Log.Info ("Registering enum {0}", Name);

      O.Register_Package (Enum.Pkg_Name, Enum.Package_Def);
      if Enum.Package_Def.Enums.Find (Name) /= null then
         raise Name_Exist with "Enum '" & Name & "' already defined";
      end if;
      Enum.Package_Def.Enums.Append (Enum.all'Access);
      Enum.Package_Def.Types.Include (Enum.Name, Enum.all'Access);
      O.Enums.Append (Enum.all'Access);
      Gen.Model.Mappings.Register_Type (Enum.Get_Name, Enum.all'Access,
                                        Gen.Model.Mappings.T_ENUM);
   end Register_Enum;

   --  ------------------------------
   --  Register the declaration of the given data type in the model.
   --  ------------------------------
   procedure Register_Stype (O     : in out Model_Definition;
                             Stype : access Gen.Model.Stypes.Stype_Definition'Class) is
      use type Mappings.Mapping_Definition_Access;

      Name   : constant String := Stype.Get_Name;
      Result : Gen.Model.Mappings.Mapping_Definition_Access := null;
      Kind   : Mappings.Basic_Type := Mappings.T_INTEGER;
   begin
      Log.Info ("Registering simple data type {0}", Name);

      O.Register_Package (Stype.Pkg_Name, Stype.Package_Def);
      if Stype.Package_Def.Stypes.Find (Name) /= null then
         raise Name_Exist with "Data type '" & Name & "' already defined";
      end if;
      Stype.Package_Def.Stypes.Append (Stype.all'Access);
      Stype.Package_Def.Types.Include (Stype.Name, Stype.all'Access);
      O.Stypes.Append (Stype.all'Access);

      if Length (Stype.Parent_Type) > 0 then
         Result := Gen.Model.Mappings.Find_Type (Stype.Parent_Type, False);
         if Result /= null then
            Kind := Result.Kind;
         end if;
      end if;
      Gen.Model.Mappings.Register_Type (Stype.Get_Name, Stype.all'Access,
                                        Kind);
   end Register_Stype;

   --  ------------------------------
   --  Register the declaration of the given table in the model.
   --  ------------------------------
   procedure Register_Table (O     : in out Model_Definition;
                             Table : access Gen.Model.Tables.Table_Definition'Class) is
      Name           : constant String := Table.Get_Name;
   begin
      Log.Info ("Registering table {0}", Name);

      O.Register_Package (Table.Pkg_Name, Table.Package_Def);
      if Table.Package_Def.Tables.Find (Name) /= null then
         raise Name_Exist with "Table '" & Name & "' already defined";
      end if;
      Table.Package_Def.Tables.Append (Table.all'Access);
      Table.Package_Def.Types.Include (Table.Name, Table.all'Access);
      if O.Is_Generation_Enabled (To_String (Table.Pkg_Name)) then
         O.Tables.Append (Table.all'Access);
      end if;
   end Register_Table;

   --  ------------------------------
   --  Register the declaration of the given query in the model.
   --  ------------------------------
   procedure Register_Query (O     : in out Model_Definition;
                             Table : access Gen.Model.Queries.Query_File_Definition'Class) is
   begin
      O.Register_Package (Table.Pkg_Name, Table.Package_Def);
      Table.Package_Def.Queries.Append (Table.all'Access);
      O.Queries.Append (Table.all'Access);
   end Register_Query;

   --  ------------------------------
   --  Register the declaration of the given bean in the model.
   --  ------------------------------
   procedure Register_Bean (O     : in out Model_Definition;
                            Bean  : access Gen.Model.Beans.Bean_Definition'Class) is
   begin
      O.Register_Package (Bean.Pkg_Name, Bean.Package_Def);
      Bean.Package_Def.Beans.Append (Bean.all'Access);
      Bean.Package_Def.Types.Include (Bean.Name, Bean.all'Access);
      O.Queries.Append (Bean.all'Access);
   end Register_Bean;

   --  ------------------------------
   --  Register or find the package knowing its name
   --  ------------------------------
   procedure Register_Package (O      : in out Model_Definition;
                               Name   : in UString;
                               Result : out Package_Definition_Access) is
      Pkg : constant String := Util.Strings.Transforms.To_Upper_Case (To_String (Name));
      Key : constant UString := To_UString (Pkg);
      Pos : constant Package_Map.Cursor := O.Packages.Find (Key);
   begin
      if not Package_Map.Has_Element (Pos) then
         declare
            Map : Ada.Strings.Maps.Character_Mapping;
            Base_Name : UString;
         begin
            Map := Ada.Strings.Maps.To_Mapping (From => ".", To => "-");
            Base_Name := Translate (Name, Map);

            Result := new Package_Definition;
            Result.Set_Name (Name);
            Result.Model := O'Unchecked_Access;
            Result.Tables_Bean := UBO.To_Object (Result.Tables'Access, UBO.STATIC);
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
   --  Enable the generation of the Ada package given by the name.  By default all the Ada
   --  packages found in the model are generated.  When called, this enables the generation
   --  only for the Ada packages registered here.
   --  ------------------------------
   procedure Enable_Package_Generation (Model : in out Model_Definition;
                                        Name  : in String) is
   begin
      Model.Gen_Packages.Include (Util.Strings.Transforms.To_Upper_Case (Name));
   end Enable_Package_Generation;

   --  ------------------------------
   --  Returns True if the generation is enabled for the given package name.
   --  ------------------------------
   function Is_Generation_Enabled (Model : in Model_Definition;
                                   Name  : in String) return Boolean is
      Upper_Name : constant String := Util.Strings.Transforms.To_Upper_Case (Name);
      Key        : constant UString := To_UString (Upper_Name);
   begin
      return not Model.Packages.Element (Key).Is_Predefined
        and then (Model.Gen_Packages.Is_Empty or else Model.Gen_Packages.Contains (Upper_Name));
   end Is_Generation_Enabled;

   --  ------------------------------
   --  Iterate over the model tables.
   --  ------------------------------
   procedure Iterate_Tables (Model   : in Model_Definition;
                             Process : not null access
                               procedure (Item : in out Tables.Table_Definition)) is

      procedure Process_Definition (Item : in Definition_Access);

      procedure Process_Definition (Item : in Definition_Access) is
      begin
         Process (Tables.Table_Definition (Item.all));
      end Process_Definition;

   begin
      Model.Tables.Iterate (Process_Definition'Access);
   end Iterate_Tables;

   --  ------------------------------
   --  Iterate over the model enums.
   --  ------------------------------
   procedure Iterate_Enums (Model   : in Model_Definition;
                             Process : not null access
                               procedure (Item : in out Enums.Enum_Definition)) is

      procedure Process_Definition (Item : in Definition_Access);

      procedure Process_Definition (Item : in Definition_Access) is
      begin
         Process (Enums.Enum_Definition (Item.all));
      end Process_Definition;

   begin
      Model.Enums.Iterate (Process_Definition'Access);
   end Iterate_Enums;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Package_Definition) is
      use Gen.Model.Tables;

      procedure Prepare_Operations (List : in Operation_List.List_Definition);
      procedure Prepare_Table (Table : in Table_Definition_Access);
      procedure Prepare_Definition (Def : in Definition_Access);
      procedure Collect_Dependencies (Table : in Definition_Access);
      procedure Set_Used_Packages (Into       : in out List_Object;
                                   Used_Types : in Gen.Utils.String_Set.Set);

      Used_Spec_Types  : Gen.Utils.String_Set.Set;
      Used_Body_Types  : Gen.Utils.String_Set.Set;

      --  ------------------------------
      --  Look at the operations used to add the necessary with clauses for parameters.
      --  ------------------------------
      procedure Prepare_Operations (List : in Tables.Operation_List.List_Definition) is
         use type Operations.Operation_Type;

         Iter : Operation_List.Cursor := List.First;
      begin
         while Operation_List.Has_Element (Iter) loop
            case Operation_List.Element (Iter).Get_Type is
               when Operations.UNKNOWN =>
                  null;

               when Operations.ASF_ACTION =>
                  Used_Spec_Types.Include (To_UString ("Util.Beans.Methods"));
                  Used_Body_Types.Include (To_UString ("ASF.Events.Faces.Actions"));

               when Operations.ASF_UPLOAD =>
                  Used_Spec_Types.Include (To_UString ("Util.Beans.Methods"));
                  Used_Spec_Types.Include (To_UString ("ASF.Parts"));
                  Used_Body_Types.Include (To_UString ("ASF.Parts.Upload_Method"));

               when Operations.AWA_EVENT =>
                  Used_Spec_Types.Include (To_UString ("Util.Beans.Methods"));
                  Used_Spec_Types.Include (To_UString ("AWA.Events"));
                  Used_Body_Types.Include (To_UString ("AWA.Events.Action_Method"));

            end case;
            Operation_List.Next (Iter);
         end loop;
      end Prepare_Operations;

      procedure Prepare_Table (Table : in Table_Definition_Access) is
         C     : Column_List.Cursor := Table.Members.First;
      begin
         Table.Prepare;

         --  Walk the columns to get their type.
         while Column_List.Has_Element (C) loop
            declare
               use type Model.Mappings.Basic_Type;
               use type Model.Mappings.Mapping_Definition_Access;

               Col  : constant Column_Definition_Access := Column_List.Element (C);
               T    : constant Model.Mappings.Mapping_Definition_Access := Col.Get_Type_Mapping;
               Name : constant String := To_String (Col.Type_Name);
               Pkg  : constant String := Gen.Utils.Get_Package_Name (Name);
            begin
               if T = null then
                  Log.Error ("Column {0} has null type in table {1} - type is name {2}",
                             Col.Get_Name, Table.Get_Name, Name);

               else
                  case T.Kind is
                     when Model.Mappings.T_DATE =>
                        O.Uses_Calendar_Time := True;

                     when Model.Mappings.T_ENUM | Model.Mappings.T_BEAN | Model.Mappings.T_TABLE =>
                        if Pkg'Length > 0
                          and then Pkg /= O.Name
                          and then not Col.Use_Foreign_Key_Type
                        then
                           Used_Spec_Types.Include (To_UString (Pkg));
                        end if;

                     when others =>
                        if T.Kind /= Model.Mappings.T_DATE
                          and then Name in "Date" | "DateTime" | "Time"
                        then
                           Log.Error ("Date type {0} is invalid in table {1} - type is name {2}",
                                      Model.Mappings.Basic_Type'Image (T.Kind), Table.Get_Name,
                                      Name);
                        end if;
                  end case;
               end if;
            end;
            Column_List.Next (C);
         end loop;
         Prepare_Operations (Table.Operations);

         --  If the table is using serialization, add the Serializable.IO package.
         if Table.Is_Serializable then
            Used_Spec_Types.Include (To_UString ("Util.Serialize.IO"));
            Used_Body_Types.Include (To_UString ("ADO.Utils.Serialize"));
         end if;
         if Table.Is_Auditable then
            Used_Spec_Types.Include (To_UString ("ADO.Audits"));
         end if;
      end Prepare_Table;

      procedure Prepare_Definition (Def : in Definition_Access) is
      begin
         if Def.all in Table_Definition'Class then
            Prepare_Table (Table_Definition_Access (Def));
         else
            Def.Prepare;
         end if;
      end Prepare_Definition;

      procedure Collect_Dependencies (Table : in Definition_Access) is
      begin
         if Table.all in Table_Definition'Class then
            Table_Definition'Class (Table.all).Collect_Dependencies;
         end if;
      end Collect_Dependencies;

      procedure Set_Used_Packages (Into       : in out List_Object;
                                   Used_Types : in Gen.Utils.String_Set.Set) is
         P : Gen.Utils.String_Set.Cursor := Used_Types.First;
      begin
         while Gen.Utils.String_Set.Has_Element (P) loop
            declare
               Name : constant UString := Gen.Utils.String_Set.Element (P);
            begin
               Log.Info ("with {0}", Name);
               Into.Values.Append (UBO.To_Object (Name));
            end;
            Gen.Utils.String_Set.Next (P);
         end loop;
      end Set_Used_Packages;

   begin
      Log.Info ("Preparing package {0}", O.Name);

      O.Used_Spec_Types.Row := 0;
      O.Used_Spec_Types.Values.Clear;
      O.Used_Body_Types.Row := 0;
      O.Used_Body_Types.Values.Clear;
      O.Uses_Calendar_Time := False;

      O.Enums.Sort;
      O.Queries.Sort;
      O.Enums.Iterate (Process => Prepare_Definition'Access);
      O.Tables.Iterate (Process => Prepare_Definition'Access);
      O.Queries.Iterate (Process => Prepare_Definition'Access);
      O.Beans.Iterate (Process => Prepare_Definition'Access);

      --  Collect the table dependencies and sort the tables so that tables that depend on
      --  others are processed at the end.
      O.Tables.Iterate (Process => Collect_Dependencies'Access);
      Dependency_Sort (O.Tables);

      Set_Used_Packages (O.Used_Spec_Types, Used_Spec_Types);
      Set_Used_Packages (O.Used_Body_Types, Used_Body_Types);
   end Prepare;

   --  ------------------------------
   --  Validate the definition by checking and reporting problems to the logger interface.
   --  ------------------------------
   overriding
   procedure Validate (Def : in out Package_Definition;
                       Log : in out Util.Log.Logging'Class) is
      procedure Validate_Definition (Def : in Definition_Access);
      procedure Validate_Definition (Def : in Definition_Access) is
      begin
         Def.Validate (Log);
      end Validate_Definition;
   begin
      Def.Tables.Iterate (Process => Validate_Definition'Access);
      Def.Beans.Iterate (Process => Validate_Definition'Access);
   end Validate;

   --  ------------------------------
   --  Initialize the package instance
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Package_Definition) is
   begin
      O.Enums_Bean   := UBO.To_Object (O.Enums'Unchecked_Access, UBO.STATIC);
      O.Stypes_Bean  := UBO.To_Object (O.Stypes'Unchecked_Access, UBO.STATIC);
      O.Tables_Bean  := UBO.To_Object (O.Tables'Unchecked_Access, UBO.STATIC);
      O.Queries_Bean := UBO.To_Object (O.Queries'Unchecked_Access, UBO.STATIC);
      O.Beans_Bean   := UBO.To_Object (O.Beans'Unchecked_Access, UBO.STATIC);
      O.Used_Spec    := UBO.To_Object (O.Used_Spec_Types'Unchecked_Access, UBO.STATIC);
      O.Used_Body    := UBO.To_Object (O.Used_Body_Types'Unchecked_Access, UBO.STATIC);
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
   function Get_Row (From  : List_Object) return UBO.Object is
   begin
      Log.Debug ("Getting row {0}", Natural'Image (From.Row));
      return From.Values.Element (From.Row);
   end Get_Row;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in List_Object;
                       Name : in String) return UBO.Object is
      pragma Unreferenced (From);
      pragma Unreferenced (Name);
   begin
      return UBO.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Model_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "tables" then
         return From.Tables_Bean;

      elsif Name = "dirname" then
         return UBO.To_Object (From.Dir_Name);

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
                          Target_Dir : in String;
                          Model_Dir  : in String) is
   begin
      O.Dir_Name := To_UString (Target_Dir);
      O.DB_Name  := To_UString (Model_Dir);
   end Set_Dirname;

   --  ------------------------------
   --  Get the directory name associated with the model.
   --  ------------------------------
   function Get_Dirname (O : in Model_Definition) return String is
   begin
      return To_String (O.Dir_Name);
   end Get_Dirname;

   --  ------------------------------
   --  Get the directory name which contains the model.
   --  ------------------------------
   function Get_Model_Directory (O : in Model_Definition) return String is
   begin
      return To_String (O.DB_Name);
   end Get_Model_Directory;

   --  ------------------------------
   --  Initialize the model definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Model_Definition) is
      T : constant Util.Beans.Basic.Readonly_Bean_Access := O.Tables'Unchecked_Access;
   begin
      O.Tables_Bean := UBO.To_Object (T, UBO.STATIC);
      O.Dir_Name    := To_UString ("src");
   end Initialize;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Model_Definition) is
   begin
      for P of O.Packages loop
         P.Prepare;
      end loop;
      O.Tables.Sort;
   end Prepare;

   --  ------------------------------
   --  Validate the definition by checking and reporting problems to the logger interface.
   --  ------------------------------
   overriding
   procedure Validate (Def : in out Model_Definition;
                       Log : in out Util.Log.Logging'Class) is
   begin
      for P of Def.Packages loop
         P.Validate (Log);
      end loop;
   end Validate;

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

   --  ------------------------------
   --  Returns False if the <tt>Left</tt> table does not depend on <tt>Right</tt>.
   --  Returns True if the <tt>Left</tt> table depends on the <tt>Right</tt> table.
   --  ------------------------------
   function Dependency_Compare (Left, Right : in Definition_Access) return Boolean is
      use Gen.Model.Tables;

      T_Left  : constant Table_Definition_Access := Table_Definition'Class (Left.all)'Access;
      T_Right : constant Table_Definition_Access := Table_Definition'Class (Right.all)'Access;
   begin
      Log.Info ("Table {0} and {1} do not depend on each other",
                To_String (Left.Name), To_String (Right.Name));

      case Gen.Model.Tables.Depends_On (T_Left, T_Right) is
         when FORWARD =>
            return False;

         when BACKWARD =>
            return True;

         when others =>
            --  Two tables that don't depend on each other are sorted on their name.
            return Left.Name < Right.Name;
      end case;
   end Dependency_Compare;

   --  ------------------------------
   --  Find the type identified by the name.
   --  ------------------------------
   function Find_Type (From : in Model_Definition;
                       Name : in UString)
                       return Gen.Model.Mappings.Mapping_Definition_Access is
      N : constant Natural := Ada.Strings.Unbounded.Index (Name, ".", Ada.Strings.Backward);
      L : constant Natural := Ada.Strings.Unbounded.Length (Name);
   begin
      if N = 0 then
         return null;
      end if;
      declare
         Pkg_Name  : constant String := Ada.Strings.Unbounded.Slice (Name, 1, N - 1);
         Base_Name : constant String := Ada.Strings.Unbounded.Slice (Name, N + 1, L);
         Key       : constant String := Util.Strings.Transforms.To_Upper_Case (Pkg_Name);
         Pos       : constant Package_Map.Cursor := From.Packages.Find (To_UString (Key));
      begin
         if Package_Map.Has_Element (Pos) then
            return Package_Map.Element (Pos).Find_Type (To_UString (Base_Name));
         else
            return null;
         end if;
      end;
   end Find_Type;

end Gen.Model.Packages;
