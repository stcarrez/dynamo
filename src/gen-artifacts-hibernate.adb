-----------------------------------------------------------------------
--  gen-artifacts-hibernate -- Hibernate artifact for Code Generator
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Exceptions;

with DOM.Core.Nodes;

with Gen.Configs;
with Gen.Utils;
with Gen.Model.Enums;
with Gen.Model.Tables;
with Gen.Model.Projects;

with Util.Files;
with Util.Log.Loggers;
with Util.Strings.Sets;
with Util.Encoders;

--  The <b>Gen.Artifacts</b> package represents the methods and process to prepare,
--  control and realize the code generation.
package body Gen.Artifacts.Hibernate is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Enums;
   use Gen.Model.Tables;
   use Gen.Configs;

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Hibernate");

   --  Register the column definition in the table
   procedure Register_Column (Table  : in out Table_Definition;
                              Column : in DOM.Core.Node);

   --  Register the association definition in the table
   procedure Register_Association (Table  : in out Table_Definition;
                                   Column : in DOM.Core.Node);

   --  Register all the columns defined in the table
   procedure Register_Columns (Table : in out Table_Definition);

   procedure Register_Class (O    : in out Gen.Model.Packages.Model_Definition;
                             Node : in DOM.Core.Node);

   --  Register a new enum definition in the model.
   procedure Register_Enum (O    : in out Gen.Model.Packages.Model_Definition;
                            Node : in DOM.Core.Node);

   --  Register the value definition in the enum
   procedure Register_Enum_Value (Enum  : in out Enum_Definition;
                                  Value : in DOM.Core.Node);

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

         Log.Debug ("Register column {0} of type {1}", Name, To_String (C.Type_Name));
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
      Log.Debug ("Register association {0}", Name);

      C.Node := Column;
      C.Number := Table.Members.Get_Count;
      Table.Members.Append (C.all'Access);
      Table.Has_Associations := True;

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
      procedure Iterate is
        new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                     Process => Register_Column);
      procedure Iterate_Association is
        new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                     Process => Register_Association);
   begin
      Log.Debug ("Register columns from table {0}", Table.Name);

      Iterate (Table, Table.Node, "id");
      Iterate (Table, Table.Node, "version");
      Iterate (Table, Table.Node, "property");
      Iterate_Association (Table, Table.Node, "many-to-one");
   end Register_Columns;

   --  ------------------------------
   --  Register a new class definition in the model.
   --  ------------------------------
   procedure Register_Class (O    : in out Gen.Model.Packages.Model_Definition;
                             Node : in DOM.Core.Node) is
      Table : constant Table_Definition_Access := new Table_Definition;
   begin
      Table.Node := Node;
      Table.Name := Table.Get_Attribute ("name");
      Log.Debug ("Register class {0}", Table.Name);

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

      O.Register_Table (Table);
      Register_Columns (Table_Definition (Table.all));
   end Register_Class;

   --  ------------------------------
   --  Register the value definition in the enum
   --  ------------------------------
   procedure Register_Enum_Value (Enum  : in out Enum_Definition;
                                  Value : in DOM.Core.Node) is
      Name : constant DOM.Core.DOM_String      := DOM.Core.Nodes.Node_Name (Value);
      C    : constant Value_Definition_Access := new Value_Definition;
   begin
      Log.Debug ("Register enum value {0}", Name);

      C.Node := Value;
      C.Number := Enum.Values.Get_Count;
      Enum.Values.Append (C);

--        C.Is_Inserted := Get_Attribute (Column, "value", True);
--        C.Is_Updated  := Get_Attribute (Column, "update", True);

   end Register_Enum_Value;

   --  ------------------------------
   --  Register a new enum definition in the model.
   --  ------------------------------
   procedure Register_Enum (O    : in out Gen.Model.Packages.Model_Definition;
                            Node : in DOM.Core.Node) is
      procedure Iterate is
        new Gen.Utils.Iterate_Nodes (T       => Enum_Definition,
                                     Process => Register_Enum_Value);

      Enum : constant Enum_Definition_Access := new Enum_Definition;
   begin
      Enum.Node := Node;
      Enum.Name := Enum.Get_Attribute ("name");
      Log.Debug ("Register enum {0}", Enum.Name);

      declare
         Pos : constant Natural := Index (Enum.Name, ".", Ada.Strings.Backward);
      begin
         if Pos > 0 then
            Enum.Pkg_Name := Unbounded_Slice (Enum.Name, 1, Pos - 1);
            Enum.Type_Name := Unbounded_Slice (Enum.Name, Pos + 1, Length (Enum.Name));
         else
            Enum.Pkg_Name := To_Unbounded_String ("ADO");
            Enum.Type_Name := Enum.Name;
         end if;
      end;

      O.Register_Enum (Enum);

      Log.Debug ("Register enum values from enum {0}", Enum.Name);

      Iterate (Enum_Definition (Enum.all), Enum.Node, "value");

   end Register_Enum;

   --  ------------------------------
   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   --  ------------------------------
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is

      procedure Register_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node);

      --  ------------------------------
      --  Register a model mapping
      --  ------------------------------
      procedure Register_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                        Process => Register_Class);
         procedure Iterate_Enum is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                        Process => Register_Enum);
      begin
         Iterate_Enum (Model, Node, "enum");
         Iterate (Model, Node, "class");
         Iterate (Model, Node, "subclass");
      end Register_Mapping;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mapping);

   begin
      Log.Debug ("Initializing hibernate artifact for the configuration");

      Gen.Artifacts.Artifact (Handler).Initialize (Path, Node, Model, Context);
      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "hibernate-mapping");

   exception
      when E : Name_Exist =>
         Context.Error (Ada.Exceptions.Exception_Message (E));
   end Initialize;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Context : in out Generator'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for hibernate");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE);
         Context.Add_Generation (Name => GEN_MYSQL_SQL_FILE, Mode => ITERATION_TABLE);
         Context.Add_Generation (Name => GEN_MYSQL_DROP_SQL_FILE, Mode => ITERATION_TABLE);
         Context.Add_Generation (Name => GEN_SQLITE_SQL_FILE, Mode => ITERATION_TABLE);
         Context.Add_Generation (Name => GEN_SQLITE_DROP_SQL_FILE, Mode => ITERATION_TABLE);
      end if;
   end Prepare;

   --  ------------------------------
   --  After the generation, perform a finalization step for the generation process.
   --  For each database SQL mapping, collect the schema files from the different modules
   --  of the project and generate an SQL script that can be used to create the database tables.
   --  ------------------------------
   overriding
   procedure Finish (Handler : in out Artifact;
                     Model   : in out Gen.Model.Packages.Model_Definition'Class;
                     Project : in out Gen.Model.Projects.Project_Definition'Class;
                     Context : in out Generator'Class) is
      pragma Unreferenced (Handler, Context);

      procedure Collect_SQL (Project : in Gen.Model.Projects.Project_Definition'Class;
                             Dir     : in String;
                             Driver  : in String;
                             Prefix  : in String;
                             Content : in out Unbounded_String);

      procedure Build_SQL_Schemas (Driver : in String;
                                   Prefix : in String;
                                   Name   : in String;
                                   Is_Reverse : in Boolean);

      use Util.Encoders;

      SQL_Content : Unbounded_String;

      --  SHA for each SQL content that is appended in the SQL content.
      --  This is used to avoid appending the same SQL file several times in the final SQL file.
      --  (this happens due to the SQL path being different in some cases)
      SHA_Files   : Util.Strings.Sets.Set;
      SHA_Encoder : constant Encoder := Util.Encoders.Create (Util.Encoders.HASH_SHA1);

      Model_Dir   : constant String := Model.Get_Model_Directory;

      --  ------------------------------
      --  Check if an SQL file exists for the given driver.  If such file exist,
      --  read the content and append it to the <b>Content</b> buffer.
      --  ------------------------------
      procedure Collect_SQL (Project : in Gen.Model.Projects.Project_Definition'Class;
                             Dir     : in String;
                             Driver  : in String;
                             Prefix  : in String;
                             Content : in out Unbounded_String) is
         Name : constant String := Project.Get_Project_Name;
         Dir2 : constant String := Util.Files.Compose (Dir, Driver);
         Path : constant String := Util.Files.Compose (Dir2, Name & "-"
                                                       & Prefix & Driver & ".sql");
         SQL  : Unbounded_String;
      begin
         Log.Debug ("Checking SQL file {0}", Path);

         if Ada.Directories.Exists (Path) then
            Util.Files.Read_File (Path => Path,
                                  Into => SQL);
            declare
               H : constant String := SHA_Encoder.Encode (To_String (SQL));
            begin
               if not SHA_Files.Contains (H) then
                  SHA_Files.Include (H);
                  Append (Content, "/* Copied from ");
                  Append (Content, Ada.Directories.Simple_Name (Path));
                  Append (Content, "*/");
                  Append (Content, ASCII.LF);
                  Append (Content, SQL);
               end if;
            end;
         end if;
      end Collect_SQL;

      --  ------------------------------
      --  Collect the SQL schemas defined by the projects and modules used by the main project.
      --  ------------------------------
      procedure Build_SQL_Schemas (Driver     : in String;
                                   Prefix     : in String;
                                   Name       : in String;
                                   Is_Reverse : in Boolean) is
         use type Gen.Model.Projects.Project_Definition_Access;
         use Ada.Directories;

         Out_Dir     : constant String := Util.Files.Compose (Model_Dir, Driver);
         Path        : constant String := Util.Files.Compose (Out_Dir, Name);
         Pos         : Integer;
         Incr        : Integer;
      begin
         SQL_Content := Null_Unbounded_String;
         if Is_Reverse then
            Pos  := Project.Dynamo_Files.Last_Index;
            Incr := -1;
            Collect_SQL (Project, Model_Dir, Driver, Prefix, SQL_Content);
         else
            Pos  := Project.Dynamo_Files.First_Index;
            Incr := 1;
         end if;
         while Pos >= Project.Dynamo_Files.First_Index
           and Pos <= Project.Dynamo_Files.Last_Index loop
            declare
               Name : constant String := Project.Dynamo_Files.Element (Pos);
               Dir  : constant String := Ada.Directories.Containing_Directory (Name);
               Prj  : constant Gen.Model.Projects.Project_Definition_Access
                 := Project.Find_Project (Name);
            begin
               Log.Debug ("Checking project {0}", Name);
               if Prj /= null then
                  Collect_SQL (Prj.all, Util.Files.Compose (Dir, "db"), Driver,
                               Prefix, SQL_Content);
               end if;
            end;
            Pos := Pos + Incr;
         end loop;
         if not Is_Reverse then
            Collect_SQL (Project, Model_Dir, Driver, Prefix, SQL_Content);
         end if;

         Log.Info ("Generating " & Driver & " creation schema in '{0}'",
                   Path & "-" & Driver & ".sql");
         Util.Files.Write_File (Path    => Path & "-" & Driver & ".sql",
                                Content => SQL_Content);

      end Build_SQL_Schemas;

      Name       : constant String := Project.Get_Project_Name;
   begin
      Build_SQL_Schemas ("mysql", "", "create-" & Name, False);
      Build_SQL_Schemas ("sqlite", "", "create-" & Name, False);
      Build_SQL_Schemas ("mysql", "drop-", "drop-" & Name, True);
      Build_SQL_Schemas ("sqlite", "drop-", "drop-" & Name, True);
   end Finish;

end Gen.Artifacts.Hibernate;
