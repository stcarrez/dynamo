-----------------------------------------------------------------------
--  gen-artifacts-hibernate -- Hibernate artifact for Code Generator
--  Copyright (C) 2011 Stephane Carrez
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

with DOM.Core.Nodes;

with Gen.Utils;
with Gen.Model.Tables;
with Gen.Model.Projects;

with Util.Files;
with Util.Log.Loggers;


--  The <b>Gen.Artifacts</b> package represents the methods and process to prepare,
--  control and realize the code generation.
package body Gen.Artifacts.Hibernate is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;

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

         Log.Debug ("Register column {0}", C.Type_Name);
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
   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   --  ------------------------------
   overriding
   procedure Initialize (Handler : in Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is
      pragma Unreferenced (Handler, Path);

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
      begin
         Iterate (Model, Node, "class");
         Iterate (Model, Node, "subclass");
      end Register_Mapping;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mapping);

   begin
      Log.Debug ("Initializing hibernate artifact for the configuration");

      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "hibernate-mapping");
   end Initialize;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (Handler : in Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Context : in out Generator'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for hibernate");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE);
         Context.Add_Generation (Name => GEN_MYSQL_SQL_FILE, Mode => ITERATION_TABLE);
         Context.Add_Generation (Name => GEN_SQLITE_SQL_FILE, Mode => ITERATION_TABLE);
      end if;
   end Prepare;

   --  ------------------------------
   --  After the generation, perform a finalization step for the generation process.
   --  For each database SQL mapping, collect the schema files from the different modules
   --  of the project and generate an SQL script that can be used to create the database tables.
   --  ------------------------------
   overriding
   procedure Finish (Handler : in Artifact;
                     Model   : in out Gen.Model.Packages.Model_Definition'Class;
                     Project : in out Gen.Model.Projects.Project_Definition'Class;
                     Context : in out Generator'Class) is
      pragma Unreferenced (Handler, Context);

      procedure Collect_SQL (Project : in Gen.Model.Projects.Project_Definition'Class;
                             Dir     : in String;
                             Driver  : in String;
                             Content : in out Unbounded_String);

      procedure Build_SQL_Schemas;

      MySQL_Content  : Unbounded_String;
      Sqlite_Content : Unbounded_String;
      Model_Dir      : constant String := Model.Get_Model_Directory;

      --  ------------------------------
      --  Check if an SQL file exists for the given driver.  If such file exist,
      --  read the content and append it to the <b>Content</b> buffer.
      --  ------------------------------
      procedure Collect_SQL (Project : in Gen.Model.Projects.Project_Definition'Class;
                             Dir     : in String;
                             Driver  : in String;
                             Content : in out Unbounded_String) is
         Name : constant String := Project.Get_Project_Name;
         Path : constant String := Util.Files.Compose (Dir, Name & "-" & Driver & ".sql");
         SQL  : Unbounded_String;
      begin
         Log.Debug ("Checking SQL file {0}", Path);

         if Ada.Directories.Exists (Path) then
            Append (Content, "/* Copied from ");
            Append (Content, Path);
            Append (Content, "*/");
            Append (Content, ASCII.LF);
            Util.Files.Read_File (Path => Path,
                                  Into => SQL);
            Append (Content, SQL);
         end if;
      end Collect_SQL;

      --  ------------------------------
      --  Collect the SQL schemas defined by the projects and modules used by the main project.
      --  ------------------------------
      procedure Build_SQL_Schemas is
         use type Gen.Model.Projects.Project_Definition_Access;

         Iter : Gen.Utils.String_List.Cursor := Project.Dynamo_Files.First;
      begin
         while Gen.Utils.String_List.Has_Element (Iter) loop
            declare
               Name : constant String := Gen.Utils.String_List.Element (Iter);
               Dir  : constant String := Ada.Directories.Containing_Directory (Name);
               Prj  : constant Gen.Model.Projects.Project_Definition_Access
                 := Project.Find_Project (Name);
            begin
               if Prj /= null then
                  Collect_SQL (Prj.all, Util.Files.Compose (Dir, "db"), "mysql", MySQL_Content);
                  Collect_SQL (Prj.all, Util.Files.Compose (Dir, "db"), "sqlite", Sqlite_Content);
               end if;
            end;
            Gen.Utils.String_List.Next (Iter);
         end loop;
         Collect_SQL (Project, Model_Dir, "mysql", MySQL_Content);
         Collect_SQL (Project, Model_Dir, "sqlite", Sqlite_Content);
      end Build_SQL_Schemas;

      use Ada.Directories;

      Name       : constant String := Project.Get_Project_Name;
      Base_Path  : constant String := Util.Files.Compose (Model_Dir, "create-") & Name;
   begin
      Build_SQL_Schemas;

      Log.Info ("Generating MySQL creation schema in '{0}'", Base_Path & "-mysql.sql");
      Util.Files.Write_File (Path    => Base_Path & "-mysql.sql",
                             Content => MySQL_Content);

      Log.Info ("Generating Sqlite creation schema in '{0}'", Base_Path & "-sqlite.sql");
      Util.Files.Write_File (Path    => Base_Path & "-sqlite.sql",
                             Content => Sqlite_Content);
   end Finish;

end Gen.Artifacts.Hibernate;
