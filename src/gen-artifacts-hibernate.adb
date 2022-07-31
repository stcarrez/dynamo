-----------------------------------------------------------------------
--  gen-artifacts-hibernate -- Hibernate artifact for Code Generator
--  Copyright (C) 2011 - 2022 Stephane Carrez
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Containers;

with Gen.Configs;
with Gen.Utils;
with Gen.Model.Enums;
with Gen.Model.Tables;
with Gen.Model.Projects;
with Gen.Model.Mappings;

with Util.Files;
with Util.Log.Loggers;
with Util.Strings.Sets;
with Util.Encoders;

package body Gen.Artifacts.Hibernate is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Enums;
   use Gen.Model.Tables;
   use Gen.Configs;

   use type DOM.Core.Node;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.Hibernate");

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

      --  Register the column definition in the table
      procedure Register_Column (Table  : in out Table_Definition;
                                 Column : in DOM.Core.Node);

      --  Register the association definition in the table
      procedure Register_Association (Table  : in out Table_Definition;
                                      Column : in DOM.Core.Node);

      --  Register all the columns defined in the table
      procedure Register_Columns (Table : in out Table_Definition;
                                  Node  : in DOM.Core.Node);

      procedure Register_Class (O    : in out Gen.Model.Packages.Model_Definition;
                                Node : in DOM.Core.Node);

      --  Register a new enum definition in the model.
      procedure Register_Enum (O    : in out Gen.Model.Packages.Model_Definition;
                               Node : in DOM.Core.Node);

      --  Register the value definition in the enum
      procedure Register_Enum_Value (Enum  : in out Enum_Definition;
                                     Value : in DOM.Core.Node);

      Is_Version : Boolean := False;
      Is_Key     : Boolean := False;

      --  ------------------------------
      --  Register the column definition in the table
      --  ------------------------------
      procedure Register_Column (Table  : in out Table_Definition;
                                 Column : in DOM.Core.Node) is
         Name : constant UString := Gen.Utils.Get_Attribute (Column, "name");
         G    : constant DOM.Core.Node := Gen.Utils.Get_Child (Column, "generator");
         C    : Column_Definition_Access;
      begin
         Table.Add_Column (Name, C);
         C.Initialize (Name, Column);
         C.Set_Location (Path);
         C.Is_Inserted := Gen.Utils.Get_Attribute (Column, "insert", True);
         C.Is_Updated  := Gen.Utils.Get_Attribute (Column, "update", True);
         C.Is_Version  := Is_Version;
         C.Is_Key := Is_Key;

         if G /= null then
            C.Generator := Gen.Utils.Get_Attribute (G, "class");
         end if;

         --  Get the SQL mapping from an optional <column> element.
         declare
            N : DOM.Core.Node := Gen.Utils.Get_Child (Column, "column");
            T : constant DOM.Core.Node := Gen.Utils.Get_Child (Column, "type");
         begin
            if T /= null then
               C.Set_Type (Gen.Utils.Get_Normalized_Type (T, "name"));
            else
               C.Set_Type (Gen.Utils.Get_Normalized_Type (Column, "type"));
            end if;

            Log.Debug ("Register column {0} of type {1}", Name, To_String (C.Type_Name));
            if N /= null then
               C.Sql_Name := Gen.Utils.Get_Attribute (N, "name");
               C.Sql_Type := Gen.Utils.Get_Attribute (N, "sql-type");
            else
               N := Column;
               C.Sql_Name := Gen.Utils.Get_Attribute (N, "column");
               C.Sql_Type := C.Type_Name;
            end if;
            if C.Is_Version then
               C.Not_Null := True;
            else
               C.Not_Null := Gen.Utils.Get_Attribute (N, "not-null");
            end if;
            C.Unique   := Gen.Utils.Get_Attribute (N, "unique");
         end;
      end Register_Column;

      --  ------------------------------
      --  Register the association definition in the table
      --  ------------------------------
      procedure Register_Association (Table  : in out Table_Definition;
                                      Column : in DOM.Core.Node) is
         Name : constant UString := Gen.Utils.Get_Attribute (Column, "name");
         C    : Association_Definition_Access;
      begin
         Log.Debug ("Register association {0}", Name);

         Table.Add_Association (Name, C);

         C.Initialize (Name, Column);
         C.Set_Location (Path);

         --  Get the SQL mapping from an optional <column> element.
         declare
            N : DOM.Core.Node := Gen.Utils.Get_Child (Column, "column");
         begin
            C.Set_Type (Gen.Utils.Get_Attribute (Column, "class"));
            if N /= null then
               C.Sql_Name := Gen.Utils.Get_Attribute (N, "name");
               C.Sql_Type := Gen.Utils.Get_Attribute (N, "sql-type");
            else
               N := Column;
               C.Sql_Name := Gen.Utils.Get_Attribute (N, "column");
               C.Sql_Type := C.Type_Name;
            end if;
            C.Not_Null := Gen.Utils.Get_Attribute (N, "not-null");
            C.Unique   := Gen.Utils.Get_Attribute (N, "unique");
         end;
      end Register_Association;

      --  ------------------------------
      --  Register all the columns defined in the table
      --  ------------------------------
      procedure Register_Columns (Table : in out Table_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate is
           new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                        Process => Register_Column);
         procedure Iterate_Association is
           new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                        Process => Register_Association);
      begin
         Log.Debug ("Register columns from table {0}", Table.Name);

         Is_Key := True;
         Is_Version := False;
         Iterate (Table, Node, "id");

         Is_Key := False;
         Is_Version := True;
         Iterate (Table, Node, "version");

         Is_Key := False;
         Is_Version := False;
         Iterate (Table, Node, "property");
         Iterate_Association (Table, Node, "many-to-one");
      end Register_Columns;

      --  ------------------------------
      --  Register a new class definition in the model.
      --  ------------------------------
      procedure Register_Class (O    : in out Gen.Model.Packages.Model_Definition;
                                Node : in DOM.Core.Node) is
         Name       : constant UString := Gen.Utils.Get_Attribute (Node, "name");
         Table_Name : constant UString := Gen.Utils.Get_Attribute (Node, "table");
         Table      : constant Table_Definition_Access := Gen.Model.Tables.Create_Table (Name);
      begin
         Table.Initialize (Name, Node);
         Table.Set_Location (Path);
         Log.Debug ("Register class {0}", Table.Name);

         if Length (Table_Name) > 0 then
            Table.Table_Name := Table_Name;
         end if;
         Table.Has_List := Gen.Utils.Get_Attribute (Node, "list", True);
         O.Register_Table (Table);
         Register_Columns (Table_Definition (Table.all), Node);
      end Register_Class;

      --  ------------------------------
      --  Register the value definition in the enum
      --  ------------------------------
      procedure Register_Enum_Value (Enum  : in out Enum_Definition;
                                     Value : in DOM.Core.Node) is
         Name : constant UString := Gen.Utils.Get_Attribute (Value, "name");
         V    : Value_Definition_Access;
      begin
         Log.Debug ("Register enum value {0}", Name);

         Enum.Add_Value (To_String (Name), V);
      end Register_Enum_Value;

      --  ------------------------------
      --  Register a new enum definition in the model.
      --  ------------------------------
      procedure Register_Enum (O    : in out Gen.Model.Packages.Model_Definition;
                               Node : in DOM.Core.Node) is
         procedure Iterate is
           new Gen.Utils.Iterate_Nodes (T       => Enum_Definition,
                                        Process => Register_Enum_Value);

         Name  : constant UString := Gen.Utils.Get_Attribute (Node, "name");
         Enum  : constant Enum_Definition_Access := Gen.Model.Enums.Create_Enum (Name);
      begin
         Enum.Initialize (Name, Node);
         Enum.Set_Location (Path);
         Log.Debug ("Register enum {0}", Enum.Name);

         O.Register_Enum (Enum);

         Log.Debug ("Register enum values from enum {0}", Enum.Name);

         Iterate (Enum_Definition (Enum.all), Node, "value");
      end Register_Enum;

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
                      Project : in out Gen.Model.Projects.Project_Definition'Class;
                      Context : in out Generator'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for hibernate");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
         if Project.Use_Mysql then
            Context.Add_Generation (Name => GEN_MYSQL_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.MySQL_MAPPING);
            Context.Add_Generation (Name => GEN_MYSQL_DROP_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.MySQL_MAPPING);
         end if;
         if Project.Use_Sqlite then
            Context.Add_Generation (Name => GEN_SQLITE_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.SQLite_MAPPING);
            Context.Add_Generation (Name => GEN_SQLITE_DROP_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.SQLite_MAPPING);
         end if;
         if Project.Use_Postgresql then
            Context.Add_Generation (Name => GEN_POSTGRESQL_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.Postgresql_MAPPING);
            Context.Add_Generation (Name => GEN_POSTGRESQL_DROP_SQL_FILE, Mode => ITERATION_TABLE,
                                    Mapping => Gen.Model.Mappings.Postgresql_MAPPING);
         end if;
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
                             Dynamo  : in String;
                             Content : in out UString);

      procedure Build_SQL_Schemas (Driver : in String;
                                   Prefix : in String;
                                   Name   : in String;
                                   Is_Reverse : in Boolean);

      procedure Print_Info;

      use Util.Encoders;

      SQL_Content  : UString;

      --  SHA for each SQL content that is appended in the SQL content.
      --  This is used to avoid appending the same SQL file several times in the final SQL file.
      --  (this happens due to the SQL path being different in some cases)
      SHA_Files   : Util.Strings.Sets.Set;
      SHA_Encoder : constant Encoder := Util.Encoders.Create (Util.Encoders.HASH_SHA1);

      --  The module names whose data model is imported by the current project.
      --  This is only used to report a message to the user.
      Modules     : Util.Strings.Sets.Set;

      Model_Dir   : constant String := Model.Get_Model_Directory;

      --  ------------------------------
      --  Check if an SQL file exists for the given driver.  If such file exist,
      --  read the content and append it to the <b>Content</b> buffer.
      --  ------------------------------
      procedure Collect_SQL (Project : in Gen.Model.Projects.Project_Definition'Class;
                             Dir     : in String;
                             Driver  : in String;
                             Prefix  : in String;
                             Dynamo  : in String;
                             Content : in out UString) is
         Name : constant String := Project.Get_Project_Name;
         Dir2 : constant String := Util.Files.Compose (Dir, Driver);
         Path : constant String := Util.Files.Compose (Dir2, Name & "-"
                                                       & Prefix & Driver & ".sql");
         SQL  : UString;
      begin
         Log.Debug ("Checking SQL file {0}", Path);

         if Ada.Directories.Exists (Path) then
            Util.Files.Read_File (Path => Path,
                                  Into => SQL);
            declare
               H : constant String := SHA_Encoder.Encode (To_String (SQL));
            begin
               if not SHA_Files.Contains (H) then
                  if Dynamo'Length > 0 then
                     Modules.Include (Dynamo);
                  end if;
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

         Out_Dir     : constant String := Util.Files.Compose (Model_Dir, Driver);
         Path        : constant String := Util.Files.Compose (Out_Dir, Name);
         Pos         : Integer;
         Incr        : Integer;
      begin
         SQL_Content := Null_Unbounded_String;
         SHA_Files.Clear;
         if Driver = "sqlite" then
            Append (SQL_Content, "pragma synchronous=OFF;" & ASCII.LF);
         end if;
         if Prefix = "" then
            Collect_SQL (Project, Model_Dir, Driver, "pre-", "", SQL_Content);
         end if;
         if Is_Reverse then
            Pos  := Project.Dynamo_Files.Last_Index;
            Incr := -1;
            Collect_SQL (Project, Model_Dir, Driver, Prefix, "", SQL_Content);
         else
            Pos  := Project.Dynamo_Files.First_Index;
            Incr := 1;
         end if;
         while Pos >= Project.Dynamo_Files.First_Index
           and then Pos <= Project.Dynamo_Files.Last_Index loop
            declare
               Name : constant String := Project.Dynamo_Files.Element (Pos);
               Prj  : constant Gen.Model.Projects.Project_Definition_Access
                 := Project.Find_Project (Name);
            begin
               Log.Debug ("Checking project {0}", Name);
               if Prj /= null then
                  Collect_SQL (Prj.all, Prj.Get_Database_Dir, Driver,
                               Prefix, Name, SQL_Content);
               end if;
            end;
            Pos := Pos + Incr;
         end loop;
         if not Is_Reverse then
            Collect_SQL (Project, Model_Dir, Driver, Prefix, "", SQL_Content);
         end if;
         if Prefix = "" then
            Collect_SQL (Project, Model_Dir, Driver, "init-", "", SQL_Content);
         end if;

         Log.Info ("Generating " & Driver & " creation schema in '{0}'",
                   Path & "-" & Driver & ".sql");
         Util.Files.Write_File (Path    => Path & "-" & Driver & ".sql",
                                Content => SQL_Content);

      end Build_SQL_Schemas;

      --  ------------------------------
      --  Print information about the generated SQLfiles.
      --  ------------------------------
      procedure Print_Info is
         Iter : Util.Strings.Sets.Cursor := Modules.First;
      begin
         if Util.Strings.Sets.Has_Element (Iter) then
            Log.Info ("Generated the SQL model from{0} Dynamo projects: ",
                      Ada.Containers.Count_Type'Image (Modules.Length));

            while Util.Strings.Sets.Has_Element (Iter) loop
               Log.Info ("  {0}", Util.Strings.Sets.Element (Iter));
               Util.Strings.Sets.Next (Iter);
            end loop;
         end if;
      end Print_Info;

      Name       : constant String := Project.Get_Project_Name;
   begin
      if not Project.Is_Plugin then
         if Project.Use_Mysql then
            Build_SQL_Schemas ("mysql", "", "create-" & Name, False);
         end if;
         if Project.Use_Postgresql then
            Build_SQL_Schemas ("postgresql", "", "create-" & Name, False);
         end if;
         if Project.Use_Sqlite then
            Build_SQL_Schemas ("sqlite", "", "create-" & Name, False);
         end if;
         if Project.Use_Mysql then
            Build_SQL_Schemas ("mysql", "drop-", "drop-" & Name, True);
         end if;
         if Project.Use_Postgresql then
            Build_SQL_Schemas ("postgresql", "drop-", "drop-" & Name, True);
         end if;
         if Project.Use_Sqlite then
            Build_SQL_Schemas ("sqlite", "drop-", "drop-" & Name, True);
         end if;
         Print_Info;
      end if;
   end Finish;

end Gen.Artifacts.Hibernate;
