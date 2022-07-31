-----------------------------------------------------------------------
--  gen-artifacts-query -- Query artifact for Code Generator
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2018, 2021, 2022 Stephane Carrez
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

with Gen.Configs;
with Gen.Utils;
with Gen.Model.Tables;
with Gen.Model.Queries;
with Gen.Model.Mappings;
with Gen.Model.Operations;

with Util.Log.Loggers;
with Util.Encoders.HMAC.SHA1;

--  The <b>Gen.Artifacts.Query</b> package is an artifact for the generation of
--  data structures returned by queries.
package body Gen.Artifacts.Query is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;
   use Gen.Model.Queries;
   use Gen.Configs;

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Query");

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

      procedure Register_Sort (Query  : in out Query_Definition;
                               Node   : in DOM.Core.Node);

      procedure Register_Sorts (Query : in out Query_Definition;
                                Node  : in DOM.Core.Node);

      procedure Register_Column (Table  : in out Query_Definition;
                                 Column : in DOM.Core.Node);

      procedure Register_Columns (Table : in out Query_Definition;
                                  Node  : in DOM.Core.Node);

      procedure Register_Operation (Table : in out Query_Definition;
                                    Node  : in DOM.Core.Node);

      procedure Register_Operations (Table : in out Query_Definition;
                                     Node  : in DOM.Core.Node);

      procedure Register_Mapping (Query : in out Gen.Model.Queries.Query_File_Definition;
                                  Node  : in DOM.Core.Node);

      procedure Register_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node);

      procedure Register_Query (Query : in out Gen.Model.Queries.Query_File_Definition;
                                Node  : in DOM.Core.Node);

      Hash : UString;

      --  ------------------------------
      --  Register the method definition in the table
      --  ------------------------------
      procedure Register_Operation (Table  : in out Query_Definition;
                                    Node   : in DOM.Core.Node) is
         Name      : constant UString := Gen.Utils.Get_Attribute (Node, "name");
         Operation : Gen.Model.Operations.Operation_Definition_Access;
         Param     : Gen.Model.Operations.Parameter_Definition_Access;
      begin
         Table.Add_Operation (Name, Operation);
         Operation.Add_Parameter (To_UString ("Outcome"),
                                  To_UString ("String"),
                                  Param);
      end Register_Operation;

      --  ------------------------------
      --  Register all the operations defined in the table
      --  ------------------------------
      procedure Register_Operations (Table : in out Query_Definition;
                                     Node  : in DOM.Core.Node) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Query_Definition,
                                                           Process => Register_Operation);
      begin
         Log.Debug ("Register operations from bean {0}", Table.Name);

         Iterate (Table, Node, "method");
      end Register_Operations;

      --  ------------------------------
      --  Register the column definition in the table
      --  ------------------------------
      procedure Register_Column (Table  : in out Query_Definition;
                                 Column : in DOM.Core.Node) is
         Name  : constant UString := Gen.Utils.Get_Attribute (Column, "name");
         C     : Column_Definition_Access;
      begin
         Table.Add_Column (Name, C);
         C.Initialize (Name, Column);
         C.Set_Location (Path);

         C.Set_Type (Gen.Utils.Get_Normalized_Type (Column, "type"));

         C.Is_Inserted := False;
         C.Is_Updated  := Gen.Utils.Get_Attribute (Column, "update", True);
         C.Not_Null    := Gen.Utils.Get_Attribute (Column, "not-null", True);
         C.Unique      := False;

         --  Construct the hash for this column mapping.
         Append (Hash, ",type=");
         Append (Hash, C.Type_Name);
         Append (Hash, ",name=");
         Append (Hash, Name);
      end Register_Column;

      --  ------------------------------
      --  Register all the columns defined in the table
      --  ------------------------------
      procedure Register_Columns (Table : in out Query_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Query_Definition,
                                                           Process => Register_Column);
      begin
         Log.Debug ("Register columns from query {0}", Table.Name);

         Iterate (Table, Node, "property");
      end Register_Columns;

      --  ------------------------------
      --  Register the sort definition in the query
      --  ------------------------------
      procedure Register_Sort (Query  : in out Query_Definition;
                               Node   : in DOM.Core.Node) is
         Name : constant UString := Gen.Utils.Get_Attribute (Node, "name");
         Sql  : constant String := Gen.Utils.Get_Data_Content (Node);
      begin
         Query.Add_Sort (Name, To_UString (Sql));
      end Register_Sort;

      --  ------------------------------
      --  Register all the sort modes defined in the query
      --  ------------------------------
      procedure Register_Sorts (Query : in out Query_Definition;
                                Node  : in DOM.Core.Node) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Query_Definition,
                                                           Process => Register_Sort);
      begin
         Log.Debug ("Register sorts from query {0}", Query.Name);

         Iterate (Query, Node, "order");
      end Register_Sorts;

      --  ------------------------------
      --  Register a new class definition in the model.
      --  ------------------------------
      procedure Register_Mapping (Query : in out Gen.Model.Queries.Query_File_Definition;
                                  Node  : in DOM.Core.Node) is
         Name  : constant UString := Gen.Utils.Get_Attribute (Node, "name");
      begin
         Query.Initialize (Name, Node);
         Query.Set_Table_Name (To_String (Name));
         Query.Set_Location (Path);

         if Name /= "" then
            Query.Set_Name (Name);
         else
            Query.Set_Name (Gen.Utils.Get_Query_Name (Path));
         end if;

         --  Construct the hash for this column mapping.
         Append (Hash, "class=");
         Append (Hash, Query.Name);
         Query.Is_Serializable := Gen.Utils.Get_Attribute (Node, "serializable", False);

         Log.Debug ("Register query {0} with type {1}", Query.Name, Query.Type_Name);
         Register_Columns (Query_Definition (Query), Node);
         Register_Operations (Query_Definition (Query), Node);
      end Register_Mapping;

      --  ------------------------------
      --  Register a new query.
      --  ------------------------------
      procedure Register_Query (Query : in out Gen.Model.Queries.Query_File_Definition;
                                Node  : in DOM.Core.Node) is
         Name : constant UString := Gen.Utils.Get_Attribute (Node, "name");
         C    : Gen.Model.Queries.Query_Definition_Access;
      begin
         Query.Add_Query (Name, C);
         C.Set_Location (Path);
         Register_Sorts (Query_Definition (C.all), Node);
      end Register_Query;

      --  ------------------------------
      --  Register a model mapping
      --  ------------------------------
      procedure Register_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate_Mapping is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Queries.Query_File_Definition,
                                        Process => Register_Mapping);
         procedure Iterate_Query is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Queries.Query_File_Definition,
                                        Process => Register_Query);

         Table : constant Query_File_Definition_Access := new Query_File_Definition;
         Pkg   : constant UString := Gen.Utils.Get_Attribute (Node, "package");
         Name  : constant UString := Gen.Utils.Get_Attribute (Node, "table");
      begin
         Table.Initialize (Name, Node);
         Table.Set_Location (Path);
         Table.File_Name := To_UString (Ada.Directories.Simple_Name (Path));
         Table.Pkg_Name  := Pkg;
         Iterate_Mapping (Query_File_Definition (Table.all), Node, "class");
         Iterate_Query (Query_File_Definition (Table.all), Node, "query");
         if Length (Table.Pkg_Name) = 0 then
            Context.Error ("Missing or empty package attribute");
         else
            Model.Register_Query (Table);
         end if;

         Log.Info ("Query hash for {0} is {1}", Path, To_String (Hash));
         Table.Sha1 := Util.Encoders.HMAC.SHA1.Sign (Key  => "ADO.Queries",
                                                     Data => To_String (Hash));
      end Register_Mapping;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mapping);

   begin
      Log.Debug ("Initializing query artifact for the configuration");

      Gen.Artifacts.Artifact (Handler).Initialize (Path, Node, Model, Context);
      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "query-mapping");
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
      pragma Unreferenced (Handler, Project);
   begin
      Log.Debug ("Preparing the model for query");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
      end if;
   end Prepare;

end Gen.Artifacts.Query;
