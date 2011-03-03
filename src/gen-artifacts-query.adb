-----------------------------------------------------------------------
--  gen-artifacts-query -- Query artifact for Code Generator
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

with DOM.Core.Nodes;

with Gen.Utils;
with Gen.Model.Tables;
with Util.Log.Loggers;

--  The <b>Gen.Artifacts.Query</b> package is an artifact for the generation of
--  data structures returned by queries.
package body Gen.Artifacts.Query is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Query");

   --  ------------------------------
   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   --  ------------------------------
   procedure Initialize (Handler : in Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class) is
      pragma Unreferenced (Handler);

      --  ------------------------------
      --  Register the column definition in the table
      --  ------------------------------
      procedure Register_Column (Table  : in out Table_Definition;
                                 Column : in DOM.Core.Node) is
         C : constant Column_Definition_Access := new Column_Definition;
      begin
         C.Node := Column;
         C.Number := Table.Members.Get_Count;
         Table.Members.Append (C);

         C.Type_Name := To_Unbounded_String (Get_Normalized_Type (Column, "type"));

         C.Is_Inserted := False;
         C.Is_Updated  := False;
         C.Not_Null    := False;
         C.Unique      := False;
      end Register_Column;

      --  ------------------------------
      --  Register all the columns defined in the table
      --  ------------------------------
      procedure Register_Columns (Table : in out Table_Definition) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Table_Definition,
                                                           Process => Register_Column);
      begin
         Log.Debug ("Register columns from query {0}", Table.Name);

         Iterate (Table, Table.Node, "column");
      end Register_Columns;

      --  ------------------------------
      --  Register a new class definition in the model.
      --  ------------------------------
      procedure Register_Query (O    : in out Gen.Model.Packages.Model_Definition;
                                Node : in DOM.Core.Node) is
         Table : constant Table_Definition_Access := new Table_Definition;
         Name  : constant Unbounded_String := Gen.Model.Get_Attribute (Node, "name");
      begin
         Table.Node := Node;
         Table.Set_Table_Name (Table.Get_Attribute ("className"));

         if Name /= "" then
            Table.Name := Name;
         else
            Table.Name := To_Unbounded_String (Gen.Utils.Get_Query_Name (Path));
         end if;

         Log.Debug ("Register query {0} with type {0}", Table.Name, Table.Type_Name);
         O.Register_Query (Table);
         Register_Columns (Table_Definition (Table.all));
      end Register_Query;

      --  ------------------------------
      --  Register a model mapping
      --  ------------------------------
      procedure Register_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                           Process => Register_Query);
      begin
         Iterate (Model, Node, "map");
      end Register_Mapping;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mapping);

   begin
      Log.Debug ("Initializing query artifact for the configuration");

      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "query");
   end Initialize;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (Handler : in Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for query");

      Model.Prepare;
   end Prepare;

end Gen.Artifacts.Query;
