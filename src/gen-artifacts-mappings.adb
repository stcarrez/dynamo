-----------------------------------------------------------------------
--  gen-artifacts-mappings -- Type mapping artifact for Code Generator
--  Copyright (C) 2011, 2012, 2015, 2018, 2019, 2021, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Gen.Utils;
with Gen.Model;
with Gen.Model.Mappings;

--  The <b>Gen.Artifacts.Mappings</b> package is an artifact to map XML-based types
--  into Ada types.
package body Gen.Artifacts.Mappings is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.Query");

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

      procedure Register_Mapping (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node);

      procedure Register_Mappings (Model : in out Gen.Model.Packages.Model_Definition;
                                   Node  : in DOM.Core.Node);

      --  ------------------------------
      --  Register a new type mapping.
      --  ------------------------------
      procedure Register_Mapping (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node) is

         procedure Register_Type (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node);

         N          : constant DOM.Core.Node := Gen.Utils.Get_Child (Node, "to");
         To         : constant String := Gen.Utils.Get_Data_Content (N);
         Kind       : constant String := To_String (Gen.Utils.Get_Attribute (Node, "type"));
         Allow_Null : constant Boolean := Gen.Utils.Get_Attribute (Node, "allow-null", False);
         Kind_Type  : Gen.Model.Mappings.Basic_Type;

         procedure Register_Type (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node) is
            pragma Unreferenced (O);

            From : constant String := Gen.Utils.Get_Data_Content (Node);
         begin
            Gen.Model.Mappings.Register_Type (Target => To,
                                              From   => From,
                                              Kind   => Kind_Type,
                                              Allow_Null => Allow_Null);
         end Register_Type;

         procedure Iterate is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                        Process => Register_Type);

      begin
         if Kind = "date" or else To = "Ada.Calendar.Time" then
            Kind_Type := Gen.Model.Mappings.T_DATE;

         elsif Kind = "identifier" or else To = "ADO.Identifier" then
            Kind_Type := Gen.Model.Mappings.T_IDENTIFIER;

         elsif Kind = "boolean" then
            Kind_Type := Gen.Model.Mappings.T_BOOLEAN;

         elsif Kind = "float" then
            Kind_Type := Gen.Model.Mappings.T_FLOAT;

         elsif Kind = "string" or else To = "Ada.Strings.Unbounded.Unbounded_String" then
            Kind_Type := Gen.Model.Mappings.T_STRING;

         elsif Kind = "blob" or else To = "ADO.Blob_Ref" then
            Kind_Type := Gen.Model.Mappings.T_BLOB;

         elsif Kind = "entity_type" or else To = "ADO.Entity_Type" then
            Kind_Type := Gen.Model.Mappings.T_ENTITY_TYPE;

         else
            Kind_Type := Gen.Model.Mappings.T_INTEGER;
         end if;
         Iterate (O, Node, "from");
      end Register_Mapping;

      --  ------------------------------
      --  Register a model mapping
      --  ------------------------------
      procedure Register_Mappings (Model : in out Gen.Model.Packages.Model_Definition;
                                   Node  : in DOM.Core.Node) is
         procedure Iterate is
           new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                        Process => Register_Mapping);

         Name : constant String := Gen.Utils.Get_Attribute (Node, "name");
      begin
         Gen.Model.Mappings.Set_Mapping_Name (Name);
         Iterate (Model, Node, "mapping");
      end Register_Mappings;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mappings);
   begin
      Log.Debug ("Initializing mapping artifact for the configuration");

      Gen.Artifacts.Artifact (Handler).Initialize (Path, Node, Model, Context);
      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "mappings");
   end Initialize;

end Gen.Artifacts.Mappings;
