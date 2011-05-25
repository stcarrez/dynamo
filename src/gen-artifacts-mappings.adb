-----------------------------------------------------------------------
--  gen-artifacts-mappings -- Type mapping artifact for Code Generator
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

with Util.Log.Loggers;
with Gen.Utils;
with Gen.Model;
with Gen.Model.Mappings;

--  The <b>Gen.Artifacts.Mappings</b> package is an artifact to map XML-based types
--  into Ada types.
package body Gen.Artifacts.Mappings is

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Query");

   --  ------------------------------
   --  Mappings artifact
   --  ------------------------------


   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   overriding
   procedure Initialize (Handler : in Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class) is

      use Ada.Strings.Unbounded;

      pragma Unreferenced (Handler, Path);

      --  ------------------------------
      --  Register a new type mapping.
      --  ------------------------------
      procedure Register_Mapping (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node) is

         N    : constant DOM.Core.Node := Gen.Model.Get_Child (Node, "to");
         To   : constant String := Gen.Utils.Get_Data_Content (N);
         Kind : constant String := To_String (Gen.Model.Get_Attribute (Node, "type"));

         Is_Primitive  : Boolean := False;
         Is_Date       : Boolean := False;
         Is_String     : Boolean := False;
         Is_Identifier : Boolean := False;
         Is_Boolean    : Boolean := False;

         procedure Register_Type (O    : in out Gen.Model.Packages.Model_Definition;
                                  Node : in DOM.Core.Node) is
            pragma Unreferenced (O);

            From : constant String := Gen.Utils.Get_Data_Content (Node);
         begin
            Gen.Model.Mappings.Register_Type (Target => To,
                                              From  => From,
                                              Is_Primitive => Is_Primitive,
                                              Is_Boolean   => Is_Boolean,
                                              Is_Date      => Is_Date,
                                              Is_String    => Is_String,
                                              Is_Identifier => Is_Identifier);
         end Register_Type;

         procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                           Process => Register_Type);

      begin
         if Kind = "date" or To = "Ada.Calendar.Time" then
            Is_Date       := True;

         elsif Kind = "identifier" or To = "ADO.Identifier" then
            Is_Identifier := True;

         elsif Kind = "boolean" then
            Is_Boolean := True;

         elsif Kind = "string" or To = "Ada.Strings.Unbounded.Unbounded_String" then
            Is_String := True;

         else
            Is_Primitive := True;
         end if;
         Iterate (O, Node, "from");
      end Register_Mapping;

      --  ------------------------------
      --  Register a model mapping
      --  ------------------------------
      procedure Register_Mappings (Model : in out Gen.Model.Packages.Model_Definition;
                                  Node  : in DOM.Core.Node) is
         procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                           Process => Register_Mapping);
      begin
         Iterate (Model, Node, "mapping");
      end Register_Mappings;

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T => Gen.Model.Packages.Model_Definition,
                                                        Process => Register_Mappings);
   begin
      Log.Debug ("Initializing mapping artifact for the configuration");

      Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "mappings");
   end Initialize;

end Gen.Artifacts.Mappings;
