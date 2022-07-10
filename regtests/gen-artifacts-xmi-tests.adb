-----------------------------------------------------------------------
--  gen-xmi-tests -- Tests for xmi
--  Copyright (C) 2012, 2021, 2022 Stephane Carrez
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

with Util.Test_Caller;

with Gen.Configs;
with Gen.Generator;
package body Gen.Artifacts.XMI.Tests is

   package Caller is new Util.Test_Caller (Test, "Gen.XMI");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Gen.XMI.Read_UML_Configuration",
                       Test_Read_XMI'Access);
      Caller.Add_Test (Suite, "Test Gen.XMI.Find_Element",
                       Test_Find_Element'Access);
      Caller.Add_Test (Suite, "Test Gen.XMI.Find_Element",
                       Test_Find_Tag_Definition'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading the XMI files defines in the Dynamo UML configuration repository.
   --  ------------------------------
   procedure Test_Read_XMI (T : in out Test) is
      procedure Check (Namespace : in String;
                       Name      : in String;
                       Id        : in String);

      A : Artifact;
      G : Gen.Generator.Handler;
      C : constant String := Util.Tests.Get_Parameter ("config_dir", "config");

      use type Gen.Model.XMI.Model_Element_Access;

      procedure Check (Namespace : in String;
                       Name      : in String;
                       Id        : in String) is
         Empty  : Gen.Model.XMI.Model_Map.Map;
         XMI_Id : constant UString := To_UString (Namespace & "#" & Id);
         N      : constant Gen.Model.XMI.Model_Element_Access := Gen.Model.XMI.Find (A.Nodes,
                                                                                     Empty,
                                                                                     XMI_Id);
      begin
         T.Assert (N /= null, "Cannot find UML element " & To_String (XMI_Id));
         Util.Tests.Assert_Equals (T, Name, To_String (N.Name), "Invalid element name");
      end Check;

   begin
      Gen.Generator.Initialize (G, To_UString (C), False);
      A.Read_Model (G.Get_Parameter (Gen.Configs.GEN_UML_DIR) & "/Dynamo.xmi", "", G);

      --  ArgoUML Integer DataType
      Check ("default-uml14.xmi", "Integer",
             "-84-17--56-5-43645a83:11466542d86:-8000:000000000000087C");
      --  ArgoUML String DataType
      Check ("default-uml14.xmi", "String",
             "-84-17--56-5-43645a83:11466542d86:-8000:000000000000087E");
      --  ArgoUML documentation TagDefinition
      Check ("default-uml14.xmi", "documentation",
             ".:000000000000087C");
      --  ArgoUML type Stereotype
      Check ("default-uml14.xmi", "type",
             ".:0000000000000842");

      --  Persistence Table Stereotype
      Check ("Dynamo.xmi", "Table",
             "127-0-1-1--44304ba0:139c0f2a59c:-8000:0000000000001D4F");
      Check ("Dynamo.xmi", "PK",
             "127-0-1-1--44304ba0:139c0f2a59c:-8000:0000000000001D50");
      Check ("Dynamo.xmi", "FK",
             "127-0-1-1--44304ba0:139c0f2a59c:-8000:0000000000001F70");
      Check ("Dynamo.xmi", "Bean",
             "127-0-1-1--44304ba0:139c0f2a59c:-8000:0000000000001F72");

   end Test_Read_XMI;

   --  ------------------------------
   --  Test searching an XMI element by using a qualified name.
   --  ------------------------------
   procedure Test_Find_Element (T : in out Test) is
      A : Artifact;
      G : Gen.Generator.Handler;
      C : constant String := Util.Tests.Get_Parameter ("config_dir", "config");

      use Gen.Model.XMI;

      function Find_Stereotype is
        new Gen.Model.XMI.Find_Element (Element_Type        => Stereotype_Element,
                                        Element_Type_Access => Stereotype_Element_Access);

   begin
      Gen.Generator.Initialize (G, To_UString (C), False);
      A.Read_Model (G.Get_Parameter (Gen.Configs.GEN_UML_DIR) & "/Dynamo.xmi", "", G);

      declare
         S : Gen.Model.XMI.Stereotype_Element_Access;
      begin
         S := Find_Stereotype (A.Nodes, "Dynamo.xmi", "ADO.Table", Gen.Model.XMI.BY_NAME);
         T.Assert (S /= null, "Stereotype not found");

         S := Find_Stereotype (A.Nodes, "Dynamo.xmi", "ADO.PK", Gen.Model.XMI.BY_NAME);
         T.Assert (S /= null, "Stereotype not found");

         S := Find_Stereotype (A.Nodes, "Dynamo.xmi", "ADO.FK", Gen.Model.XMI.BY_NAME);
         T.Assert (S /= null, "Stereotype not found");

         S := Find_Stereotype (A.Nodes, "Dynamo.xmi", "ADO.DataModel", Gen.Model.XMI.BY_NAME);
         T.Assert (S /= null, "Stereotype not found");

         S := Find_Stereotype (A.Nodes, "Dynamo.xmi", "AWA.Bean", Gen.Model.XMI.BY_NAME);
         T.Assert (S /= null, "Stereotype not found");
      end;
   end Test_Find_Element;

   --  Test searching an XMI Tag definition element by using its name.
   procedure Test_Find_Tag_Definition (T : in out Test) is
      A : Artifact;
      G : Gen.Generator.Handler;
      C : constant String := Util.Tests.Get_Parameter ("config_dir", "config");

      use Gen.Model.XMI;

      function Find_Tag_Definition is
        new Gen.Model.XMI.Find_Element (Element_Type        => Tag_Definition_Element,
                                        Element_Type_Access => Tag_Definition_Element_Access);

   begin
      Gen.Generator.Initialize (G, To_UString (C), False);
      A.Read_Model (G.Get_Parameter (Gen.Configs.GEN_UML_DIR) & "/Dynamo.xmi", "", G);

      declare
         Tag : Tag_Definition_Element_Access;
      begin
         Tag := Find_Tag_Definition (A.Nodes, "Dynamo.xmi", "ADO.Table.@dynamo.table.hasList",
                                     Gen.Model.XMI.BY_NAME);
         T.Assert (Tag /= null, "Tag definition not found");
      end;
   end Test_Find_Tag_Definition;

end Gen.Artifacts.XMI.Tests;
