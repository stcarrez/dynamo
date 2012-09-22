-----------------------------------------------------------------------
--  gen-artifacts-query -- Query artifact for Code Generator
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

with Gen.Configs;
with Gen.Utils;
with Gen.Model.Tables;
with Gen.Model.Queries;
with Gen.Model.XMI;

with Util.Log.Loggers;
with Util.Encoders.HMAC.SHA1;

with Util.Beans;
with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;

--  The <b>Gen.Artifacts.Query</b> package is an artifact for the generation of
--  data structures returned by queries.
package body Gen.Artifacts.XMI is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;
   use Gen.Model.Queries;
   use Gen.Configs;

   use type DOM.Core.Node;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.XMIls con");

   type XMI_Fields is (FIELD_NAME,
                       FIELD_ID,
                       FIELD_CLASS_NAME, FIELD_CLASS_ID, FIELD_STEREOTYPE,
                       FIELD_ATTRIBUTE_NAME,
                       FIELD_PACKAGE_NAME,
                       FIELD_PACKAGE_END,
                       FIELD_CLASS_VISIBILITY,
                       FIELD_DATA_TYPE,
                       FIELD_ENUM_DATA_TYPE,
                       FIELD_CLASS_END,
                       FIELD_MULTIPLICITY_LOWER,
                       FIELD_MULTIPLICITY_UPPER,
                       FIELD_ASSOCIATION_AGGREGATION,
                       FIELD_ASSOCIATION_NAME,
                       FIELD_ASSOCIATION_VISIBILITY,
                       FIELD_ASSOCIATION_END_ID,
                       FIELD_OPERATION_NAME,
                       FIELD_COMMENT_NAME,
                       FIELD_COMMENT_BODY,
                       FIELD_COMMENT_CLASS_ID,

                       FIELD_TAG_DEFINITION,

                       FIELD_ENUMERATION,
                       FIELD_ENUMERATION_LITERAL,

                       FIELD_TAGGED_VALUE_TYPE,
                       FIELD_TAGGED_VALUE_VALUE);

   type XMI_Info is record
      Indent : Natural := 1;
      Class_Element    : Gen.Model.XMI.Class_Element_Access;
      Class_Name       : Util.Beans.Objects.Object;
      Class_Visibility : Util.Beans.Objects.Object;
      Class_Id         : Util.Beans.Objects.Object;

      Attr_Name          : Util.Beans.Objects.Object;
      Multiplicity_Lower : Integer := 0;
      Multiplicity_Upper : Integer := 0;

      Name               : Util.Beans.Objects.Object;
      Id                 : Util.Beans.Objects.Object;

      Data_Type          : Gen.Model.XMI.Data_Type_Element_Access;
      Enumeration        : Gen.Model.XMI.Enum_Element_Access;
      Tag_Definition     : Gen.Model.XMI.Tag_Definition_Access;
   end record;
   type XMI_Access is access all XMI_Info;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            P.Name := Value;

         when FIELD_ID =>
            P.Id := Value;

      when FIELD_CLASS_NAME =>
         P.Class_Element := new Gen.Model.XMI.Class_Element;
         P.Class_Name := Value;

      when FIELD_CLASS_VISIBILITY =>
         P.Class_Visibility := Value;

      when FIELD_CLASS_ID =>
         P.Class_Id := Value;

      when FIELD_CLASS_END =>
         P.Indent := P.Indent - 2;

      when FIELD_ATTRIBUTE_NAME =>
         P.Attr_Name := Value;

      when FIELD_MULTIPLICITY_LOWER =>
         P.Multiplicity_Lower := Util.Beans.Objects.To_Integer (Value);

      when FIELD_MULTIPLICITY_UPPER =>
         P.Multiplicity_Upper := Util.Beans.Objects.To_Integer (Value);

      when FIELD_ENUM_DATA_TYPE =>
         Print (P.Indent, "  enum-type:" & Util.Beans.Objects.To_String (Value));

      when FIELD_OPERATION_NAME =>
         P.Operation_Name := Value;

      when FIELD_ASSOCIATION_NAME =>
         Print (P.Indent, "association " & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_VISIBILITY =>
         Print (P.Indent, "visibility: " & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_AGGREGATION =>
         Print (P.Indent, "   aggregate: " & Util.Beans.Objects.To_String (Value));

      when FIELD_ASSOCIATION_END_ID =>
         Print (P.Indent, "   end-id: " & Util.Beans.Objects.To_String (Value));

      when FIELD_PACKAGE_NAME =>
         Print (P.Indent, "package " & Util.Beans.Objects.To_String (Value));
         P.Indent := P.Indent + 2;

      when FIELD_PACKAGE_END =>
         P.Indent := P.Indent - 2;

      when FIELD_TAGGED_VALUE_VALUE =>
         Print (P.Indent, "tag-value: " & Util.Beans.Objects.To_String (Value));

      when FIELD_TAGGED_VALUE_TYPE =>
         Print (P.Indent, "tag-type: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_NAME =>
         Print (P.Indent, "Comment: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_BODY =>
         Print (P.Indent, "   text: " & Util.Beans.Objects.To_String (Value));

      when FIELD_COMMENT_CLASS_ID =>
         Print (P.Indent, "   for-id: " & Util.Beans.Objects.To_String (Value));

            --  Data type mapping.
         when FIELD_DATA_TYPE =>
            P.Data_Type := new Gen.Model.XMI.Data_Type_Element;
            P.Data_Type.Name   := Util.Beans.Objects.To_Unbounded_String (P.Name);
            P.Data_Type.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);

            --  Enumeration mapping.
         when FIELD_ENUMERATION =>
            P.Enumeration := new Gen.Model.XMI.Enum_Element;
            P.Enumeration.Name := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_ENUMERATION_LITERAL =>
            P.Enumeration.Add_Literal (P.Id, P.Name);

         when FIELD_STEREOTYPE =>
            P.Stereotype := new Gen.Model.XMI.Stereotype_Element;
            P.Stereotype.Name   := Util.Beans.Objects.To_Unbounded_String (P.Name);
            P.Stereotype.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);

         when FIELD_TAG_DEFINITION =>
            P.Tag_Definition := new Gen.Model.XMI.Tag_Definition_Element;
            P.Tag_Definition.Name   := Util.Beans.Objects.To_Unbounded_String (P.Name);
            P.Tag_Definition.XMI_ID := Util.Beans.Objects.To_Unbounded_String (P.Id);

      end case;
   end Set_Member;

   package XMI_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => XMI_Info,
                                               Element_Type_Access => XMI_Access,
                                               Fields              => XMI_Fields,
                                               Set_Member          => Set_Member);

   XMI_Mapping        : aliased XMI_Mapper.Mapper;

   --  ------------------------------
   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   --  ------------------------------
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is

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
                      Context : in out Generator'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for query");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE);
      end if;
   end Prepare;

begin

   --  Define the XMI mapping.
   XMI_Mapping.Add_Mapping ("Package/*/@name",
                            FIELD_PACKAGE_NAME);
   XMI_Mapping.Add_Mapping ("Package",
                            FIELD_PACKAGE_END);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/@name",
                            FIELD_CLASS_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/@xmi.id",
                            FIELD_CLASS_ID);
   XMI_Mapping.Add_Mapping ("Package/*/Class",
                            FIELD_CLASS_END);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/@visibility",
                            FIELD_CLASS_VISIBILITY);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Stereotype/@href",
                            FIELD_STEREOTYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/@name",
                            FIELD_ATTRIBUTE_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/Stereotype/@href",
                            FIELD_STEREOTYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/TaggedValue.dataValue",
                            FIELD_TAGGED_VALUE_VALUE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/TaggedValue.dataValue/*/TagDefinition/@href",
                            FIELD_TAGGED_VALUE_TYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/DataType/@href",
                            FIELD_DATA_TYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/Enumeration/@href",
                            FIELD_ENUM_DATA_TYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/TaggedValue.dataValue",
                            FIELD_TAGGED_VALUE_VALUE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/TaggedValue.type/*/TagDefinition/@href",
                            FIELD_TAGGED_VALUE_TYPE);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/MultiplicityRange/@lower",
                            FIELD_MULTIPLICITY_LOWER);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/MultiplicityRange/@upper",
                            FIELD_MULTIPLICITY_UPPER);
   XMI_Mapping.Add_Mapping ("Package/*/Class/*/Operation/@name",
                            FIELD_OPERATION_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Association/*/@name",
                            FIELD_ASSOCIATION_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Association/*/@visibility",
                            FIELD_ASSOCIATION_VISIBILITY);
   XMI_Mapping.Add_Mapping ("Package/*/Association/*/@aggregation",
                            FIELD_ASSOCIATION_AGGREGATION);
   XMI_Mapping.Add_Mapping ("Package/*/Association/*/AssociationEnd.participant/Class/@xmi.idref",
                            FIELD_ASSOCIATION_END_ID);
   XMI_Mapping.Add_Mapping ("TagDefinition/@name",
                            FIELD_TAG_DEFINITION_NAME);
   XMI_Mapping.Add_Mapping ("TagDefinition/@xm.id",
                            FIELD_TAG_DEFINITION_ID);
   XMI_Mapping.Add_Mapping ("Package/*/Comment/@name",
                            FIELD_COMMENT_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Comment/@body",
                            FIELD_COMMENT_BODY);
   XMI_Mapping.Add_Mapping ("Package/*/Comment/Comment.annotatedElement/*/@xmi.idref",
                            FIELD_COMMENT_CLASS_ID);

   --  Tag definition mapping.
   XMI_Mapping.Add_Mapping ("Package/*/TagDefinition/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Package/*/TagDefinition/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/TagDefinition", FIELD_TAG_DEFINITION);
   XMI_Mapping.Add_Mapping ("TagDefinition/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("TagDefinition/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("TagDefinition", FIELD_TAG_DEFINITION);

   --  Stereotype mapping.
   XMI_Mapping.Add_Mapping ("Package/*/Stereotype/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Package/*/Stereotype/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/Stereotype", FIELD_STEREOTYPE);
   XMI_Mapping.Add_Mapping ("Stereotype/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Stereotype/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("Stereotype", FIELD_STEREOTYPE);

   --  Enumeration mapping.
   XMI_Mapping.Add_Mapping ("Package/*/Enumeration/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Package/*/Enumeration/@name", FIELD_ENUMERATION);
   XMI_Mapping.Add_Mapping ("Package/*/Enumeration/Enumeration.literal/EnumerationLiteral/@xmi.id",
                            FIELD_ID);
   XMI_Mapping.Add_Mapping ("Package/*/Enumeration/Enumeration.literal/EnumerationLiteral/@name",
                            FIELD_NAME);
   XMI_Mapping.Add_Mapping ("Enumeration/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Enumeration/@name", FIELD_ENUMERATION);
   XMI_Mapping.Add_Mapping ("Enumeration/Enumeration.literal/EnumerationLiteral/@xmi.id",
                            FIELD_ID);
   XMI_Mapping.Add_Mapping ("Enumeration/Enumeration.literal/EnumerationLiteral/@name",
                            FIELD_NAME);

   --  Data type mapping.
   XMI_Mapping.Add_Mapping ("Package/*/DataType/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("Package/*/DataType/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("Package/*/DataType", FIELD_DATA_TYPE);
   XMI_Mapping.Add_Mapping ("DataType/@xmi", FIELD_ID);
   XMI_Mapping.Add_Mapping ("DataType/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("DataType", FIELD_DATA_TYPE);
end Gen.Artifacts.XMI;
