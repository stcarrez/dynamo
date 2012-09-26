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

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.XMI");

   use type Gen.Model.XMI.Package_Element_Access;

   type XMI_Fields is (FIELD_NAME,
                       FIELD_ID,
                       FIELD_ID_REF,
                       FIELD_VALUE,
                       FIELD_HREF,

                       FIELD_CLASS_NAME, FIELD_CLASS_ID,

                       FIELD_STEREOTYPE,
                       FIELD_STEREOTYPE_NAME,
                       FIELD_STEREOTYPE_ID,
                       FIELD_STEREOTYPE_HREF,

                       FIELD_ATTRIBUTE_NAME,
                       FIELD_ATTRIBUTE_ID,
                       FIELD_ATTRIBUTE,

                       FIELD_PACKAGE_ID,
                       FIELD_PACKAGE_NAME,
                       FIELD_PACKAGE_END,
                       FIELD_CLASS_VISIBILITY,
                       FIELD_DATA_TYPE,
                       FIELD_DATA_TYPE_HREF,
                       FIELD_ENUM_DATA_TYPE,
                       FIELD_CLASS_END,
                       FIELD_MULTIPLICITY_LOWER,
                       FIELD_MULTIPLICITY_UPPER,
                       FIELD_ASSOCIATION_AGGREGATION,
                       FIELD_ASSOCIATION_NAME,
                       FIELD_ASSOCIATION_VISIBILITY,
                       FIELD_ASSOCIATION_END_ID,
                       FIELD_OPERATION_NAME,

                       FIELD_COMMENT,

                       FIELD_TAG_DEFINITION,
                       FIELD_TAGGED_VALUE,

                       FIELD_ENUMERATION,
                       FIELD_ENUMERATION_LITERAL);

   type XMI_Info is record
      Model              : Gen.Model.XMI.Model_Map_Access;
      Indent : Natural := 1;
      Class_Element    : Gen.Model.XMI.Class_Element_Access;
      Class_Name       : Util.Beans.Objects.Object;
      Class_Visibility : Util.Beans.Objects.Object;
      Class_Id         : Util.Beans.Objects.Object;

      Package_Element    : Gen.Model.XMI.Package_Element_Access;
      Package_Id         : Util.Beans.Objects.Object;

      Attr_Id            : Util.Beans.Objects.Object;
      Attr_Element       : Gen.Model.XMI.Attribute_Element_Access;
      Multiplicity_Lower : Integer := 0;
      Multiplicity_Upper : Integer := 0;

      Name               : Util.Beans.Objects.Object;
      Id                 : Util.Beans.Objects.Object;
      Ref_Id             : Util.Beans.Objects.Object;
      Value              : Util.Beans.Objects.Object;
      Href               : Util.Beans.Objects.Object;

      Stereotype_Id      : Util.Beans.Objects.Object;
      Data_Type          : Gen.Model.XMI.Data_Type_Element_Access;
      Enumeration        : Gen.Model.XMI.Enum_Element_Access;
      Tag_Definition     : Gen.Model.XMI.Tag_Definition_Element_Access;

      Stereotype         : Gen.Model.XMI.Stereotype_Element_Access;
      Tagged_Value       : Gen.Model.XMI.Tagged_Value_Element_Access;
      Comment            : Gen.Model.XMI.Comment_Element_Access;
   end record;
   type XMI_Access is access all XMI_Info;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object);

   procedure Add_Element (P    : in out XMI_Info;
                          Item : in Gen.Model.XMI.Model_Element_Access) is
   begin
      Item.Name   := Util.Beans.Objects.To_Unbounded_String (P.Name);
      Item.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);
      P.Model.Insert (Item.XMI_Id, Item);
      P.Id   := Util.Beans.Objects.Null_Object;
      P.Name := Util.Beans.Objects.Null_Object;
   end Add_Element;

   use type Gen.Model.XMI.Attribute_Element_Access;
   use type Gen.Model.XMI.Class_Element_Access;
   use type Gen.Model.XMI.Package_Element_Access;

   procedure Add_Tagged_Value (P : in out XMI_Info) is
      Tagged_Value : constant Model.XMI.Tagged_Value_Element_Access
        := new Model.XMI.Tagged_Value_Element (P.Model);
   begin
      Log.Info ("Add tag {0} - {1}",
                Util.Beans.Objects.To_String (P.Id),
                Util.Beans.Objects.To_String (P.Ref_Id));

      Tagged_Value.Value  := Util.Beans.Objects.To_Unbounded_String (P.Value);
      if not Util.Beans.Objects.Is_Null (P.Ref_Id) then
         Tagged_Value.Ref_Id := Util.Beans.Objects.To_Unbounded_String (P.Ref_Id);
      else
         Tagged_Value.Ref_Id := Util.Beans.Objects.To_Unbounded_String (P.Href);
      end if;
      Tagged_Value.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);
      P.Model.Insert (Tagged_Value.XMI_Id, Tagged_Value.all'Access);
      if P.Attr_Element /= null then
         P.Attr_Element.Tagged_Values.Append (Tagged_Value.all'Access);
      elsif P.Class_Element /= null then
         P.Class_Element.Tagged_Values.Append (Tagged_Value.all'Access);
      elsif P.Package_Element /= null then
         P.Package_Element.Tagged_Values.Append (Tagged_Value.all'Access);
      else
         Log.Error ("Tagged value ignored");
      end if;
   end Add_Tagged_Value;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            P.Name := Value;

         when FIELD_ID =>
            P.Id := Value;

         when FIELD_ID_REF =>
            P.Ref_Id := Value;

         when FIELD_VALUE =>
            P.Value := Value;

         when FIELD_HREF =>
            P.Href := Value;

         when FIELD_CLASS_NAME =>
            P.Class_Element := new Gen.Model.XMI.Class_Element (P.Model);
            P.Class_Element.Set_Name (Value);

         when FIELD_CLASS_VISIBILITY =>
            P.Class_Visibility := Value;

         when FIELD_CLASS_ID =>
            P.Class_Id := Value;

         when FIELD_CLASS_END =>
            if P.Class_Element /= null then
               P.Class_Element.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Class_Id);
               Log.Info ("Adding class {0}", P.Class_Element.XMI_Id);
               P.Model.Insert (P.Class_Element.XMI_Id, P.Class_Element.all'Access);
               P.Class_Element := null;
            end if;

         when FIELD_ATTRIBUTE_ID =>
            P.Attr_Id := Value;

         when FIELD_ATTRIBUTE_NAME =>
            P.Attr_Element := new Gen.Model.XMI.Attribute_Element (P.Model);
            P.Attr_Element.Set_Name (Value);

         when FIELD_ATTRIBUTE =>
            P.Attr_Element.Set_XMI_Id (P.Attr_Id);
            P.Model.Insert (P.Attr_Element.XMI_Id, P.Attr_Element.all'Access);
            if P.Class_Element /= null then
               P.Class_Element.Elements.Append (P.Attr_Element.all'Access);
            end if;
            P.Attr_Element := null;

         when FIELD_MULTIPLICITY_LOWER =>
            P.Multiplicity_Lower := Util.Beans.Objects.To_Integer (Value);

         when FIELD_MULTIPLICITY_UPPER =>
            P.Multiplicity_Upper := Util.Beans.Objects.To_Integer (Value);

         when FIELD_ENUM_DATA_TYPE =>
            --           Print (P.Indent, "  enum-type:" & Util.Beans.Objects.To_String (Value));
            null;

      when FIELD_OPERATION_NAME =>
            --              P.Operation_Name := Value;
            null;


      when FIELD_ASSOCIATION_NAME =>
            --           Print (P.Indent, "association " & Util.Beans.Objects.To_String (Value));
            null;

      when FIELD_ASSOCIATION_VISIBILITY =>
            --           Print (P.Indent, "visibility: " & Util.Beans.Objects.To_String (Value));
            null;

      when FIELD_ASSOCIATION_AGGREGATION =>
            --           Print (P.Indent, "   aggregate: " & Util.Beans.Objects.To_String (Value));
            null;

      when FIELD_ASSOCIATION_END_ID =>
            --           Print (P.Indent, "   end-id: " & Util.Beans.Objects.To_String (Value));
            null;

         when FIELD_PACKAGE_ID =>
            P.Package_Id := Value;

         when FIELD_PACKAGE_NAME =>
            declare
               Parent : constant Gen.Model.XMI.Package_Element_Access := P.Package_Element;
            begin
               if Parent /= null then
                  Parent.Set_XMI_Id (P.Package_Id);
               end if;
               P.Package_Element := new Gen.Model.XMI.Package_Element (P.Model);
               P.Package_Element.Set_Name (Value);
               P.Package_Element.Parent := Parent;
            end;

         when FIELD_PACKAGE_END =>
            P.Package_Element.Set_XMI_Id (P.Package_Id);
            P.Model.Include (P.Package_Element.XMI_Id, P.Package_Element.all'Access);
            P.Package_Element := P.Package_Element.Parent;
            if P.Package_Element /= null then
               P.Package_Id := Util.Beans.Objects.To_Object (P.Package_Element.XMI_Id);
            end if;

            --  Tagged value associated with an attribute, operation, class, package.
         when FIELD_TAGGED_VALUE =>
            Add_Tagged_Value (P);

            --  Data type mapping.
         when FIELD_DATA_TYPE =>
            if P.Attr_Element /= null then
               P.Data_Type := new Gen.Model.XMI.Data_Type_Element (P.Model);
               P.Data_Type.Set_Name (P.Name);
               P.Data_Type.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);
               P.Model.Insert (P.Data_Type.XMI_Id, P.Data_Type.all'Access);
            end if;

         when FIELD_DATA_TYPE_HREF =>
            if P.Attr_Element /= null then
               P.Attr_Element.Ref_Id := Util.Beans.Objects.To_Unbounded_String (Value);
            end if;

            --  Enumeration mapping.
         when FIELD_ENUMERATION =>
            P.Enumeration := new Gen.Model.XMI.Enum_Element (P.Model);
            P.Enumeration.Set_Name (Value);
            P.Enumeration.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);
            P.Model.Insert (P.Enumeration.XMI_Id, P.Enumeration.all'Access);

         when FIELD_ENUMERATION_LITERAL =>
            P.Enumeration.Add_Literal (P.Id, P.Name);

         when FIELD_STEREOTYPE_NAME =>
            P.Stereotype := new Gen.Model.XMI.Stereotype_Element (P.Model);
            P.Stereotype.Set_Name (Value);

         when FIELD_STEREOTYPE_ID =>
            P.Stereotype_Id := Value;

            --  Stereotype mapping.
         when FIELD_STEREOTYPE =>
            if not Util.Beans.Objects.Is_Null (P.Stereotype_Id) then
               P.Stereotype.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Stereotype_Id);
               P.Model.Insert (P.Stereotype.XMI_Id, P.Stereotype.all'Access);
            end if;

         when FIELD_STEREOTYPE_HREF =>

            null;

            --  Tag definition mapping.
         when FIELD_TAG_DEFINITION =>
            P.Tag_Definition := new Gen.Model.XMI.Tag_Definition_Element (P.Model);
            P.Tag_Definition.Set_Name (P.Name);
            P.Tag_Definition.XMI_Id := Util.Beans.Objects.To_Unbounded_String (P.Id);
            P.Model.Insert (P.Tag_Definition.XMI_Id, P.Tag_Definition.all'Access);

            --  Comment mapping.
         when FIELD_COMMENT =>
            P.Comment := new Gen.Model.XMI.Comment_Element (P.Model);
            P.Comment.Set_Name (P.Name);
            P.Comment.XMI_Id  := Util.Beans.Objects.To_Unbounded_String (P.Id);
            P.Comment.Comment := Util.Beans.Objects.To_Unbounded_String (P.Value);
            P.Comment.Ref_Id  := Util.Beans.Objects.To_Unbounded_String (P.Ref_Id);
            P.Model.Insert (P.Comment.XMI_Id, P.Comment.all'Access);

      end case;
   exception
      when E : others =>
         Log.Error ("Exception on " & XMI_Fields'Image (Field), E);
   end Set_Member;

   package XMI_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => XMI_Info,
                                               Element_Type_Access => XMI_Access,
                                               Fields              => XMI_Fields,
                                               Set_Member          => Set_Member);

   type Mapper is new XMI_Mapper.Mapper with null record;

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
--        Iterate (Gen.Model.Packages.Model_Definition (Model), Node, "query-mapping");
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

   --  ------------------------------
   --  Read the UML configuration files that define the pre-defined types, stereotypes
   --  and other components used by a model.  These files are XMI files as well.
   --  All the XMI files in the UML config directory are read.
   --  ------------------------------
   procedure Read_UML_Configuration (Handler : in out Artifact;
                                     Context : in out Generator'Class) is
      use Ada.Directories;

      Path    : constant String := Context.Get_Parameter (Gen.Configs.GEN_UML_DIR);
      Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
      Search  : Search_Type;
      Ent     : Directory_Entry_Type;
   begin
      Log.Info ("Reading the UML configuration files from {0}", Path);

      Start_Search (Search, Directory => Path, Pattern => "*.xmi", Filter => Filter);

      --  Collect the files in the vector array.
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);

         Handler.Read_Model (Full_Name (Ent), Context);
      end loop;
   end Read_UML_Configuration;

   --  Read the UML/XMI model file.
   procedure Read_Model (Handler : in out Artifact;
                         File    : in String;
                         Context : in out Generator'Class) is

      procedure Read (Key   : in Ada.Strings.Unbounded.Unbounded_String;
                      Model : in out Gen.Model.XMI.Model_Map.Map) is
         Info   : aliased XMI_Info;
         Reader : Util.Serialize.IO.XML.Parser;
      begin
         Info.Model := Model'Unchecked_Access;
         Reader.Add_Mapping ("XMI", XMI_Mapping'Access);
         Reader.Dump (Log);
         XMI_Mapper.Set_Context (Reader, Info'Unchecked_Access);
         Reader.Parse (File);
      end Read;

      UML  : Gen.Model.XMI.Model_Map.Map;
      Name : constant Ada.Strings.Unbounded.Unbounded_String
        := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Simple_Name (File));
   begin
      Log.Info ("Reading XMI {0}", File);

      if not Handler.Has_Config then
         Handler.Has_Config := True;
         Handler.Read_UML_Configuration (Context);
      end if;
      Handler.Nodes.Include (Name, UML);
      Handler.Nodes.Update_Element (Handler.Nodes.Find (Name),
                                    Read'Access);
      Gen.Model.XMI.Reconcile (Handler.Nodes);
   end Read_Model;

begin

   --  Define the XMI mapping.
   XMI_Mapping.Add_Mapping ("**/Package/@name", FIELD_PACKAGE_NAME);
   XMI_Mapping.Add_Mapping ("**/Package/@xmi.id", FIELD_PACKAGE_ID);
   XMI_Mapping.Add_Mapping ("**/Package", FIELD_PACKAGE_END);

   XMI_Mapping.Add_Mapping ("**/Class/@name", FIELD_CLASS_NAME);
   XMI_Mapping.Add_Mapping ("**/Class/@xmi.id", FIELD_CLASS_ID);
   XMI_Mapping.Add_Mapping ("**/Class/@visibility", FIELD_CLASS_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/Class", FIELD_CLASS_END);

   --  Class attribute mapping.
   XMI_Mapping.Add_Mapping ("**/Attribute/@name", FIELD_ATTRIBUTE_NAME);
   XMI_Mapping.Add_Mapping ("**/Attribute/@xmi.id", FIELD_ATTRIBUTE_ID);
   XMI_Mapping.Add_Mapping ("**/Attribute", FIELD_ATTRIBUTE);

--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/Stereotype/@href",
--                              FIELD_STEREOTYPE);
--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/DataType/@href",
--                              FIELD_DATA_TYPE);
--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/Enumeration/@href",
--                              FIELD_ENUM_DATA_TYPE);
--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/MultiplicityRange/@lower",
--                              FIELD_MULTIPLICITY_LOWER);
--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Attribute/*/MultiplicityRange/@upper",
--                              FIELD_MULTIPLICITY_UPPER);
--     XMI_Mapping.Add_Mapping ("Package/*/Class/*/Operation/@name",
--                              FIELD_OPERATION_NAME);
--     XMI_Mapping.Add_Mapping ("Package/*/Association/*/@name",
--                              FIELD_ASSOCIATION_NAME);
--     XMI_Mapping.Add_Mapping ("Package/*/Association/*/@visibility",
--                              FIELD_ASSOCIATION_VISIBILITY);
--     XMI_Mapping.Add_Mapping ("Package/*/Association/*/@aggregation",
--                              FIELD_ASSOCIATION_AGGREGATION);
--     XMI_Mapping.Add_Mapping ("Package/*/Association/*/AssociationEnd.participant/Class/@xmi.idref",
--                              FIELD_ASSOCIATION_END_ID);

   --  Comment mapping.
   XMI_Mapping.Add_Mapping ("**/Comment/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("**/Comment/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/Comment/@body", FIELD_VALUE);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotated/Class/xmi.idref", FIELD_ID_REF);

   --  Tagged value mapping.
   XMI_Mapping.Add_Mapping ("**/TaggedValue/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.dataValue", FIELD_VALUE);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/@xmi.idref", FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/TagDefinition/@xmi.idref", FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/TagDefinition/@href", FIELD_HREF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue", FIELD_TAGGED_VALUE);

   --  Tag definition mapping.
   XMI_Mapping.Add_Mapping ("**/TagDefinition/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/TagDefinition/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("**/TagDefinition", FIELD_TAG_DEFINITION);

   --  Stereotype mapping.
   XMI_Mapping.Add_Mapping ("**/Stereotype/@href", FIELD_STEREOTYPE_HREF);
   XMI_Mapping.Add_Mapping ("**/Stereotype/@xmi.id", FIELD_STEREOTYPE_ID);
   XMI_Mapping.Add_Mapping ("**/Stereotype/@name", FIELD_STEREOTYPE_NAME);
   XMI_Mapping.Add_Mapping ("**/Stereotype", FIELD_STEREOTYPE);

   --  Enumeration mapping.
   XMI_Mapping.Add_Mapping ("**/Enumeration/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/Enumeration/@name", FIELD_ENUMERATION);
   XMI_Mapping.Add_Mapping ("**/Enumeration/Enumeration.literal/EnumerationLiteral/@xmi.id",
                            FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/Enumeration/Enumeration.literal/EnumerationLiteral/@name",
                            FIELD_NAME);

   --  Data type mapping.
   XMI_Mapping.Add_Mapping ("**/DataType/@xmi", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/DataType/@name", FIELD_NAME);
   XMI_Mapping.Add_Mapping ("**/DataType", FIELD_DATA_TYPE);
   XMI_Mapping.Add_Mapping ("**/DataType/@href", FIELD_DATA_TYPE_HREF);

end Gen.Artifacts.XMI;
