-----------------------------------------------------------------------
--  gen-artifacts-xmi -- UML-XMI artifact for Code Generator
--  Copyright (C) 2012, 2013, 2014, 2015, 2016, 2018, 2021, 2022 Stephane Carrez
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
with Ada.Strings.Fixed;

with Gen.Configs;
with Gen.Utils;
with Gen.Model.Tables;
with Gen.Model.Enums;
with Gen.Model.Mappings;
with Gen.Model.Beans;
with Gen.Model.Operations;
with Gen.Model.Stypes;

with Util.Log.Loggers;
with Util.Strings;
with Util.Strings.Transforms;
with Util.Files;
with Util.Beans;
with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;
with Util.Processes;
with Util.Streams.Pipes;
with Util.Streams.Buffered;

package body Gen.Artifacts.XMI is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Configs;

   package UBO renames Util.Beans.Objects;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.XMI");

   --  Get the visibility from the XMI visibility value.
   function Get_Visibility (Value : in UBO.Object) return Model.XMI.Visibility_Type;

   --  Get the changeability from the XMI visibility value.
   function Get_Changeability (Value : in UBO.Object)
                               return Model.XMI.Changeability_Type;

   --  Get the parameter kind from the XMI parameter kind value.
   function Get_Parameter_Type (Value : in UBO.Object)
                                return Model.XMI.Parameter_Type;

   procedure Iterate_For_Table is
     new Gen.Model.XMI.Iterate_Elements (T => Gen.Model.Tables.Table_Definition'Class);

   procedure Iterate_For_Bean is
     new Gen.Model.XMI.Iterate_Elements (T => Gen.Model.Beans.Bean_Definition'Class);

   procedure Iterate_For_Package is
     new Gen.Model.XMI.Iterate_Elements (T => Gen.Model.Packages.Package_Definition'Class);

   procedure Iterate_For_Enum is
     new Gen.Model.XMI.Iterate_Elements (T => Gen.Model.Enums.Enum_Definition'Class);

   procedure Iterate_For_Operation is
     new Gen.Model.XMI.Iterate_Elements (T => Gen.Model.Operations.Operation_Definition'Class);

   function Find_Stereotype is
     new Gen.Model.XMI.Find_Element (Element_Type        => Model.XMI.Stereotype_Element,
                                     Element_Type_Access => Model.XMI.Stereotype_Element_Access);

   function Find_Tag_Definition is
     new Gen.Model.XMI.Find_Element
       (Element_Type        => Model.XMI.Tag_Definition_Element,
        Element_Type_Access => Model.XMI.Tag_Definition_Element_Access);

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
                       FIELD_ATTRIBUTE_VISIBILITY,
                       FIELD_ATTRIBUTE_CHANGEABILITY,
                       FIELD_ATTRIBUTE_INITIAL_VALUE,
                       FIELD_ATTRIBUTE,

                       FIELD_MULTIPLICITY_UPPER,
                       FIELD_MULTIPLICITY_LOWER,

                       FIELD_PACKAGE_ID,
                       FIELD_PACKAGE_NAME,
                       FIELD_PACKAGE_END,
                       FIELD_CLASS_VISIBILITY,
                       FIELD_DATA_TYPE,
                       FIELD_DATA_TYPE_NAME,
                       FIELD_DATA_TYPE_HREF,
                       FIELD_CLASS_END,
                       FIELD_ASSOCIATION_NAME,
                       FIELD_ASSOCIATION_ID,
                       FIELD_ASSOCIATION,
                       FIELD_ASSOCIATION_CLASS_ID,

                       FIELD_CLASSIFIER_HREF,

                       FIELD_GENERALIZATION_ID,
                       FIELD_GENERALIZATION_CHILD_ID,
                       FIELD_GENERALIZATION_PARENT_ID,
                       FIELD_GENERALIZATION_END,

                       FIELD_ASSOCIATION_END_ID,
                       FIELD_ASSOCIATION_END_NAME,
                       FIELD_ASSOCIATION_END_VISIBILITY,
                       FIELD_ASSOCIATION_END_NAVIGABLE,
                       FIELD_ASSOCIATION_END,

                       FIELD_OPERATION_ID,
                       FIELD_OPERATION_NAME,
                       FIELD_OPERATION_END,
                       FIELD_PARAMETER_ID,
                       FIELD_PARAMETER_NAME,
                       FIELD_PARAMETER_KIND,
                       FIELD_PARAMETER_END,

                       FIELD_COMMENT,
                       FIELD_COMMENT_ID,

                       FIELD_TAG_DEFINITION,
                       FIELD_TAG_DEFINITION_ID,
                       FIELD_TAG_DEFINITION_NAME,

                       FIELD_TAGGED_ID,
                       FIELD_TAGGED_VALUE,

                       FIELD_ENUMERATION,
                       FIELD_ENUMERATION_LITERAL,
                       FIELD_ENUMERATION_LITERAL_END,
                       FIELD_ENUMERATION_HREF);

   type XMI_Info is record
      Model                : Gen.Model.XMI.Model_Map_Access;
      Default_Type         : UString;
      File                 : UString;
      Parser               : access Util.Serialize.IO.XML.Parser'Class;
      Profiles             : access Util.Strings.Sets.Set;
      Is_Profile           : Boolean := False;

      Class_Element        : Gen.Model.XMI.Class_Element_Access;
      Class_Name           : UBO.Object;
      Class_Visibility     : Gen.Model.XMI.Visibility_Type := Gen.Model.XMI.VISIBILITY_PUBLIC;
      Class_Id             : UBO.Object;

      --  UML Generalization.
      Child_Id             : UBO.Object;
      Parent_Id            : UBO.Object;
      Generalization_Id    : UBO.Object;
      Generalization       : Gen.Model.XMI.Generalization_Element_Access;

      Package_Element      : Gen.Model.XMI.Package_Element_Access;
      Package_Id           : UBO.Object;

      Attr_Id              : UBO.Object;
      Attr_Element         : Gen.Model.XMI.Attribute_Element_Access;
      Attr_Visibility      : Gen.Model.XMI.Visibility_Type := Gen.Model.XMI.VISIBILITY_PUBLIC;
      Attr_Changeability   : Gen.Model.XMI.Changeability_Type
        := Gen.Model.XMI.CHANGEABILITY_CHANGEABLE;
      Attr_Value           : UBO.Object;
      Multiplicity_Lower   : Integer := 0;
      Multiplicity_Upper   : Integer := 0;

      Association          : Gen.Model.XMI.Association_Element_Access;
      Assos_End_Element    : Gen.Model.XMI.Association_End_Element_Access;
      Assos_End_Name       : UBO.Object;
      Assos_End_Visibility : Gen.Model.XMI.Visibility_Type := Gen.Model.XMI.VISIBILITY_PUBLIC;
      Assos_End_Navigable  : Boolean := False;

      Operation_Id         : UBO.Object;
      Operation            : Gen.Model.XMI.Operation_Element_Access;
      Parameter            : Gen.Model.XMI.Parameter_Element_Access;
      Parameter_Type       : Gen.Model.XMI.Parameter_Type;

      Name                 : UBO.Object;
      Id                   : UBO.Object;
      Ref_Id               : UBO.Object;
      Value                : UBO.Object;
      Href                 : UBO.Object;
      Tag_Name             : UBO.Object;
      Tagged_Id            : UBO.Object;

      Association_Id       : UBO.Object;
      Stereotype_Id        : UBO.Object;
      Data_Type            : Gen.Model.XMI.Data_Type_Element_Access;
      Enumeration          : Gen.Model.XMI.Enum_Element_Access;
      Enumeration_Literal  : Gen.Model.XMI.Literal_Element_Access;
      Tag_Definition       : Gen.Model.XMI.Tag_Definition_Element_Access;

      Stereotype           : Gen.Model.XMI.Stereotype_Element_Access;
      Tagged_Value         : Gen.Model.XMI.Tagged_Value_Element_Access;
      Comment              : Gen.Model.XMI.Comment_Element_Access;

      Has_Package_Id       : Boolean := False;
      Has_Package_Name     : Boolean := False;
   end record;
   type XMI_Access is access all XMI_Info;

   procedure Add_Tagged_Value (P : in out XMI_Info);

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in UBO.Object);

   --  Set the package name and or XMI id.
   procedure Set_Package (P    : in out XMI_Info;
                          Name : in UBO.Object;
                          Id   : in UBO.Object);

   use type Gen.Model.XMI.Model_Element_Access;
   use type Gen.Model.XMI.Attribute_Element_Access;
   use type Gen.Model.XMI.Class_Element_Access;
   use type Gen.Model.XMI.Package_Element_Access;
   use type Gen.Model.XMI.Tag_Definition_Element_Access;
   use type Gen.Model.XMI.Association_End_Element_Access;
   use type Gen.Model.XMI.Stereotype_Element_Access;
   use type Gen.Model.XMI.Enum_Element_Access;
   use type Gen.Model.XMI.Literal_Element_Access;
   use type Gen.Model.XMI.Comment_Element_Access;
   use type Gen.Model.XMI.Operation_Element_Access;
   use type Gen.Model.XMI.Association_Element_Access;
   use type Gen.Model.XMI.Ref_Type_Element_Access;
   use type Gen.Model.XMI.Data_Type_Element_Access;

   --  ------------------------------
   --  Get the visibility from the XMI visibility value.
   --  ------------------------------
   function Get_Visibility (Value : in UBO.Object)
                            return Model.XMI.Visibility_Type is
      S : constant String := UBO.To_String (Value);
   begin
      if S = "public" then
         return Model.XMI.VISIBILITY_PUBLIC;

      elsif S = "package" then
         return Model.XMI.VISIBILITY_PACKAGE;

      elsif S = "protected" then
         return Model.XMI.VISIBILITY_PROTECTED;

      elsif S = "private" then
         return Model.XMI.VISIBILITY_PRIVATE;

      else
         return Model.XMI.VISIBILITY_PUBLIC;
      end if;
   end Get_Visibility;

   --  ------------------------------
   --  Get the changeability from the XMI visibility value.
   --  ------------------------------
   function Get_Changeability (Value : in UBO.Object)
                            return Model.XMI.Changeability_Type is
      S : constant String := UBO.To_String (Value);
   begin
      if S = "frozen" then
         return Model.XMI.CHANGEABILITY_FROZEN;

      elsif S = "changeable" then
         return Model.XMI.CHANGEABILITY_CHANGEABLE;

      elsif S = "addOnly" then
         return Model.XMI.CHANGEABILITY_INSERT;

      else
         return Model.XMI.CHANGEABILITY_CHANGEABLE;
      end if;
   end Get_Changeability;

   --  ------------------------------
   --  Get the parameter kind from the XMI parameter kind value.
   --  ------------------------------
   function Get_Parameter_Type (Value : in UBO.Object)
                                return Model.XMI.Parameter_Type is
      S : constant String := UBO.To_String (Value);
   begin
      if S = "return" then
         return Model.XMI.PARAM_RETURN;

      elsif S = "in" then
         return Model.XMI.PARAM_IN;

      elsif S = "out" then
         return Model.XMI.PARAM_OUT;

      elsif S = "inout" then
         return Model.XMI.PARAM_INOUT;

      else
         return Model.XMI.PARAM_INOUT;
      end if;
   end Get_Parameter_Type;

   procedure Add_Tagged_Value (P : in out XMI_Info) is
      Id    : constant UString := UBO.To_Unbounded_String (P.Tagged_Id);
      Value : constant UString := UBO.To_Unbounded_String (P.Value);
      Tagged_Value : constant Model.XMI.Tagged_Value_Element_Access
        := new Model.XMI.Tagged_Value_Element (P.Model);
   begin
      Log.Info ("Add tag {0} - {1}", Id, To_String (Value));

      Tagged_Value.Value  := Value;
      if not UBO.Is_Null (P.Ref_Id) then
         Tagged_Value.Set_Reference_Id (UBO.To_String (P.Ref_Id), P.Profiles.all);
         P.Ref_Id := UBO.Null_Object;
      else
         Tagged_Value.Set_Reference_Id (UBO.To_String (P.Href), P.Profiles.all);
         P.Href := UBO.Null_Object;
      end if;
      Tagged_Value.XMI_Id := Id;
      P.Model.Insert (Tagged_Value.XMI_Id, Tagged_Value.all'Access);

      --  Insert the tag value into the current element.
      if P.Data_Type /= null then
         P.Data_Type.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Assos_End_Element /= null then
         P.Assos_End_Element.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Association /= null then
         P.Association.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Attr_Element /= null then
         P.Attr_Element.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Class_Element /= null then
         Log.Info ("Adding in {0}", To_String (P.Class_Element.Name));
         P.Class_Element.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Enumeration_Literal /= null then
         P.Enumeration_Literal.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Enumeration /= null then
         P.Enumeration.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Package_Element /= null then
         P.Package_Element.Tagged_Values.Append (Tagged_Value.all'Access);

      elsif P.Tag_Definition /= null then
         P.Tag_Definition.Tagged_Values.Append (Tagged_Value.all'Access);

      else
         Log.Info ("Tagged value {0} ignored", Id);
      end if;
   end Add_Tagged_Value;

   --  ------------------------------
   --  Set the package name and or XMI id.
   --  ------------------------------
   procedure Set_Package (P    : in out XMI_Info;
                          Name : in UBO.Object;
                          Id   : in UBO.Object) is
      Parent : constant Gen.Model.XMI.Package_Element_Access := P.Package_Element;
   begin
      --  This is a new nested package, create it.
      if Parent /= null and then P.Has_Package_Name and then P.Has_Package_Id then
         P.Package_Element := null;
      end if;
      if P.Package_Element = null then
         P.Package_Element := new Gen.Model.XMI.Package_Element (P.Model);
         P.Package_Element.Set_Location (P.Parser.Get_Location);
         P.Package_Element.Is_Profile := P.Is_Profile;
         if Parent /= null then
            P.Package_Element.Parent := Parent.all'Access;
         else
            P.Package_Element.Parent := null;
         end if;
         P.Has_Package_Name := False;
         P.Has_Package_Id := False;
      end if;
      if not UBO.Is_Null (Id) then
         P.Package_Element.Set_XMI_Id (Id);
         P.Model.Include (P.Package_Element.XMI_Id, P.Package_Element.all'Access);
         P.Has_Package_Id := True;
      end if;
      if not UBO.Is_Null (Name) then
         P.Package_Element.Set_Name (Name);
         P.Has_Package_Name := True;
      end if;
   end Set_Package;

   procedure Set_Member (P     : in out XMI_Info;
                         Field : in XMI_Fields;
                         Value : in UBO.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            P.Name := Value;

         when FIELD_ID =>
            P.Id     := Value;
            P.Ref_Id := UBO.Null_Object;
            P.Href   := UBO.Null_Object;

         when FIELD_ID_REF =>
            P.Ref_Id := Value;

         when FIELD_VALUE =>
            P.Value := Value;

         when FIELD_HREF =>
            P.Href := Value;

         when FIELD_MULTIPLICITY_LOWER =>
            P.Multiplicity_Lower := UBO.To_Integer (Value);

         when FIELD_MULTIPLICITY_UPPER =>
            P.Multiplicity_Upper := UBO.To_Integer (Value);

         when FIELD_CLASS_NAME =>
            P.Class_Element := new Gen.Model.XMI.Class_Element (P.Model);
            P.Class_Element.Set_Name (Value);
            P.Class_Element.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Ref_Id := UBO.Null_Object;
            P.Href   := UBO.Null_Object;

         when FIELD_CLASS_VISIBILITY =>
            P.Class_Visibility := Get_Visibility (Value);

         when FIELD_CLASS_ID =>
            P.Class_Id := Value;

         when FIELD_CLASS_END =>
            if P.Class_Element /= null then
               P.Class_Element.XMI_Id := UBO.To_Unbounded_String (P.Class_Id);
               P.Class_Element.Visibility := P.Class_Visibility;
               Log.Info ("Adding class {0} - {1}",
                         P.Class_Element.XMI_Id, To_String (P.Class_Element.Name));
               P.Model.Insert (P.Class_Element.XMI_Id, P.Class_Element.all'Access);
               if P.Package_Element /= null then
                  P.Package_Element.Classes.Append (P.Class_Element.all'Access);
                  P.Package_Element.Elements.Append (P.Class_Element.all'Access);
                  P.Class_Element.Parent := P.Package_Element.all'Access;
               end if;
               P.Class_Element := null;
               P.Class_Visibility := Gen.Model.XMI.VISIBILITY_PUBLIC;
            end if;

         when FIELD_GENERALIZATION_CHILD_ID =>
            P.Child_Id := Value;

         when FIELD_GENERALIZATION_PARENT_ID =>
            P.Parent_Id := Value;

         when FIELD_GENERALIZATION_ID =>
            P.Generalization_Id := Value;

         when FIELD_GENERALIZATION_END =>
            if not UBO.Is_Null (P.Child_Id)
              and then not UBO.Is_Null (P.Parent_Id)
              and then not UBO.Is_Null (P.Generalization_Id)
            then
               P.Generalization := new Gen.Model.XMI.Generalization_Element (P.Model);
               P.Generalization.Set_XMI_Id (P.Generalization_Id);
               P.Model.Insert (P.Generalization.XMI_Id, P.Generalization.all'Access);
               P.Generalization.Set_Reference_Id (UBO.To_String (P.Parent_Id),
                                                P.Profiles.all);
               P.Generalization.Child_Id := UBO.To_Unbounded_String (P.Child_Id);
            end if;
            P.Child_Id          := UBO.Null_Object;
            P.Generalization_Id := UBO.Null_Object;
            P.Parent_Id         := UBO.Null_Object;

         when FIELD_OPERATION_ID =>
            P.Operation_Id := Value;

         when FIELD_ATTRIBUTE_ID | FIELD_PARAMETER_ID =>
            P.Attr_Id := Value;

         when FIELD_ATTRIBUTE_VISIBILITY =>
            P.Attr_Visibility := Get_Visibility (Value);

         when FIELD_ATTRIBUTE_CHANGEABILITY =>
            P.Attr_Changeability := Get_Changeability (Value);

         when FIELD_ATTRIBUTE_NAME =>
            P.Attr_Element := new Gen.Model.XMI.Attribute_Element (P.Model);
            P.Attr_Element.Set_Name (Value);
            P.Attr_Element.Set_Location (To_String (P.File) & P.Parser.Get_Location);

         when FIELD_ATTRIBUTE_INITIAL_VALUE =>
            P.Attr_Value := Value;

         when FIELD_ATTRIBUTE =>
            P.Attr_Element.Set_XMI_Id (P.Attr_Id);
            P.Attr_Element.Visibility    := P.Attr_Visibility;
            P.Attr_Element.Changeability := P.Attr_Changeability;
            P.Attr_Element.Multiplicity_Lower := P.Multiplicity_Lower;
            P.Attr_Element.Multiplicity_Upper := P.Multiplicity_Upper;
            P.Attr_Element.Initial_Value      := P.Attr_Value;

            --  Prepare for next attribute.
            P.Attr_Visibility    := Gen.Model.XMI.VISIBILITY_PUBLIC;
            P.Attr_Changeability := Gen.Model.XMI.CHANGEABILITY_CHANGEABLE;
            P.Multiplicity_Lower := 0;
            P.Multiplicity_Upper := 0;

            --  Sanity check and add this attribute to the class.
            if P.Class_Element /= null then
               P.Model.Insert (P.Attr_Element.XMI_Id, P.Attr_Element.all'Access);
               P.Attr_Element.Parent := P.Class_Element.all'Access;
               P.Class_Element.Elements.Append (P.Attr_Element.all'Access);
               P.Class_Element.Attributes.Append (P.Attr_Element.all'Access);
               if Length (P.Attr_Element.Ref_Id) = 0 then
                  P.Attr_Element.Ref_Id := P.Default_Type;
                  declare
                     Msg : constant String := "attribute '" & To_String (P.Attr_Element.Name)
                       & "' in table '" & To_String (P.Class_Element.Name)
                       & "' has no type.";
                  begin
                     P.Attr_Element := null;
                     raise Util.Serialize.Mappers.Field_Error with Msg;
                  end;
               end if;
            end if;
            P.Attr_Element       := null;

         when FIELD_OPERATION_NAME =>
            P.Operation := new Gen.Model.XMI.Operation_Element (P.Model);
            P.Operation.Set_Name (Value);
            P.Operation.Set_Location (To_String (P.File) & P.Parser.Get_Location);

         when FIELD_PARAMETER_NAME =>
            P.Attr_Element := new Gen.Model.XMI.Attribute_Element (P.Model);
            P.Attr_Element.Set_Name (Value);
            P.Attr_Element.Set_Location (To_String (P.File) & P.Parser.Get_Location);

         when FIELD_PARAMETER_KIND =>
            P.Parameter_Type := Get_Parameter_Type (Value);

         when FIELD_PARAMETER_END =>
            if P.Attr_Element /= null and then P.Operation /= null then
               P.Attr_Element.Set_XMI_Id (P.Attr_Id);
               P.Operation.Elements.Append (P.Attr_Element.all'Access);
               P.Model.Insert (P.Attr_Element.XMI_Id, P.Attr_Element.all'Access);
            end if;
            P.Attr_Element := null;

         when FIELD_OPERATION_END =>
            if P.Operation /= null and then P.Class_Element /= null then
               P.Operation.Set_XMI_Id (P.Operation_Id);
               P.Model.Insert (P.Operation.XMI_Id, P.Operation.all'Access);
               P.Class_Element.Operations.Append (P.Operation.all'Access);
            end if;
            P.Operation := null;

            --  Extract an association.
         when FIELD_ASSOCIATION_ID =>
            P.Association_Id := Value;

         when FIELD_ASSOCIATION_NAME =>
            P.Association := new Gen.Model.XMI.Association_Element (P.Model);
            P.Association.Set_Name (Value);
            P.Association.Set_Location (To_String (P.File) & P.Parser.Get_Location);

         when FIELD_ASSOCIATION_END_NAME =>
            P.Assos_End_Name := Value;

         when FIELD_ASSOCIATION_END_VISIBILITY =>
            P.Assos_End_Visibility := Get_Visibility (Value);

         when FIELD_ASSOCIATION_END_NAVIGABLE =>
            P.Assos_End_Navigable := UBO.To_Boolean (Value);

         when FIELD_ASSOCIATION_END_ID =>
            P.Assos_End_Element := new Gen.Model.XMI.Association_End_Element (P.Model);
            P.Assos_End_Element.Set_XMI_Id (Value);
            P.Assos_End_Element.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Model.Include (P.Assos_End_Element.XMI_Id, P.Assos_End_Element.all'Access);

         when FIELD_ASSOCIATION_CLASS_ID =>
            if P.Assos_End_Element /= null then
               P.Assos_End_Element.Set_Reference_Id (UBO.To_String (Value),
                                                     P.Profiles.all);
            end if;

         when FIELD_ASSOCIATION_END =>
            if P.Assos_End_Element /= null and then P.Association /= null then
               P.Assos_End_Element.Set_Name (P.Assos_End_Name);
               P.Assos_End_Element.Visibility := P.Assos_End_Visibility;
               P.Assos_End_Element.Navigable := P.Assos_End_Navigable;
               P.Assos_End_Element.Multiplicity_Lower := P.Multiplicity_Lower;
               P.Assos_End_Element.Multiplicity_Upper := P.Multiplicity_Upper;
               P.Assos_End_Element.Parent := P.Association.all'Access;

               --  Keep the association if the target class is specified.
               --  We ignore association to a UML Component for example.
               if Length (P.Assos_End_Element.Ref_Id) > 0 then
                  P.Association.Connections.Append (P.Assos_End_Element.all'Access);
               else
                  Log.Info ("Association end {0} ignored", P.Assos_End_Element.Name);
               end if;
            end if;
            P.Multiplicity_Lower := 0;
            P.Multiplicity_Upper := 0;
            P.Assos_End_Name := UBO.Null_Object;
            P.Assos_End_Navigable := False;
            if P.Association = null then
               raise Util.Serialize.Mappers.Field_Error with "invalid association";
            end if;
            P.Assos_End_Element := null;

         when FIELD_ASSOCIATION =>
            if P.Association /= null then
               P.Association.Set_XMI_Id (P.Association_Id);
               P.Model.Include (P.Association.XMI_Id, P.Association.all'Access);
               if P.Package_Element /= null then
                  P.Package_Element.Associations.Append (P.Association.all'Access);
               end if;
            end if;
            P.Association := null;

         when FIELD_PACKAGE_ID =>
            Set_Package (P, UBO.Null_Object, Value);

         when FIELD_PACKAGE_NAME =>
            Set_Package (P, Value, UBO.Null_Object);

         when FIELD_PACKAGE_END =>
            if P.Package_Element /= null then
               if P.Package_Element.Parent /= null then
                  P.Package_Element
                    := Gen.Model.XMI.Package_Element (P.Package_Element.Parent.all)'Access;
               else
                  P.Package_Element := null;
               end if;
            end if;

         when FIELD_TAGGED_ID =>
            P.Tagged_Id     := Value;

            --  Tagged value associated with an attribute, operation, class, package.
         when FIELD_TAGGED_VALUE =>
            Add_Tagged_Value (P);

            --  Data type mapping.
         when FIELD_DATA_TYPE_NAME =>
            P.Data_Type := new Gen.Model.XMI.Data_Type_Element (P.Model);
            P.Data_Type.Set_Name (Value);
            P.Data_Type.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Data_Type.XMI_Id := UBO.To_Unbounded_String (P.Id);
            P.Ref_Id := UBO.Null_Object;
            P.Href   := UBO.Null_Object;

         when FIELD_DATA_TYPE =>
            if P.Attr_Element = null
              and then P.Operation = null
              and then UBO.Is_Null (P.Generalization_Id)
              and then P.Data_Type /= null
            then
               if P.Package_Element /= null and then not P.Is_Profile then
                  P.Data_Type.Parent := P.Package_Element.all'Access;
               end if;
               P.Model.Insert (P.Data_Type.XMI_Id, P.Data_Type.all'Access);
               if P.Package_Element /= null and then not P.Is_Profile then
                  P.Package_Element.Types.Append (P.Data_Type.all'Access);
               end if;
            end if;
            P.Data_Type := null;

         when FIELD_DATA_TYPE_HREF | FIELD_ENUMERATION_HREF | FIELD_CLASSIFIER_HREF =>
            if P.Attr_Element /= null then
               P.Attr_Element.Set_Reference_Id (UBO.To_String (Value),
                                                P.Profiles.all);
               Log.Debug ("Attribute {0} has type {1}",
                          P.Attr_Element.Name, P.Attr_Element.Ref_Id);
            end if;

            --  Enumeration mapping.
         when FIELD_ENUMERATION =>
            P.Enumeration := new Gen.Model.XMI.Enum_Element (P.Model);
            P.Enumeration.Set_Name (Value);
            P.Enumeration.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Enumeration.XMI_Id := UBO.To_Unbounded_String (P.Id);
            if P.Package_Element /= null then
               P.Enumeration.Parent := P.Package_Element.all'Access;
            end if;
            P.Model.Insert (P.Enumeration.XMI_Id, P.Enumeration.all'Access);
            Log.Info ("Adding enumeration {0}", P.Enumeration.Name);
            if P.Package_Element /= null then
               P.Package_Element.Enums.Append (P.Enumeration.all'Access);
            end if;

         when FIELD_ENUMERATION_LITERAL =>
            P.Enumeration.Add_Literal (Value, P.Name, P.Enumeration_Literal);

         when FIELD_ENUMERATION_LITERAL_END =>
            P.Enumeration_Literal.Set_Name (P.Name);
            P.Enumeration_Literal := null;

         when FIELD_STEREOTYPE_NAME =>
            P.Stereotype := new Gen.Model.XMI.Stereotype_Element (P.Model);
            P.Stereotype.Set_Name (Value);
            P.Stereotype.Set_Location (To_String (P.File) & P.Parser.Get_Location);

         when FIELD_STEREOTYPE_ID =>
            P.Stereotype_Id := Value;

            --  Stereotype mapping.
         when FIELD_STEREOTYPE =>
            if not UBO.Is_Null (P.Stereotype_Id) and then P.Stereotype /= null then
               P.Stereotype.XMI_Id := UBO.To_Unbounded_String (P.Stereotype_Id);
               P.Model.Insert (P.Stereotype.XMI_Id, P.Stereotype.all'Access);
               if P.Class_Element /= null then
                  P.Class_Element.Elements.Append (P.Stereotype.all'Access);
               elsif P.Package_Element /= null then
                  P.Package_Element.Elements.Append (P.Stereotype.all'Access);
               end if;
               P.Stereotype := null;
            end if;

         when FIELD_STEREOTYPE_HREF =>
            declare
               S : constant Gen.Model.XMI.Ref_Type_Element_Access
                 := new Gen.Model.XMI.Ref_Type_Element (P.Model);
            begin
               S.Set_Location (To_String (P.File) & P.Parser.Get_Location);
               S.Set_Reference_Id (UBO.To_String (Value), P.Profiles.all);

               if P.Enumeration_Literal /= null then
                  P.Enumeration_Literal.Stereotypes.Append (S.all'Access);
               elsif P.Assos_End_Element /= null then
                  Log.Info ("Stereotype {0} added", UBO.To_String (Value));
                  P.Assos_End_Element.Stereotypes.Append (S.all'Access);
               elsif P.Association /= null then
                  P.Association.Stereotypes.Append (S.all'Access);
               elsif P.Attr_Element /= null then
                  P.Attr_Element.Stereotypes.Append (S.all'Access);
               elsif P.Class_Element /= null then
                  P.Class_Element.Stereotypes.Append (S.all'Access);
               elsif P.Package_Element /= null then
                  P.Package_Element.Stereotypes.Append (S.all'Access);
               else
                  Log.Info ("Stereotype {0} ignored", UBO.To_String (Value));
               end if;
            end;

            --  Tag definition mapping.
         when FIELD_TAG_DEFINITION_NAME =>
            P.Tag_Name := Value;

         when FIELD_TAG_DEFINITION_ID =>
            P.Tag_Definition := new Gen.Model.XMI.Tag_Definition_Element (P.Model);
            P.Tag_Definition.Set_XMI_Id (Value);
            P.Tag_Definition.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Model.Insert (P.Tag_Definition.XMI_Id, P.Tag_Definition.all'Access);

         when FIELD_TAG_DEFINITION =>
            P.Tag_Definition.Set_Name (P.Tag_Name);
            Log.Info ("Adding tag definition {0}", P.Tag_Definition.Name);
            if P.Stereotype /= null then
               P.Stereotype.Elements.Append (P.Tag_Definition.all'Access);

            elsif P.Package_Element /= null then
               P.Package_Element.Elements.Append (P.Tag_Definition.all'Access);

            end if;
            P.Tag_Definition := null;

         when FIELD_COMMENT_ID =>
            P.Comment := new Gen.Model.XMI.Comment_Element (P.Model);
            P.Comment.Set_Location (To_String (P.File) & P.Parser.Get_Location);
            P.Comment.XMI_Id := UBO.To_Unbounded_String (Value);
            P.Ref_Id := UBO.Null_Object;

            --  Comment mapping.
         when FIELD_COMMENT =>
            if P.Comment /= null then
               P.Comment.Text    := UBO.To_Unbounded_String (P.Value);
               P.Comment.Ref_Id  := UBO.To_Unbounded_String (P.Ref_Id);
               P.Model.Insert (P.Comment.XMI_Id, P.Comment.all'Access);
            end if;
            P.Ref_Id := UBO.Null_Object;
            P.Comment := null;

      end case;

   exception
      when Util.Serialize.Mappers.Field_Error =>
         raise;

      when E : others =>
         Log.Error ("Extraction of field {0} with value '{1}' failed",
                    XMI_Fields'Image (Field), UBO.To_String (Value));
         Log.Error ("Cause", E);
         raise;
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
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is

   begin
      Log.Debug ("Initializing query artifact for the configuration");

      Gen.Artifacts.Artifact (Handler).Initialize (Path, Node, Model, Context);
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
      pragma Unreferenced (Project);

      --  Collect the enum literal for the enum definition.
      procedure Prepare_Enum_Literal (Enum : in out Gen.Model.Enums.Enum_Definition'Class;
                                      Item : in Gen.Model.XMI.Model_Element_Access);

      --  Register the enum in the model for the generation.
      procedure Prepare_Enum (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                              Item : in Gen.Model.XMI.Model_Element_Access);

      use Gen.Model.XMI;
      use Gen.Model.Tables;
      use Gen.Model.Beans;

      --  Register the attribute in the table
      procedure Prepare_Attribute (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                   Column : in Model_Element_Access);

      --  Register the attribute in the bean definition.
      procedure Prepare_Attribute (Bean   : in out Gen.Model.Beans.Bean_Definition'Class;
                                   Column : in Model_Element_Access);

      --  Reorder the associations according to optional @dynamo.assoc.order tagged values.
      procedure Order_Associations (Associations : in out Model_Vector);

      --  Identify the UML association and create an entry for it in the table.
      procedure Prepare_Association (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                     Node   : in Model_Element_Access);

      procedure Prepare_Parameter (Operation : in out
                                     Gen.Model.Operations.Operation_Definition'Class;
                                   Node      : in Model_Element_Access);

      --  Identify the UML operation and create an entry for it in the table.
      procedure Prepare_Operation (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                   Node   : in Model_Element_Access);

      --  Prepare a UML/XMI class:
      --   o if the class has the <<Dynamo.ADO.table>> stereotype, create a table definition.
      --   o if the class has the <<Dynamo.AWA.bean>> stereotype, create a bean
      procedure Prepare_Class (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                               Item : in Gen.Model.XMI.Model_Element_Access);

      procedure Prepare_Type (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                              Item : in Gen.Model.XMI.Model_Element_Access);

      --  Scan the package for the model generation.
      procedure Prepare_Package (Id   : in UString;
                                 Item : in Gen.Model.XMI.Model_Element_Access);

      procedure Prepare_Model (Key   : in UString;
                               Model : in out Gen.Model.XMI.Model_Map.Map);

      procedure Prepare_Profile (Id   : in UString;
                                 Item : in Gen.Model.XMI.Model_Element_Access);

      --  ------------------------------
      --  Register the attribute in the table
      --  ------------------------------
      procedure Prepare_Attribute (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                   Column : in Model_Element_Access) is
         use Util.Beans.Objects;

         Msg  : constant String := Column.Get_Error_Message;
         Sql  : constant String := Column.Find_Tag_Value (Handler.Sql_Type_Tag, "");
         Len  : constant String := Column.Find_Tag_Value (Handler.Sql_Length_Tag, "");
         C    : Column_Definition_Access;
      begin
         Log.Info ("Prepare class attribute {0}", Column.Name);

         if Msg'Length /= 0 then
            Context.Error (Column.Get_Location & ": " & Msg);
         end if;

         Table.Add_Column (Column.Name, C);
         C.Set_Comment (Column.Get_Comment);
         C.Set_Location (Column.Get_Location);
         if Column.all in Attribute_Element'Class then
            declare
               Attr : constant Attribute_Element_Access
                 := Attribute_Element'Class (Column.all)'Access;
            begin
               if Attr.Data_Type /= null then
                  C.Set_Type (Attr.Data_Type.Get_Qualified_Name);
               end if;
               C.Not_Null     := Attr.Multiplicity_Lower > 0;
               C.Is_Key       := Column.Has_Stereotype (Handler.PK_Stereotype);
               C.Is_Version   := Column.Has_Stereotype (Handler.Version_Stereotype);
               C.Is_Auditable := Column.Has_Stereotype (Handler.Auditable_Stereotype);
               C.Is_Updated   := Attr.Changeability /= CHANGEABILITY_FROZEN;
               C.Is_Inserted  := True; --  Attr.Changeability = CHANGEABILITY_INSERT;
               C.Sql_Type     := To_UString (Sql);

               if Column.Has_Stereotype (Handler.Not_Null_Stereotype) then
                  C.Not_Null := True;
               end if;
               if Column.Has_Stereotype (Handler.Nullable_Stereotype) then
                  C.Not_Null := False;
               end if;
               if C.Is_Version then
                  C.Not_Null := True;
               end if;
               if C.Is_Key then
                  C.Generator := To_Object (Column.Find_Tag_Value (Handler.Generator_Tag, ""));
               end if;

               if Len'Length > 0 then
                  C.Set_Sql_Length (Len, Context);
               end if;
            end;
         end if;
      end Prepare_Attribute;

      --  ------------------------------
      --  Register the attribute in the bean definition.
      --  ------------------------------
      procedure Prepare_Attribute (Bean   : in out Gen.Model.Beans.Bean_Definition'Class;
                                   Column : in Model_Element_Access) is
         Msg  : constant String := Column.Get_Error_Message;
         C    : Column_Definition_Access;
      begin
         Log.Info ("Prepare class attribute {0}", Column.Name);

         if Msg'Length /= 0 then
            Context.Error (Column.Get_Location & ": " & Msg);
         end if;

         Bean.Add_Attribute (Column.Name, C);
         C.Set_Comment (Column.Get_Comment);
         C.Set_Location (Column.Get_Location);
         if Column.all in Attribute_Element'Class then
            declare
               Attr : constant Attribute_Element_Access
                 := Attribute_Element'Class (Column.all)'Access;
            begin
               if Attr.Data_Type /= null then
                  C.Set_Type (To_String (Attr.Data_Type.Name));
               end if;
               C.Not_Null := Attr.Multiplicity_Lower > 0;
            end;
         end if;
      end Prepare_Attribute;

      --  ------------------------------
      --  Identify the UML association and create an entry for it in the table.
      --  ------------------------------
      procedure Prepare_Association (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                     Node   : in Model_Element_Access) is
         A     : Association_Definition_Access;
         Assoc : constant Association_End_Element_Access
           := Association_End_Element'Class (Node.all)'Access;
         Msg   : constant String := Node.Get_Error_Message;
      begin
         Log.Info ("Prepare class association {0}", Assoc.Name);

         if Msg'Length /= 0 then
            Context.Error (Assoc.Get_Location & ": " & Msg);
         end if;
         if Assoc.Multiplicity_Upper /= 1 then
            Context.Error (Assoc.Get_Location &
                             ": multiple association '{0}' for table '{1}' is not supported.",
                           To_String (Assoc.Name), Table.Get_Name);
         else
            Table.Add_Association (Assoc.Name, A);
            A.Set_Comment (Assoc.Get_Comment);
            A.Set_Location (Assoc.Get_Location);
            A.Set_Type (Assoc.Source_Element.Get_Qualified_Name);
            A.Not_Null  := Assoc.Multiplicity_Lower > 0;

            --  If the <<use foreign key>> stereotype is set on the association, to not use
            --  the Ada tagged object but create an attribute using the foreign key type.
            A.Use_Foreign_Key_Type := Node.Parent.Has_Stereotype (Handler.Use_FK_Stereotype);
            A.Is_Key := Node.Has_Stereotype (Handler.PK_Stereotype);
            if A.Use_Foreign_Key_Type then
               Log.Info ("Association {0} type is using foreign key", Assoc.Name);
            end if;
         end if;
      end Prepare_Association;

      procedure Prepare_Parameter (Operation : in out
                                     Gen.Model.Operations.Operation_Definition'Class;
                                   Node      : in Model_Element_Access) is
         Param : constant Attribute_Element_Access := Attribute_Element'Class (Node.all)'Access;
         P     : Gen.Model.Operations.Parameter_Definition_Access;
      begin
         if Param.Data_Type /= null then
            Log.Info ("Prepare operation parameter {0} : {1}",
                      Param.Name, Param.Data_Type.Get_Qualified_Name);
            Operation.Add_Parameter (Param.Name,
                                     To_UString (Param.Data_Type.Get_Qualified_Name), P);
         end if;
      end Prepare_Parameter;

      --  ------------------------------
      --  Identify the UML operation and create an entry for it in the table.
      --  ------------------------------
      procedure Prepare_Operation (Table  : in out Gen.Model.Tables.Table_Definition'Class;
                                   Node   : in Model_Element_Access) is
         Op   : constant Operation_Element_Access := Operation_Element'Class (Node.all)'Access;
         Msg  : constant String := Node.Get_Error_Message;
         Operation : Gen.Model.Operations.Operation_Definition_Access;

      begin
         Log.Info ("Prepare class operation {0}", Op.Name);

         if Msg'Length /= 0 then
            Context.Error (Op.Get_Location & ": " & Msg);
         end if;
         Table.Add_Operation (Op.Name, Operation);
         Operation.Set_Location (Op.Get_Location);
         Operation.Set_Comment (Op.Get_Comment);
         Iterate_For_Operation (Operation.all, Op.Elements, Prepare_Parameter'Access);
      end Prepare_Operation;

      --  ------------------------------
      --  Reorder the associations according to optional @dynamo.assoc.order tagged values.
      --  ------------------------------
      procedure Order_Associations (Associations : in out Model_Vector) is
         Count : constant Natural := Natural (Associations.Length);

         type Assoc_Array is array (Positive range 1 .. Count) of Model_Element_Access;
         Ordered_List : Assoc_Array := (others => null);
         Remain_List  : Assoc_Array := (others => null);
         Last  : Natural := 0;
         First : Natural := 0;
      begin
         --  Step 1: build in Ordered_List the list of associations according to their
         --  ordered defined by the @dynamo.assoc.order tagged values and keep in
         --  Remain_List the associations that are not specified.
         for I in 1 .. Count loop
            declare
               Assoc : constant Model_Element_Access := Associations.Element (I);
               Order : constant String := Assoc.Find_Tag_Value (Handler.Assoc_Order_Tag);
               Pos   : Positive;
            begin
               if Order'Length > 0 then
                  Pos := Positive'Value (Order);
                  if Ordered_List (Pos) = null then
                     Ordered_List (Pos) := Assoc;
                  else
                     Last := Last + 1;
                     Remain_List (Last) := Assoc;
                  end if;
               else
                  Last := Last + 1;
                  Remain_List (Last) := Assoc;
               end if;

            exception
               when Constraint_Error =>
                  Log.Error ("Association order '{0}' is not a number", Order);
                  Last := Last + 1;
                  Remain_List (Last) := Assoc;

            end;
         end loop;

         --  Step 2: reorganize the association array to honor the ordered list
         --  and then follow the remain list order.
         First := 1;
         for I in 1 .. Count loop
            if Ordered_List (I) /= null then
               Associations.Replace_Element (I, Ordered_List (I));
            elsif First <= Last then
               Associations.Replace_Element (I, Remain_List (First));
               First := First + 1;
            end if;
         end loop;
      end Order_Associations;

      --  ------------------------------
      --  Prepare a UML/XMI class:
      --   o if the class has the <<Dynamo.ADO.table>> stereotype, create a table definition.
      --   o if the class has the <<Dynamo.AWA.bean>> stereotype, create a bean
      --  ------------------------------
      procedure Prepare_Class (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                               Item : in Gen.Model.XMI.Model_Element_Access) is
         Class : constant Class_Element_Access := Class_Element'Class (Item.all)'Access;
         Name  : constant UString := Gen.Utils.Qualify_Name (Pkg.Name, Class.Name);
      begin
         Log.Info ("Prepare class {0}", Name);

         if Item.Has_Stereotype (Handler.Table_Stereotype) then
            Log.Debug ("Class {0} recognized as a database table", Name);
            declare
               Table : constant Table_Definition_Access := Gen.Model.Tables.Create_Table (Name);
               Has_List : constant String := Item.Find_Tag_Value (Handler.Has_List_Tag, "true");
               T_Name   : constant String := Item.Find_Tag_Value (Handler.Table_Name_Tag, "");
            begin
               Log.Info ("Has list: {0}", Has_List);
               Table.Set_Comment (Item.Get_Comment);
               Table.Set_Location (Item.Get_Location);
               Model.Register_Table (Table);
               Table.Has_List := Has_List = "true";
               Table.Is_Serializable := Item.Has_Stereotype (Handler.Serialize_Stereotype);
               if T_Name'Length /= 0 then
                  Log.Info ("Using table name {0}", Name);
                  Table.Table_Name := To_UString (T_Name);
               end if;
               Order_Associations (Class.Associations);
               Iterate_For_Table (Table.all, Class.Attributes, Prepare_Attribute'Access);
               Iterate_For_Table (Table.all, Class.Associations, Prepare_Association'Access);
            end;

         elsif Item.Has_Stereotype (Handler.Bean_Stereotype)
           or else Item.Has_Stereotype (Handler.Limited_Bean_Stereotype)
         then
            Log.Debug ("Class {0} recognized as a bean", Name);
            declare
               Bean : constant Bean_Definition_Access := Gen.Model.Beans.Create_Bean (Name);
            begin
               Model.Register_Bean (Bean);
               Bean.Set_Comment (Item.Get_Comment);
               Bean.Set_Location (Item.Get_Location);
               Bean.Target := Name;
               Bean.Is_Limited := Item.Has_Stereotype (Handler.Limited_Bean_Stereotype);
               Bean.Is_Serializable := Item.Has_Stereotype (Handler.Serialize_Stereotype);
               if Class.Parent_Class /= null then
                  Log.Info ("Bean {0} inherit from {1}", Name,
                            To_String (Class.Parent_Class.Name));
                  Bean.Parent_Name := To_UString (Class.Parent_Class.Get_Qualified_Name);
               end if;
               Iterate_For_Bean (Bean.all, Class.Attributes, Prepare_Attribute'Access);
               Iterate_For_Table (Bean.all, Class.Associations, Prepare_Association'Access);
               Iterate_For_Table (Bean.all, Class.Operations, Prepare_Operation'Access);
            end;

         else
            Log.Info ("UML class {0} not generated: no <<Bean>> and no <<Table>> stereotype",
                      To_String (Name));
         end if;
      exception
         when E : others =>
            Log.Error ("Exception", E);
      end Prepare_Class;

      --  ------------------------------
      --  Collect the enum literal for the enum definition.
      --  ------------------------------
      procedure Prepare_Enum_Literal (Enum : in out Gen.Model.Enums.Enum_Definition'Class;
                                      Item : in Gen.Model.XMI.Model_Element_Access) is
         Literal : Gen.Model.Enums.Value_Definition_Access;
         Value   : constant String := Item.Find_Tag_Value (Handler.Literal_Tag, "");
      begin
         Log.Info ("Prepare enum literal {0}", Item.Name);

         Enum.Add_Value (To_String (Item.Name), Literal);

         if Value'Length > 0 then
            begin
               Literal.Number := Natural'Value (Value);

            exception
               when others =>
                  Context.Error (Item.Get_Location &
                                   ": value '{0}' for enum literal '{1}' must be a number",
                                 Value, To_String (Item.Name));
            end;
         end if;
      end Prepare_Enum_Literal;

      --  ------------------------------
      --  Register the enum in the model for the generation.
      --  ------------------------------
      procedure Prepare_Type (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                              Item : in Gen.Model.XMI.Model_Element_Access) is
         Data_Type : constant Data_Type_Element_Access
           := Data_Type_Element'Class (Item.all)'Access;
         Name      : constant String := Data_Type.Get_Qualified_Name;
         Msg       : constant String := Data_Type.Get_Error_Message;
         Sql       : constant String := Data_Type.Find_Tag_Value (Handler.Sql_Type_Tag, "");
         Stype     : Gen.Model.Stypes.Stype_Definition_Access;
      begin
         Log.Info ("Prepare data type {0} - {1}", Name, Sql);

         if Msg'Length > 0 then
            Context.Error (Item.Get_Location & ": " & Msg);
         end if;
         if Data_Type.Parent_Type /= null then
            Stype := Gen.Model.Stypes.Create_Stype
              (To_UString (Name),
               To_UString (Data_Type.Parent_Type.Get_Qualified_Name));
         else
            Stype := Gen.Model.Stypes.Create_Stype (To_UString (Name),
                                                    Null_Unbounded_String);
         end if;
         Stype.Set_Comment (Item.Get_Comment);
         Stype.Set_Location (Item.Get_Location);
         Stype.Sql_Type := To_UString (Sql);
         Model.Register_Stype (Stype);

      exception
         when Gen.Model.Name_Exist =>
            --  Ignore the Name_Exist exception for pre-defined package
            --  because the type is already defined through a XML mapping definition.
            if not Pkg.Is_Predefined then
               raise;
            end if;
      end Prepare_Type;

      --  ------------------------------
      --  Register the enum in the model for the generation.
      --  ------------------------------
      procedure Prepare_Enum (Pkg  : in out Gen.Model.Packages.Package_Definition'Class;
                              Item : in Gen.Model.XMI.Model_Element_Access) is
         pragma Unreferenced (Pkg);

         Name  : constant String := Item.Get_Qualified_Name;
         Msg   : constant String := Item.Get_Error_Message;
         Enum  : Gen.Model.Enums.Enum_Definition_Access;
         Sql   : constant String := Item.Find_Tag_Value (Handler.Sql_Type_Tag, "");
      begin
         Log.Info ("Prepare enum {0}", Name);

         if Msg'Length > 0 then
            Context.Error (Item.Get_Location & ": " & Msg);
         end if;
         Enum := Gen.Model.Enums.Create_Enum (To_UString (Name));
         Enum.Set_Comment (Item.Get_Comment);
         Enum.Set_Location (Item.Get_Location);
         Enum.Sql_Type := To_UString (Sql);
         Model.Register_Enum (Enum);

         Iterate_For_Enum (Enum.all, Item.Elements, Prepare_Enum_Literal'Access);
      end Prepare_Enum;

      --  ------------------------------
      --  Scan the package for the model generation.
      --  ------------------------------
      procedure Prepare_Package (Id   : in UString;
                                 Item : in Gen.Model.XMI.Model_Element_Access) is
         pragma Unreferenced (Id);

         Pkg  : constant Package_Element_Access := Package_Element'Class (Item.all)'Access;
         Name : constant String := Pkg.Get_Qualified_Name;
         P    : Gen.Model.Packages.Package_Definition_Access;
      begin
         if Pkg.Is_Profile then
            return;
         end if;

         Log.Info ("Prepare package {0}", Name);

         Model.Register_Package (To_UString (Name), P);

         if Item.Has_Stereotype (Handler.Data_Model_Stereotype) then
            Log.Info ("Package {0} has the <<DataModel>> stereotype", Name);

         else
            Log.Info ("Package {0} does not have the <<DataModel>> stereotype.", Name);

            --  Do not generate packages that don't have the <<DataModel>> stereotype.
            --  But still get their UML definition so that we can use their classes.
            P.Set_Predefined;
         end if;

         P.Set_Comment (Pkg.Get_Comment);
         Iterate_For_Package (P.all, Pkg.Types, Prepare_Type'Access);
         Iterate_For_Package (P.all, Pkg.Enums, Prepare_Enum'Access);
         Iterate_For_Package (P.all, Pkg.Classes, Prepare_Class'Access);
      end Prepare_Package;

      --  ------------------------------
      --  Scan the profile packages for the model generation.
      --  ------------------------------
      procedure Prepare_Profile (Id   : in UString;
                                 Item : in Gen.Model.XMI.Model_Element_Access) is
         pragma Unreferenced (Id);

         Pkg  : constant Package_Element_Access := Package_Element'Class (Item.all)'Access;
         Name : constant String := Pkg.Get_Qualified_Name;
         P    : Gen.Model.Packages.Package_Definition_Access;
      begin
         if not Pkg.Is_Profile then
            return;
         end if;

         Log.Info ("Prepare profile package {0}", Name);

         Model.Register_Package (To_UString (Name), P);

         P.Set_Predefined;
         P.Set_Comment (Pkg.Get_Comment);
         --  Iterate_For_Package (P.all, Pkg.Types, Prepare_Type'Access);
         Iterate_For_Package (P.all, Pkg.Enums, Prepare_Enum'Access);
         Iterate_For_Package (P.all, Pkg.Classes, Prepare_Class'Access);
      end Prepare_Profile;

      procedure Prepare_Model (Key   : in UString;
                               Model : in out Gen.Model.XMI.Model_Map.Map) is
      begin
         Log.Info ("Preparing model {0}", Key);

         Gen.Model.XMI.Iterate (Model   => Model,
                                On      => Gen.Model.XMI.XMI_PACKAGE,
                                Process => Prepare_Package'Access);
      end Prepare_Model;

      Iter : Gen.Model.XMI.UML_Model_Map.Cursor := Handler.Nodes.First;
   begin
      Log.Debug ("Preparing the XMI model for generation");

      Gen.Model.XMI.Reconcile (Handler.Nodes,
                               Context.Get_Parameter (Gen.Configs.GEN_DEBUG_ENABLE));

      --  Get the Dynamo stereotype definitions.
      Handler.Table_Stereotype := Find_Stereotype (Handler.Nodes,
                                                   "Dynamo.xmi",
                                                   "ADO.Table",
                                                   Gen.Model.XMI.BY_NAME);
      Handler.PK_Stereotype := Find_Stereotype (Handler.Nodes,
                                                "Dynamo.xmi",
                                                "ADO.PK",
                                                Gen.Model.XMI.BY_NAME);
      Handler.FK_Stereotype := Find_Stereotype (Handler.Nodes,
                                                "Dynamo.xmi",
                                                "ADO.FK",
                                                Gen.Model.XMI.BY_NAME);
      Handler.Version_Stereotype := Find_Stereotype (Handler.Nodes,
                                                     "Dynamo.xmi",
                                                     "ADO.Version",
                                                     Gen.Model.XMI.BY_NAME);
      Handler.Nullable_Stereotype := Find_Stereotype (Handler.Nodes,
                                                      "Dynamo.xmi",
                                                      "ADO.Nullable",
                                                      Gen.Model.XMI.BY_NAME);
      Handler.Not_Null_Stereotype := Find_Stereotype (Handler.Nodes,
                                                      "Dynamo.xmi",
                                                      "ADO.Not Null",
                                                      Gen.Model.XMI.BY_NAME);
      Handler.Data_Model_Stereotype := Find_Stereotype (Handler.Nodes,
                                                        "Dynamo.xmi",
                                                        "ADO.DataModel",
                                                        Gen.Model.XMI.BY_NAME);
      Handler.Use_FK_Stereotype := Find_Stereotype (Handler.Nodes,
                                                    "Dynamo.xmi",
                                                    "ADO.use foreign key",
                                                    Gen.Model.XMI.BY_NAME);
      Handler.Auditable_Stereotype := Find_Stereotype (Handler.Nodes,
                                                       "Dynamo.xmi",
                                                       "ADO.Auditable",
                                                       Gen.Model.XMI.BY_NAME);
      Handler.Bean_Stereotype := Find_Stereotype (Handler.Nodes,
                                                  "Dynamo.xmi",
                                                  "AWA.Bean",
                                                  Gen.Model.XMI.BY_NAME);
      Handler.Limited_Bean_Stereotype := Find_Stereotype (Handler.Nodes,
                                                          "Dynamo.xmi",
                                                          "AWA.Limited_Bean",
                                                          Gen.Model.XMI.BY_NAME);
      Handler.Serialize_Stereotype := Find_Stereotype (Handler.Nodes,
                                                       "Dynamo.xmi",
                                                       "ASF.Serializable",
                                                       Gen.Model.XMI.BY_NAME);
      Handler.Has_List_Tag := Find_Tag_Definition (Handler.Nodes,
                                                   "Dynamo.xmi",
                                                   "ADO.Table.@dynamo.table.hasList",
                                                   Gen.Model.XMI.BY_NAME);
      Handler.Table_Name_Tag := Find_Tag_Definition (Handler.Nodes,
                                                     "Dynamo.xmi",
                                                     "ADO.Table.@dynamo.table.name",
                                                     Gen.Model.XMI.BY_NAME);
      Handler.Sql_Type_Tag := Find_Tag_Definition (Handler.Nodes,
                                                   "Dynamo.xmi",
                                                   "ADO.@dynamo.sql.type",
                                                   Gen.Model.XMI.BY_NAME);
      Handler.Sql_Length_Tag := Find_Tag_Definition (Handler.Nodes,
                                                     "Dynamo.xmi",
                                                     "ADO.@dynamo.sql.length",
                                                     Gen.Model.XMI.BY_NAME);
      Handler.Generator_Tag := Find_Tag_Definition (Handler.Nodes,
                                                    "Dynamo.xmi",
                                                    "ADO.PK.@dynamo.pk.generator",
                                                    Gen.Model.XMI.BY_NAME);
      Handler.Assoc_Order_Tag := Find_Tag_Definition (Handler.Nodes,
                                                      "Dynamo.xmi",
                                                      "ADO.@dynamo.assoc.order",
                                                      Gen.Model.XMI.BY_NAME);
      Handler.Literal_Tag := Find_Tag_Definition (Handler.Nodes,
                                                  "Dynamo.xmi",
                                                  "ADO.Literal.@dynamo.literal",
                                                  Gen.Model.XMI.BY_NAME);

      for Model of Handler.Nodes loop
         Gen.Model.XMI.Iterate (Model   => Model,
                                On      => Gen.Model.XMI.XMI_PACKAGE,
                                Process => Prepare_Profile'Access);
      end loop;

      while Gen.Model.XMI.UML_Model_Map.Has_Element (Iter) loop
         Handler.Nodes.Update_Element (Iter, Prepare_Model'Access);
         Gen.Model.XMI.UML_Model_Map.Next (Iter);
      end loop;

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
      end if;
   end Prepare;

   function Find_Profile (Name : in String;
                          Path : in String) return String is
      procedure Find (Dir  : in String;
                      Done : out Boolean);
      function Make_Zargo (Name : in String) return String;
      function To_Lower_Case (Name : in String) return String
        renames Util.Strings.Transforms.To_Lower_Case;

      function Make_Zargo (Name : in String) return String is
         Pos : constant Natural := Util.Strings.Rindex (Name, '.');
      begin
         if Pos = 0 then
            return Name & ".zargo";
         else
            return Name (Name'First .. Pos) & "zargo";
         end if;
      end Make_Zargo;

      Zargo_Name       : constant String := Make_Zargo (Name);
      Zargo_Lower_Name : constant String := To_Lower_Case (Zargo_Name);
      Result           : UString;

      procedure Find (Dir  : in String;
                      Done : out Boolean) is
      begin
         declare
            File_Path : constant String
              := Util.Files.Compose (Dir, Name);
         begin
            if Ada.Directories.Exists (File_Path) then
               Result := To_UString (File_Path);
               Done := True;
               return;
            end if;
         end;

         declare
            File_Path : constant String
              := Util.Files.Compose (Dir, Zargo_Name);
         begin
            if Ada.Directories.Exists (File_Path) then
               Result := To_UString (File_Path);
               Done := True;
               return;
            end if;
         end;

         declare
            File_Path : constant String
              := Util.Files.Compose (Dir, Zargo_Lower_Name);
         begin
            if Ada.Directories.Exists (File_Path) then
               Result := To_UString (File_Path);
               Done := True;
               return;
            end if;
         end;

         Done := False;
      end Find;

   begin
      Util.Files.Iterate_Path (Path, Find'Access);
      return To_String (Result);
   end Find_Profile;

   --  ------------------------------
   --  Read the UML profiles that are referenced by the current models.
   --  The UML profiles are installed in the UML config directory for dynamo's installation.
   --  ------------------------------
   procedure Read_Profiles (Handler : in out Artifact;
                            Context : in out Generator'Class) is
      Path : constant String := Context.Get_Parameter (Gen.Configs.GEN_UML_DIR);
      Iter : Util.Strings.Sets.Cursor := Handler.Profiles.First;
   begin
      while Util.Strings.Sets.Has_Element (Iter) loop
         declare
            Profile : constant String := Util.Strings.Sets.Element (Iter);
         begin
            if not Handler.Nodes.Contains (To_UString (Profile)) then
               Log.Info ("Reading the UML profile {0}", Profile);

               declare
                  Profile_Path : constant String := Find_Profile (Profile, Path);
               begin
                  --  We have a profile, load the UML model.
                  Handler.Read_Model (Profile_Path, Profile, Context, True);

                  --  Verify that we have the model,
                  --  report an error and remove it from the profiles.
                  if not Handler.Nodes.Contains (To_UString (Profile)) then
                     Context.Error ("UML profile {0} was not found", Profile);
                     Handler.Profiles.Delete (Profile);
                  end if;
               end;

               --  And start again from the beginning since new profiles could be necessary.
               Iter := Handler.Profiles.First;
            else
               Util.Strings.Sets.Next (Iter);
            end if;
         end;
      end loop;
   end Read_Profiles;

   --  ------------------------------
   --  Read the UML/XMI model file.
   --  ------------------------------
   procedure Read_Model (Handler       : in out Artifact;
                         File          : in String;
                         Profile       : in String;
                         Context       : in out Generator'Class;
                         Is_Predefined : in Boolean := False) is
      procedure Read (Key   : in UString;
                      Model : in out Gen.Model.XMI.Model_Map.Map);

      procedure Read (Key   : in UString;
                      Model : in out Gen.Model.XMI.Model_Map.Map) is
         pragma Unreferenced (Key);

         N      : constant Natural := Util.Strings.Rindex (File, '.');
         Name   : constant String := Ada.Directories.Base_Name (File);

         type Parser is new Util.Serialize.IO.XML.Parser with null record;

         --  Report an error while parsing the input stream.  The error message will be reported
         --  on the logger associated with the parser.  The parser will be set as in error so that
         --  the <b>Has_Error</b> function will return True after parsing the whole file.
         overriding
         procedure Error (Handler : in out Parser;
                          Message : in String);

         --  ------------------------------
         --  Report an error while parsing the input stream.  The error message will be reported
         --  on the logger associated with the parser.  The parser will be set as in error so that
         --  the <b>Has_Error</b> function will return True after parsing the whole file.
         --  ------------------------------
         overriding
         procedure Error (Handler : in out Parser;
                          Message : in String) is
         begin
            if Ada.Strings.Fixed.Index (Message, "Invalid absolute IRI") > 0
              and then Ada.Strings.Fixed.Index (Message, "org.omg.xmi.namespace.UML") > 0
            then
               return;
            end if;
            Context.Error ("{0}: {1}",
                           Name & ".xmi" & Parser'Class (Handler).Get_Location,
                           Message);
         end Error;

         Reader   : aliased Parser;
         Mapper   : Util.Serialize.Mappers.Processing;
         Info     : aliased XMI_Info;
         Def_Type : constant String := Context.Get_Parameter (Gen.Configs.GEN_UML_DEFAULT_TYPE);
      begin
         Info.Model        := Model'Unchecked_Access;
         Info.Parser       := Reader'Unchecked_Access;
         Info.Profiles     := Handler.Profiles'Unchecked_Access;
         Info.File         := To_UString (Name & ".xmi");
         Info.Default_Type := To_UString (Def_Type);
         Info.Is_Profile   := Is_Predefined;
         Mapper.Add_Mapping ("XMI", XMI_Mapping'Access);
         if Context.Get_Parameter (Gen.Configs.GEN_DEBUG_ENABLE) then
            Mapper.Dump (Log);
         end if;
         XMI_Mapper.Set_Context (Mapper, Info'Unchecked_Access);

         if N > 0 and then File (N .. File'Last) = ".zargo" then
            declare
               Pipe   : aliased Util.Streams.Pipes.Pipe_Stream;
               Buffer : Util.Streams.Buffered.Input_Buffer_Stream;
            begin
               Pipe.Open ("unzip -cq " & File & " " & Name & ".xmi",
                          Util.Processes.READ);
               Buffer.Initialize (Pipe'Unchecked_Access, 4096);
               Reader.Parse (Buffer, Mapper);
               Pipe.Close;
            end;
         else
            Reader.Parse (File, Mapper);
         end if;
      end Read;

      UML  : Gen.Model.XMI.Model_Map.Map;
      Name : constant UString
        := To_UString ((if Is_Predefined then Profile else Ada.Directories.Simple_Name (File)));
   begin
      Log.Info ("Reading XMI {0}", File);

      Handler.Initialized := True;
      Handler.Nodes.Include (Name, UML);
      Handler.Nodes.Update_Element (Handler.Nodes.Find (Name),
                                    Read'Access);
      Handler.Read_Profiles (Context);
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

   --  Generalization (limited to single inheritance).
   XMI_Mapping.Add_Mapping ("**/Generalization/@xmi.idref", FIELD_GENERALIZATION_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/@xmi.href", FIELD_GENERALIZATION_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/@xmi.id", FIELD_GENERALIZATION_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/Generalization.child/Class/@xmi.idref",
                            FIELD_GENERALIZATION_CHILD_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/Generalization.parent/Class/@xmi.idref",
                            FIELD_GENERALIZATION_PARENT_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/Generalization.child/DataType/@xmi.idref",
                            FIELD_GENERALIZATION_CHILD_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/Generalization.parent/DataType/@xmi.idref",
                            FIELD_GENERALIZATION_PARENT_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization/Generalization.parent/DataType/@href",
                            FIELD_GENERALIZATION_PARENT_ID);
   XMI_Mapping.Add_Mapping ("**/Generalization", FIELD_GENERALIZATION_END);

   --  Class attribute mapping.
   XMI_Mapping.Add_Mapping ("**/Attribute/@name", FIELD_ATTRIBUTE_NAME);
   XMI_Mapping.Add_Mapping ("**/Attribute/@xmi.id", FIELD_ATTRIBUTE_ID);
   XMI_Mapping.Add_Mapping ("**/Attribute/@visibility", FIELD_ATTRIBUTE_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/Attribute/@changeability", FIELD_ATTRIBUTE_CHANGEABILITY);
   XMI_Mapping.Add_Mapping ("**/Attribute.initialValue/Expression/@body",
                            FIELD_ATTRIBUTE_INITIAL_VALUE);
   XMI_Mapping.Add_Mapping ("**/Attribute", FIELD_ATTRIBUTE);

   --  Field multiplicity.
   XMI_Mapping.Add_Mapping ("**/MultiplicityRange/@lower", FIELD_MULTIPLICITY_LOWER);
   XMI_Mapping.Add_Mapping ("**/MultiplicityRange/@upper", FIELD_MULTIPLICITY_UPPER);

   --  Operation mapping.
   XMI_Mapping.Add_Mapping ("**/Operation/@name", FIELD_OPERATION_NAME);
   XMI_Mapping.Add_Mapping ("**/Operation/@xmi.id", FIELD_OPERATION_ID);
   XMI_Mapping.Add_Mapping ("**/Operation", FIELD_OPERATION_END);
   XMI_Mapping.Add_Mapping ("**/Parameter/@xmi.id", FIELD_PARAMETER_ID);
   XMI_Mapping.Add_Mapping ("**/Parameter/@name", FIELD_PARAMETER_NAME);
   XMI_Mapping.Add_Mapping ("**/Parameter/@kind", FIELD_PARAMETER_KIND);
   XMI_Mapping.Add_Mapping ("**/Parameter", FIELD_PARAMETER_END);
   XMI_Mapping.Add_Mapping ("**/Parameter/Parameter.type/Class/@xmi.idref", FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Parameter/Parameter.type/Class/@xmi.href", FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Parameter/Parameter.type/Class/@href", FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Parameter/Parameter.type/DataType/@xmi.href",
                            FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Parameter/Parameter.type/DataType/@href", FIELD_CLASSIFIER_HREF);

   --  Association mapping.
   XMI_Mapping.Add_Mapping ("**/Association/@name", FIELD_ASSOCIATION_NAME);
   XMI_Mapping.Add_Mapping ("**/Association/@xmi.id", FIELD_ASSOCIATION_ID);
   XMI_Mapping.Add_Mapping ("**/Association", FIELD_ASSOCIATION);

   --  Association end mapping.
   XMI_Mapping.Add_Mapping ("**/AssociationEnd/@name", FIELD_ASSOCIATION_END_NAME);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd/@xmi.id", FIELD_ASSOCIATION_END_ID);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd/@visibility", FIELD_ASSOCIATION_END_VISIBILITY);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd/@isNavigable", FIELD_ASSOCIATION_END_NAVIGABLE);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd", FIELD_ASSOCIATION_END);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd.participant/Class/@xmi.idref",
                            FIELD_ASSOCIATION_CLASS_ID);
   XMI_Mapping.Add_Mapping ("**/AssociationEnd.participant/Class/@href",
                            FIELD_ASSOCIATION_CLASS_ID);

   --  Comment mapping.
   XMI_Mapping.Add_Mapping ("**/Comment/@xmi.id", FIELD_COMMENT_ID);
   XMI_Mapping.Add_Mapping ("**/Comment/@body", FIELD_VALUE);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotatedElement/Class/@xmi.idref", FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotatedElement/Attribute/@xmi.idref",
                            FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotatedElement/Enumeration/@xmi.idref",
                            FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/Comment/Comment.annotatedElement/AssociationEnd/@xmi.idref",
                            FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/Comment", FIELD_COMMENT);

   --  Tagged value mapping.
   XMI_Mapping.Add_Mapping ("**/TaggedValue/@xmi.id", FIELD_TAGGED_ID);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.dataValue", FIELD_VALUE);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/@xmi.idref", FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/TagDefinition/@xmi.idref",
                            FIELD_ID_REF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue/TaggedValue.type/TagDefinition/@href", FIELD_HREF);
   XMI_Mapping.Add_Mapping ("**/TaggedValue", FIELD_TAGGED_VALUE);

   --  Tag definition mapping.
   XMI_Mapping.Add_Mapping ("**/TagDefinition/@xmi.id", FIELD_TAG_DEFINITION_ID);
   XMI_Mapping.Add_Mapping ("**/TagDefinition/@name", FIELD_TAG_DEFINITION_NAME);
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
                            FIELD_ENUMERATION_LITERAL);
   XMI_Mapping.Add_Mapping ("**/Enumeration/Enumeration.literal/EnumerationLiteral/@name",
                            FIELD_NAME);
   XMI_Mapping.Add_Mapping ("**/Enumeration/Enumeration.literal/EnumerationLiteral",
                            FIELD_ENUMERATION_LITERAL_END);
   XMI_Mapping.Add_Mapping ("**/Enumeration/@href", FIELD_ENUMERATION_HREF);
   XMI_Mapping.Add_Mapping ("**/Enumeration/@xmi.idref", FIELD_ENUMERATION_HREF);

   XMI_Mapping.Add_Mapping ("**/Classifier/@xmi.idref", FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Classifier/@href", FIELD_CLASSIFIER_HREF);

   XMI_Mapping.Add_Mapping ("**/Dependency.client/Class/@xmi.idref", FIELD_CLASSIFIER_HREF);
   XMI_Mapping.Add_Mapping ("**/Dependency.supplier/Class/@xmi.idref", FIELD_CLASSIFIER_HREF);

   --  Data type mapping.
   XMI_Mapping.Add_Mapping ("**/DataType/@xmi.id", FIELD_ID);
   XMI_Mapping.Add_Mapping ("**/DataType/@name", FIELD_DATA_TYPE_NAME);
   XMI_Mapping.Add_Mapping ("**/DataType", FIELD_DATA_TYPE);
   XMI_Mapping.Add_Mapping ("**/DataType/@href", FIELD_DATA_TYPE_HREF);
   XMI_Mapping.Add_Mapping ("**/DataType/@xmi.idref", FIELD_DATA_TYPE_HREF);
   XMI_Mapping.Add_Mapping ("**/StructuralFeature.type/Class/@xmi.idref",
                            FIELD_DATA_TYPE_HREF);

end Gen.Artifacts.XMI;
