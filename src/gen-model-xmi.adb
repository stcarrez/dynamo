-----------------------------------------------------------------------
--  gen-model-xmi -- UML-XMI model
--  Copyright (C) 2012 Stephane Carrez
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

with Ada.Text_IO;
with Util.Log.Loggers;
package body Gen.Model.XMI is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.XMI");

   --  ------------------------------
   --  Dump the XMI model elements.
   --  ------------------------------
   procedure Dump (Map : in Model_Map.Map) is
      Iter : Model_Map_Cursor := Map.First;
   begin
      while Has_Element (Iter) loop
         declare
            Node : constant Model_Element_Access := Element (Iter);
         begin
            Ada.Text_IO.Put_Line ("XMI " & Element_Type'Image (Node.Get_Type) & ":"
                                  & To_String (Node.XMI_Id) & " - " & To_String (Node.Name));
         end;
         Next (Iter);
      end loop;
   end Dump;

   --  ------------------------------
   --  Set the model name.
   --  ------------------------------
   procedure Set_Name (Node  : in out Model_Element;
                       Value : in Util.Beans.Objects.Object) is
   begin
      Node.Name := Util.Beans.Objects.To_Unbounded_String (Value);
   end Set_Name;

   --  ------------------------------
   --  Set the model XMI unique id.
   --  ------------------------------
   procedure Set_XMI_Id (Node  : in out Model_Element;
                         Value : in Util.Beans.Objects.Object) is
   begin
      Node.XMI_Id := Util.Beans.Objects.To_Unbounded_String (Value);
   end Set_XMI_Id;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Data_Type_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_DATA_TYPE;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Enum_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_ENUMERATION;
   end Get_Type;

   procedure Add_Literal (Node : in out Enum_Element;
                          Id   : in Util.Beans.Objects.Object;
                          Name : in Util.Beans.Objects.Object) is
   begin
      null;
   end Add_Literal;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Literal_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_ENUMERATION_LITERAL;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Stereotype_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_STEREOTYPE;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Comment_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_COMMENT;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Operation_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_OPERATION;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Attribute_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_ATTRIBUTE;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Association_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_ASSOCIATION;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Tagged_Value_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_TAGGED_VALUE;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Tag_Definition_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_TAG_DEFINITION;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Class_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_CLASS;
   end Get_Type;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Package_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_PACKAGE;
   end Get_Type;

end Gen.Model.XMI;
