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

with Ada.Tags;
with Ada.Text_IO;
with Util.Log.Loggers;
package body Gen.Model.XMI is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.XMI");

   --  ------------------------------
   --  Iterate on the model element of the type <tt>On</tt> and execute the <tt>Process</tt>
   --  procedure.
   --  ------------------------------
   procedure Iterate (Model   : in Model_Map.Map;
                      On      : in Element_Type;
                      Process : not null access procedure (Id : in Unbounded_String;
                                                           Node : in Model_Element_Access)) is
      Iter : Model_Map_Cursor := Model.First;
   begin
      while Has_Element (Iter) loop
         declare
            Node : constant Model_Element_Access := Element (Iter);
         begin
            if Node.Get_Type = On then
               Process (Model_Map.Key (Iter), Node);
            end if;
         end;
         Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Iterate over the model elements of the list.
   --  ------------------------------
   procedure Iterate_Elements (Closure : in out T;
                               List    : in Model_Vector;
                               Process : not null access
                                 procedure (Closure : in out T;
                                            Node    : in Model_Element_Access)) is
      Iter : Model_Cursor := List.First;
   begin
      while Model_Vectors.Has_Element (Iter) loop
         Process (Closure, Model_Vectors.Element (Iter));
         Model_Vectors.Next (Iter);
      end loop;
   end Iterate_Elements;

   --  ------------------------------
   --  Find the model element with the given XMI id.
   --  Returns null if the model element is not found.
   --  ------------------------------
   function Find (Model : in Model_Map.Map;
                  Id    : in Ada.Strings.Unbounded.Unbounded_String) return Model_Element_Access is
      Pos : constant Model_Map_Cursor := Model.Find (Id);
   begin
      if Has_Element (Pos) then
         return Element (Pos);
      else
         Log.Error ("Model element {0} not found", Ada.Strings.Unbounded.To_String (Id));
         return null;
      end if;
   end Find;

   --  ------------------------------
   --  Find the model element within all loaded UML models.
   --  Returns null if the model element is not found.
   --  ------------------------------
   function Find (Model   : in UML_Model;
                  Current : in Model_Map.Map;
                  Id      : in Ada.Strings.Unbounded.Unbounded_String)
                  return Model_Element_Access is

      Pos   : constant Natural := Index (Id, "#");
      First : Natural;
   begin
      if Pos = 0 then
         return Find (Current, Id);
      end if;
      First := Index (Id, "/", Pos, Ada.Strings.Backward);
      if First = 0 then
         First := 1;
      else
         First := First + 1;
      end if;
      declare
         Len       : constant Natural := Length (Id);
         Name      : constant Unbounded_String := Unbounded_Slice (Id, First, Pos - 1);
         Model_Pos : constant UML_Model_Map.Cursor := Model.Find (Name);
      begin
         if UML_Model_Map.Has_Element (Model_Pos) then
            return Find (UML_Model_Map.Element (Model_Pos),
                         Unbounded_Slice (Id, Pos + 1, Len));
         else
            Log.Error ("Model element {0} not found", To_String (Id));
            return null;
         end if;
      end;
   end Find;

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
   --  Reconcile all the UML model elements by resolving all the references to UML elements.
   --  ------------------------------
   procedure Reconcile (Model : in out UML_Model) is
      procedure Reconcile_Model (Key : in Ada.Strings.Unbounded.Unbounded_String;
                        Map : in out Model_Map.Map);

      procedure Reconcile_Model (Key : in Ada.Strings.Unbounded.Unbounded_String;
                                 Map : in out Model_Map.Map) is
         pragma Unreferenced (Key);

         Iter : Model_Map_Cursor := Map.First;
      begin
         while Has_Element (Iter) loop
            declare
               Node : constant Model_Element_Access := Element (Iter);
            begin
               Node.Reconcile (Model);
            end;
            Next (Iter);
         end loop;
         Gen.Model.XMI.Dump (Map);
      end Reconcile_Model;

      Iter : UML_Model_Map.Cursor := Model.First;
   begin
      while UML_Model_Map.Has_Element (Iter) loop
         UML_Model_Map.Update_Element (Model, Iter, Reconcile_Model'Access);
         UML_Model_Map.Next (Iter);
      end loop;
   end Reconcile;

   --  ------------------------------
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   procedure Reconcile (Node  : in out Model_Element;
                        Model : in UML_Model) is
   begin
      null;
   end Reconcile;

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
   --  Find the tag value element with the given name.
   --  Returns null if there is no such tag.
   --  ------------------------------
   function Find_Tag_Value (Node : in Model_Element;
                            Name : in String) return Tagged_Value_Element_Access is
      Pos : Model_Cursor := Node.Tagged_Values.First;
      Tag : Model_Element_Access;
   begin
      while Model_Vectors.Has_Element (Pos) loop
         Tag := Model_Vectors.Element (Pos);
         if Tag.Name = Name then
            return Tagged_Value_Element'Class (Tag.all)'Access;
         end if;
         Model_Vectors.Next (Pos);
      end loop;
      return null;
   end Find_Tag_Value;

   --  ------------------------------
   --  Returns True if the model element has the stereotype with the given name.
   --  ------------------------------
   function Has_Stereotype (Node : in Model_Element;
                            Name : in String) return Boolean is
      Iter : Model_Cursor := Node.Stereotypes.First;
   begin
      while Model_Vectors.Has_Element (Iter) loop
         declare
            S : constant Model_Element_Access := Model_Vectors.Element (Iter);
         begin
            if S.Name = Name then
               return True;
            end if;
         end;
         Model_Vectors.Next (Iter);
      end loop;
      return False;
   end Has_Stereotype;

   --  ------------------------------
   --  Get the documentation and comment associated with the model element.
   --  Returns the empty string if there is no comment.
   --  ------------------------------
   function Get_Comment (Node : in Model_Element) return String is
      Doc    : constant Tagged_Value_Element_Access := Node.Find_Tag_Value (TAG_DOCUMENTATION);
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Doc /= null then
         Ada.Strings.Unbounded.Append (Result, Doc.Value);
      end if;
      return Ada.Strings.Unbounded.To_String (Result);
   end Get_Comment;

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
   function Get_Type (Node : in Association_End_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_ASSOCIATION_END;
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
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Tagged_Value_Element;
                        Model : in UML_Model) is
      Item : constant Model_Element_Access := Find (Model, Node.Model.all, Node.Ref_Id);
   begin
      if Item /= null then
         Node.Name := Item.Name;
         if not (Item.all in Tag_Definition_Element'Class) then
            Log.Error ("Element {0} is not a tag definition.  Tag is {1}, reference is {2}",
                       Ada.Strings.Unbounded.To_String (Item.Name),
                       Ada.Tags.Expanded_Name (Item'Tag),
                       Ada.Strings.Unbounded.To_String (Node.Ref_Id));
         else
            Node.Tag_Def := Tag_Definition_Element'Class (Item.all)'Access;
         end if;
      end if;
   end Reconcile;

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
