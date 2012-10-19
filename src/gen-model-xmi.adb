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

with Util.Strings;
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
   --  Find the model element with the given XMI id or given name.
   --  Returns null if the model element is not found.
   --  ------------------------------
   function Find (Model : in Model_Map.Map;
                  Key   : in String;
                  Mode  : in Search_Type := BY_ID) return Model_Element_Access is
   begin
      if Mode = BY_ID then
         declare
            Pos : constant Model_Map_Cursor := Model.Find (To_Unbounded_String (Key));
         begin
            if Has_Element (Pos) then
               return Element (Pos);
            else
               Log.Error ("Model element id '{0}' not found", Key);
               return null;
            end if;
         end;
      else
         declare
            Iter : Model_Map_Cursor := Model.First;
            Pos  : constant Natural := Util.Strings.Index (Key, '.');
         begin
            while Has_Element (Iter) loop
               declare
                  Node : Model_Element_Access := Element (Iter);
               begin
                  --  Find in the package only.  If there is no '.', check the package name only.
                  if Node.Get_Type = XMI_PACKAGE then
                     if Pos = 0 and Node.Name = Key then
                        return Node;
                     end if;

                     --  Check that the package name matches and look in it.
                     if Pos > 0 and then Node.Name = Key (Key'First .. Pos - 1) then
                        Node := Node.Find (Key (Pos + 1 .. Key'Last));
                        if Node /= null then
                           return Node;
                        end if;
                     end if;
                  end if;
               end;
               Next (Iter);
            end loop;
         end;
         return null;
      end if;
   end Find;

   --  ------------------------------
   --  Find from the model file identified by <tt>Name</tt>, the model element with the
   --  identifier or name represented by <tt>Key</tt>.
   --  Returns null if the model element is not found.
   --  ------------------------------
   function Find_Element (Model   : in UML_Model;
                          Name    : in String;
                          Key     : in String;
                          Mode    : in Search_Type := BY_ID)
                          return Element_Type_Access is
      Model_Pos : constant UML_Model_Map.Cursor := Model.Find (To_Unbounded_String (Name));
   begin
      if UML_Model_Map.Has_Element (Model_Pos) then
         declare
            Item : constant Model_Element_Access := Find (UML_Model_Map.Element (Model_Pos),
                                                          Key, Mode);
         begin
            if Item = null then
               Log.Error ("The model file {0} does not define {1}",
                          Name, Key);
               return null;
            end if;
            if not (Item.all in Element_Type'Class) then
               Log.Error ("The model file {0} defines the element {1}",
                          Name, Key);
               return null;
            end if;
            return Element_Type'Class (Item.all)'Access;
         end;
      else
         Log.Error ("Model file {0} not found", Name);
         return null;
      end if;
   end Find_Element;

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
         return Find (Current, To_String (Id));
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
                         Slice (Id, Pos + 1, Len));
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
   procedure Reconcile (Model : in out UML_Model;
                        Debug : in Boolean := False) is
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
         if Debug then
            Gen.Model.XMI.Dump (Map);
         end if;
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
      Iter : Model_Cursor := Node.Stereotypes.First;
   begin
      while Model_Vectors.Has_Element (Iter) loop
         Model_Vectors.Element (Iter).Reconcile (Model);
         Model_Vectors.Next (Iter);
      end loop;
   end Reconcile;

   --  ------------------------------
   --  Find the element with the given name.  If the name is a qualified name, navigate
   --  down the package/class to find the appropriate element.
   --  Returns null if the element was not found.
   --  ------------------------------
   function Find (Node : in Model_Element;
                  Name : in String) return Model_Element_Access is
      Pos   : constant Natural := Util.Strings.Index (Name, '.');
      Iter  : Model_Cursor;
      Item  : Model_Element_Access;
   begin
      if Pos = 0 then
         Iter := Node.Elements.First;
         while Model_Vectors.Has_Element (Iter) loop
            Item := Model_Vectors.Element (Iter);
            if Item.Name = Name then
               return Item;
            end if;
            Model_Vectors.Next (Iter);
         end loop;
         return null;
      else
         Item := Node.Find (Name (Name'First .. Pos - 1));
         if Item = null then
            return null;
         end if;
         return Item.Find (Name (Pos + 1 .. Name'Last));
      end if;
   end Find;

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
   --  Get the documentation and comment associated with the model element.
   --  Returns the empty string if there is no comment.
   --  ------------------------------
   function Get_Comment (Node : in Model_Element) return String is
      procedure Collect_Comment (Id   : in Unbounded_String;
                                 Item : in Model_Element_Access);

      Doc    : constant Tagged_Value_Element_Access := Node.Find_Tag_Value (TAG_DOCUMENTATION);
      Result : Ada.Strings.Unbounded.Unbounded_String;

      procedure Collect_Comment (Id   : in Unbounded_String;
                                 Item : in Model_Element_Access) is
         pragma Unreferenced (Id);

         Comment : constant Comment_Element_Access := Comment_Element'Class (Item.all)'Access;
      begin
         if Comment.Ref_Id = Node.XMI_Id then
            Ada.Strings.Unbounded.Append (Result, Comment.Text);
         end if;
      end Collect_Comment;

   begin
      Iterate (Node.Model.all, XMI_COMMENT, Collect_Comment'Access);
      if Doc /= null then
         Ada.Strings.Unbounded.Append (Result, Doc.Value);
      end if;
      return Ada.Strings.Unbounded.To_String (Result);
   end Get_Comment;

   --  ------------------------------
   --  Get the full qualified name for the element.
   --  ------------------------------
   function Get_Qualified_Name (Node : in Model_Element) return String is
   begin
      if Node.Parent /= null then
         return Node.Parent.Get_Qualified_Name & "." & To_String (Node.Name);
      else
         return To_String (Node.Name);
      end if;
   end Get_Qualified_Name;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Ref_Type_Element) return Element_Type is
   begin
      if Node.Ref /= null then
         return Node.Ref.Get_Type;
      else
         return XMI_UNKNOWN;
      end if;
   end Get_Type;

   --  ------------------------------
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Ref_Type_Element;
                        Model : in UML_Model) is
      Item : constant Model_Element_Access := Find (Model, Node.Model.all, Node.Href);
   begin
      if Item /= null then
         Node.Name := Item.Name;
         Node.Ref  := Item;
         Node.XMI_Id := Item.XMI_Id;
      end if;
      Model_Element (Node).Reconcile (Model);
   end Reconcile;

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
      Literal : constant Literal_Element_Access := new Literal_Element (Node.Model);
   begin
      Literal.XMI_Id := Util.Beans.Objects.To_Unbounded_String (Id);
      Literal.Name   := Util.Beans.Objects.To_Unbounded_String (Name);
      Node.Elements.Append (Literal.all'Access);
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
   --  Returns True if the model element has the stereotype with the given name.
   --  ------------------------------
   function Has_Stereotype (Node       : in Model_Element'Class;
                            Stereotype : in Stereotype_Element_Access) return Boolean is
      Iter : Model_Cursor := Node.Stereotypes.First;
   begin
      if Stereotype = null then
         return False;
      end if;
      while Model_Vectors.Has_Element (Iter) loop
         declare
            S : constant Model_Element_Access := Model_Vectors.Element (Iter);
         begin
            if S = Stereotype.all'Access then
               return True;
            end if;
            if S.XMI_Id = Stereotype.XMI_Id then
               return True;
            end if;
         end;
         Model_Vectors.Next (Iter);
      end loop;
      return False;
   end Has_Stereotype;

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
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Attribute_Element;
                        Model : in UML_Model) is
      Item : constant Model_Element_Access := Find (Model, Node.Model.all, Node.Ref_Id);
   begin
      Model_Element (Node).Reconcile (Model);
      if Item = null then
         return;
      end if;
      if not (Item.all in Data_Type_Element'Class) then
         Log.Error ("Invalid data type {0}", To_String (Node.Ref_Id));
         return;
      end if;
      Node.Data_Type := Data_Type_Element'Class (Item.all)'Access;
   end Reconcile;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Parameter_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_PARAMETER;
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
   --  Make the association between the two ends.
   --  ------------------------------
   procedure Make_Association (From  : in out Association_End_Element;
                               To    : in out Association_End_Element'Class;
                               Model : in UML_Model) is
      Target : Model_Element_Access;
      Source : Model_Element_Access;
   begin
      Log.Info ("Reconcile association {0} - {1}",
                To_String (From.Name), To_String (To.Name));

      Target := Find (Model, From.Model.all, To.Target);
      if Target = null then
         Log.Error ("Association end {0} not found", To_String (From.Name));
         return;
      end if;
      Source := Find (Model, From.Model.all, From.Target);
      if Source = null then
         Log.Error ("Association end {0} not found", To_String (To.Name));
         return;
      end if;

      if From.Navigable then
         Class_Element'Class (Target.all).Associations.Append (From'Unchecked_Access);
         From.Target_Element := Target.all'Access;
         From.Source_Element := Source.all'Access;
         Log.Info ("Class {0} { {1}: {2} }",
                   To_String (Target.Name),
                   To_String (From.Name),
                   To_String (Source.Name));
         if Length (From.Name) = 0 then
            Log.Error ("Class {0}: missing association end name to class {1}",
                       To_String (Target.Name), To_String (Source.Name));
         end if;
      end if;
   end Make_Association;

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
   --  Reconcile the association between classes in the package.  Find the association
   --  ends and add the necessary links to the corresponding class elements.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Association_Element;
                        Model : in UML_Model) is
      use type Ada.Containers.Count_Type;
   begin
      if Node.Connections.Length >= 2 then
         declare
            First, Second : Association_End_Element_Access;
         begin
            First  := Association_End_Element'Class (Node.Connections.Element (1).all)'Access;
            Second := Association_End_Element'Class (Node.Connections.Element (2).all)'Access;

            First.Make_Association (Second.all, Model);
            Second.Make_Association (First.all, Model);
         end;
      else
         Log.Error ("Association {0} needs 2 association ends",
                    To_String (Node.Name));
      end if;
   end Reconcile;

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
      Model_Element (Node).Reconcile (Model);
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
