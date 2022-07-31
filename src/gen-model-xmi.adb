-----------------------------------------------------------------------
--  gen-model-xmi -- UML-XMI model
--  Copyright (C) 2012, 2013, 2015, 2016, 2021, 2022 Stephane Carrez
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

with Util.Strings;
with Util.Log.Loggers;
package body Gen.Model.XMI is

   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.XMI");

   procedure Append_Message (Into : in out UString;
                             Message : in String);

   --  ------------------------------
   --  Append a message to the error message.  A newline is inserted if the buffer contains
   --  an existing message.
   --  ------------------------------
   procedure Append_Message (Into    : in out UString;
                             Message : in String) is
   begin
      if Length (Into) > 0 then
         Append (Into, ASCII.LF);
      end if;
      Append (Into, Message);
   end Append_Message;

   --  ------------------------------
   --  Iterate on the model element of the type <tt>On</tt> and execute the <tt>Process</tt>
   --  procedure.
   --  ------------------------------
   procedure Iterate (Model   : in Model_Map.Map;
                      On      : in Element_Type;
                      Process : not null access procedure (Id : in UString;
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
            Pos : constant Model_Map_Cursor := Model.Find (To_UString (Key));
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
            Pos  : Natural;
         begin
            if Key (Key'First) /= '@' then
               Pos := Util.Strings.Index (Key, '.');
            else
               Pos := 0;
            end if;
            while Has_Element (Iter) loop
               declare
                  Node : Model_Element_Access := Element (Iter);
               begin
                  --  Find in the package only.  If there is no '.', check the package name only.
                  if Node.Get_Type = XMI_PACKAGE then
                     if Pos = 0 and then Node.Name = Key then
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
      Model_Pos : constant UML_Model_Map.Cursor := Model.Find (To_UString (Name));
      Item      : Model_Element_Access;
   begin
      if UML_Model_Map.Has_Element (Model_Pos) then
         if Mode = BY_ID or else Mode = BY_NAME then
            Item := Find (UML_Model_Map.Element (Model_Pos), Key, Mode);
         else
            declare
               Iter : Model_Map_Cursor := UML_Model_Map.Element (Model_Pos).First;
            begin
               while Has_Element (Iter) loop
                  declare
                     Node : constant Model_Element_Access := Element (Iter);
                  begin
                     if Node.all in Element_Type'Class and then Node.Name = Key then
                        return Element_Type'Class (Node.all)'Access;
                     end if;
                  end;
                  Next (Iter);
               end loop;
            end;
         end if;
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
                  Id      : in UString)
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
         Name      : constant UString := Unbounded_Slice (Id, First, Pos - 1);
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
         Element (Iter).Dump;
         Next (Iter);
      end loop;
   end Dump;

   --  ------------------------------
   --  Reconcile all the UML model elements by resolving all the references to UML elements.
   --  ------------------------------
   procedure Reconcile (Model : in out UML_Model;
                        Debug : in Boolean := False) is
      procedure Reconcile_Model (Key : in UString;
                        Map : in out Model_Map.Map);

      procedure Reconcile_Model (Key : in UString;
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
      if Pos = 0 or else Name (Name'First) = '@' then
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
                       Value : in UBO.Object) is
   begin
      if not UBO.Is_Null (Value) then
         Node.Set_Name (UBO.To_Unbounded_String (Value));
      end if;
   end Set_Name;

   --  ------------------------------
   --  Set the model XMI unique id.
   --  ------------------------------
   procedure Set_XMI_Id (Node  : in out Model_Element;
                         Value : in UBO.Object) is
   begin
      Node.XMI_Id := UBO.To_Unbounded_String (Value);
   end Set_XMI_Id;

   --  ------------------------------
   --  Validate the node definition as much as we can before the reconcile phase.
   --  If an error is detected, return a message.  Returns an empty string if everything is ok.
   --  ------------------------------
   function Get_Error_Message (Node : in Model_Element) return String is
      Result : UString;
   begin
      if Length (Node.XMI_Id) = 0 then
         Append (Result, "the 'xmi.id' attribute is empty");
      end if;
      return To_String (Result);
   end Get_Error_Message;

   --  ------------------------------
   --  Dump the node to get some debugging description about it.
   --  ------------------------------
   procedure Dump (Node : in Model_Element) is
   begin
      Log.Info ("XMI {0} - {2}: {1}",
                Element_Type'Image (Model_Element'Class (Node).Get_Type),
                To_String (Node.XMI_Id), To_String (Node.Name));
      if Node.Parent /= null then
         Log.Info ("  Parent: {0} ({1})", To_String (Node.Parent.Name),
                   Element_Type'Image (Node.Parent.Get_Type));
      end if;
      declare
         Iter : Model_Cursor := Node.Tagged_Values.First;
         Tag  : Tagged_Value_Element_Access;
      begin
         while Model_Vectors.Has_Element (Iter) loop
            Tag := Tagged_Value_Element'Class (Model_Vectors.Element (Iter).all)'Access;
            if Tag.Tag_Def /= null then
               Log.Info ("  Tag: {0} = {1}",
                         To_String (Tag.Tag_Def.Name),
                         To_String (Tag.Value));
            else
               Log.Info ("  Undef tag: {0} = {1}",
                         To_String (Tag.XMI_Id), To_String (Tag.Value));
            end if;
            Model_Vectors.Next (Iter);
         end loop;
      end;
      declare
         Stereotype : Model_Cursor := Node.Stereotypes.First;
      begin
         while Model_Vectors.Has_Element (Stereotype) loop
            Log.Info ("  Stereotype: <<{0}>>: {1}",
                      To_String (Model_Vectors.Element (Stereotype).Name),
                      To_String (Model_Vectors.Element (Stereotype).XMI_Id));
            Model_Vectors.Next (Stereotype);
         end loop;
      end;
   end Dump;

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
   --  Find the tag value associated with the given tag definition.
   --  Returns the tag value if it was found, otherwise returns the default
   --  ------------------------------
   function Find_Tag_Value (Node       : in Model_Element;
                            Definition : in Tag_Definition_Element_Access;
                            Default    : in String := "") return String is
      Pos : Model_Cursor := Node.Tagged_Values.First;
      Tag : Model_Element_Access;
   begin
      while Model_Vectors.Has_Element (Pos) loop
         Tag := Model_Vectors.Element (Pos);
         if Tag.all in Tagged_Value_Element'Class and then
           Tagged_Value_Element'Class (Tag.all).Tag_Def = Definition
         then
            return To_String (Tagged_Value_Element'Class (Tag.all).Value);
         end if;
         Model_Vectors.Next (Pos);
      end loop;
      return Default;
   end Find_Tag_Value;

   --  ------------------------------
   --  Get the documentation and comment associated with the model element.
   --  Returns the empty string if there is no comment.
   --  ------------------------------
   function Get_Comment (Node : in Model_Element) return String is
      procedure Collect_Comment (Id   : in UString;
                                 Item : in Model_Element_Access);

      Doc    : constant Tagged_Value_Element_Access := Node.Find_Tag_Value (TAG_DOCUMENTATION);
      Result : UString;

      procedure Collect_Comment (Id   : in UString;
                                 Item : in Model_Element_Access) is
         pragma Unreferenced (Id);

         Comment : constant Comment_Element_Access := Comment_Element'Class (Item.all)'Access;
      begin
         if Comment.Ref_Id = Node.XMI_Id then
            Append (Result, Comment.Text);
         end if;
      end Collect_Comment;

   begin
      Iterate (Node.Model.all, XMI_COMMENT, Collect_Comment'Access);
      if Doc /= null then
         Append (Result, Doc.Value);
      end if;
      return To_String (Result);
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
      Item : constant Model_Element_Access := Find (Model, Node.Model.all, Node.Ref_Id);
   begin
      if Item /= null then
         Node.Set_Name (Item.Name);
         Node.Ref    := Item;
         Node.XMI_Id := Item.XMI_Id;
      end if;
      Model_Element (Node).Reconcile (Model);
   end Reconcile;

   --  ------------------------------
   --  Set the reference id and collect in the profiles set the UML profiles that must
   --  be loaded to get the reference.
   --  ------------------------------
   procedure Set_Reference_Id (Node     : in out Ref_Type_Element;
                               Ref      : in String;
                               Profiles : in out Util.Strings.Sets.Set) is
      Pos : constant Natural := Util.Strings.Index (Ref, '#');
   begin
      Node.Ref_Id := To_UString (Ref);
      if Pos > 0 then
         declare
            First : constant Natural := Util.Strings.Rindex (Ref, '/', Pos);
         begin
            Profiles.Include (Ref (First + 1 .. Pos - 1));
         end;
      end if;
   end Set_Reference_Id;

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

   --  ------------------------------
   --  Validate the node definition as much as we can before the reconcile phase.
   --  An enum must not be empty, it must have at least one literal.
   --  If an error is detected, return a message.  Returns an empty string if everything is ok.
   --  ------------------------------
   overriding
   function Get_Error_Message (Node : in Enum_Element) return String is
      Result : UString;
   begin
      Append (Result, Model_Element (Node).Get_Error_Message);
      if Node.Elements.Is_Empty then
         Append_Message (Result, "the enum '" & To_String (Node.Name) & "' is empty.");
      end if;
      return To_String (Result);
   end Get_Error_Message;

   --  ------------------------------
   --  Create an enum literal and add it to the enum.
   --  ------------------------------
   procedure Add_Literal (Node    : in out Enum_Element;
                          Id      : in UBO.Object;
                          Name    : in UBO.Object;
                          Literal : out Literal_Element_Access) is
   begin
      Literal := new Literal_Element (Node.Model);
      Literal.XMI_Id := UBO.To_Unbounded_String (Id);
      Literal.Set_Name (UBO.To_Unbounded_String (Name));
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
      Item : Model_Element_Access;
   begin
      if Length (Node.Ref_Id) = 0 then
         return;
      end if;
      Item := Find (Model, Node.Model.all, Node.Ref_Id);
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
   --  Get the documentation and comment associated with the model element.
   --  Integrates the comment from the association itself as well as this association end.
   --  Returns the empty string if there is no comment.
   --  ------------------------------
   overriding
   function Get_Comment (Node : in Association_End_Element) return String is
      Comment             : constant String := Model_Element (Node).Get_Comment;
      Association_Comment : constant String := Node.Parent.Get_Comment;
   begin
      if Association_Comment'Length = 0 then
         return Comment;
      elsif Comment'Length = 0 then
         return Association_Comment;
      else
         return Association_Comment & ASCII.LF & Comment;
      end if;
   end Get_Comment;

   --  ------------------------------
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Association_End_Element;
                        Model : in UML_Model) is
   begin
      Model_Element (Node).Reconcile (Model);
   end Reconcile;

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

      Target := Find (Model, From.Model.all, To.Ref_Id);
      if Target = null then
         Log.Error ("Association end {0} not found", To_String (From.Name));
         return;
      end if;
      Source := Find (Model, From.Model.all, From.Ref_Id);
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
   --  Validate the node definition as much as we can before the reconcile phase.
   --  An association must contain two ends and a name is necessary on the navigable ends.
   --  If an error is detected, return a message.  Returns an empty string if everything is ok.
   --  ------------------------------
   overriding
   function Get_Error_Message (Node : in Association_Element) return String is
      use type Ada.Containers.Count_Type;

      Result : UString;
   begin
      Append (Result, Model_Element (Node).Get_Error_Message);
      if Length (Node.Name) = 0 then
         Append_Message (Result, "Association has an empty name.");
      end if;
      if Node.Connections.Length = 2 then
         declare
            First, Second : Association_End_Element_Access;
         begin
            First  := Association_End_Element'Class (Node.Connections.Element (1).all)'Access;
            Second := Association_End_Element'Class (Node.Connections.Element (2).all)'Access;

            if First.Navigable and then Length (First.Name) = 0 then
               Append_Message (Result, "Association '" & To_String (Node.Name) &
                                 "' has a navigable association end with an empty name.");
            end if;
            if Second.Navigable and then Length (Second.Name) = 0 then
               Append_Message (Result, "Association '" & To_String (Node.Name) &
                                 "' has a navigable association end with an empty name.");
            end if;
            if not First.Navigable and then not Second.Navigable then
               Append_Message (Result, "Association '" & To_String (Node.Name) &
                                 "' has no navigable association ends.");
            end if;

         end;
      elsif Node.Connections.Length /= 0 then
         Append_Message (Result, "Association '" & To_String (Node.Name)
                         & "' needs 2 association ends");
      end if;
      return To_String (Result);
   end Get_Error_Message;

   --  ------------------------------
   --  Reconcile the association between classes in the package.  Find the association
   --  ends and add the necessary links to the corresponding class elements.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Association_Element;
                        Model : in UML_Model) is
      use type Ada.Containers.Count_Type;
   begin
      Model_Element (Node).Reconcile (Model);
      if Node.Connections.Length >= 2 then
         declare
            First, Second : Association_End_Element_Access;
         begin
            First  := Association_End_Element'Class (Node.Connections.Element (1).all)'Access;
            Second := Association_End_Element'Class (Node.Connections.Element (2).all)'Access;

            First.Make_Association (Second.all, Model);
            Second.Make_Association (First.all, Model);
         end;
      elsif Node.Connections.Length > 0 then
         Log.Info ("Association {0} needs 2 association ends", To_String (Node.Name));
      end if;
   end Reconcile;

   --  ------------------------------
   --  Get the element type.
   --  ------------------------------
   overriding
   function Get_Type (Node : in Generalization_Element) return Element_Type is
      pragma Unreferenced (Node);
   begin
      return XMI_GENERALIZATION;
   end Get_Type;

   --  ------------------------------
   --  Reconcile the association between classes in the package.  Find the association
   --  ends and add the necessary links to the corresponding class elements.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Generalization_Element;
                        Model : in UML_Model) is
   begin
      Ref_Type_Element (Node).Reconcile (Model);
      Node.Child_Class := Find (Model, Node.Model.all, Node.Child_Id);
      if Node.Child_Class /= null then
         if Node.Child_Class.all in Class_Element'Class then
            Class_Element'Class (Node.Child_Class.all).Parent_Class := Node'Unchecked_Access;
         elsif Node.Child_Class.all in Data_Type_Element'Class then
            Data_Type_Element'Class (Node.Child_Class.all).Parent_Type := Node'Unchecked_Access;
         end if;
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
         Node.Set_Name (Item.Name);
         if not (Item.all in Tag_Definition_Element'Class) then
            Log.Error ("Element {0} is not a tag definition.  Tag is {1}, reference is {2}",
                       To_String (Item.Name),
                       Ada.Tags.Expanded_Name (Item'Tag),
                       To_String (Node.Ref_Id));
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
   --  Reconcile the element by resolving the references to other elements in the model.
   --  ------------------------------
   overriding
   procedure Reconcile (Node  : in out Class_Element;
                        Model : in UML_Model) is
   begin
      if Node.Parent_Class /= null then
         Node.Parent_Class.Reconcile (Model);
      end if;
      Data_Type_Element (Node).Reconcile (Model);
   end Reconcile;

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
