-----------------------------------------------------------------------
--  gen-model -- Model for Code Generator
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Character_Datas;
package body Gen.Model is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Get the attribute identified by <b>Name</b> on the DOM node
   --  and return it as an EL object.
   --  ------------------------------
   function Get_Attribute (Node : DOM.Core.Node;
                           Name : String) return Util.Beans.Objects.Object is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      return Util.Beans.Objects.To_Object (V);
   end Get_Attribute;

   --  ------------------------------
   --  Set the DOM node associated with the definition object
   --  ------------------------------
   procedure Set_Node (Def   : in out Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural := 0) is
      pragma Unreferenced (Index);
   begin
      Def.Node := Node;
   end Set_Node;

   --  --------------------
   --  Get the comment associated with a node
   --  --------------------
   function Get_Comment (Def : in Definition) return String is
      Children : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Def.Node);
      Size     : constant Natural := DOM.Core.Nodes.Length (Children);
      Result   : Unbounded_String;
   begin
      for I in 0 .. Size - 1 loop
         declare
            N    : constant DOM.Core.Node       := DOM.Core.Nodes.Item (Children, I);
            Name : constant DOM.Core.DOM_String := DOM.Core.Nodes.Node_Name (N);
         begin
            if Name = "comment" then
               declare
                  Nodes : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (N);
                  S     : constant Natural            := DOM.Core.Nodes.Length (Nodes);
               begin
                  for J in 0 .. S - 1 loop
                     Append (Result,
                             DOM.Core.Character_Datas.Data (DOM.Core.Nodes.Item (Nodes, J)));
                  end loop;
               end;
            end if;
         end;
      end loop;
      return To_String (Result);
   end Get_Comment;

   --  ------------------------------
   --  Get the object unique name.
   --  ------------------------------
   function Get_Name (From : in Definition) return String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (From.Node, "name");
   begin
      return V;
   end Get_Name;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Definition;
                       Name : String) return Util.Beans.Objects.Object is
      use type DOM.Core.Node;
   begin
      if From.Node = null then
         return Util.Beans.Objects.Null_Object;
      elsif Name = "comment" then
         return Util.Beans.Objects.To_Object (From.Get_Comment);
      else
         return Get_Attribute (From.Node, Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : Definition;
                           Name : String) return String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (From.Node, Name);
   begin
      return V;
   end Get_Attribute;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : Definition;
                           Name : String) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (From.Get_Attribute (Name));
   end Get_Attribute;

   --  ------------------------------
   --  Get a boolean attribute
   --  ------------------------------
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : Boolean := False) return Boolean is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "yes" or V = "true" then
         return True;
      elsif V = "no" or V = "false" then
         return False;
      else
         return Default;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get a string attribute
   --  ------------------------------
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : String := "") return Ada.Strings.Unbounded.Unbounded_String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "" then
         return To_Unbounded_String (Default);
      else
         return To_Unbounded_String (V);
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get the value identified by the name from the attribute.
   --  Normalize the result string.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Normalized_Type (Node : DOM.Core.Node;
                                 Name : String) return String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V'Length > 11 and then V (V'First .. V'First + 9) = "java.lang." then
         return V (V'First + 10 .. V'Last);

      elsif V'Length > 10 and then V (V'First .. V'First + 8) = "java.sql." then
         return V (V'First + 9 .. V'Last);

      else
         return V;
      end if;
   end Get_Normalized_Type;

   --  ------------------------------
   --  Get the first DOM child from the given entity tag
   --  ------------------------------
   function Get_Child (Node : DOM.Core.Node;
                       Name : String) return DOM.Core.Node is
      Nodes : DOM.Core.Node_List :=
        DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
   begin
      if DOM.Core.Nodes.Length (Nodes) = 0 then
         DOM.Core.Free (Nodes);
         return null;
      else
         return Result : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, 0) do
            DOM.Core.Free (Nodes);
         end return;
      end if;
   end Get_Child;

end Gen.Model;
