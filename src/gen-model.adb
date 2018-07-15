-----------------------------------------------------------------------
--  gen-model -- Model for Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2018 Stephane Carrez
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
with Gen.Utils;
package body Gen.Model is

   --  ------------------------------
   --  Get the object unique name.
   --  ------------------------------
   function Get_Name (From : in Definition) return String is
   begin
      return Ada.Strings.Unbounded.To_String (From.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : in Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "comment" then
         return From.Comment;

      elsif Name = "rowIndex" then
         return Util.Beans.Objects.To_Object (From.Row_Index);

      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      else
         return From.Attrs.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : in Definition;
                           Name : in String) return String is
      V : constant Util.Beans.Objects.Object := From.Get_Value (Name);
   begin
      return Util.Beans.Objects.To_String (V);
   end Get_Attribute;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : in Definition;
                           Name : in String) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (From.Get_Attribute (Name));
   end Get_Attribute;

   --  ------------------------------
   --  Set the comment associated with the element.
   --  ------------------------------
   procedure Set_Comment (Def     : in out Definition;
                          Comment : in String) is
   begin
      Def.Comment := Util.Beans.Objects.To_Object (Comment);
   end Set_Comment;

   --  ------------------------------
   --  Initialize the definition from the DOM node attributes.
   --  ------------------------------
   procedure Initialize (Def  : in out Definition;
                         Name : in Ada.Strings.Unbounded.Unbounded_String;
                         Node : in DOM.Core.Node) is
      use type DOM.Core.Node;

      Attrs : constant DOM.Core.Named_Node_Map := DOM.Core.Nodes.Attributes (Node);
   begin
      Def.Name := Name;
      Def.Comment := Util.Beans.Objects.To_Object (Gen.Utils.Get_Comment (Node));

      for I in 0 .. DOM.Core.Nodes.Length (Attrs) loop
         declare
            A : constant DOM.Core.Node := DOM.Core.Nodes.Item (Attrs, I);
         begin
            if A /= null then
               declare
                  Name  : constant DOM.Core.DOM_String := DOM.Core.Nodes.Node_Name (A);
                  Value : constant DOM.Core.DOM_String := DOM.Core.Nodes.Node_Value (A);
               begin
                  Def.Attrs.Include (Name, Util.Beans.Objects.To_Object (Value));
               end;
            end if;
         end;
      end loop;
   end Initialize;

end Gen.Model;
