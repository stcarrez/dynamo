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
with Ada.Finalization;
with Ada.Strings.Unbounded;

with EL.Beans;
with EL.Objects;

with DOM.Core;
package Gen.Model is

   --  Get the attribute identified by <b>Name</b> on the DOM node
   --  and return it as an EL object.
   function Get_Attribute (Node : DOM.Core.Node;
                           Name : String) return EL.Objects.Object;

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   type Definition is new Ada.Finalization.Limited_Controlled
     and EL.Beans.Readonly_Bean with private;
   type Definition_Access is access all Definition'Class;

   --  Set the DOM node associated with the definition object
   procedure Set_Node (Def   : in out Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural := 0);

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : Definition;
                       Name : String) return EL.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : Definition;
                           Name : String) return String;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : Definition;
                           Name : String) return Ada.Strings.Unbounded.Unbounded_String;

   --  Get the comment associated with the definition.
   function Get_Comment (Def : Definition) return String;

   --  Get a boolean attribute
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : Boolean := False) return Boolean;

   --  Get a string attribute
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : String := "") return Ada.Strings.Unbounded.Unbounded_String;

   --  Get the first DOM child from the given entity tag
   function Get_Child (Node : DOM.Core.Node;
                       Name : String) return DOM.Core.Node;

private

   type Definition is new Ada.Finalization.Limited_Controlled
     and EL.Beans.Readonly_Bean with record
      Node : DOM.Core.Node;
   end record;

end Gen.Model;
