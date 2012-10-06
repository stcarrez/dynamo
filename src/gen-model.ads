-----------------------------------------------------------------------
--  gen-model -- Model for Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Objects.Maps;

with DOM.Core;
package Gen.Model is

   --  Exception raised if a name is already registered in the model.
   --  This exception is raised if a table, an enum is already defined.
   Name_Exist : exception;

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   type Definition is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with record
      Row_Index  : Natural;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Attrs      : Util.Beans.Objects.Maps.Map_Bean;
      Comment    : Util.Beans.Objects.Object;
   end record;
   type Definition_Access is access all Definition'Class;

   --  Prepare the generation of the model.
   procedure Prepare (O : in out Definition) is null;

   --  Get the object unique name.
   function Get_Name (From : in Definition) return String;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : in Definition;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : in Definition;
                           Name : in String) return String;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : in Definition;
                           Name : in String) return Ada.Strings.Unbounded.Unbounded_String;

   --  Set the comment associated with the element.
   procedure Set_Comment (Def     : in out Definition;
                          Comment : in String);

   --  Initialize the definition from the DOM node attributes.
   procedure Initialize (Def  : in out Definition;
                         Name : in Ada.Strings.Unbounded.Unbounded_String;
                         Node : in DOM.Core.Node);

end Gen.Model;
