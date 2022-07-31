-----------------------------------------------------------------------
--  gen-model -- Model for Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2018, 2020, 2021, 2022 Stephane Carrez
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

with Util.Log;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Objects.Maps;

with DOM.Core;
package Gen.Model is

   package UBO renames Util.Beans.Objects;

   --  Exception raised if a name is already registered in the model.
   --  This exception is raised if a table, an enum is already defined.
   Name_Exist : exception;

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   type Definition is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with private;
   type Definition_Access is access all Definition'Class;

   --  Prepare the generation of the model.
   procedure Prepare (O : in out Definition) is null;

   --  Get the object unique name.
   function Get_Name (From : in Definition) return String;
   function Name (From : in Definition) return UString;

   --  Set the object unique name.
   procedure Set_Name (Def  : in out Definition;
                       Name : in String);
   procedure Set_Name (Def  : in out Definition;
                       Name : in UString);

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Definition;
                       Name : in String) return UBO.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : in Definition;
                           Name : in String) return String;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Attribute (From : in Definition;
                           Name : in String) return UString;

   --  Set the comment associated with the element.
   procedure Set_Comment (Def     : in out Definition;
                          Comment : in String);

   --  Get the comment associated with the element.
   function Get_Comment (Def : in Definition) return UBO.Object;

   --  Set the location (file and line) where the model element is defined in the XMI file.
   procedure Set_Location (Node     : in out Definition;
                           Location : in String);

   --  Get the location file and line where the model element is defined.
   function Get_Location (Node : in Definition) return String;

   --  Initialize the definition from the DOM node attributes.
   procedure Initialize (Def  : in out Definition;
                         Name : in UString;
                         Node : in DOM.Core.Node);

   --  Validate the definition by checking and reporting problems to the logger interface.
   procedure Validate (Def : in out Definition;
                       Log : in out Util.Log.Logging'Class);

private

   procedure Set_Index (Def   : in out Definition;
                        Index : in Natural);

   type Definition is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with record
      Row_Index  : Natural;
      Def_Name   : UString;
      Attrs      : UBO.Maps.Map_Bean;
      Comment    : UBO.Object;
      Location   : UString;
   end record;

end Gen.Model;
