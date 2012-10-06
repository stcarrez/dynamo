-----------------------------------------------------------------------
--  gen-model-beans -- Ada Bean declarations
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

with Ada.Strings.Unbounded;

with Util.Beans.Objects;

with Gen.Model.Packages;
with Gen.Model.Tables;
package Gen.Model.Beans is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Bean Definition
   --  ------------------------------
   type Bean_Definition is new Definition with record
      Members        : aliased Gen.Model.Tables.Column_List.List_Definition;
      Members_Bean   : Util.Beans.Objects.Object;
      Package_Def    : Gen.Model.Packages.Package_Definition_Access;
      Type_Name      : Unbounded_String;
      Pkg_Name       : Unbounded_String;
   end record;
   type Bean_Definition_Access is access all Bean_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Bean_Definition;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Bean_Definition);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Bean_Definition);

   --  Create an attribute with the given name and add it to the bean.
   procedure Add_Attribute (Bean   : in out Bean_Definition;
                            Name   : in Unbounded_String;
                            Column : out Gen.Model.Tables.Column_Definition_Access);

   --  Create a bean with the given name.
   function Create_Bean (Name : in Unbounded_String) return Bean_Definition_Access;

end Gen.Model.Beans;
