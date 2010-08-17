-----------------------------------------------------------------------
--  gen-model-list -- List bean interface for model objects
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

with EL.Beans;
with EL.Objects;
with Gen.Model;
with DOM.Core.Nodes;

generic
   type T is new Gen.Model.Definition with private;
package Gen.Model.List is

   type List_Definition is limited new EL.Beans.List_Bean with private;

   --  Set the DOM nodes associated with the list
   procedure Set_List (Def   : in out List_Definition;
                       Nodes : in DOM.Core.Node_List);

   --  Get the DOM nodes representing the children
   function Get_List (Def : List_Definition) return DOM.Core.Node_List;

   --  Get the number of elements in the list.
   function Get_Count (From : List_Definition) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   procedure Set_Row_Index (From  : in out List_Definition;
                            Index : in Natural);

   --  Get the element at the current row index.
   function Get_Row (From  : List_Definition) return EL.Objects.Object;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : List_Definition;
                       Name : String) return EL.Objects.Object;
private

   type List_Definition is limited new EL.Beans.List_Bean with record
      Row        : Natural;
      Nodes      : DOM.Core.Node_List;
      Value      : aliased T;
      Value_Bean : EL.Objects.Object;
   end record;

end Gen.Model.List;
