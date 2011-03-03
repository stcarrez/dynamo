-----------------------------------------------------------------------
--  gen-utils -- Utilities for model generator
--  Copyright (C) 2010 Stephane Carrez
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

with DOM.Core;
package Gen.Utils is

   --  Generic procedure to iterate over the DOM nodes children of <b>node</b>
   --  and having the entity name <b>name</b>.
   generic
      type T is limited private;
      with procedure Process (Closure : in out T;
                              Node    : DOM.Core.Node);
   procedure Iterate_Nodes (Closure : in out T;
                            Node    : DOM.Core.Node;
                            Name    : String);

   --  Get the content of the node
   function Get_Data_Content (Node : in DOM.Core.Node) return String;

   --  Get the Ada package name from a qualified type
   function Get_Package_Name (Name : in String) return String;

   --  Get a query name from the XML query file name
   function Get_Query_Name (Path : in String) return String;

end Gen.Utils;
