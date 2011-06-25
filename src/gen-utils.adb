-----------------------------------------------------------------------
--  gen-utils -- Utilities for model generator
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with Util.Strings;

package body Gen.Utils is

   --  ------------------------------
   --  Generic procedure to iterate over the DOM nodes children of <b>node</b>
   --  and having the entity name <b>name</b>.
   --  ------------------------------
   procedure Iterate_Nodes (Closure : in out T;
                            Node    : DOM.Core.Node;
                            Name    : String) is
      Nodes : DOM.Core.Node_List := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
      Size  : constant Natural   := DOM.Core.Nodes.Length (Nodes);
   begin
      for I in 0 .. Size - 1 loop
         declare
            N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
         begin
            Process (Closure, N);
         end;
      end loop;
      DOM.Core.Free (Nodes);
   end Iterate_Nodes;

   --  ------------------------------
   --  Get the content of the node
   --  ------------------------------
   function Get_Data_Content (Node : in DOM.Core.Node) return String is
      Nodes  : DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
      S      : constant Natural   := DOM.Core.Nodes.Length (Nodes);
      Result : Unbounded_String;
   begin
      for J in 0 .. S - 1 loop
         Append (Result, DOM.Core.Character_Datas.Data (DOM.Core.Nodes.Item (Nodes, J)));
      end loop;
      return To_String (Result);
   end Get_Data_Content;

   --  ------------------------------
   --  Get the Ada package name from a qualified type
   --  ------------------------------
   function Get_Package_Name (Name : in String) return String is
      Pos : constant Natural := Util.Strings.Rindex (Name, '.');
   begin
      if Pos > Name'First then
         return Name (Name'First .. Pos - 1);
      else
         return "";
      end if;
   end Get_Package_Name;

   --  ------------------------------
   --  Get a query name from the XML query file name
   --  ------------------------------
   function Get_Query_Name (Path : in String) return String is
      Pos : Natural := Util.Strings.Rindex (Path, '/');
   begin
      if Pos = 0 then
         Pos := Util.Strings.Rindex (Path, '.');
         if Pos > Path'First then
            return Path (Path'First .. Pos - 1);
         else
            return Path;
         end if;
      else
         return Get_Query_Name (Path (Pos + 1 .. Path'Last));
      end if;
   end Get_Query_Name;

   --  ------------------------------
   --  Returns True if the Name is a valid project or module name.
   --  The name must be a valid Ada identifier.
   --  ------------------------------
   function Is_Valid_Name (Name : in String) return Boolean is
      C : Character;
   begin
      if Name'Length = 0 then
         return False;
      end if;
      C := Name (Name'First);
      if not (C >= 'a' and C <= 'z') and not (C >= 'A' and C <= 'Z') then
         return False;
      end if;
      for I in Name'First + 1 .. Name'Last loop
         C := Name (I);
         if not (C >= 'a' and C <= 'z') and not (C >= 'A' and C <= 'Z')
           and not (C >= '0' and C <= '9') and C /= '_' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Name;

end Gen.Utils;
