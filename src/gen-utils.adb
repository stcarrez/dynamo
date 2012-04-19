-----------------------------------------------------------------------
--  gen-utils -- Utilities for model generator
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
                            Node    : in DOM.Core.Node;
                            Name    : in String;
                            Recurse : in Boolean := True) is
      Nodes : DOM.Core.Node_List;
      Size  : Natural;
   begin
      if Recurse then
         Nodes := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
      else
         Nodes := DOM.Core.Nodes.Child_Nodes (Node);
      end if;
      Size := DOM.Core.Nodes.Length (Nodes);
      for I in 0 .. Size - 1 loop
         declare
            N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
         begin
            Process (Closure, N);
         end;
      end loop;
      if Recurse then
         DOM.Core.Free (Nodes);
      end if;
   end Iterate_Nodes;

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

   --  ------------------------------
   --  Get the content of the node
   --  ------------------------------
   function Get_Data_Content (Node : in DOM.Core.Node) return String is
      Nodes  : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
      S      : constant Natural   := DOM.Core.Nodes.Length (Nodes);
      Result : Unbounded_String;
   begin
      for J in 0 .. S - 1 loop
         Append (Result, DOM.Core.Character_Datas.Data (DOM.Core.Nodes.Item (Nodes, J)));
      end loop;
      return To_String (Result);
   end Get_Data_Content;

   --  ------------------------------
   --  Get the content of the node identified by <b>Name</b> under the given DOM node.
   --  ------------------------------
   function Get_Data_Content (Node : in DOM.Core.Node;
                              Name : in String) return String is
      use type DOM.Core.Node;

      N    : constant DOM.Core.Node := Get_Child (Node, Name);
   begin
      if N = null then
         return "";
      else
         return Gen.Utils.Get_Data_Content (N);
      end if;
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
   --  Get the Ada type name from a full qualified type
   --  ------------------------------
   function Get_Type_Name (Name : in String) return String is
      Pos : constant Natural := Util.Strings.Rindex (Name, '.');
   begin
      if Pos > Name'First then
         return Name (Pos + 1 .. Name'Last);
      else
         return Name;
      end if;
   end Get_Type_Name;

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
