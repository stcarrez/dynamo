-----------------------------------------------------------------------
--  gen-utils -- Utilities for model generator
--  Copyright (C) 2010, 2011, 2012, 2015, 2021, 2022 Stephane Carrez
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
with Ada.Directories;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Character_Datas;

with Util.Strings;
with Util.Files;

package body Gen.Utils is

   --  ------------------------------
   --  Generic procedure to iterate over the DOM nodes children of <b>node</b>
   --  and having the entity name <b>name</b>.
   --  ------------------------------
   procedure Iterate_Nodes (Closure : in out T;
                            Node    : in DOM.Core.Node;
                            Name    : in String;
                            Recurse : in Boolean := True) is
   begin
      if Recurse then
         declare
            Nodes : DOM.Core.Node_List := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, Name);
            Size  : constant Natural := DOM.Core.Nodes.Length (Nodes);
         begin
            for I in 0 .. Size - 1 loop
               declare
                  N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
               begin
                  Process (Closure, N);
               end;
            end loop;
            DOM.Core.Free (Nodes);

         end;
      else
         declare
            use type DOM.Core.Node_Types;

            Nodes : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
            Size  : constant Natural := DOM.Core.Nodes.Length (Nodes);
         begin
            for I in 0 .. Size - 1 loop
               declare
                  N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
                  T : constant DOM.Core.Node_Types := DOM.Core.Nodes.Node_Type (N);
               begin
                  if T = DOM.Core.Element_Node and then Name = DOM.Core.Nodes.Node_Name (N) then
                     Process (Closure, N);
                  end if;
               end;
            end loop;
         end;
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
      Result : UString;
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
   --  Get a boolean attribute
   --  ------------------------------
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "yes" or else V = "true" then
         return True;
      elsif V = "no" or else V = "false" then
         return False;
      else
         return Default;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get a string attribute
   --  ------------------------------
   function Get_Attribute (Node    : in DOM.Core.Node;
                           Name    : in String;
                           Default : in String := "") return String is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "" then
         return Default;
      else
         return V;
      end if;
   end Get_Attribute;

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
      if C not in 'a' .. 'z' | 'A' .. 'Z' then
         return False;
      end if;
      for I in Name'First + 1 .. Name'Last loop
         C := Name (I);
         if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Name;

   --  ------------------------------
   --  Returns True if the path referred to by <b>Path</b> is an absolute path.
   --  ------------------------------
   function Is_Absolute_Path (Path : in String) return Boolean is
   begin
      if Path'Length = 0 then
         return False;
      elsif Path (Path'First) = '/' or else Path (Path'First) = '\' then
         return True;
      elsif Path'Length = 1 then
         return False;
      elsif Path (Path'First + 1) = ':' then
         return True;
      else
         return False;
      end if;
   end Is_Absolute_Path;

   --  ------------------------------
   --  Returns the path if this is an absolute path, otherwise build and return an absolute path.
   --  ------------------------------
   function Absolute_Path (Path : in String) return String is
   begin
      if Is_Absolute_Path (Path) then
         return Path;
      else
         return Util.Files.Compose (Ada.Directories.Current_Directory, Path);
      end if;
   end Absolute_Path;

   --  ------------------------------
   --  Returns True if the file name must be ignored (.svn, CVS, .git, are ignored).
   --  ------------------------------
   function Is_File_Ignored (Name : in String) return Boolean is
   begin
      if Name =  ".svn" then
         return True;
      elsif Name = ".git" then
         return True;
      elsif Name = "CVS" then
         return True;
      elsif Name = "." or else Name = ".." then
         return True;
      else
         return False;
      end if;
   end Is_File_Ignored;

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

   --  --------------------
   --  Get the comment associated with a node
   --  --------------------
   function Get_Comment (Node : in DOM.Core.Node) return String is
      Children : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Node);
      Size     : constant Natural := DOM.Core.Nodes.Length (Children);
      Result   : UString;
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
   --  Get a string attribute
   --  ------------------------------
   function Get_Attribute (Node    : DOM.Core.Node;
                           Name    : String;
                           Default : String := "") return UString is
      V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (Node, Name);
   begin
      if V = "" then
         return To_UString (Default);
      else
         return To_UString (V);
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
   --  Returns a qualified name from a package name and a name.
   --  ------------------------------
   function Qualify_Name (Pkg_Name : in UString;
                          Name     : in UString) return UString is
   begin
      return Pkg_Name & "." & Name;
   end Qualify_Name;

end Gen.Utils;
