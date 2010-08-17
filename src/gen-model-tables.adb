-----------------------------------------------------------------------
--  gen-model-tables -- Database table model representation
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

with DOM.Core.Nodes;
with DOM.Core.Elements;
package body Gen.Model.Tables is

   use type DOM.Core.Node;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Column_Definition;
                       Name : String) return EL.Objects.Object is
      Type_Name : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (From.Node, "type");
   begin
      if Name = "type" then
         return EL.Objects.To_Object (Type_Name);

      elsif Name = "index" then
         return EL.Objects.To_Object (From.Number);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the DOM node associated with the definition object
   --  ------------------------------
   overriding
   procedure Set_Node (O     : in out Column_Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural) is
   begin
      O.Node := Node;
      O.Number := Index;
   end Set_Node;

   procedure Initialize (O : in out Table_Definition) is
   begin
      O.Members_Bean := EL.Objects.To_Object (O.Members'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Set the DOM node associated with the definition object
   --  ------------------------------
   overriding
   procedure Set_Node (O     : in out Table_Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural) is
      pragma Unreferenced (Index);

      Nodes : constant DOM.Core.Node_List
        := DOM.Core.Elements.Get_Elements_By_Tag_Name (Node, "column");
   begin
      O.Node := Node;
      O.Members.Set_List (Nodes);
   end Set_Node;

--     function Get_Member_Sql_Mode (Member : in DOM.Core.Node) return String is
--        Type_Name : constant DOM_String := Get_Attribute (Member, "type");
--     begin
--        if Type_Name = "String" then
--           return "10";
--        elsif Type_Name = "Integer" then
--           return "0";
--        elsif Type_Name = "Identifier" then
--           return "1";
--        elsif Type_Name = "Time" then
--           return "3";
--        else
--           return "11";
--        end if;
--     end Get_Member_Sql_Mode;

--     function Get_Member_Value (Member : in Node) return String is
--        Type_Name : constant DOM_String := Get_Attribute (Member, "type");
--     begin
--        if Type_Name = "String" then
--           return "Null_Unbounded_String";
--        elsif Type_Name = "Integer" then
--           return "0";
--        else
--           return "0";
--        end if;
--     end Get_Member_Value;

   --  ------------------------------
   --  Get the primary key for this table
   --  ------------------------------
   function Get_Primary_Key (From : Table_Definition) return DOM.Core.Node is
      Nodes : constant DOM.Core.Node_List := From.Members.Get_List;
      Count : constant Natural := DOM.Core.Nodes.Length (Nodes);
   begin
      for I in 0 .. Count - 1 loop
         declare
            N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Nodes, I);
            V : constant DOM.Core.DOM_String := DOM.Core.Elements.Get_Attribute (N, "primary-key");
         begin
            if V = "true" then
               return N;
            end if;
         end;
      end loop;
      return null;
   end Get_Primary_Key;

   --  ------------------------------
   --  Get the primary key type for this table
   --  ------------------------------
   function Get_Primary_Key_Type (From : Table_Definition) return String is
      Node : constant DOM.Core.Node := From.Get_Primary_Key;
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute (Node, "type");
      else
         return "Identifier";
      end if;
   end Get_Primary_Key_Type;

   --  ------------------------------
   --  Get the primary key type for this table
   --  ------------------------------
   function Get_Primary_Key_Name (From : Table_Definition) return String is
      Node : constant DOM.Core.Node := From.Get_Primary_Key;
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute (Node, "name");
      else
         return "id";
      end if;
   end Get_Primary_Key_Name;

   VERSION_NAME : constant String := "version";

   --  ------------------------------
   --  Get the DOM node <b>version</b> if there is one.
   --  ------------------------------
   function Get_Version_Column (From : Table_Definition) return DOM.Core.Node is
      --  Check if there is a version column to generate.
      Nodes : constant DOM.Core.Node_List
        := DOM.Core.Elements.Get_Elements_By_Tag_Name (From.Node, VERSION_NAME);
   begin
      if DOM.Core.Nodes.Length (Nodes) > 0 then
         return DOM.Core.Nodes.Item (Nodes, 0);
      else
         return null;
      end if;
   end Get_Version_Column;

   --  ------------------------------
   --  Get the <b>version</b> column name used by the lazy lock implementation
   --  ------------------------------
   function Get_Version_Column_Name (From : Table_Definition) return String is
      Node : constant DOM.Core.Node := From.Get_Version_Column;
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute (Node, "name");
      else
         return "";
      end if;
   end Get_Version_Column_Name;

   --  ------------------------------
   --  Get the <b>version</b> column type used by the lazy lock implementation
   --  ------------------------------
   function Get_Version_Column_Type (From : Table_Definition) return String is
      Node : constant DOM.Core.Node := From.Get_Version_Column;
   begin
      if Node /= null then
         return DOM.Core.Elements.Get_Attribute (Node, "type");
      else
         return "";
      end if;
   end Get_Version_Column_Type;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Table_Definition;
                       Name : String) return EL.Objects.Object is
   begin
      if Name = "members" then
         return From.Members_Bean;

      elsif Name = "primaryKeyType" then
         return EL.Objects.To_Object (From.Get_Primary_Key_Type);

      elsif Name = "primaryKeyName" then
         return EL.Objects.To_Object (From.Get_Primary_Key_Name);

      elsif Name = "version" then
         return EL.Objects.To_Object (From.Get_Version_Column_Name);

      elsif Name = "versionType" then
         return EL.Objects.To_Object (From.Get_Version_Column_Type);

      elsif Name ="versionIndex" then
         return EL.Objects.To_Object (DOM.Core.Nodes.Length (From.Members.Get_List) + 1);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   procedure Initialize (O : in out Model_Definition) is
   begin
      O.Tables_Bean := EL.Objects.To_Object (O.Tables'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Model_Definition;
                       Name : String) return EL.Objects.Object is
   begin
      if Name = "tables" then
         return From.Tables_Bean;
      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   procedure Initialize (O : in out Model_Definition;
                         N : in DOM.Core.Node) is
      Nodes : constant DOM.Core.Node_List
        := DOM.Core.Elements.Get_Elements_By_Tag_Name (N, "table");
   begin
      O.Set_Node (N);
      O.Tables.Set_List (Nodes);
   end Initialize;

end Gen.Model.Tables;
