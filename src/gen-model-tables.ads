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
with EL.Objects;

with Gen.Model.List;
package Gen.Model.Tables is

   --  ------------------------------
   --  Column Definition
   --  ------------------------------
   type Column_Definition is new Definition with private;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Column_Definition;
                       Name : String) return EL.Objects.Object;

   --  Set the DOM node associated with the definition object
   overriding
   procedure Set_Node (O     : in out Column_Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural);

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Table_Definition is new Definition with private;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Table_Definition;
                       Name : String) return EL.Objects.Object;

   --  Set the DOM node associated with the definition object
   overriding
   procedure Set_Node (O     : in out Table_Definition;
                       Node  : in DOM.Core.Node;
                       Index : in Natural);

   --  Get the primary key for this table
   function Get_Primary_Key (From : Table_Definition) return DOM.Core.Node;

   --  Get the primary key type for this table
   function Get_Primary_Key_Type (From : Table_Definition) return String;

   --  Get the DOM node <b>version</b> if there is one.
   function Get_Version_Column (From : Table_Definition) return DOM.Core.Node;

   --  Get the <b>version</b> column name used by the lazy lock implementation
   function Get_Version_Column_Name (From : Table_Definition) return String;

   --  Get the <b>version</b> column type used by the lazy lock implementation
   function Get_Version_Column_Type (From : Table_Definition) return String;

   --  ------------------------------
   --  Model Definition
   --  ------------------------------
   type Model_Definition is new Definition with private;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Model_Definition;
                       Name : String) return EL.Objects.Object;

   procedure Initialize (O : in out Model_Definition;
                         N : in DOM.Core.Node);

private

   type Column_Definition is new Definition with record
      Number : Natural := 0;
   end record;

   package Column_List is new Gen.Model.List (Column_Definition);

   procedure Initialize (O : in out Table_Definition);

   type Table_Definition is new Definition with record
      Members      : aliased Column_List.List_Definition;
      Members_Bean : EL.Objects.Object;
   end record;

   package Table_List is new Gen.Model.List (Table_Definition);

   procedure Initialize (O : in out Model_Definition);

   type Model_Definition is new Definition with record
      Tables      : aliased Table_List.List_Definition;
      Tables_Bean : EL.Objects.Object;
   end record;

end Gen.Model.Tables;
