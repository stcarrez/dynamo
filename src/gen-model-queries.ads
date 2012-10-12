-----------------------------------------------------------------------
--  gen-model-queries -- XML Mapped Database queries representation
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Util.Encoders.SHA1;

with Gen.Model.Tables;
package Gen.Model.Queries is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Query_Definition is new Gen.Model.Tables.Table_Definition with record
      File_Name      : Unbounded_String;
      Sha1           : Util.Encoders.SHA1.Digest;
      Queries        : aliased Gen.Model.Tables.Column_List.List_Definition;
      Queries_Bean   : Util.Beans.Objects.Object;
   end record;
   type Query_Definition_Access is access all Query_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Query_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Query_Definition);

   --  Add a new query to the definition.
   procedure Add_Query (Into   : in out Query_Definition;
                        Name   : in Unbounded_String;
                        Query  : out Gen.Model.Tables.Column_Definition_Access);

private

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Query_Definition);

end Gen.Model.Queries;
