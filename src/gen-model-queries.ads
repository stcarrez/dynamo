-----------------------------------------------------------------------
--  gen-model-queries -- XML Mapped Database queries representation
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2021 Stephane Carrez
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

with Util.Encoders.SHA1;

with Gen.Model.List;
with Gen.Model.Tables;
package Gen.Model.Queries is

   --  ------------------------------
   --  Sort definition
   --  ------------------------------
   type Sort_Definition is new Definition with record
      Sql    : UString;
   end record;
   type Sort_Definition_Access is access all Sort_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Sort_Definition;
                       Name : in String) return UBO.Object;

   package Sort_List is new Gen.Model.List (T         => Sort_Definition,
                                            T_Access  => Sort_Definition_Access);

   --  ------------------------------
   --  Query definition
   --  ------------------------------
   type Query_Definition is new Gen.Model.Tables.Table_Definition with record
      Sorts_Bean     : UBO.Object;
      Sorts          : aliased Sort_List.List_Definition;
   end record;
   type Query_Definition_Access is access all Query_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Query_Definition;
                       Name : in String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Query_Definition);

   --  Add a new sort mode for the query definition.
   procedure Add_Sort (Into : in out Query_Definition;
                       Name : in UString;
                       Sql  : in UString);

   --  Initialize the table definition instance.
   overriding
   procedure Initialize (O : in out Query_Definition);

   --  ------------------------------
   --  Table Definition
   --  ------------------------------
   type Query_File_Definition is new Query_Definition with record
      File_Name      : UString;
      Sha1           : Util.Encoders.SHA1.Digest;
      Queries        : aliased Gen.Model.Tables.Table_List.List_Definition;
      Queries_Bean   : UBO.Object;
   end record;
   type Query_File_Definition_Access is access all Query_File_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Query_File_Definition;
                       Name : in String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Query_File_Definition);

   --  Add a new query to the definition.
   procedure Add_Query (Into   : in out Query_File_Definition;
                        Name   : in UString;
                        Query  : out Query_Definition_Access);

private

   overriding
   procedure Initialize (O : in out Query_File_Definition);

end Gen.Model.Queries;
