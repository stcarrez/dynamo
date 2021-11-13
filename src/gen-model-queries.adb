-----------------------------------------------------------------------
--  gen-model-queries -- XML Mapped Database queries representation
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2018, 2021 Stephane Carrez
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

package body Gen.Model.Queries is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Sort_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "name" then
         return UBO.To_Object (From.Name);

      elsif Name = "sql" then
         return UBO.To_Object (From.Sql);

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Query_Definition;
                       Name : String) return UBO.Object is
   begin
      if Name = "sorts" then
         return From.Sorts_Bean;

      elsif Name = "isBean" then
         return UBO.To_Object (True);

      else
         return Tables.Table_Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Prepare the generation of the model.
   --  ------------------------------
   overriding
   procedure Prepare (O : in out Query_Definition) is
   begin
      Tables.Table_Definition (O).Prepare;
   end Prepare;

   --  ------------------------------
   --  Add a new sort mode for the query definition.
   --  ------------------------------
   procedure Add_Sort (Into : in out Query_Definition;
                       Name : in UString;
                       Sql  : in UString) is
      Item : constant Sort_Definition_Access := new Sort_Definition;
   begin
      Item.Set_Name (Name);
      Item.Sql  := Sql;
      Into.Sorts.Append (Item);
   end Add_Sort;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Query_Definition) is
   begin
      O.Sorts_Bean := UBO.To_Object (O.Sorts'Unchecked_Access, UBO.STATIC);
      Tables.Table_Definition (O).Initialize;
   end Initialize;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Query_File_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "queries" then
         return From.Queries_Bean;

      elsif Name = "path" then
         return UBO.To_Object (From.File_Name);

      elsif Name = "sha1" then
         return UBO.To_Object (From.Sha1);

      else
         return Query_Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Query_File_Definition) is
   begin
      Tables.Table_Definition (O).Prepare;
   end Prepare;

   --  ------------------------------
   --  Add a new query to the definition.
   --  ------------------------------
   procedure Add_Query (Into   : in out Query_File_Definition;
                        Name   : in UString;
                        Query  : out Query_Definition_Access) is
   begin
      Query := new Query_Definition;
      Query.Set_Name (Name);
      Into.Queries.Append (Query.all'Access);
      --  Query.Number := Into.Queries.Get_Count;
   end Add_Query;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Query_File_Definition) is
   begin
      O.Queries_Bean := UBO.To_Object (O.Queries'Unchecked_Access, UBO.STATIC);
      Tables.Table_Definition (O).Initialize;
   end Initialize;

end Gen.Model.Queries;
