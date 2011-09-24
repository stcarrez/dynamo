-----------------------------------------------------------------------
--  gen-model-enums -- Enum definitions
--  Copyright (C) 2011 Stephane Carrez
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

with Gen.Model.List;
with Gen.Model.Packages;
package body Gen.Model.Enums is

   --  ------------------------------
   --  Enum value definition
   --  ------------------------------

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Value_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      return Definition (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Table Definition

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Enum_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "values" then
         return From.Values_Bean;
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Type_Name);
      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Enum_Definition) is
   begin
      null;
   end Prepare;

   --  ------------------------------
   --  Initialize the table definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Enum_Definition) is
   begin
      O.Values_Bean := Util.Beans.Objects.To_Object (O.Values'Unchecked_Access,
                                                     Util.Beans.Objects.STATIC);
   end Initialize;

end Gen.Model.Enums;
