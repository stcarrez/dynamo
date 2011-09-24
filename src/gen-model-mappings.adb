-----------------------------------------------------------------------
--  gen-model-mappings -- Type mappings for Code Generator
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
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with Util.Log.Loggers;

--  The <b>Gen.Model.Mappings</b> package controls the mappings to convert an XML
--  type into the Ada type.
package body Gen.Model.Mappings is

   use Ada.Strings.Unbounded;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Model.Mappings");

   package Mapping_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
                                     Element_Type    => Mapping_Definition_Access,
                                     Hash            => Ada.Strings.Unbounded.Hash,
                                     Equivalent_Keys => Ada.Strings.Unbounded."=");

   Types : Mapping_Maps.Map;

   --  ------------------------------
   --  Mapping Definition

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Mapping_Definition;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "name" then
         return Util.Beans.Objects.To_Object (From.Target);
      elsif Name = "isBoolean" then
         return Util.Beans.Objects.To_Object (From.Is_Boolean);
      elsif Name = "isInteger" then
         return Util.Beans.Objects.To_Object (From.Is_Primitive);
      elsif Name = "isString" then
         return Util.Beans.Objects.To_Object (From.Is_String);
      elsif Name = "isIdentifier" then
         return Util.Beans.Objects.To_Object (From.Is_Identifier);
      elsif Name = "isDate" then
         return Util.Beans.Objects.To_Object (From.Is_Date);
      elsif Name = "isEnum" then
         return Util.Beans.Objects.To_Object (False);
      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Find the mapping for the given type name.
   --  ------------------------------
   function Find_Type (Name : in Ada.Strings.Unbounded.Unbounded_String)
                       return Mapping_Definition_Access is
      Pos : constant Mapping_Maps.Cursor := Types.Find (Name);
   begin
      if Mapping_Maps.Has_Element (Pos) then
         return Mapping_Maps.Element (Pos);
      else
         Log.Info ("Type {0} not found in mapping table", Name);
         return null;
      end if;
   end Find_Type;

   procedure Register_Type (Name    : in String;
                            Mapping : in Mapping_Definition_Access) is
      N    : Unbounded_String := To_Unbounded_String (Name);
      Pos  : constant Mapping_Maps.Cursor := Types.Find (N);
   begin
      Log.Debug ("Register type '{0}'", Name);

      if not Mapping_Maps.Has_Element (Pos) then
         Types.Insert (N, Mapping);
      end if;
   end Register_Type;

   --  ------------------------------
   --  Register a type mapping <b>From</b> that is mapped to <b>Target</b>.
   --  ------------------------------
   procedure Register_Type (Target : in String;
                            From   : in String;
                            Is_Primitive : in Boolean;
                            Is_Boolean    : in Boolean;
                            Is_Date       : in Boolean;
                            Is_Identifier : in Boolean;
                            Is_String     : in Boolean) is
      Name    : constant Unbounded_String := To_Unbounded_String (From);
      Pos     : constant Mapping_Maps.Cursor := Types.Find (Name);
      Mapping : Mapping_Definition_Access;
   begin
      Log.Debug ("Register type '{0}' mapped to '{1}'", From, Target);

      if Mapping_Maps.Has_Element (Pos) then
         Mapping := Mapping_Maps.Element (Pos);
      else
         Mapping := new Mapping_Definition;
         Types.Insert (Name, Mapping);
      end if;
      Mapping.Target        := To_Unbounded_String (Target);
      Mapping.Is_Primitive  := Is_Primitive;
      Mapping.Is_Date       := Is_Date;
      Mapping.Is_Identifier := Is_Identifier;
      Mapping.Is_String     := Is_String;
      Mapping.Is_Boolean    := Is_Boolean;
   end Register_Type;

end Gen.Model.Mappings;
