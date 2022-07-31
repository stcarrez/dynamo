-----------------------------------------------------------------------
--  gen-model-operations -- Operation declarations
--  Copyright (C) 2012, 2016, 2017, 2018, 2021, 2022 Stephane Carrez
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

package body Gen.Model.Operations is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Operation_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name in "parameters" | "columns" then
         return From.Parameters_Bean;

      elsif Name = "return" then
         return UBO.To_Object (From.Return_Type);

      elsif Name = "type" then
         return UBO.To_Object (Operation_Type'Image (From.Kind));

      else
         return Definition (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Operation_Definition) is
   begin
      null;
   end Prepare;

   --  ------------------------------
   --  Initialize the operation definition instance.
   --  ------------------------------
   overriding
   procedure Initialize (O : in out Operation_Definition) is
   begin
      O.Parameters_Bean := UBO.To_Object (O.Parameters'Unchecked_Access,
                                          UBO.STATIC);

   end Initialize;

   --  ------------------------------
   --  Add an operation parameter with the given name and type.
   --  ------------------------------
   procedure Add_Parameter (Into      : in out Operation_Definition;
                            Name      : in UString;
                            Of_Type   : in UString;
                            Parameter : out Parameter_Definition_Access) is
   begin
      Parameter := new Parameter_Definition;
      Parameter.Set_Name (Name);
      Parameter.Type_Name := Of_Type;
      Into.Parameters.Append (Parameter);
      if Into.Kind = UNKNOWN and then Of_Type = "ASF.Parts.Part" then
         Into.Kind := ASF_UPLOAD;
      elsif Into.Kind = UNKNOWN and then Of_Type = "AWA.Events.Module_Event" then
         Into.Kind := AWA_EVENT;
      elsif Into.Kind = UNKNOWN then
         Into.Kind := ASF_ACTION;
      end if;
   end Add_Parameter;

   --  ------------------------------
   --  Get the operation type.
   --  ------------------------------
   function Get_Type (From : in Operation_Definition) return Operation_Type is
   begin
      return From.Kind;
   end Get_Type;

   --  ------------------------------
   --  Create an operation with the given name.
   --  ------------------------------
   function Create_Operation (Name : in UString) return Operation_Definition_Access is
      pragma Unreferenced (Name);

      Result : constant Operation_Definition_Access := new Operation_Definition;
   begin
      return Result;
   end Create_Operation;

end Gen.Model.Operations;
