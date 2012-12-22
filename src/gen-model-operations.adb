-----------------------------------------------------------------------
--  gen-model-operations -- Operation declarations
--  Copyright (C) 2012 Stephane Carrez
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
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "parameters" or Name = "columns" then
         return From.Parameters_Bean;

      elsif Name = "return" then
         return Util.Beans.Objects.To_Object (From.Return_Type);

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
      O.Parameters_Bean := Util.Beans.Objects.To_Object (O.Parameters'Unchecked_Access,
                                                         Util.Beans.Objects.STATIC);

   end Initialize;

   --  ------------------------------
   --  Add an operation parameter with the given name and type.
   --  ------------------------------
   procedure Add_Parameter (Into      : in out Operation_Definition;
                            Name      : in Unbounded_String;
                            Of_Type   : in Unbounded_String;
                            Parameter : out Parameter_Definition_Access) is
   begin
      Parameter := new Parameter_Definition;
      Parameter.Name := Name;
      Parameter.Type_Name := Of_Type;
      Into.Parameters.Append (Parameter);
   end Add_Parameter;

   --  ------------------------------
   --  Create an operation with the given name.
   --  ------------------------------
   function Create_Operation (Name : in Unbounded_String) return Operation_Definition_Access is
      pragma Unreferenced (Name);

      Result : constant Operation_Definition_Access := new Operation_Definition;
   begin
      return Result;
   end Create_Operation;

end Gen.Model.Operations;
