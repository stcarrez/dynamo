-----------------------------------------------------------------------
--  gen-model-operations -- Operation declarations
--  Copyright (C) 2012, 2016, 2017, 2021 Stephane Carrez
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

with Gen.Model.List;
package Gen.Model.Operations is

   type Operation_Type is (UNKNOWN, ASF_ACTION, ASF_UPLOAD, AWA_EVENT);

   --  ------------------------------
   --  Parameter Definition
   --  ------------------------------
   type Parameter_Definition is new Definition with private;
   type Parameter_Definition_Access is access all Parameter_Definition'Class;

   --  ------------------------------
   --  Operation Definition
   --  ------------------------------
   type Operation_Definition is new Definition with private;
   type Operation_Definition_Access is access all Operation_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Operation_Definition;
                       Name : in String) return UBO.Object;

   --  Prepare the generation of the model.
   overriding
   procedure Prepare (O : in out Operation_Definition);

   --  Initialize the operation definition instance.
   overriding
   procedure Initialize (O : in out Operation_Definition);

   --  Add an operation parameter with the given name and type.
   procedure Add_Parameter (Into      : in out Operation_Definition;
                            Name      : in UString;
                            Of_Type   : in UString;
                            Parameter : out Parameter_Definition_Access);

   --  Get the operation type.
   function Get_Type (From : in Operation_Definition) return Operation_Type;

   --  Create an operation with the given name.
   function Create_Operation (Name : in UString) return Operation_Definition_Access;

private

   type Parameter_Definition is new Definition with record
      --  The parameter type name.
      Type_Name : UString;
   end record;

   package Parameter_List is new Gen.Model.List (T         => Parameter_Definition,
                                                 T_Access  => Parameter_Definition_Access);

   type Operation_Definition is new Definition with record
      Parameters      : aliased Parameter_List.List_Definition;
      Parameters_Bean : UBO.Object;
      Return_Type     : UString;
      Kind            : Operation_Type := UNKNOWN;
   end record;

end Gen.Model.Operations;
