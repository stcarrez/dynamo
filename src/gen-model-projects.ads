-----------------------------------------------------------------------
--  gen-model-projects -- Projects meta data
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
with Ada.Containers.Vectors;

with Util.Beans.Objects;

with Util.Properties;
package Gen.Model.Projects is

   use Ada.Strings.Unbounded;

   type Project_Definition;
   type Project_Definition_Access is access all Project_Definition'Class;

   package Project_Vectors is
     new Ada.Containers.Vectors (Element_Type => Project_Definition_Access,
                                 Index_Type   => Natural);

   --  ------------------------------
   --  Project Definition
   --  ------------------------------
   type Project_Definition is new Definition with record
      Name    : Unbounded_String;
      Path    : Unbounded_String;
      Props   : Util.Properties.Manager;
      Modules : Project_Vectors.Vector;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Project_Definition;
                       Name : String) return Util.Beans.Objects.Object;

   --  Save the project description and parameters.
   procedure Save (Project : in out Project_Definition;
                   Path    : in String);

end Gen.Model.Projects;
