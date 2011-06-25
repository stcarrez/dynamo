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

with Util.Files;
with Util.Serialize.IO.XML;
with Util.Streams.Buffered;
with Util.Streams.Texts;

package body Gen.Model.Projects is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : Project_Definition;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      else
         return Definition'Class (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the project name.
   --  ------------------------------
   function Get_Project_Name (Project : in Project_Definition) return String is
   begin
      return To_String (Project.Name);
   end Get_Project_Name;

   --  ------------------------------
   --  Find the project definition associated with the dynamo XML file <b>Path</b>.
   --  Returns null if there is no such project
   --  ------------------------------
   function Find_Project (From : in Project_Definition;
                          Path : in String) return Project_Definition_Access is
      Iter : Project_Vectors.Cursor := From.Modules.First;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         declare
            P : constant Project_Definition_Access := Project_Vectors.Element (Iter);
         begin
            if P.Path = Path then
               return P;
            end if;
         end;
         Project_Vectors.Next (Iter);
      end loop;
      return null;
   end Find_Project;

   --  ------------------------------
   --  Save the project description and parameters.
   --  ------------------------------
   procedure Save (Project : in out Project_Definition;
                   Path    : in String) is
      use Util.Streams.Buffered;

      procedure Save_Module (Pos : in Project_Vectors.Cursor);

      Output : Util.Serialize.IO.XML.Output_Stream;

      procedure Save_Module (Pos : in Project_Vectors.Cursor) is
         Module : constant Project_Definition_Access := Project_Vectors.Element (Pos);
      begin
         Output.Start_Entity (Name => "module");
         Output.Write_Attribute (Name  => "name",
                                 Value => Util.Beans.Objects.To_Object (Module.Path));
         Output.End_Entity (Name => "module");
      end Save_Module;

   begin
      Output.Initialize (Size => 10000);
      Output.Start_Entity (Name => "project");
      Output.Write_Entity (Name => "name", Value => Project.Get_Value ("name"));
      declare
         Names : constant Util.Properties.Name_Array := Project.Props.Get_Names;
      begin
         for I in Names'Range loop
            Output.Write (ASCII.LF);
            Output.Start_Entity (Name => "property");
            Output.Write_Attribute (Name  => "name",
                                    Value => Util.Beans.Objects.To_Object (Names (I)));
            Output.Write_String (Value => To_String (Project.Props.Get (Names (I))));
            Output.End_Entity (Name => "property");
         end loop;
      end;

      Project.Modules.Iterate (Save_Module'Access);
      Output.End_Entity (Name => "project");
      Util.Files.Write_File (Content => Util.Streams.Texts.To_String (Buffered_Stream (Output)),
                             Path    =>  Path);
   end Save;

end Gen.Model.Projects;
