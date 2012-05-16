-----------------------------------------------------------------------
--  gen-model-projects -- Projects meta data
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with Ada.IO_Exceptions;
with Ada.Directories;

with Util.Files;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Streams.Buffered;
with Util.Streams.Texts;
with Util.Strings.Transforms;

package body Gen.Model.Projects is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.Projects");

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
      elsif From.Props.Exists (Name) then
         return Util.Beans.Objects.To_Object (String '(From.Props.Get (Name)));
      else
         return Util.Beans.Objects.Null_Object;
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
      use Util.Streams;

      procedure Save_Module (Pos : in Project_Vectors.Cursor);
      procedure Read_Property_Line (Line : in String);

      Output      : Util.Serialize.IO.XML.Output_Stream;
      Prop_Output : Util.Streams.Texts.Print_Stream;

      procedure Save_Module (Pos : in Project_Vectors.Cursor) is
         Module : constant Project_Definition_Access := Project_Vectors.Element (Pos);
      begin
         Output.Start_Entity (Name => "module");
         Output.Write_Attribute (Name  => "name",
                                 Value => Util.Beans.Objects.To_Object (Module.Path));
         Output.End_Entity (Name => "module");
      end Save_Module;

      --  ------------------------------
      --  Read the application property file to remove all the dynamo.* properties
      --  ------------------------------
      procedure Read_Property_Line (Line : in String) is
      begin
         if Line'Length < 7 or else Line (Line'First .. Line'First + 6) /= "dynamo_" then
            Prop_Output.Write (Line);
            Prop_Output.Write (ASCII.LF);
         end if;
      end Read_Property_Line;

      Dir       : constant String := Ada.Directories.Containing_Directory (Path);
      Name      : constant Util.Beans.Objects.Object := Project.Get_Value ("name");
      Prop_Name : constant String := Util.Beans.Objects.To_String (Name) & ".properties";
      Prop_Path : constant String := Ada.Directories.Compose (Dir, Prop_Name);
   begin
      Prop_Output.Initialize (Size => 100000);
      Output.Initialize (Size => 10000);

      --  Read the current project property file, ignoring the dynamo.* properties.
      begin
         Util.Files.Read_File (Prop_Path, Read_Property_Line'Access);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;

      --  Start building the new dynamo.xml content.
      --  At the same time, we append in the project property file the list of dynamo properties.
      Output.Start_Entity (Name => "project");
      Output.Write_Entity (Name => "name", Value => Name);
      declare
         Names : constant Util.Properties.Name_Array := Project.Props.Get_Names;
      begin
         for I in Names'Range loop
            Output.Write (ASCII.LF);
            Output.Start_Entity (Name => "property");
            Output.Write_Attribute (Name  => "name",
                                    Value => Util.Beans.Objects.To_Object (Names (I)));
            Output.Write_String (Value => Util.Strings.Transforms.Escape_Xml
                                 (To_String (Project.Props.Get (Names (I)))));
            Output.End_Entity (Name => "property");
            Prop_Output.Write ("dynamo_");
            Prop_Output.Write (Names (I));
            Prop_Output.Write ("=");
            Prop_Output.Write (To_String (Project.Props.Get (Names (I))));

            Prop_Output.Write (ASCII.LF);
         end loop;
      end;

      Project.Modules.Iterate (Save_Module'Access);
      Output.End_Entity (Name => "project");
      Util.Files.Write_File (Content => Texts.To_String (Buffered_Stream (Output)),
                             Path    => Path);
      Util.Files.Write_File (Content => Texts.To_String (Buffered_Stream (Prop_Output)),
                             Path    => Prop_Path);
   end Save;

   --  ------------------------------
   --  Read the XML project description into the project description.
   --  ------------------------------
   procedure Read_Project (Project : in out Project_Definition) is

      type Project_Fields is (FIELD_PROJECT_NAME,
                              FIELD_PROPERTY_NAME,
                              FIELD_PROPERTY_VALUE,
                              FIELD_MODULE_NAME);

      type Project_Loader is record
         Name : Unbounded_String;
      end record;
      type Project_Loader_Access is access all Project_Loader;

      procedure Set_Member (Closure : in out Project_Loader;
                            Field   : in Project_Fields;
                            Value   : in Util.Beans.Objects.Object);

      --  ------------------------------
      --  Called by the de-serialization when a given field is recognized.
      --  ------------------------------
      procedure Set_Member (Closure : in out Project_Loader;
                            Field   : in Project_Fields;
                            Value   : in Util.Beans.Objects.Object) is
      begin
         case Field is
         when FIELD_PROJECT_NAME =>
            Project.Name := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_MODULE_NAME =>
            declare
               P : constant Model.Projects.Project_Definition_Access
                 := new Model.Projects.Project_Definition;
            begin
               P.Name := Util.Beans.Objects.To_Unbounded_String (Value);
               Project.Modules.Append (P);
            end;

         when FIELD_PROPERTY_NAME =>
            Closure.Name := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_PROPERTY_VALUE =>
            Project.Props.Set (Closure.Name, Util.Beans.Objects.To_Unbounded_String (Value));

         end case;
      end Set_Member;

      package Project_Mapper is
        new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Project_Loader,
                                                  Element_Type_Access => Project_Loader_Access,
                                                  Fields              => Project_Fields,
                                                  Set_Member          => Set_Member);

      Path   : constant String := To_String (Project.Path);
      Loader : aliased Project_Loader;
      Mapper : aliased Project_Mapper.Mapper;
      Reader : Util.Serialize.IO.XML.Parser;
   begin
      Log.Info ("Reading project file '{0}'", Path);

      --  Create the mapping to load the XML project file.
      Mapper.Add_Mapping ("name", FIELD_PROJECT_NAME);
      Mapper.Add_Mapping ("property/@name", FIELD_PROPERTY_NAME);
      Mapper.Add_Mapping ("property", FIELD_PROPERTY_VALUE);
      Mapper.Add_Mapping ("module/@name", FIELD_MODULE_NAME);
      Reader.Add_Mapping ("project", Mapper'Unchecked_Access);

      --  Set the context for Set_Member.
      Project_Mapper.Set_Context (Reader, Loader'Access);

      Project.Name := Null_Unbounded_String;

      --  Read the XML query file.
      Reader.Parse (Path);
--
--        if Length (Into.Name) = 0 then
--           H.Error ("Project file {0} does not contain the project name.", Path);
--        end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --           H.Error ("Project file {0} does not exist", Path);
         Log.Error ("Project file {0} does not exist", Path);
   end Read_Project;

end Gen.Model.Projects;
