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

with Gen.Utils.GNAT;

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
   --  Get the GNAT project file name.  The default is to use the Dynamo project
   --  name and add the <b>.gpr</b> extension.  The <b>gnat.project</b> configuration
   --  property allows to override this default.
   --  ------------------------------
   function Get_GNAT_Project_Name (Project : in Project_Definition) return String is
      Name : constant String := Project.Props.Get ("gnat.project", "");
   begin
      if Name'Length = 0 then
         return To_String (Project.Name) & ".gpr";
      else
         return Name;
      end if;
   end Get_GNAT_Project_Name;

   --  ------------------------------
   --  Get the directory path which holds application modules.
   --  This is controlled by the <b>modules_dir</b> configuration property.
   --  The default is <tt>plugins</tt>.
   --  ------------------------------
   function Get_Module_Dir (Project : in Project_Definition) return String is
   begin
      return Project.Props.Get ("modules_dir", "plugins");
   end Get_Module_Dir;

   --  ------------------------------
   --  Find the dependency for the <b>Name</b> plugin.
   --  Returns a null dependency if the project does not depend on that plugin.
   --  ------------------------------
   function Find_Dependency (From : in Project_Definition;
                             Name : in String) return Project_Dependency is
      Iter   : Dependency_Vectors.Cursor := From.Dependencies.First;
      Result : Project_Dependency;
   begin
      while not Dependency_Vectors.Has_Element (Iter) loop
         Result := Dependency_Vectors.Element (Iter);
         if Result.Project.Get_Name = Name then
            return Result;
         end if;
         Dependency_Vectors.Next (Iter);
      end loop;
      return Project_Dependency '(null, NONE);
   end Find_Dependency;

   --  ------------------------------
   --  Add a dependency to the plugin identified by <b>Name</b>.
   --  ------------------------------
   procedure Add_Dependency (Into : in out Project_Definition;
                             Name : in String;
                             Kind : in Dependency_Type) is
      Depend  : Project_Dependency := Into.Find_Dependency (Name);
   begin
      if Depend.Project = null then
         Depend.Project := Into.Find_Project_By_Name (Name);
         if Depend.Project = null then
            Depend.Project := new Project_Definition;
            Depend.Project.Name := To_Unbounded_String (Name);
            Root_Project_Definition'Class (Into.Root.all).Projects.Append (Depend.Project);
         end if;
         Depend.Kind := Kind;
         Into.Dependencies.Append (Depend);
      end if;
   end Add_Dependency;

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
   --  Find the project definition having the name <b>Name</b>.
   --  Returns null if there is no such project
   --  ------------------------------
   function Find_Project_By_Name (From : in Project_Definition;
                                  Name : in String) return Project_Definition_Access is
   begin
      if From.Root /= null then
         return Root_Project_Definition'Class (From.Root.all).Find_Project_By_Name (Name);
      else
         return null;
      end if;
   end Find_Project_By_Name;

   --  ------------------------------
   --  Find the project definition having the name <b>Name</b>.
   --  Returns null if there is no such project
   --  ------------------------------
   overriding
   function Find_Project_By_Name (From : in Root_Project_Definition;
                                  Name : in String) return Project_Definition_Access is
      Iter : Project_Vectors.Cursor := From.Projects.First;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         declare
            P : constant Project_Definition_Access := Project_Vectors.Element (Iter);
         begin
            if P.Name = Name then
               return P;
            end if;
         end;
         Project_Vectors.Next (Iter);
      end loop;
      return null;
   end Find_Project_By_Name;

   --  ------------------------------
   --  Save the project description and parameters.
   --  ------------------------------
   procedure Save (Project : in out Project_Definition;
                   Path    : in String) is
      use Util.Streams.Buffered;
      use Util.Streams;

      procedure Save_Dependency (Pos : in Dependency_Vectors.Cursor);
      procedure Save_Module (Pos : in Project_Vectors.Cursor);
      procedure Read_Property_Line (Line : in String);

      Output      : Util.Serialize.IO.XML.Output_Stream;
      Prop_Output : Util.Streams.Texts.Print_Stream;

      procedure Save_Dependency (Pos : in Dependency_Vectors.Cursor) is
         Depend : constant Project_Dependency := Dependency_Vectors.Element (Pos);
      begin
         if Depend.Kind = DIRECT then
            Output.Write_String (ASCII.LF & "    ");
            Output.Start_Entity (Name => "depend");
            Output.Write_Attribute (Name  => "name",
                                    Value => Util.Beans.Objects.To_Object (Depend.Project.Get_Name));
            Output.End_Entity (Name => "depend");
         end if;
      end Save_Dependency;

      procedure Save_Module (Pos : in Project_Vectors.Cursor) is
         Module : constant Project_Definition_Access := Project_Vectors.Element (Pos);
      begin
         if Length (Module.Path) > 0 then
            Output.Write_String (ASCII.LF & "    ");
            Output.Start_Entity (Name => "module");
            Output.Write_Attribute (Name  => "name",
                                    Value => Util.Beans.Objects.To_Object (Module.Path));
            Output.End_Entity (Name => "module");
         end if;
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
      Output.Write_String (ASCII.LF & "    ");
      Output.Write_Entity (Name => "name", Value => Name);
      declare
         Names : constant Util.Properties.Name_Array := Project.Props.Get_Names;
      begin
         for I in Names'Range loop
            Output.Write_String (ASCII.LF & "    ");
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
      Project.Dependencies.Iterate (Save_Dependency'Access);
      Output.Write_String (ASCII.LF & "");
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
                              FIELD_MODULE_NAME,
                              FIELD_DEPEND_NAME);

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

         when FIELD_DEPEND_NAME =>
            if not Util.Beans.Objects.Is_Empty (Value) then
               Project.Add_Dependency (Util.Beans.Objects.To_String (Value), DIRECT);
            end if;

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
      Mapper.Add_Mapping ("depend/@name", FIELD_DEPEND_NAME);
      Reader.Add_Mapping ("project", Mapper'Unchecked_Access);

      --  Set the context for Set_Member.
      Project_Mapper.Set_Context (Reader, Loader'Access);

      Project.Name := Null_Unbounded_String;

      --  Read the XML query file.
      Reader.Parse (Path);

      if Length (Project.Name) = 0 then
         Log.Error ("Project file {0} does not contain the project name.", Path);
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         --           H.Error ("Project file {0} does not exist", Path);
         Log.Error ("Project file {0} does not exist", Path);
   end Read_Project;

   --  ------------------------------
   --  Scan and read the possible modules used by the application.  Modules are stored in the
   --  <b>modules</b> directory.  Each module is stored in its own directory and has its own
   --  <b>dynamo.xml</b> file.
   --  ------------------------------
   procedure Read_Modules (Project : in out Project_Definition) is
      use Ada.Directories;

      Dir_Filter : constant Filter_Type := (Directory => True, others => False);
      Module_Dir : constant String := Project.Get_Module_Dir;
      Ent        : Directory_Entry_Type;
      Search     : Search_Type;
   begin
      if not Exists (Module_Dir) then
         Log.Debug ("Project {0} has no module", Project.Name);
         return;
      end if;
      if Kind (Module_Dir) /= Directory then
         Log.Debug ("Project {0} has no module", Project.Name);
         return;
      end if;

      Log.Info ("Scanning project modules in {0}", Module_Dir);
      Start_Search (Search, Directory => Module_Dir, Pattern => "*", Filter => Dir_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Dir_Name : constant String := Simple_Name (Ent);
            Dir      : constant String := Compose (Module_Dir, Dir_Name);
            File     : constant String := Compose (Dir, "dynamo.xml");
         begin
            if Dir_Name /= "." and then Dir_Name /= ".." and then Dir_Name /= ".svn" and then
              Exists (File) then
               declare
                  P        : Project_Definition_Access;
               begin
                  P := Project.Find_Project (File);
                  if P = null then
                     P := new Model.Projects.Project_Definition;
                     P.Path := To_Unbounded_String (File);
                     Project.Modules.Append (P);
                     Project.Dynamo_Files.Append (File);
                     P.Read_Project;
                  end if;
               end;
            end if;
         end;
      end loop;

   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         Log.Info ("Exception: {0}", Util.Log.Loggers.Traceback (E));
   end Read_Modules;

   --  ------------------------------
   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   --  ------------------------------
   procedure Read_Project (Project   : in out Root_Project_Definition;
                           File      : in String;
                           Config    : in Util.Properties.Manager'Class;
                           Recursive : in Boolean := False) is

      procedure Collect_Dynamo_Files (List   : in Gen.Utils.String_List.Vector;
                                      Result : out Gen.Utils.String_List.Vector);

      --  ------------------------------
      --  Collect the <b>dynamo.xml</b> files used by the projects.
      --  Keep the list in the dependency order so that it can be used
      --  to build the database schema and take into account schema dependencies.
      --  ------------------------------
      procedure Collect_Dynamo_Files (List   : in Gen.Utils.String_List.Vector;
                                      Result : out Gen.Utils.String_List.Vector) is
         use type Gen.Model.Projects.Project_Definition_Access;

         function Get_Dynamo_Path (Project_Path : in String) return String;

         Iter : Gen.Utils.String_List.Cursor := List.First;

         --  ------------------------------
         --  Find the Dynamo.xml path associated with the given GNAT project file.
         --  ------------------------------
         function Get_Dynamo_Path (Project_Path : in String) return String is
            Name   : constant String := Ada.Directories.Base_Name (Project_Path);
         begin
            --  Check in the directory which contains the project file.
            declare
               Dir    : constant String := Ada.Directories.Containing_Directory (Project_Path);
               Dynamo : constant String := Util.Files.Compose (Dir, "dynamo.xml");
            begin
               if Ada.Directories.Exists (Dynamo) then
                  return Dynamo;
               end if;
            end;

            declare
               Dir    : constant String := ""; --  H.Get_Install_Directory;
               Path   : constant String := Util.Files.Compose (Dir, Name);
               Dynamo : constant String := Util.Files.Compose (Path, "dynamo.xml");
            begin
               Log.Debug ("Checking dynamo file {0}", Dynamo);
               if Ada.Directories.Exists (Dynamo) then
                  return Dynamo;
               end if;
            end;
            return "";
         end Get_Dynamo_Path;

      begin
         while Gen.Utils.String_List.Has_Element (Iter) loop
            declare
               Path     : constant String := Gen.Utils.String_List.Element (Iter);
               Dynamo   : constant String := Get_Dynamo_Path (Path);
               Has_File : constant Boolean := Result.Contains (Dynamo);
               P        : Model.Projects.Project_Definition_Access;
            begin
               Gen.Utils.String_List.Next (Iter);

               --  Do not include the 'dynamo.xml' path if it is already in the list
               --  (this happens if a project uses several GNAT project files).
               --  We have to make sure that the 'dynamo.xml' stored in the current directory
               --  appears last in the list.
               if (not Has_File or else not Gen.Utils.String_List.Has_Element (Iter))
               --  Insert only if there is a file.
                 and Dynamo'Length > 0 then
                  if Has_File then
                     Result.Delete (Result.Find_Index (Dynamo));
                  end if;
                  Result.Append (Dynamo);

                  --  Find the project associated with the dynamo.xml file.
                  --  Create it and load the XML if necessary.
                  P := Project.Find_Project (Dynamo);
                  if P = null then
                     P := new Model.Projects.Project_Definition;
                     P.Path := To_Unbounded_String (Dynamo);
                     Project.Modules.Append (P);
                     P.Read_Project; --  SCz (Into => P);
                  end if;
               end if;
            end;
         end loop;
      end Collect_Dynamo_Files;

   begin
      Project.Path := To_Unbounded_String (File);
      Project.Read_Project;

      --  When necessary, read the GNAT project files.  We get a list of absolute GNAT path
      --  files that we can use known the project dependencies with other modules.
      --  This is useful for database schema generation for example.
      if Recursive then

         --  Read GNAT project files.
         declare
            Name : constant String := Project.Get_GNAT_Project_Name;
         begin
            if not Ada.Directories.Exists (Name) then
               Log.Warn ("GNAT project file {0} does not exist.", Name);
               return;
            end if;

            Gen.Utils.GNAT.Initialize (Config);
            Gen.Utils.GNAT.Read_GNAT_Project_List (Name, Project.Project_Files);
            if Project.Project_Files.Is_Empty then
--                 H.Error ("Error while reading GNAT project {0}", Name);
               return;
            end if;
         end;

         --  Mark the fact we did a recursive scan.
         Project.Recursive_Scan := True;

         --  Look for the projects that define the 'dynamo.xml' configuration.
         Collect_Dynamo_Files (Project.Project_Files, Project.Dynamo_Files);

         Project.Read_Modules;
      end if;
   end Read_Project;

end Gen.Model.Projects;
