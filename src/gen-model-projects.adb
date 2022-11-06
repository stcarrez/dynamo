-----------------------------------------------------------------------
--  gen-model-projects -- Projects meta data
--  Copyright (C) 2011 - 2022 Stephane Carrez
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Command_Line;

with Util.Files;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Streams.Texts;
with Util.Strings.Vectors;

package body Gen.Model.Projects is

   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Model.Projects");

   --  Find the Dynamo.xml path associated with the given GNAT project file or installed
   --  in the Dynamo installation path.
   function Get_Dynamo_Path (Name         : in String;
                             Project_Path : in String;
                             Install_Dir  : in String) return String;

   function Get (Project : in Project_Definition;
                 Name    : in String;
                 Default : in Boolean) return Boolean;

   function Get (Project : in Project_Definition;
                 Name    : in String;
                 Default : in Boolean) return Boolean is
   begin
      if not Project.Props.Exists (Name) then
         return Default;
      else
         declare
            Value : constant String := Project.Props.Get (Name);
         begin
            return Value = "TRUE" or else Value = "true" or else Value = "1";
         end;
      end if;
   end Get;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Project_Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "name" then
         return UBO.To_Object (From.Name);
      elsif Name = "modelVersion" then
         return UBO.To_Object (From.Model_Version);
      elsif From.Props.Exists (Name) then
         return UBO.To_Object (String '(From.Props.Get (Name)));
      else
         return UBO.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the project name.
   --  ------------------------------
   function Get_Project_Name (Project : in Project_Definition) return String is
   begin
      return To_String (Project.Name);
   end Get_Project_Name;

   To_GNAT_Mapping : Ada.Strings.Maps.Character_Mapping;

   --  ------------------------------
   --  Get the GNAT project file name.  The default is to use the Dynamo project
   --  name and add the <b>.gpr</b> extension.  The <b>gnat_project</b> configuration
   --  property allows to override this default.
   --  ------------------------------
   function Get_GNAT_Project_Name (Project : in Project_Definition) return String is
      Name : constant String := Project.Props.Get ("gnat_project", "");
   begin
      if Name'Length = 0 then
         return Ada.Strings.Fixed.Translate (To_String (Project.Name), To_GNAT_Mapping) & ".gpr";
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
   --  Get the directory path which holds database model files.
   --  This is controlled by the <b>db_dir</b> configuration property.
   --  The default is <tt>db</tt>.
   --  ------------------------------
   function Get_Database_Dir (Project : in Project_Definition) return String is
      Dir : constant String := Ada.Directories.Containing_Directory (To_String (Project.Path));
   begin
      if not Project.Props.Exists ("db_dir") then
         return Util.Files.Compose (Dir, "db");
      end if;
      return Util.Files.Compose (Dir, Project.Props.Get ("db_dir", "db"));
   end Get_Database_Dir;

   --  ------------------------------
   --  Get the directory path which is the base dir for the 'web, 'config' and 'bundles'.
   --  This is controlled by the <b>base_dir</b> configuration property.
   --  The default is <tt>.</tt>.
   --  ------------------------------
   function Get_Base_Dir (Project : in Project_Definition) return String is
      Dir : constant String := Ada.Directories.Containing_Directory (To_String (Project.Path));
   begin
      if not Project.Props.Exists ("base_dir") then
         return Dir;
      end if;
      return Util.Files.Compose (Dir, Project.Props.Get ("base_dir", ""));
   end Get_Base_Dir;

   --  ------------------------------
   --  Find the Dynamo.xml path associated with the given GNAT project file or installed
   --  in the Dynamo installation path.
   --  ------------------------------
   function Get_Dynamo_Path (Name         : in String;
                             Project_Path : in String;
                             Install_Dir  : in String) return String is
   begin
      --  Check in the directory which contains the project file.
      if Project_Path'Length /= 0 then
         declare
            Dir    : constant String := Ada.Directories.Containing_Directory (Project_Path);
            Dynamo : constant String := Util.Files.Compose (Dir, "dynamo.xml");
         begin
            Log.Debug ("Checking dynamo file {0}", Dynamo);
            if Ada.Directories.Exists (Dynamo) then
               return Dynamo;
            end if;
         end;
      end if;

      --  Look in the Dynamo installation directory.
      if Name'Length > 0 then
         declare
            Dynamo : constant String := Util.Files.Compose (Name, "dynamo.xml");
            Path   : constant String := Util.Files.Find_File_Path (Dynamo, Install_Dir);
         begin
            Log.Debug ("Checking dynamo file {0}", Path);
            if Ada.Directories.Exists (Path) then
               return Path;
            end if;
         end;

         --  If the name contains a '_' and it was not found, look for the same name
         --  with '_' replaced by '-'.
         if Util.Strings.Index (Name, '_') > 0 then
            declare
               Replace : constant Ada.Strings.Maps.Character_Mapping
                 := Ada.Strings.Maps.To_Mapping (From => "_", To   => "-");
               Dir_Name : constant String := Ada.Strings.Fixed.Translate (Name, Replace);
               Dynamo   : constant String := Util.Files.Compose (Dir_Name, "dynamo.xml");
               Path     : constant String := Util.Files.Find_File_Path (Dynamo, Install_Dir);
            begin
               Log.Debug ("Checking dynamo file {0}", Path);
               if Ada.Directories.Exists (Path) then
                  return Path;
               end if;
            end;
         end if;

      else
         declare
            Name   : constant String := Ada.Directories.Base_Name (Project_Path);
            Dynamo : constant String := Util.Files.Compose (Name, "dynamo.xml");
            Path   : constant String := Util.Files.Find_File_Path (Dynamo, Install_Dir);
         begin
            Log.Debug ("Checking dynamo file {0}", Path);
            if Ada.Directories.Exists (Path) then
               return Path;
            end if;
         end;
      end if;
      return "";
   end Get_Dynamo_Path;

   --  ------------------------------
   --  Find the dependency for the <b>Name</b> plugin.
   --  Returns a null dependency if the project does not depend on that plugin.
   --  ------------------------------
   function Find_Dependency (From : in Project_Definition;
                             Name : in String) return Project_Reference is
      Iter   : Project_Vectors.Cursor := From.Dependencies.First;
      Result : Project_Reference;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         Result := Project_Vectors.Element (Iter);
         if Result.Name = Name then
            return Result;
         end if;
         Project_Vectors.Next (Iter);
      end loop;
      return Project_Reference '(null, To_UString (Name), NONE);
   end Find_Dependency;

   --  ------------------------------
   --  Add a dependency to the plugin identified by <b>Name</b>.
   --  ------------------------------
   procedure Add_Dependency (Into : in out Project_Definition;
                             Name : in String;
                             Kind : in Dependency_Type) is
      Depend  : Project_Reference := Into.Find_Dependency (Name);
   begin
      Log.Debug ("Adding dependency {0}", Name);

      if Depend.Project = null and then Depend.Kind = NONE then
         Depend.Project := Into.Find_Project_By_Name (Name);
         Depend.Kind := Kind;
         Into.Dependencies.Append (Depend);
      end if;
   end Add_Dependency;

   --  ------------------------------
   --  Add a dependency to the plugin identified by <b>Project</b>.
   --  ------------------------------
   procedure Add_Dependency (Into    : in out Project_Definition;
                             Project : in Project_Definition_Access;
                             Kind    : in Dependency_Type) is
      procedure Update (Ref : in out Project_Reference);

      procedure Update (Ref : in out Project_Reference) is
      begin
         Ref.Project := Project;
      end Update;

      Iter   : Project_Vectors.Cursor := Into.Dependencies.First;
      Result : Project_Reference;
   begin
      Log.Debug ("Adding dependency {0}", Project.Name);

      while Project_Vectors.Has_Element (Iter) loop
         Result := Project_Vectors.Element (Iter);
         if Result.Name = Project.Name then
            Into.Dependencies.Update_Element (Iter, Update'Access);
            return;
         end if;
         Project_Vectors.Next (Iter);
      end loop;
      Result.Project := Project;
      Result.Kind    := Kind;
      Result.Name    := Project.Name;
      Into.Dependencies.Append (Result);
   end Add_Dependency;

   --  ------------------------------
   --  Add the project in the global project list on the root project instance.
   --  ------------------------------
   procedure Add_Project (Into    : in out Project_Definition;
                          Project : in Project_Definition_Access) is
   begin
      if Into.Root /= null then
         Root_Project_Definition'Class (Into.Root.all).Add_Project (Project);
      else
         Log.Error ("Project not added");
      end if;
   end Add_Project;

   --  ------------------------------
   --  Create a project definition instance to record a project with the dynamo XML file path.
   --  ------------------------------
   procedure Create_Project (Into    : in out Project_Definition;
                             Name    : in String;
                             Path    : in String;
                             Project : out Project_Definition_Access) is
   begin
      Project      := new Project_Definition;
      Project.Path := To_UString (Path);
      Project.Set_Name (Name);
      Log.Info ("Creating project {0} - {1}", Name, Path);
      Project_Definition'Class (Into).Add_Project (Project);
   end Create_Project;

   --  ------------------------------
   --  Add the project <b>Name</b> as a module.
   --  ------------------------------
   procedure Add_Module (Into : in out Project_Definition;
                         Name : in String) is
      Project : Project_Reference := Find_Project (Into.Modules, Name);
   begin
      if Project.Name /= Null_Unbounded_String then
         Log.Debug ("Module {0} already present", Name);
         return;
      end if;

      Log.Debug ("Adding module {0}", Name);
      Project.Name    := To_UString (Name);
      Project.Project := Into.Find_Project (Name);
      if Project.Project /= null then
         Project.Name := Project.Project.Name;
      end if;
      Into.Modules.Append (Project);
   end Add_Module;

   --  ------------------------------
   --  Add the project represented by <b>Project</b> if it is not already part of the modules.
   --  ------------------------------
   procedure Add_Module (Into    : in out Project_Definition;
                         Project : in Project_Definition_Access) is

      procedure Update (Item : in out Project_Reference);

      procedure Update (Item : in out Project_Reference) is
      begin
         Item.Name    := Project.Name;
         Item.Project := Project;
      end Update;

      Iter : Project_Vectors.Cursor := Into.Modules.First;
      P    : Project_Reference;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         P := Project_Vectors.Element (Iter);
         if P.Project = Project then
            return;
         end if;
         if P.Name = Project.Name then
            Project_Vectors.Update_Element (Into.Modules, Iter, Update'Access);
            return;
         end if;
         Project_Vectors.Next (Iter);
      end loop;

      if Project.Name = Into.Name then
         Log.Debug ("Ignoring recursive reference to {0}", Into.Name);
         return;
      end if;

      Log.Debug ("Adding module {0} in {1}-{2}", Project.Name, Into.Name & "-" & Into.Path);
      P.Project := Project;
      P.Name    := Project.Name;
      Into.Modules.Append (P);
   end Add_Module;

   --  ------------------------------
   --  Iterate over the project referenced in the list and execute the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate (List    : in out Project_Vectors.Vector;
                      Process : access procedure (Item : in out Project_Reference)) is
      Iter : Project_Vectors.Cursor := List.First;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         Project_Vectors.Update_Element (List, Iter, Process);
         Project_Vectors.Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Find a project from the list
   --  ------------------------------
   function Find_Project (List : in Project_Vectors.Vector;
                          Name : in String) return Project_Reference is
      Iter    : Project_Vectors.Cursor := List.First;
   begin
      while Project_Vectors.Has_Element (Iter) loop
         declare
            P : constant Project_Reference := Project_Vectors.Element (Iter);
         begin
            if P.Name = Name then
               return P;
            end if;
            if P.Project /= null and then P.Project.Path = Name then
               return P;
            end if;
         end;
         Project_Vectors.Next (Iter);
      end loop;
      Log.Debug ("Project {0} not read yet", Name);
      return Project_Reference '(null, Null_Unbounded_String, NONE);
   end Find_Project;

   --  ------------------------------
   --  Find the project definition associated with the dynamo XML file <b>Path</b>.
   --  Returns null if there is no such project
   --  ------------------------------
   function Find_Project (From : in Project_Definition;
                          Path : in String) return Project_Definition_Access is
   begin
      return Find_Project (From.Modules, Path).Project;
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
   --  Add the project in the global project list on the root project instance.
   --  ------------------------------
   overriding
   procedure Add_Project (Into    : in out Root_Project_Definition;
                          Project : in Project_Definition_Access) is
      Ref : Project_Reference;
   begin
      Project.Root := Into'Unchecked_Access;
      Ref.Project  := Project;
      Ref.Name     := Project.Name;
      Into.Projects.Append (Ref);
   end Add_Project;

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
            P : constant Project_Reference := Project_Vectors.Element (Iter);
         begin
            if P.Name = Name then
               return P.Project;
            end if;
         end;
         Project_Vectors.Next (Iter);
      end loop;
      Log.Debug ("Project {0} not found", Name);
      return null;
   end Find_Project_By_Name;

   --  ------------------------------
   --  Update the project references after a project is found and initialized.
   --  ------------------------------
   procedure Update_References (Root    : in out Root_Project_Definition;
                                Project : in Project_Definition_Access) is

      procedure Update (Item : in out Project_Reference);

      procedure Update (Item : in out Project_Reference) is
      begin
         if Item.Name = Project.Name or else Item.Name = Project.Path then
            if Item.Project = null then
               Item.Project := Project;

            elsif Item.Project /= Project then
               Log.Error ("Project {0} found in {1} and {2}",
                          To_String (Item.Name), To_String (Item.Project.Path),
                          To_String (Project.Path));
            end if;

         elsif Item.Project /= null and then Item.Project.Path = Project.Path then
            Item.Name := Project.Name;

         end if;
         if Item.Project /= null and then not Item.Project.Recursing then
            Item.Project.Recursing := True;
            Iterate (Item.Project.Modules, Update'Access);
            Iterate (Item.Project.Dependencies, Update'Access);
            Item.Project.Recursing := False;
         end if;
      end Update;

   begin
      Root.Recursing := True;
      Iterate (Root.Projects, Update'Access);
      Root.Recursing := False;
   end Update_References;

   --  ------------------------------
   --  Find the project definition associated with the dynamo XML file <b>Path</b>.
   --  Returns null if there is no such project
   --  ------------------------------
   overriding
   function Find_Project (From : in Root_Project_Definition;
                          Path : in String) return Project_Definition_Access is
   begin
      return Find_Project (From.Projects, Path).Project;
   end Find_Project;

   --  ------------------------------
   --  Save the project description and parameters.
   --  ------------------------------
   procedure Save (Project : in out Project_Definition;
                   Path    : in String) is
      use Util.Streams;

      procedure Save_Dependency (Pos : in Project_Vectors.Cursor);
      procedure Save_Module (Pos : in Project_Vectors.Cursor);
      procedure Read_Property_Line (Line : in String);
      function Is_Default_Value (Name : in String) return Boolean;

      Buffer      : aliased Util.Streams.Texts.Print_Stream;
      Output      : Util.Serialize.IO.XML.Output_Stream;
      Prop_Output : Util.Streams.Texts.Print_Stream;

      procedure Save_Dependency (Pos : in Project_Vectors.Cursor) is
         Depend : constant Project_Reference := Project_Vectors.Element (Pos);
      begin
         if Depend.Kind = DIRECT then
            Output.Write_String (ASCII.LF & "    ");
            Output.Start_Entity (Name => "depend");
            Output.Write_Attribute (Name  => "name",
                                    Value => UBO.To_Object (Depend.Name));
            Output.End_Entity (Name => "depend");
         end if;
      end Save_Dependency;

      procedure Save_Module (Pos : in Project_Vectors.Cursor) is
         Module : constant Project_Reference := Project_Vectors.Element (Pos);
         Name   : constant String := To_String (Module.Name);
      begin
         if Name'Length > 0 then
            Output.Write_String (ASCII.LF & "    ");
            Output.Start_Entity (Name => "module");
            Output.Write_Attribute (Name  => "name",
                                    Value => UBO.To_Object (Name));
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

      function Is_Default_Value (Name : in String) return Boolean is
      begin
         if Name /= "use_mysql"
           and then Name /= "use_sqlite"
           and then Name /= "use_postgresql"
         then
            return False;
         end if;

         return Get (Project, Name, True) = True;
      end Is_Default_Value;

      Dir       : constant String := Ada.Directories.Containing_Directory (Path);
      Name      : constant String := Project.Get_Project_Name;
      Prop_Name : constant String := Name & ".properties";
      Prop_Path : constant String := Ada.Directories.Compose (Dir, Prop_Name);
      Dis_Prop  : constant Boolean := Project.Get ("no_properties", False);
   begin
      Prop_Output.Initialize (Size => 100000);
      Buffer.Initialize (Size => 100000);
      Output.Initialize (Output => Buffer'Unchecked_Access);

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
      Output.Write_Entity (Name => "name", Value => UBO.To_Object (Name));
      declare
         Names : Util.Strings.Vectors.Vector;
      begin
         Project.Props.Get_Names (Names);
         for Name of Names loop
            if not Is_Default_Value (Name) then
               Output.Write_String (ASCII.LF & "    ");
               Output.Start_Entity (Name => "property");
               Output.Write_Attribute (Name  => "name",
                                       Value => UBO.To_Object (Name));
               Output.Write_String (Value => To_String (Project.Props.Get (Name)));
               Output.End_Entity (Name => "property");
            end if;
            Prop_Output.Write ("dynamo_");
            Prop_Output.Write (Name);
            Prop_Output.Write ("=");
            Prop_Output.Write (To_String (Project.Props.Get (Name)));

            Prop_Output.Write (ASCII.LF);
         end loop;
      end;

      Prop_Output.Write ("dynamo_path=");
      Prop_Output.Write (Ada.Command_Line.Command_Name);
      Prop_Output.Write (ASCII.LF);

      Project.Modules.Iterate (Save_Module'Access);
      Project.Dependencies.Iterate (Save_Dependency'Access);
      Output.Write_String (ASCII.LF & "");
      Output.End_Entity (Name => "project");
      Util.Files.Write_File (Content => Texts.To_String (Buffer),
                             Path    => Path);
      if not Dis_Prop then
         Util.Files.Write_File (Content => Texts.To_String (Prop_Output),
                                Path    => Prop_Path);
      end if;
   end Save;

   --  ------------------------------
   --  Update the project definition from the properties.
   --  ------------------------------
   procedure Update_From_Properties (Project : in out Project_Definition) is
   begin
      Project.Is_Plugin := Get (Project, "is_plugin", Project.Is_Plugin);
      Project.Use_Mysql := Get (Project, "use_mysql", Project.Use_Mysql);
      Project.Use_Postgresql := Get (Project, "use_postgresql", Project.Use_Postgresql);
      Project.Use_Sqlite := Get (Project, "use_sqlite", Project.Use_Sqlite);
      Project.Model_Version := 1;
      if Project.Props.Exists ("model_version") then
         declare
            Value : constant String := Project.Props.Get ("model_version");
         begin
            Project.Model_Version := Positive'Value (Value);

         exception
            when Constraint_Error =>
               null;
         end;
      else
         Project.Props.Set ("model_version", "1");
      end if;
   end Update_From_Properties;

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
         Name : UString;
      end record;
      type Project_Loader_Access is access all Project_Loader;

      procedure Set_Member (Closure : in out Project_Loader;
                            Field   : in Project_Fields;
                            Value   : in UBO.Object);

      --  ------------------------------
      --  Called by the de-serialization when a given field is recognized.
      --  ------------------------------
      procedure Set_Member (Closure : in out Project_Loader;
                            Field   : in Project_Fields;
                            Value   : in UBO.Object) is
      begin
         case Field is
         when FIELD_PROJECT_NAME =>
            Project.Set_Name (UBO.To_Unbounded_String (Value));

         when FIELD_MODULE_NAME =>
            declare
               Name : constant String := UBO.To_String (Value);
            begin
               Project.Add_Module (Name);
            end;

         when FIELD_DEPEND_NAME =>
            if not UBO.Is_Empty (Value) then
               Project.Add_Dependency (UBO.To_String (Value), DIRECT);
            end if;

         when FIELD_PROPERTY_NAME =>
            Closure.Name := UBO.To_Unbounded_String (Value);

         when FIELD_PROPERTY_VALUE =>
            Project.Props.Set (Closure.Name, UBO.To_Unbounded_String (Value));

         end case;
      end Set_Member;

      package Project_Mapper is
        new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Project_Loader,
                                                  Element_Type_Access => Project_Loader_Access,
                                                  Fields              => Project_Fields,
                                                  Set_Member          => Set_Member);

      Path       : constant String := To_String (Project.Path);
      Loader     : aliased Project_Loader;
      Mapper     : aliased Project_Mapper.Mapper;
      Reader     : Util.Serialize.IO.XML.Parser;
      Prj_Mapper : Util.Serialize.Mappers.Processing;
   begin
      Log.Info ("Reading project file '{0}'", Path);

      if Path /= "" and then Ada.Directories.Exists (Path) then
         --  Create the mapping to load the XML project file.
         Mapper.Add_Mapping ("name", FIELD_PROJECT_NAME);
         Mapper.Add_Mapping ("property/@name", FIELD_PROPERTY_NAME);
         Mapper.Add_Mapping ("property", FIELD_PROPERTY_VALUE);
         Mapper.Add_Mapping ("module/@name", FIELD_MODULE_NAME);
         Mapper.Add_Mapping ("depend/@name", FIELD_DEPEND_NAME);
         Prj_Mapper.Add_Mapping ("project", Mapper'Unchecked_Access);

         --  Set the context for Set_Member.
         Project_Mapper.Set_Context (Prj_Mapper, Loader'Access);

         Project.Set_Name (Null_Unbounded_String);

         --  Read the XML query file.
         Reader.Parse (Path, Prj_Mapper);
         Project.Update_From_Properties;
      end if;

      if Length (Project.Name) = 0 then
         Log.Error ("Project file {0} does not contain the project name.", Path);

      elsif Project.Root /= null then
         Root_Project_Definition'Class (Project.Root.all).
           Update_References (Project'Unchecked_Access);

      end if;

      Log.Info ("Project {0} is {1}", To_String (Project.Path), To_String (Project.Name));
   end Read_Project;

   --  ------------------------------
   --  Scan and read the possible modules used by the application.  Modules are stored in the
   --  <b>plugins</b> directory.  Each module is stored in its own directory and has its own
   --  <b>dynamo.xml</b> file.
   --  ------------------------------
   procedure Read_Modules (Project : in out Project_Definition) is
      use Ada.Directories;

      Dir_Filter : constant Filter_Type := (Directory => True, others => False);
      Module_Dir : constant String := Gen.Utils.Absolute_Path (Project.Get_Module_Dir);
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
              Exists (File)
            then
               declare
                  P : Project_Definition_Access;
               begin
                  P := Project.Find_Project (File);
                  if P = null then
                     Project.Create_Project (Path => File, Name => "", Project => P);

                     Log.Info ("Adding dynamo file '{0}'", File);
                     Project.Dynamo_Files.Append (File);
                     P.Read_Project;
                     Project.Add_Dependency (P, DIRECT);
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

      procedure Append_Dynamo_File (Name   : in String;
                                    Dynamo : in String;
                                    Result : in out Gen.Utils.String_List.Vector);
      procedure Collect_Dynamo_Files (List   : in Gen.Utils.GNAT.Project_Info_Vectors.Vector;
                                      Result : out Gen.Utils.String_List.Vector);

      procedure Append_Dynamo_File (Name   : in String;
                                    Dynamo : in String;
                                    Result : in out Gen.Utils.String_List.Vector) is
         Has_File : constant Boolean := Result.Contains (Dynamo);
         P        : Model.Projects.Project_Definition_Access := Project.Find_Project (Dynamo);
      begin
         Log.Debug ("Dynamo file {0} is used", Dynamo);
         if Has_File then
            Log.Info ("Delete dynamo file '{0}' at {1}", Dynamo,
                      Natural'Image (Natural (Result.Find_Index (Dynamo))));
            Result.Delete (Result.Find_Index (Dynamo));
         end if;
         Log.Info ("Adding dynamo file '{0}'", Dynamo);
         Result.Append (Dynamo);

         --  Find the project associated with the dynamo.xml file.
         --  Create it and load the XML if necessary.
         if P = null then
            Log.Debug ("Create dependency for {0} on {1}", Name, Dynamo);
            Project.Create_Project (Path => Dynamo, Name => Name, Project => P);
            P.Read_Project;
            Log.Debug ("Loaded project {0}", P.Name);
            Project.Add_Dependency (P, DIRECT);
         end if;
      end Append_Dynamo_File;

      --  ------------------------------
      --  Collect the <b>dynamo.xml</b> files used by the projects.
      --  Keep the list in the dependency order so that it can be used
      --  to build the database schema and take into account schema dependencies.
      --  ------------------------------
      procedure Collect_Dynamo_Files (List   : in Gen.Utils.GNAT.Project_Info_Vectors.Vector;
                                      Result : out Gen.Utils.String_List.Vector) is
         use Gen.Utils.GNAT;

         Iter : Gen.Utils.GNAT.Project_Info_Vectors.Cursor := List.First;
         Dir  : constant String := To_String (Project.Install_Dir);
      begin
         while Gen.Utils.GNAT.Project_Info_Vectors.Has_Element (Iter) loop
            declare
               Info     : constant Project_Info := Project_Info_Vectors.Element (Iter);
               Name     : constant String := To_String (Info.Name);
               Dynamo   : constant String := Get_Dynamo_Path (Name,
                                                              To_String (Info.Path),
                                                              Dir);
               Has_File : constant Boolean := Result.Contains (Dynamo);
            begin
               Log.Info ("GNAT project {0}", Name);
               Project_Info_Vectors.Next (Iter);

               --  Do not include the 'dynamo.xml' path if it is already in the list
               --  (this happens if a project uses several GNAT project files).
               --  We have to make sure that the 'dynamo.xml' stored in the current directory
               --  appears last in the list.  Insert only if there is a file.
               if not Info.Is_Abstract
                 and then (not Has_File or else not Project_Info_Vectors.Has_Element (Iter))
                 and then Dynamo'Length > 0
               then
                  Append_Dynamo_File (Name, Dynamo, Result);
               end if;
            end;
         end loop;
      end Collect_Dynamo_Files;

   begin
      Project.Path := To_UString (Gen.Utils.Absolute_Path (File));
      Project.Root := null;
      Project.Add_Project (Project'Unchecked_Access);
      Project.Read_Project;

      --  When necessary, read the GNAT project files.  We get a list of absolute GNAT path
      --  files that we can use to known the project dependencies with other modules.
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
         end;

         --  Mark the fact we did a recursive scan.
         Project.Recursive_Scan := True;

         --  Look for the projects that define the 'dynamo.xml' configuration.
         Collect_Dynamo_Files (Project.Project_Files, Project.Dynamo_Files);

         Project.Read_Modules;

         --  Last step, look for each project that was not yet found and try to load it
         --  from the Dynamo installation.
         declare
            Install_Dir : constant String := To_String (Project.Install_Dir);
            Pending     : Project_Vectors.Vector;

            procedure Update (Item : in out Project_Reference);
            procedure Read_Dependencies (Def : in out Project_Definition'Class);

            --  ------------------------------
            --  Read the dynamo project dependencies and find the associated dynamo.xml file.
            --  This is necessary for the plugins that don't have a GNAT project file associated.
            --  Such plugin don't contain any Ada code but they could provide either database files
            --  and web presentation pages (HTML, CSS, Javascript).
            --  ------------------------------
            procedure Read_Dependencies (Def : in out Project_Definition'Class) is
               Iter   : Project_Vectors.Cursor := Def.Dependencies.First;
               Result : Project_Reference;
               Found  : Project_Reference;
            begin
               Log.Debug ("Read dependencies of '{0}' with install dir '{1}'",
                          Def.Name, Install_Dir);
               while Project_Vectors.Has_Element (Iter) loop
                  Result := Project_Vectors.Element (Iter);
                  if Result.Project = null then
                     Found  := Find_Project (Pending, To_String (Result.Name));
                     if Found.Kind = NONE then
                        Result.Project := Def.Find_Project_By_Name (To_String (Result.Name));
                        if Result.Project = null then
                           Result.Project      := new Project_Definition;
                           Result.Project.Path := To_UString ("");
                           Result.Project.Set_Name (Result.Name);
                           Pending.Append (Result);
                        end if;
                        declare
                           Path     : constant String := To_String (Result.Project.Path);
                           Dynamo   : constant String := Get_Dynamo_Path (Result.Project.Get_Name,
                                                                          Path,
                                                                          Install_Dir);
                           Has_File : constant Boolean := Project.Dynamo_Files.Contains (Dynamo);
                        begin
                           if Dynamo /= "" then
                              Log.Info ("Project '{0}' depends on '{1}' found dynamo file '{2}'",
                                        Def.Get_Name, Result.Project.Get_Name, Dynamo);
                              if Path = "" then
                                 Result.Project.Path := To_UString (Dynamo);
                              end if;
                              if not Has_File then
                                 Log.Info ("Adding dynamo file '{0}'", Dynamo);
                                 Project.Dynamo_Files.Append (Dynamo);
                              end if;
                           end if;
                        end;
                     end if;
                  end if;
                  Project_Vectors.Next (Iter);
               end loop;
            end Read_Dependencies;

            procedure Update (Item : in out Project_Reference) is
            begin
               Log.Debug ("Checking project {0}", Item.Name);
               if Item.Project = null then
                  Item.Project := Project.Find_Project_By_Name (To_String (Item.Name));
               end if;
               if Item.Project = null then
                  declare
                     Name   : constant String := To_String (Item.Name);
                     Path   : constant String := Util.Files.Compose (Install_Dir, Name);
                     Dynamo : constant String := Util.Files.Compose (Path, "dynamo.xml");
                  begin
                     Log.Debug ("Checking dynamo file {0}", Dynamo);
                     if Ada.Directories.Exists (Dynamo) then
                        if not Project.Dynamo_Files.Contains (Dynamo) then
                           Log.Info ("Adding dynamo file '{0}'", Dynamo);
                           Project.Dynamo_Files.Append (Dynamo);
                        end if;
                        Item.Project      := new Project_Definition;
                        Item.Project.Path := To_UString (Dynamo);
                        Item.Project.Set_Name (Item.Name);
                        Pending.Append (Item);
                        Log.Info ("Preparing to load {0} from {1}", Name, Dynamo);
                     else
                        Log.Info ("Project {0} not found in dynamo search path", Name);
                     end if;
                  end;
               end if;
               if Item.Project /= null and then not Item.Project.Recursive_Scan then
                  Item.Project.Recursive_Scan := True;
                  Iterate (Item.Project.Modules, Update'Access);
               end if;
               if Item.Project /= null and then not Item.Project.Depend_Scan then
                  Item.Project.Depend_Scan := True;
                  Read_Dependencies (Item.Project.all);
               end if;
            end Update;

            Info   : constant Gen.Utils.GNAT.Project_Info := Project.Project_Files.Last_Element;
            Name   : constant String := To_String (Info.Name);
            Dynamo : constant String := Get_Dynamo_Path (Name,
                                                         To_String (Info.Path),
                                                         Install_Dir);
         begin
            Iterate (Project.Modules, Update'Access);
            for Pass in 1 .. 4 loop
               Log.Info ("Checking {0} projects",
                         Ada.Containers.Count_Type'Image (Project.Projects.Length));
               Iterate (Project.Projects, Update'Access);

               exit when Pending.Is_Empty;
               while not Pending.Is_Empty loop
                  Project.Projects.Append (Pending.First_Element);
                  Pending.First_Element.Project.Read_Project;
                  Pending.Delete_First;
               end loop;
            end loop;

            --  Make sure the project's dynamo.xml file appears last in the list so that
            --  the generated search path is correct (search path is in reverse order and we
            --  want to search locally first, then in dependent modules).
            Append_Dynamo_File (Name, Dynamo, Project.Dynamo_Files);
         end;
      end if;
   end Read_Project;

begin
   To_GNAT_Mapping := Ada.Strings.Maps.To_Mapping (From => "-", To => "_");
end Gen.Model.Projects;
