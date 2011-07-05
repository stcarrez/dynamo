-----------------------------------------------------------------------
--  Gen -- Code Generator
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Ada.Directories;
with Ada.IO_Exceptions;

with Input_Sources.File;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Components.Root;
with ASF.Components.Base;

with Util.Beans.Basic;
with EL.Functions;

with Gen.Utils;
with Gen.Utils.GNAT;
with Gen.Model;
with Gen.Model.Tables;
with Gen.Model.Mappings;
with Gen.Commands.Templates;

with GNAT.Traceback.Symbolic;
with Util.Strings;
with Util.Files;
with Util.Strings.Transforms;
with Util.Log.Loggers;

with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;

--  with Util.Beans.Objects.To_Access;
package body Gen.Generator is

   use ASF;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Generator");

   RESULT_DIR : constant String := "generator.output.dir";

   function To_Ada_Type (Value : in Util.Beans.Objects.Object;
                         Param : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object;
   function Indent (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL Function to translate a model type to the key enum value
   function To_Key_Enum (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL function to create an Ada identifier from a file name
   function To_Ada_Ident (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL function to indent the code
   function To_Sql_Type (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL Function to check whether a type is an integer type
   function Is_Integer_Type (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL Function to check whether a type is an date or a time type
   function Is_Date_Type (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

   --  ------------------------------
   --  EL Function to translate a model type to an Ada implementation type
   --  Param values:
   --    0 : Get the type for a record declaration
   --    1 : Get the type for a parameter declaration or a return type
   --    2 : Get the type for the generation of the ADO.Statements.Get procedure name
   --  ------------------------------
   function To_Ada_Type (Value : in Util.Beans.Objects.Object;
                         Param : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      use Gen.Model.Tables;
      use Gen.Model;
      use type Gen.Model.Mappings.Mapping_Definition_Access;

      function To_Ada_Type (Value : in String) return Util.Beans.Objects.Object;

--        Def    : constant Definition_Access := To_Definition_Access (Value);
      Column : Column_Definition_Access := null; --  To_Definition_Access (Value);

      function To_Ada_Type (Value : in String) return Util.Beans.Objects.Object is
      begin
         if Value = "String" or Value = "java.lang.String" then
            return Util.Beans.Objects.To_Object
              (String '("Ada.Strings.Unbounded.Unbounded_String"));
         elsif Value = "Integer" or Value = "int" or Value = "java.lang.Integer" then
            return Util.Beans.Objects.To_Object (String '("Integer"));
         elsif Value = "Timestamp" then
            if Util.Beans.Objects.To_Integer (Param) = 2 then
               return Util.Beans.Objects.To_Object (String '("Time"));
            else
               return Util.Beans.Objects.To_Object (String '("Ada.Calendar.Time"));
            end if;
         else
            return Util.Beans.Objects.To_Object (Value);
         end if;
      end To_Ada_Type;

      Ptr : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Value);
   begin
      if Ptr /= null and then Ptr.all in Column_Definition'Class then
         Column := Column_Definition'Class (Ptr.all)'Unchecked_Access;
      else
         Column := null;
      end if;
      if Column /= null then
         if Column.Type_Mapping /= null then
            if Column.Type_Mapping.Is_Date and Util.Beans.Objects.To_Integer (Param) = 2 then
               return Util.Beans.Objects.To_Object (String '("Time"));
            else
               return Util.Beans.Objects.To_Object (Column.Type_Mapping.Target);
            end if;
         elsif Column.Is_Basic_Type then
            return To_Ada_Type (Column.Get_Type);
         elsif Util.Beans.Objects.To_Integer (Param) = 1 then
            return Util.Beans.Objects.To_Object (Column.Get_Type & "_Ref'Class");
         else
            return Util.Beans.Objects.To_Object (Column.Get_Type & "_Ref");
         end if;
      else
         return To_Ada_Type (Util.Beans.Objects.To_String (Value));
      end if;
   end To_Ada_Type;

   --  ------------------------------
   --  EL Function to check whether a type is an integer type
   --  ------------------------------
   function Is_Integer_Type (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Value : constant String := Util.Beans.Objects.To_String (Name);
   begin
      if Value = "Integer" or Value = "int" then
         return Util.Beans.Objects.To_Object (True);
      else
         return Util.Beans.Objects.To_Object (False);
      end if;
   end Is_Integer_Type;

   --  ------------------------------
   --  EL Function to check whether a type is an date or a time type
   --  ------------------------------
   function Is_Date_Type (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Value : constant String := Util.Beans.Objects.To_String (Name);
   begin
      if Value = "Date" or Value = "Time" or Value = "Timestamp" then
         return Util.Beans.Objects.To_Object (True);
      else
         return Util.Beans.Objects.To_Object (False);
      end if;
   end Is_Date_Type;

   KEY_INTEGER_LABEL : constant String := "KEY_INTEGER";
   KEY_STRING_LABEL  : constant String := "KEY_STRING";

   --  ------------------------------
   --  EL Function to translate a model type to the key enum value
   --  ------------------------------
   function To_Key_Enum (Name : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Value : constant String := Util.Beans.Objects.To_String (Name);
   begin
      if Value = "Integer" or Value = "int" or Value = "Identifier"
         or Value = "ADO.Identifier" then
         return Util.Beans.Objects.To_Object (KEY_INTEGER_LABEL);
      else
         return Util.Beans.Objects.To_Object (KEY_STRING_LABEL);
      end if;
   end To_Key_Enum;

   --  ------------------------------
   --  EL function to indent the code
   --  ------------------------------
   function Indent (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      S      : constant String := Util.Beans.Objects.To_String (Value);
      Result : constant String (S'Range) := (others => ' ');
   begin
      return Util.Beans.Objects.To_Object (Result);
   end Indent;

   --  ------------------------------
   --  EL function to indent the code
   --  ------------------------------
   function To_Sql_Type (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Name   : constant String := Util.Beans.Objects.To_String (Value);
      Result : Unbounded_String;
   begin
      if Name = "Identifier" then
         Append (Result, "BIGINT NOT NULL");
      elsif Name = "Integer" then
         Append (Result, "INTEGER");
      elsif Name = "String" then
         Append (Result, "VARCHAR(255)");
      elsif Name = "Date" then
         Append (Result, "DATE");
      else
         Append (Result, "VARCHAR(255)");
      end if;
      return Util.Beans.Objects.To_Object (Result);
   end To_Sql_Type;

   --  ------------------------------
   --  EL function to create an Ada identifier from a file name
   --  ------------------------------
   function To_Ada_Ident (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Name   : constant String := Util.Beans.Objects.To_String (Value);
      Result : Unbounded_String;
      C      : Character;
   begin
      for I in Name'Range loop
         C := Name (I);
         if C = '-' then
            Append (Result, '_');

         elsif C >= 'a' and C <= 'z' then
            Append (Result, C);

         elsif C >= 'A' and C <= 'Z' then
            Append (Result, C);

         elsif C >= '0' and C <= '9' then
            Append (Result, C);
         end if;
      end loop;
      return Util.Beans.Objects.To_Object (Result);
   end To_Ada_Ident;

   --  ------------------------------
   --  Register the generator EL functions
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
      URI : constant String := "http://code.google.com/p/ada-ado/generator";
   begin
      Mapper.Set_Function (Name      => "adaIdent",
                           Namespace => URI,
                           Func      => To_Ada_Ident'Access);
      Mapper.Set_Function (Name      => "adaType",
                           Namespace => URI,
                           Func      => To_Ada_Type'Access);
      Mapper.Set_Function (Name      => "indent",
                           Namespace => URI,
                           Func      => Indent'Access);
      Mapper.Set_Function (Name      => "isInteger",
                           Namespace => URI,
                           Func      => Is_Integer_Type'Access);
      Mapper.Set_Function (Name      => "isDate",
                           Namespace => URI,
                           Func      => Is_Date_Type'Access);
      Mapper.Set_Function (Name      => "sqlType",
                           Namespace => URI,
                           Func      => To_Sql_Type'Access);
      Mapper.Set_Function (Name      => "keyEnum",
                           Namespace => URI,
                           Func      => To_Key_Enum'Access);
   end Set_Functions;

   --  ------------------------------
   --  Initialize the generator
   --  ------------------------------
   procedure Initialize (H : in out Handler;
                         Config_Dir : in Ada.Strings.Unbounded.Unbounded_String) is
      use Ada.Directories;

      procedure Register_Funcs is
        new ASF.Applications.Main.Register_Functions (Set_Functions);

      Dir     : constant String := Ada.Strings.Unbounded.To_String (Config_Dir);
      Factory : ASF.Applications.Main.Application_Factory;
      Path    : constant String := Compose (Dir, "generator.properties");
   begin
      begin
         H.Conf.Load_Properties (Path => Path);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            H.Error ("Cannot load configuration file {0}", Path);
      end;
      H.Conf.Set (ASF.Applications.VIEW_DIR, Compose (Dir,  "templates"));
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_WHITE_SPACES, "false");
      H.Conf.Set (ASF.Applications.VIEW_ESCAPE_UNKNOWN_TAGS, "false");
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_EMPTY_LINES, "true");
      H.Conf.Set (ASF.Applications.VIEW_FILE_EXT, "");
      H.Conf.Set ("ado.queries.paths", Compose (Dir, "db"));
      H.Initialize (H.Conf, Factory);

      H.Config_Dir := To_Unbounded_String (Dir);
      H.Output_Dir := To_Unbounded_String (H.Conf.Get (RESULT_DIR, "./"));

      Register_Funcs (H);
      H.File := new Util.Beans.Objects.Object;

      begin
         Gen.Commands.Templates.Read_Commands (H);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            H.Error ("Cannot read external commands");
      end;
   end Initialize;

   --  ------------------------------
   --  Get the configuration properties.
   --  ------------------------------
   function Get_Properties (H : in Handler) return Util.Properties.Manager is
   begin
      return Util.Properties.Manager (H.Conf);
   end Get_Properties;

   --  ------------------------------
   --  Set the directory where template files are stored.
   --  ------------------------------
   procedure Set_Template_Directory (H    : in out Handler;
                                     Path : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      H.Conf.Set (ASF.Applications.VIEW_DIR, Path);
   end Set_Template_Directory;

   --  ------------------------------
   --  Set the directory where results files are generated.
   --  ------------------------------
   procedure Set_Result_Directory (H    : in out Handler;
                                   Path : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      H.Conf.Set (RESULT_DIR, Path);
      H.Output_Dir := Path;
   end Set_Result_Directory;

   --  ------------------------------
   --  Get the result directory path.
   --  ------------------------------
   function Get_Result_Directory (H : in Handler) return String is
   begin
      return To_String (H.Output_Dir);
   end Get_Result_Directory;

   --  ------------------------------
   --  Get the config directory path.
   --  ------------------------------
   function Get_Config_Directory (H : in Handler) return String is
   begin
      return To_String (H.Config_Dir);
   end Get_Config_Directory;

   --  ------------------------------
   --  Get the dynamo installation directory path.
   --  ------------------------------
   function Get_Install_Directory (H : in Handler) return String is
   begin
      return Ada.Directories.Containing_Directory (To_String (H.Config_Dir));
   end Get_Install_Directory;

   --  ------------------------------
   --  Get the exit status
   --  Returns 0 if the generation was successful
   --  Returns 1 if there was a generation error
   --  ------------------------------
   function Get_Status (H : in Handler) return Ada.Command_Line.Exit_Status is
   begin
      return H.Status;
   end Get_Status;

   --  ------------------------------
   --  Get the configuration parameter.
   --  ------------------------------
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in String := "") return String is
   begin
      return H.Conf.Get (Name, Default);
   end Get_Parameter;

   --  ------------------------------
   --  Set the force-save file mode.  When False, if the generated file exists already,
   --  an error message is reported.
   --  ------------------------------
   procedure Set_Force_Save (H  : in out Handler;
                             To : in Boolean) is
   begin
      H.Force_Save := To;
   end Set_Force_Save;

   --  ------------------------------
   --  Set the project name.
   --  ------------------------------
   procedure Set_Project_Name (H    : in out Handler;
                               Name : in String) is
      Code : constant String := Util.Strings.Transforms.To_Upper_Case (Name);
   begin
      if not Gen.Utils.Is_Valid_Name (Name) then
         H.Error ("The project name should be a valid Ada identifier ([A-Za-z][A-Za-z0-9_]*).");
         raise Fatal_Error with "Invalid project name: " & Name;
      end if;
      H.Project.Name := To_Unbounded_String (Name);
      H.Set_Global ("projectName", Name);
      H.Set_Global ("projectCode", Code);
   end Set_Project_Name;

   --  ------------------------------
   --  Get the project name.
   --  ------------------------------
   function Get_Project_Name (H : in Handler) return String is
   begin
      return H.Project.Get_Project_Name;
   end Get_Project_Name;

   --  ------------------------------
   --  Get the GNAT project file name.  The default is to use the Dynamo project
   --  name and add the <b>.gpr</b> extension.  The <b>gnat.project</b> configuration
   --  property allows to override this default.
   --  ------------------------------
   function Get_GNAT_Project_Name (H : in Handler) return String is
      Name : constant String := H.Project.Props.Get ("gnat.project", "");
   begin
      if Name'Length = 0 then
         return To_String (H.Project.Name) & ".gpr";
      else
         return Name;
      end if;
   end Get_GNAT_Project_Name;

   --  ------------------------------
   --  Set the project property.
   --  ------------------------------
   procedure Set_Project_Property (H     : in out Handler;
                                   Name  : in String;
                                   Value : in String) is
   begin
      Log.Debug ("Set property {0} to {1}", Name, Value);

      H.Project.Props.Set (Name, Value);
   end Set_Project_Property;

   --  ------------------------------
   --  Get the project property identified by the given name.  If the project property
   --  does not exist, returns the default value.  Project properties are loaded
   --  by <b>Read_Project</b>.
   --  ------------------------------
   function Get_Project_Property (H       : in Handler;
                                  Name    : in String;
                                  Default : in String := "") return String is
   begin
      return H.Project.Props.Get (Name, Default);
   end Get_Project_Property;

   --  ------------------------------
   --  Save the project description and parameters.
   --  ------------------------------
   procedure Save_Project (H : in out Handler) is
      Path : constant String := Ada.Directories.Compose (H.Get_Result_Directory, "dynamo.xml");
   begin
      H.Project.Save (Path);
   end Save_Project;

   --  ------------------------------
   --  Get the path of the last generated file.
   --  ------------------------------
   function Get_Generated_File (H : in Handler) return String is
   begin
      return Util.Beans.Objects.To_String (H.File.all);
   end Get_Generated_File;

   --  ------------------------------
   --  Report an error and set the exit status accordingly
   --  ------------------------------
   procedure Error (H : in out Handler;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "") is
   begin
      Log.Error (Message, Arg1, Arg2);
      H.Status := 1;
   end Error;

   --  ------------------------------
   --  Read the XML project description into the project description.
   --  ------------------------------
   procedure Read_Project (H : in out Handler;
                           Into : in Gen.Model.Projects.Project_Definition_Access) is

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
            Into.Name := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_MODULE_NAME =>
            declare
               P : constant Model.Projects.Project_Definition_Access
                 := new Model.Projects.Project_Definition;
            begin
               P.Name := Util.Beans.Objects.To_Unbounded_String (Value);
               Into.Modules.Append (P);
            end;

         when FIELD_PROPERTY_NAME =>
            Closure.Name := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_PROPERTY_VALUE =>
            Into.Props.Set (Closure.Name, Util.Beans.Objects.To_Unbounded_String (Value));

         end case;
      end Set_Member;

      package Project_Mapper is
        new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Project_Loader,
                                                  Element_Type_Access => Project_Loader_Access,
                                                  Fields              => Project_Fields,
                                                  Set_Member          => Set_Member);

      Path   : constant String := To_String (Into.Path);
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

      Into.Name := Null_Unbounded_String;

      --  Read the XML query file.
      Reader.Parse (Path);

      if Length (Into.Name) = 0 then
         H.Error ("Project file {0} does not contain the project name.", Path);
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Project file {0} does not exist", Path);
   end Read_Project;

   --  ------------------------------
   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   --  ------------------------------
   procedure Read_Project (H         : in out Handler;
                           File      : in String;
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
            Pos    : Natural;
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

            Pos := Util.Strings.Index (Name, '.');
            if Pos > Name'First then
               declare
                  Dir  : constant String := H.Get_Install_Directory;
                  Path : constant String := Util.Files.Compose (Dir,
                                                                Name (Name'First .. Pos - 1));
                  Dynamo : constant String := Util.Files.Compose (Path, "dynamo.xml");
               begin
                  if Ada.Directories.Exists (Dynamo) then
                     return Dynamo;
                  end if;
               end;
            end if;
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
                  P := H.Project.Find_Project (Dynamo);
                  if P = null then
                     P := new Model.Projects.Project_Definition;
                     P.Path := To_Unbounded_String (Dynamo);
                     H.Project.Modules.Append (P);
                     H.Read_Project (Into => P);
                  end if;
               end if;
            end;
         end loop;
      end Collect_Dynamo_Files;

   begin
      H.Project.Path := To_Unbounded_String (File);
      H.Read_Project (H.Project'Unchecked_Access);
      H.Set_Global ("projectName", H.Get_Project_Name);

      --  When necessary, read the GNAT project files.  We get a list of absolute GNAT path
      --  files that we can use known the project dependencies with other modules.
      --  This is useful for database schema generation for example.
      if Recursive then

         --  Read GNAT project files.
         declare
            Name : constant String := H.Get_GNAT_Project_Name;
         begin
            Gen.Utils.GNAT.Initialize (H.Conf);
            Gen.Utils.GNAT.Read_GNAT_Project_List (Name, H.Project.Project_Files);
            if H.Project.Project_Files.Is_Empty then
               H.Error ("Error while reading GNAT project {0}", Name);
               return;
            end if;
         end;

         --  Look for the projects that define the 'dynamo.xml' configuration.
         Collect_Dynamo_Files (H.Project.Project_Files, H.Project.Dynamo_Files);

      end if;
   end Read_Project;

   --  ------------------------------
   --  Read the XML model file
   --  ------------------------------
   procedure Read_Model (H    : in out Handler;
                         File : in String) is
      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;

   begin
      --  Before loading a model file, we should know the type mappings.
      --  Load them first if needed.
      if not H.Type_Mapping_Loaded then
         declare
            Mapping : constant String := H.Get_Parameter ("generator.mapping",
                                                          "AdaMappings.xml");
            Dir     : constant String := H.Get_Config_Directory;
         begin
            --  Read the type mappings
            H.Type_Mapping_Loaded := True;
            H.Read_Model (File => Ada.Directories.Compose (Dir, Mapping));
         end;
      end if;

      Log.Info ("Reading model file '{0}'", File);

      --  Base file name should be used as the public Id
      Name_Start := File'Last;
      while Name_Start >= File'First  and then File (Name_Start) /= '/' loop
         Name_Start := Name_Start - 1;
      end loop;
      Input_Sources.File.Open (File, Read);

      --  Full name is used as the system id
      Input_Sources.File.Set_System_Id (Read, File);
      Input_Sources.File.Set_Public_Id (Read, File (Name_Start + 1 .. File'Last));

      DOM.Readers.Set_Feature (My_Tree_Reader, Sax.Readers.Validation_Feature, False);

      DOM.Readers.Parse (My_Tree_Reader, Read);
      Input_Sources.File.Close (Read);

      declare
         Doc  : constant DOM.Core.Document := DOM.Readers.Get_Tree (My_Tree_Reader);
         Root : constant DOM.Core.Element  := DOM.Core.Documents.Get_Element (Doc);
      begin
         H.Mappings.Initialize (Path => File, Model => H.Model, Node => Root, Context => H);
         H.Hibernate.Initialize (Path => File, Model => H.Model, Node => Root, Context => H);
         H.Query.Initialize (Path => File, Model => H.Model, Node => Root, Context => H);
      end;

--        DOM.Readers.Free (My_Tree_Reader);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Model file {0} does not exist", File);

   end Read_Model;

   --  ------------------------------
   --  Read the model and query files stored in the application directory <b>db</b>.
   --  ------------------------------
   procedure Read_Models (H : in out Handler;
                          Dirname : in String) is
      use Ada.Directories;
      Path    : constant String := Util.Files.Compose (H.Get_Result_Directory, Dirname);
      Name    : constant String := Ada.Directories.Base_Name (Path);
      Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
      Search  : Search_Type;
      Ent     : Directory_Entry_Type;
   begin
      Log.Info ("Reading model file stored in '{0}'", Path);

      --  No argument specified, look at the model files in the db directory.
      if Exists (Path) then
         if Name = "regtests" then
            H.Model.Set_Dirname ("regtests", Path);
         elsif Name = "samples" then
            H.Model.Set_Dirname ("samples", Path);
         else
            H.Model.Set_Dirname ("src", Path);
         end if;
         Start_Search (Search, Directory => Path, Pattern => "*.xml", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Name : constant String := Full_Name (Ent);
            begin
               H.Read_Model (Name);
            end;
         end loop;
      end if;
   end Read_Models;

   --  ------------------------------
   --  Execute the lifecycle phases on the faces context.
   --  ------------------------------
   overriding
   procedure Execute_Lifecycle (App     : in Handler;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      ASF.Applications.Main.Application (App).Execute_Lifecycle (Context);

      declare
         View    : constant Components.Root.UIViewRoot := Context.Get_View_Root;
         Root    : constant access Components.Base.UIComponent'Class
           := Components.Root.Get_Root (View);
      begin
         App.File.all := Root.Get_Attribute (Context, "file");
      end;
   end Execute_Lifecycle;

   --  ------------------------------
   --  Prepare the model by checking, verifying and initializing it after it is completely known.
   --  ------------------------------
   procedure Prepare (H : in out Handler) is
   begin
      H.Model.Prepare;
      H.Hibernate.Prepare (Model => H.Model, Context => H);
      H.Query.Prepare (Model => H.Model, Context => H);
   end Prepare;

   --  ------------------------------
   --  Finish the generation.  Some artifacts could generate other files that take into
   --  account files generated previously.
   --  ------------------------------
   procedure Finish (H : in out Handler) is
   begin
      H.Hibernate.Finish (Model => H.Model, Project => H.Project, Context => H);
      H.Query.Finish (Model => H.Model, Project => H.Project, Context => H);
   end Finish;

   --  ------------------------------
   --  Tell the generator to activate the generation of the given template name.
   --  The name is a property name that must be defined in generator.properties to
   --  indicate the template file.  Several artifacts can trigger the generation
   --  of a given template.  The template is generated only once.
   --  ------------------------------
   procedure Add_Generation (H    : in out Handler;
                             Name : in String;
                             Mode : in Gen.Artifacts.Iteration_Mode) is
      Value : constant String := H.Conf.Get (Name, "");
   begin
      Log.Debug ("Adding template {0} to the generation", Name);

      if Value'Length = 0 then
         H.Error ("Template '{0}' is not defined.", Name);
      else
         H.Templates.Include (To_Unbounded_String (Value), Mode);
      end if;
   end Add_Generation;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access) is
      use Util.Beans.Objects;

      Req   : ASF.Requests.Mockup.Request;
      Reply : ASF.Responses.Mockup.Response;
      Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := Model.all'Unchecked_Access;
      Bean  : constant Object := Util.Beans.Objects.To_Object (Ptr, Util.Beans.Objects.STATIC);

      Model_Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := H.Model'Unchecked_Access;
      Model_Bean  : constant Object := To_Object (Model_Ptr, Util.Beans.Objects.STATIC);

   begin
      Log.Debug ("With template '{0}'", File);

      Req.Set_Path_Info (File);
      Req.Set_Method ("GET");
      Req.Set_Attribute (Name => "package", Value => Bean);
      Req.Set_Attribute (Name => "model", Value => Model_Bean);
      Req.Set_Attribute (Name => "genRevision", Value => Util.Beans.Objects.To_Object (SVN_REV));
      Req.Set_Attribute (Name => "genURL", Value => Util.Beans.Objects.To_Object (SVN_URL));

      H.Dispatch (Page     => File,
                  Request  => Req,
                  Response => Reply);

      declare
         Dir         : constant String := To_String (H.Output_Dir);
         File        : constant String := Util.Beans.Objects.To_String (H.File.all);
         Path        : constant String := Util.Files.Compose (Dir, File);
         Exists      : constant Boolean := Ada.Directories.Exists (Path);
         Content     : Unbounded_String;
         Old_Content : Unbounded_String;
      begin
         if not H.Force_Save and Exists then
            H.Error ("Cannot generate file: '{0}' exists already.", Path);
         else
            Log.Info ("Generating file '{0}'", Path);
            Reply.Read_Content (Content);

            if Exists then
               Util.Files.Read_File (Path     => Path,
                                     Into     => Old_Content,
                                     Max_Size => Natural'Last);
            end if;
            if not Exists or else Content /= Old_Content then
               Util.Files.Write_File (Path => Path, Content => Content);
            end if;
         end if;
      end;
   end Generate;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       Mode  : in Iteration_Mode;
                       File  : in String) is
   begin
      Log.Debug ("Generating with template {0} in mode {1}",
                 File, Iteration_Mode'Image (Mode));

      case Mode is
         when ITERATION_PACKAGE =>
            declare
               Pos : Gen.Model.Packages.Package_Cursor := H.Model.First;
            begin
               while Gen.Model.Packages.Has_Element (Pos) loop
                  Log.Debug ("  Generate for package");
                  H.Generate (File, Gen.Model.Packages.Element (Pos).all'Access);
                  Gen.Model.Packages.Next (Pos);
               end loop;
            end;

         when ITERATION_TABLE =>
            H.Generate (File, H.Model'Unchecked_Access);

      end case;
   end Generate;

   --  ------------------------------
   --  Generate all the code for the templates activated through <b>Add_Generation</b>.
   --  ------------------------------
   procedure Generate_All (H    : in out Handler) is
      Iter : Template_Map.Cursor := H.Templates.First;
   begin
      Log.Debug ("Generating the files {0}",
                Ada.Containers.Count_Type'Image (H.Templates.Length));

      while Template_Map.Has_Element (Iter) loop
         H.Generate (File => To_String (Template_Map.Key (Iter)),
                     Mode => Template_Map.Element (Iter));
         Template_Map.Next (Iter);
      end loop;
   end Generate_All;

   --  ------------------------------
   --  Generate all the code generation files stored in the directory
   --  ------------------------------
   procedure Generate_All (H    : in out Handler;
                           Mode : in Iteration_Mode;
                           Name : in String) is
      use Ada.Directories;

      Search     : Search_Type;
      Filter     : constant Filter_Type := (Ordinary_File => True, others => False);
      Dir_Filter : constant Filter_Type := (Directory => True, others => False);
      Ent        : Directory_Entry_Type;
      Dir        : constant String := H.Conf.Get (ASF.Applications.VIEW_DIR);
      Path       : constant String := Util.Files.Compose (Dir, Name);
      Base_Dir   : constant Unbounded_String := H.Output_Dir;
   begin
      if Kind (Path) /= Directory then
         Ada.Text_IO.Put_Line ("Cannot read model directory: " & Path);
      end if;

      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Base_Name : constant String := Simple_Name (Ent);
            File_Path : constant String := Full_Name (Ent);
            Ext       : constant String := Extension (Base_Name);
            Content   : Unbounded_String;
         begin
            if Ext = "xhtml" then
               H.Generate (Mode, File_Path);
            elsif Util.Strings.Index (Base_Name, '~') = 0 then
               Util.Files.Read_File (Path => File_Path, Into => Content);
               Util.Files.Write_File (Path => Compose (To_String (Base_Dir), Base_Name),
                                      Content => Content);
            end if;
         end;
      end loop;

      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Dir_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Dir_Name : constant String := Simple_Name (Ent);
         begin
            if Dir_Name /= "." and Dir_Name /= ".." and Dir_Name /= ".svn" then
               H.Output_Dir := To_Unbounded_String (Compose (To_String (Base_Dir), Dir_Name));
               H.Generate_All (Mode, Compose (Name, Dir_Name));
            end if;
         end;
      end loop;

      H.Output_Dir := Base_Dir;
   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         H.Error ("Template directory {0} does not exist", Path);
         Log.Info ("Exception: {0}", GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Generate_All;

   --  ------------------------------
   --  Update the project model through the <b>Process</b> procedure.
   --  ------------------------------
   procedure Update_Project (H : in out Handler;
                             Process : not null access
                               procedure (Project : in out Model.Projects.Project_Definition)) is
   begin
      Process (H.Project);
   end Update_Project;

end Gen.Generator;
