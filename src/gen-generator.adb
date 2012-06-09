-----------------------------------------------------------------------
--  Gen -- Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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
with Ada.Strings.Fixed;

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
with Util.Strings.Vectors;
with EL.Functions;
with EL.Utils;
with EL.Contexts.Default;

with Gen.Utils;
with Gen.Model;
with Gen.Model.Tables;
with Gen.Model.Mappings;
with Gen.Commands.Templates;

with Util.Strings;
with Util.Files;
with Util.Strings.Transforms;
with Util.Log.Loggers;

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

   --  EL function to format an Ada comment
   function Comment (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  EL function to return a singular form of a name
   function To_Singular (Value : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   --  Returns a string resulting from replacing in an input string all occurrences of
   --  a "Item" string into an "By" substring.
   function Replace (Value, Item, By : in Util.Beans.Objects.Object)
                     return Util.Beans.Objects.Object;

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
      use Gen.Model.Mappings;
      use Gen.Model;

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
            if Column.Type_Mapping.Kind = T_DATE and Util.Beans.Objects.To_Integer (Param) = 2 then
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
   --  EL function to return a singular form of a name
   --  ------------------------------
   function To_Singular (Value : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Name   : constant String := Util.Beans.Objects.To_String (Value);
   begin
      if Name'Length > 1 and then Name (Name'Last) = 's' then
         return Util.Beans.Objects.To_Object (Name (Name'First .. Name'Last - 1));
      else
         return Value;
      end if;
   end To_Singular;

   --  ------------------------------
   --  Returns a string resulting from replacing in an input string all occurrences of
   --  a "Item" string into an "By" substring.
   --  ------------------------------
   function Replace (Value, Item, By : in Util.Beans.Objects.Object)
                     return Util.Beans.Objects.Object is
      Content : constant String := Util.Beans.Objects.To_String (Value);
      Pattern : constant String := Util.Beans.Objects.To_String (Item);
      Token   : constant String := Util.Beans.Objects.To_String (By);
      Last    : Natural := Content'First;
      Result  : Ada.Strings.Unbounded.Unbounded_String;
      Pos     : Natural;
   begin
      if Pattern'Length = 0 then
         return Value;
      end if;
      while Last <= Content'Last loop
         Pos := Ada.Strings.Fixed.Index (Content, Pattern, Last);
         if Pos = 0 then
            Append (Result, Content (Last .. Content'Last));
            exit;
         else
            if Last < Pos then
               Append (Result, Content (Last .. Pos - 1));
            end if;
            Append (Result, Token);
         end if;
         Last := Pos + Pattern'Length;
      end loop;
      return Util.Beans.Objects.To_Object (Result);
   end Replace;

   --  ------------------------------
   --  EL function to format an Ada comment
   --  ------------------------------
   function Comment (Value : Util.Beans.Objects.Object) return Util.Beans.Objects.Object is

      START_POS : constant Natural := 8;

      Comment   : constant String := Util.Beans.Objects.To_String (Value);
      Result    : Unbounded_String;
      C         : Character;
      Pos       : Natural := START_POS;
   begin
      for I in Comment'Range loop
         C := Comment (I);
         if Pos > START_POS then
            if C = ASCII.LF then
               Pos := START_POS;
            else
               Append (Result, C);
               Pos := Pos + 1;
            end if;
         elsif C /= ' ' and C /= ASCII.LF then
            if Length (Result) > 0 then
               Append (Result, ASCII.LF);
               Append (Result, "   --  ");
            end if;
            Append (Result, C);
            Pos := Pos + 1;
         end if;
      end loop;
      return Util.Beans.Objects.To_Object (Result);
   end Comment;

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
      Mapper.Set_Function (Name      => "sqlType",
                           Namespace => URI,
                           Func      => To_Sql_Type'Access);
      Mapper.Set_Function (Name      => "keyEnum",
                           Namespace => URI,
                           Func      => To_Key_Enum'Access);
      Mapper.Set_Function (Name      => "comment",
                           Namespace => URI,
                           Func      => Comment'Access);
      Mapper.Set_Function (Name      => "singular",
                           Namespace => URI,
                           Func      => To_Singular'Access);
      Mapper.Set_Function (Name      => "replace",
                           Namespace => URI,
                           Func      => Replace'Access);
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
      Context : EL.Contexts.Default.Default_Context;
   begin
      Log.Debug ("Initialize dynamo with {0}", Path);

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
      H.Conf.Set ("generator.config.dir", Dir);
      EL.Utils.Expand (H.Conf, H.Conf, Context);

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
                                   Path : in String) is
   begin
      H.Conf.Set (RESULT_DIR, Path);
      H.Output_Dir := To_Unbounded_String (Path);
   end Set_Result_Directory;

   --  ------------------------------
   --  Get the result directory path.
   --  ------------------------------
   function Get_Result_Directory (H : in Handler) return String is
   begin
      return To_String (H.Output_Dir);
   end Get_Result_Directory;

   --  ------------------------------
   --  Get the project plugin directory path.
   --  ------------------------------
   function Get_Plugin_Directory (H : in Handler) return String is
   begin
      return Util.Files.Compose (H.Get_Result_Directory, H.Project.Get_Module_Dir);
   end Get_Plugin_Directory;

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
      --  Set the 'search_dirs' property only if we did a recursive scan of GNAT project files.
      --  Otherwise we don't know which Dynamo module or library is used.
      if H.Project.Recursive_Scan then
         if H.Get_Project_Property ("search_dirs", ".") = "." then
            H.Read_Project ("dynamo.xml", True);
         end if;
         H.Set_Project_Property ("search_dirs", H.Get_Search_Directories);
      end if;
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
      Log.Error ("error: " & Message, Arg1, Arg2);
      H.Status := 1;
   end Error;

   --  ------------------------------
   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   --  ------------------------------
   procedure Read_Project (H         : in out Handler;
                           File      : in String;
                           Recursive : in Boolean := False) is
      Dir : constant String := Ada.Directories.Containing_Directory (To_String (H.Config_Dir));
   begin
      H.Project.Install_Dir := To_Unbounded_String (Dir);
      H.Project.Read_Project (File      => File,
                              Config    => H.Conf,
                              Recursive => Recursive);
      H.Set_Global ("projectName", H.Get_Project_Name);
   end Read_Project;

   --  ------------------------------
   --  Read the XML package file
   --  ------------------------------
   procedure Read_Package (H    : in out Handler;
                           File : in String) is
      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;
   begin
      Log.Info ("Reading package file '{0}'", File);

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
         H.Distrib.Initialize (Path => File, Model => H.Model, Node => Root, Context => H);
      end;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Package file {0} does not exist", File);

   end Read_Package;

   --  ------------------------------
   --  Read the model mapping types and initialize the hibernate artifact.
   --  ------------------------------
   procedure Read_Mappings (H    : in out Handler) is
      Mapping : constant String := H.Get_Parameter ("generator.mapping",
                                                    "AdaMappings.xml");
      Dir     : constant String := H.Get_Config_Directory;
   begin
      --  Read the type mappings
      H.Type_Mapping_Loaded := True;
      H.Read_Model (File => Ada.Directories.Compose (Dir, Mapping));
   end Read_Mappings;

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
         H.Read_Mappings;
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
      Files   : Util.Strings.Vectors.Vector;

      package Sort_Names is
         new Util.Strings.Vectors.Generic_Sorting;
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

         --  Collect the files in the vector array.
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            Files.Append (Full_Name (Ent));
         end loop;

         --  Sort the files on their name to get a reproducible generation of some database
         --  models.
         Sort_Names.Sort (Files);

         --  Read the model files
         declare
            Iter : Util.Strings.Vectors.Cursor := Files.First;
         begin
            while Util.Strings.Vectors.Has_Element (Iter) loop
               H.Read_Model (Util.Strings.Vectors.Element (Iter));
               Util.Strings.Vectors.Next (Iter);
            end loop;
         end;
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
         if Root /= null then
            App.File.all := Root.Get_Attribute (Context, "file");
         end if;
      end;
   end Execute_Lifecycle;

   --  ------------------------------
   --  Prepare the model by checking, verifying and initializing it after it is completely known.
   --  ------------------------------
   procedure Prepare (H : in out Handler) is
   begin
      H.Model.Prepare;
      if H.Hibernate.Is_Initialized then
         H.Hibernate.Prepare (Model => H.Model, Context => H);
      end if;
      if H.Query.Is_Initialized then
         H.Query.Prepare (Model => H.Model, Context => H);
      end if;
      if H.Distrib.Is_Initialized then
         H.Distrib.Prepare (Model => H.Model, Context => H);
      end if;
   end Prepare;

   --  ------------------------------
   --  Finish the generation.  Some artifacts could generate other files that take into
   --  account files generated previously.
   --  ------------------------------
   procedure Finish (H : in out Handler) is
   begin
      if H.Hibernate.Is_Initialized then
         H.Hibernate.Finish (Model => H.Model, Project => H.Project, Context => H);
      end if;
      if H.Query.Is_Initialized then
         H.Query.Finish (Model => H.Model, Project => H.Project, Context => H);
      end if;
      if H.Distrib.Is_Initialized then
         H.Distrib.Finish (Model => H.Model, Project => H.Project, Context => H);
      end if;
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

      Prj_Ptr  : constant Util.Beans.Basic.Readonly_Bean_Access := H.Project'Unchecked_Access;
      Prj_Bean : constant Object := To_Object (Prj_Ptr, Util.Beans.Objects.STATIC);
   begin
      Log.Debug ("With template '{0}'", File);

      Req.Set_Path_Info (File);
      Req.Set_Method ("GET");
      Req.Set_Attribute (Name => "project", Value => Prj_Bean);
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
         elsif not Util.Beans.Objects.Is_Null (H.File.all) then
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
            Target    : constant String := Compose (To_String (Base_Dir), Base_Name);
            Content   : Unbounded_String;
         begin
            if Ext = "xhtml" then
               H.Generate (Mode, File_Path);
            elsif Util.Strings.Index (Base_Name, '~') = 0 then
               if Ada.Directories.Exists (Target) and not H.Force_Save then
                  H.Error ("Cannot copy file: '{0}' exists already.", Target);
               else
                  Util.Files.Read_File (Path => File_Path, Into => Content);
                  Util.Files.Write_File (Path => Target,
                                         Content => Content);
               end if;
            end if;
         end;
      end loop;

      Start_Search (Search, Directory => Path, Pattern => "*", Filter => Dir_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Dir_Name : constant String := Simple_Name (Ent);
            Dir      : constant String := Compose (To_String (Base_Dir), Dir_Name);
         begin
            if Dir_Name /= "." and Dir_Name /= ".." and Dir_Name /= ".svn" then
               H.Output_Dir := To_Unbounded_String (Dir);
               if not Ada.Directories.Exists (Dir) then
                  Ada.Directories.Create_Directory (Dir);
               end if;
               H.Generate_All (Mode, Compose (Name, Dir_Name));
            end if;
         end;
      end loop;

      H.Output_Dir := Base_Dir;
   exception
      when E : Ada.IO_Exceptions.Name_Error =>
         H.Error ("Template directory {0} does not exist", Path);
         Log.Info ("Exception: {0}", Util.Log.Loggers.Traceback (E));
   end Generate_All;

   --  ------------------------------
   --  Update the project model through the <b>Process</b> procedure.
   --  ------------------------------
   procedure Update_Project (H : in out Handler;
                             Process : not null access
                               procedure (P : in out Model.Projects.Root_Project_Definition)) is
   begin
      Process (H.Project);
   end Update_Project;

   --  ------------------------------
   --  Scan the dynamo directories and execute the <b>Process</b> procedure with the
   --  directory path.
   --  ------------------------------
   procedure Scan_Directories (H : in Handler;
                               Process : not null access
                                 procedure (Dir : in String)) is
      Iter : Gen.Utils.String_List.Cursor := H.Project.Dynamo_Files.First;
   begin
      while Gen.Utils.String_List.Has_Element (Iter) loop
         declare
            Path : constant String := Gen.Utils.String_List.Element (Iter);
         begin
            Process (Ada.Directories.Containing_Directory (Path));
         end;
         Gen.Utils.String_List.Next (Iter);
      end loop;
   end Scan_Directories;

   --  ------------------------------
   --  Return the search directories that the AWA application can use to find files.
   --  The search directories is built by using the project dependencies.
   --  ------------------------------
   function Get_Search_Directories (H : in Handler) return String is
      Current_Dir : constant String := Ada.Directories.Current_Directory;
      Iter : Gen.Utils.String_List.Cursor := H.Project.Dynamo_Files.Last;
      Dirs : Ada.Strings.Unbounded.Unbounded_String;
   begin
      while Gen.Utils.String_List.Has_Element (Iter) loop
         declare
            Path : constant String := Gen.Utils.String_List.Element (Iter);
            Dir  : constant String := Ada.Directories.Containing_Directory (Path);
         begin
            if Length (Dirs) > 0 then
               Append (Dirs, ";");
            end if;
            Append (Dirs, Util.Files.Get_Relative_Path (Current_Dir, Dir));
         end;
         Gen.Utils.String_List.Previous (Iter);
      end loop;
      return To_String (Dirs);
   end Get_Search_Directories;

end Gen.Generator;
