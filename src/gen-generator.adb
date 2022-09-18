-----------------------------------------------------------------------
--  gen-generator -- Code Generator
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with Ada.Calendar;
with Ada.Environment_Variables;

with Input_Sources.File;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Components.Root;
with ASF.Components.Base;
with ASF.Servlets.Faces;
with Servlet.Core;

with EL.Functions;
with EL.Utils;
with EL.Contexts.Default;

with Gen.Utils;
with Gen.Configs;
with Gen.Model;
with Gen.Model.Enums;
with Gen.Model.Tables;
with Gen.Model.Mappings;
with Gen.Commands.Templates;

with Util.Strings;
with Util.Strings.Vectors;
with Util.Files;
with Util.Log.Loggers;
with Util.Beans.Basic;
with Util.Beans.Objects.Time;
with Util.Systems.Os;

package body Gen.Generator is

   use ASF;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Generator");

   RESULT_DIR : constant String := "generator.output.dir";

   Windows  : constant Boolean := Util.Systems.Os.Directory_Separator = '\';

   function Get_Ada_Type (Type_Name : in UString) return String;

   function To_Ada_Type (Value : in UBO.Object;
                         Param : in UBO.Object) return UBO.Object;
   function Indent (Value : UBO.Object) return UBO.Object;

   --  EL Function to translate a model type to the key enum value
   function To_Key_Enum (Name : UBO.Object) return UBO.Object;

   --  EL function to create an Ada identifier from a file name
   function To_Ada_Ident (Value : UBO.Object) return UBO.Object;

   --  EL function to format an Ada comment
   function Comment (Value  : in UBO.Object;
                     Prefix : in UBO.Object) return UBO.Object;

   --  EL function to return a singular form of a name
   function To_Singular (Value : in UBO.Object) return UBO.Object;

   --  Returns a string resulting from replacing in an input string all occurrences of
   --  a "Item" string into an "By" substring.
   function Replace (Value, Item, By : in UBO.Object)
                     return UBO.Object;

   --  Concat the arguments converted as a string.
   function Concat (Arg1, Arg2, Arg3, Arg4 : in UBO.Object)
                    return UBO.Object;

   --  EL function to check if a file exists.
   function File_Exists (Path : in UBO.Object) return UBO.Object;

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

   Current_Package : UString;

   function Get_Ada_Type (Type_Name : in UString) return String is
      Name : constant String := To_String (Type_Name);
      Pkg  : constant String := To_String (Current_Package);
   begin
      if Name'Length > Pkg'Length
        and then Util.Strings.Starts_With (Name, Pkg)
        and then Name (Name'First + Pkg'Length) = '.'
      then
         return Name (Name'First + Pkg'Length + 1 .. Name'Last);
      else
         return Name;
      end if;
   end Get_Ada_Type;

   --  ------------------------------
   --  EL Function to translate a model type to an Ada implementation type
   --  Param values:
   --    0 : Get the type for a record declaration
   --    1 : Get the type for a parameter declaration or a return type
   --    2 : Get the type for the generation of the ADO.Statements.Get procedure name
   --  ------------------------------
   function To_Ada_Type (Value : in UBO.Object;
                         Param : in UBO.Object) return UBO.Object is
      use Gen.Model.Tables;
      use Gen.Model.Mappings;
      use Gen.Model;

      Column : Column_Definition_Access := null;

      Ptr : constant access Util.Beans.Basic.Readonly_Bean'Class
        := UBO.To_Bean (Value);
      Type_Mapping : Gen.Model.Mappings.Mapping_Definition_Access;
   begin
      if Ptr /= null and then Ptr.all in Column_Definition'Class then
         Column := Column_Definition'Class (Ptr.all)'Unchecked_Access;
      else
         Column := null;
      end if;
      if Column /= null then
         Type_Mapping := Column.Get_Type_Mapping;
         if Type_Mapping /= null then

            if Type_Mapping.Kind = T_DATE and then UBO.To_Integer (Param) = 2 then
               return UBO.To_Object (String '("Time"));

            elsif Type_Mapping.Kind = T_ENUM then
               if Column.Not_Null or else UBO.To_Integer (Param) = 2 then
                  return UBO.To_Object (Get_Ada_Type (Column.Type_Name));
               else
                  declare
                     Result : constant UString
                       := Gen.Model.Enums.Enum_Definition (Type_Mapping.all).Nullable_Type;
                  begin
                     return UBO.To_Object (Get_Ada_Type (Result));
                  end;
               end if;

            elsif Type_Mapping.Kind /= T_TABLE then
               return Type_Mapping.To_Ada_Type (UBO.To_Integer (Param));

            elsif Column.Use_Foreign_Key_Type then
               return UBO.To_Object (Type_Mapping.Target);

            elsif UBO.To_Integer (Param) = 1 then
               return UBO.To_Object (Get_Ada_Type (Type_Mapping.Target) & "_Ref'Class");

            else
               return UBO.To_Object (Get_Ada_Type (Type_Mapping.Target) & "_Ref");
            end if;
         elsif Column.Is_Basic_Type then
            return UBO.To_Object (Column.Get_Type);
         elsif UBO.To_Integer (Param) = 1 then
            return UBO.To_Object (Get_Ada_Type (To_UString (Column.Get_Type)) & "_Ref'Class");
         else
            return UBO.To_Object (Get_Ada_Type (To_UString (Column.Get_Type)) & "_Ref");
         end if;
      else
         return Value;
      end if;
   end To_Ada_Type;

   --  ------------------------------
   --  Concat the arguments converted as a string.
   --  ------------------------------
   function Concat (Arg1, Arg2, Arg3, Arg4 : in UBO.Object)
                    return UBO.Object is
      Result : UString;
   begin
      if not UBO.Is_Null (Arg1) then
         Append (Result, UBO.To_String (Arg1));
      end if;
      if not UBO.Is_Null (Arg2) then
         Append (Result, UBO.To_String (Arg2));
      end if;
      if not UBO.Is_Null (Arg3) then
         Append (Result, UBO.To_String (Arg3));
      end if;
      if not UBO.Is_Null (Arg4) then
         Append (Result, UBO.To_String (Arg4));
      end if;
      return UBO.To_Object (Result);
   end Concat;

   KEY_INTEGER_LABEL : constant String := "KEY_INTEGER";
   KEY_STRING_LABEL  : constant String := "KEY_STRING";

   --  ------------------------------
   --  EL Function to translate a model type to the key enum value
   --  ------------------------------
   function To_Key_Enum (Name : UBO.Object) return UBO.Object is
      Value : constant String := UBO.To_String (Name);
   begin
      if Value in "Integer" | "int" | "Identifier" | "ADO.Identifier" then
         return UBO.To_Object (KEY_INTEGER_LABEL);
      else
         return UBO.To_Object (KEY_STRING_LABEL);
      end if;
   end To_Key_Enum;

   --  ------------------------------
   --  EL function to indent the code
   --  ------------------------------
   function Indent (Value : UBO.Object) return UBO.Object is
      S      : constant String := UBO.To_String (Value);
      Result : constant String (S'Range) := (others => ' ');
   begin
      return UBO.To_Object (Result);
   end Indent;

   --  ------------------------------
   --  EL function to create an Ada identifier from a file name
   --  ------------------------------
   function To_Ada_Ident (Value : UBO.Object) return UBO.Object is
      Name   : constant String := UBO.To_String (Value);
      Result : UString;
      C      : Character;
   begin
      for I in Name'Range loop
         C := Name (I);
         if C = '-' then
            Append (Result, '_');

         elsif C in 'a' .. 'z' then
            Append (Result, C);

         elsif C in 'A' .. 'Z' then
            Append (Result, C);

         elsif C in '0' .. '9' then
            Append (Result, C);
         end if;
      end loop;
      return UBO.To_Object (Result);
   end To_Ada_Ident;

   --  ------------------------------
   --  EL function to return a singular form of a name
   --  ------------------------------
   function To_Singular (Value : in UBO.Object) return UBO.Object is
      Name   : constant String := UBO.To_String (Value);
   begin
      if Name'Length > 1 and then Name (Name'Last) = 's' then
         return UBO.To_Object (Name (Name'First .. Name'Last - 1));
      else
         return Value;
      end if;
   end To_Singular;

   --  ------------------------------
   --  Returns a string resulting from replacing in an input string all occurrences of
   --  a "Item" string into an "By" substring.
   --  ------------------------------
   function Replace (Value, Item, By : in UBO.Object)
                     return UBO.Object is
      Content : constant String := UBO.To_String (Value);
      Pattern : constant String := UBO.To_String (Item);
      Token   : constant String := UBO.To_String (By);
      Last    : Natural := Content'First;
      Result  : UString;
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
      return UBO.To_Object (Result);
   end Replace;

   --  ------------------------------
   --  EL function to format an Ada comment
   --  ------------------------------
   function Comment (Value  : in UBO.Object;
                     Prefix : in UBO.Object) return UBO.Object is

      START_POS : constant Natural := 8;

      Comment   : constant String := Ada.Strings.Fixed.Trim (UBO.To_String (Value),
                                                             Ada.Strings.Both);
      Result    : UString;
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
         elsif C /= ' ' and then C /= ASCII.LF then
            if Length (Result) > 0 then
               Append (Result, ASCII.LF);
               Append (Result, "   --  ");
            else
               Append (Result, "  ");
               if not UBO.Is_Null (Prefix)
                 and then not UBO.Is_Empty (Prefix)
               then
                  Append (Result, UBO.To_String (Prefix));
               end if;
            end if;
            Append (Result, C);
            Pos := Pos + 1;
         end if;
      end loop;
      Append (Result, ASCII.LF);
      return UBO.To_Object (Result);
   end Comment;

   --  ------------------------------
   --  EL function to check if a file exists.
   --  ------------------------------
   function File_Exists (Path : in UBO.Object) return UBO.Object is
      P : constant String := UBO.To_String (Path);
   begin
      return UBO.To_Object (Ada.Directories.Exists (P));
   end File_Exists;

   --  ------------------------------
   --  Register the generator EL functions
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => "adaIdent",
                           Namespace => G_URI,
                           Func      => To_Ada_Ident'Access);
      Mapper.Set_Function (Name      => "adaType",
                           Namespace => G_URI,
                           Func      => To_Ada_Type'Access);
      Mapper.Set_Function (Name      => "indent",
                           Namespace => G_URI,
                           Func      => Indent'Access);
      Mapper.Set_Function (Name      => "keyEnum",
                           Namespace => G_URI,
                           Func      => To_Key_Enum'Access);
      Mapper.Set_Function (Name      => "comment",
                           Namespace => G_URI,
                           Func      => Comment'Access);
      Mapper.Set_Function (Name      => "singular",
                           Namespace => G_URI,
                           Func      => To_Singular'Access);
      Mapper.Set_Function (Name      => "replace",
                           Namespace => G_URI,
                           Func      => Replace'Access);
      Mapper.Set_Function (Name      => "exists",
                           Namespace => G_URI,
                           Func      => File_Exists'Access);
      Mapper.Set_Function (Name      => "concat",
                           Namespace => G_URI,
                           Func      => Concat'Access);
   end Set_Functions;

   --  ------------------------------
   --  Set the global configuration identified by the name by pre-pending the
   --  environtment variable if it is defined.
   --  ------------------------------
   procedure Set_Configuration (H        : in out Handler;
                                Name     : in String;
                                Env_Name : in String) is
   begin
      if not Ada.Environment_Variables.Exists (Env_Name) then
         return;
      end if;
      declare
         Value : String
           := Ada.Environment_Variables.Value (Env_Name);
      begin
         if not Windows then
            for I in Value'Range loop
               if Value (I) = ':' then
                  Value (I) := ';';
               end if;
            end loop;
         end if;
         if H.Conf.Exists (Name) then
            H.Conf.Set (Name, String '(Value & ";" & H.Conf.Get (Name)));
         else
            H.Conf.Set (Name, Value);
         end if;
      end;
   end Set_Configuration;

   --  ------------------------------
   --  Initialize the generator
   --  ------------------------------
   procedure Initialize (H : in out Handler;
                         Config_Dir : in UString;
                         Debug : in Boolean) is
      use Ada.Directories;

      procedure Register_Funcs is
        new ASF.Applications.Main.Register_Functions (Set_Functions);

      Dir     : constant String := To_String (Config_Dir);
      Factory : ASF.Applications.Main.Application_Factory;
      Path    : constant String := Compose (Dir, "generator.properties");
      Context : EL.Contexts.Default.Default_Context;
      Props   : Util.Properties.Manager;
   begin
      Log.Debug ("Initialize dynamo with {0}", Path);

      begin
         Props.Load_Properties (Path => Path);
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
      if Debug then
         Log.Info ("Setting debug mode");
         H.Conf.Set (Gen.Configs.GEN_DEBUG_ENABLE, "1");
      end if;
      Props.Set ("generator_config_dir", Dir);
      EL.Utils.Expand (Source => Props, Into => H.Conf, Context => Context);
      H.Set_Configuration (Gen.Configs.GEN_DYNAMO_SEARCH_DIRS,
                           Gen.Configs.ENV_DYNAMO_SEARCH_PATH);
      H.Set_Configuration (Gen.Configs.GEN_UML_DIR,
                           Gen.Configs.ENV_DYNAMO_UML_PATH);
      H.Set_Configuration ("bundle.dir",
                           Gen.Configs.ENV_DYNAMO_BUNDLE_PATH);
      H.Initialize (H.Conf, Factory);

      H.Config_Dir := To_UString (Dir);
      H.Output_Dir := To_UString (H.Conf.Get (RESULT_DIR, "./"));

      Register_Funcs (H);
      H.File   := new UBO.Object;
      H.Mode   := new UBO.Object;
      H.Ignore := new UBO.Object;
      H.Servlet := new ASF.Servlets.Faces.Faces_Servlet;
      H.Add_Servlet (Name => "file", Server => H.Servlet);
      H.Add_Mapping ("*.xhtml", "file");
      H.Start;

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
                                     Path : in UString) is
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
      H.Output_Dir := To_UString (Path);
   end Set_Result_Directory;

   --  ------------------------------
   --  Get the result directory path.
   --  ------------------------------
   overriding
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
   overriding
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
   overriding
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in String := "") return String is
   begin
      if Util.Strings.Starts_With (Name, "dynamo.") then
         return H.Get_Project_Property (Name (Name'First + 7 .. Name'Last), Default);
      end if;
      return H.Conf.Get (Name, Default);
   end Get_Parameter;

   --  ------------------------------
   --  Get the configuration parameter.
   --  ------------------------------
   overriding
   function Get_Parameter (H       : in Handler;
                           Name    : in String;
                           Default : in Boolean := False) return Boolean is
   begin
      if not H.Conf.Exists (Name) then
         return Default;
      else
         declare
            V : constant String := H.Conf.Get (Name);
         begin
            return V in "1" | "true" | "yes";
         end;
      end if;
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
      Pos : constant Natural := Util.Strings.Index (Name, '-');
   begin
      if not Gen.Utils.Is_Valid_Name (Name)
        and then (Pos <= Name'First
                  or else not Gen.Utils.Is_Valid_Name (Name (Name'First .. Pos - 1)))
      then
         H.Error ("The project name should be a valid Ada identifier ([A-Za-z][A-Za-z0-9_]*).");
         raise Fatal_Error with "Invalid project name: " & Name;
      end if;
      H.Project.Set_Name (Name);
      H.Set_Global ("projectName", Name);
      if Pos > Name'First then
         H.Set_Global ("projectAdaName", Name (Name'First .. Pos - 1));
      else
         H.Set_Global ("projectAdaName", Name);
      end if;
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
      H.Project.Update_From_Properties;
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
--           if H.Get_Project_Property ("search_dirs", ".") = "." then
--              H.Read_Project ("dynamo.xml", True);
--           end if;

         --  Do not update the search_dirs if the project is a plugin.
         --  This is only meaningful in the final project.
         if not H.Project.Is_Plugin then
            H.Set_Project_Property ("search_dirs", H.Get_Search_Directories);
         end if;
      end if;
      H.Project.Save (Path);
   end Save_Project;

   --  ------------------------------
   --  Get the path of the last generated file.
   --  ------------------------------
   function Get_Generated_File (H : in Handler) return String is
   begin
      return UBO.To_String (H.File.all);
   end Get_Generated_File;

   --  ------------------------------
   --  Report an error and set the exit status accordingly
   --  ------------------------------
   overriding
   procedure Error (H : in out Handler;
                    Message : in String;
                    Arg1    : in String;
                    Arg2    : in String := "") is
   begin
      Log.Error ("error: " & Message, Arg1, Arg2);
      H.Status := 1;
   end Error;

   overriding
   procedure Error (H       : in out Handler;
                    Message : in String) is
   begin
      Log.Error ("error: " & Message);
      H.Status := 1;
   end Error;

   --  ------------------------------
   --  Report an info message.
   --  ------------------------------
   procedure Info (H : in out Handler;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
      pragma Unreferenced (H);
   begin
      Log.Info (Message, Arg1, Arg2, Arg3);
   end Info;

   --  ------------------------------
   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   --  ------------------------------
   procedure Read_Project (H         : in out Handler;
                           File      : in String;
                           Recursive : in Boolean := False) is
      Dir        : constant String := H.Get_Install_Directory;
      Search_Dir : constant String := H.Get_Parameter (Gen.Configs.GEN_DYNAMO_SEARCH_DIRS, ".");
   begin
      H.Project.Install_Dir := To_UString (Dir & ";" & Search_Dir);
      H.Project.Read_Project (File      => File,
                              Config    => H.Conf,
                              Recursive => Recursive);
      H.Set_Project_Name (H.Get_Project_Name);
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
      procedure Read_Mapping (Name    : in String;
                              Default : in String);

      Dir     : constant String := H.Get_Config_Directory;

      procedure Read_Mapping (Name    : in String;
                              Default : in String) is
         Mapping : constant String := H.Get_Parameter (Name, Default);
      begin
         H.Read_Model (File => Util.Files.Compose (Dir, Mapping), Silent => True);
      end Read_Mapping;

   begin
      --  Read the type mappings for Ada, MySQL, Postgresql and SQLite.
      H.Type_Mapping_Loaded := True;
      Read_Mapping ("generator.mapping.ada", "AdaMappings.xml");
      Read_Mapping ("generator.mapping.mysql", "MySQLMappings.xml");
      Read_Mapping ("generator.mapping.postgresql", "PostgresqlMappings.xml");
      Read_Mapping ("generator.mapping.sqlite", "SQLiteMappings.xml");
   end Read_Mappings;

   --  ------------------------------
   --  Read the XML model file
   --  ------------------------------
   procedure Read_Model (H      : in out Handler;
                         File   : in String;
                         Silent : in Boolean) is
      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;
      Ext            : constant String := Ada.Directories.Extension (File);
   begin
      --  Before loading a model file, we should know the type mappings.
      --  Load them first if needed.
      if not H.Type_Mapping_Loaded then
         H.Read_Mappings;
      end if;

      Gen.Model.Mappings.Set_Mapping_Name (Gen.Model.Mappings.ADA_MAPPING);
      if Silent then
         Log.Debug ("Reading model file '{0}'", File);
      else
         Log.Info ("Reading model file '{0}'", File);
      end if;
      if Ext in "xmi" | "XMI" | "zargo" then
         H.XMI.Read_Model (File, "", H);
         return;
      elsif Ext in "yaml" | "YAML" then
         H.Yaml.Read_Model (File, H.Model, H);
         return;
      end if;

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
         H.Yaml.Initialize (Path => File, Model => H.Model, Node => Root, Context => H);
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
      Path    : constant String := Dirname;
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
         Start_Search (Search, Directory => Path, Pattern => "*.xm[il]", Filter => Filter);

         --  Collect the files in the vector array.
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            Files.Append (Full_Name (Ent));
         end loop;

         Start_Search (Search, Directory => Path, Pattern => "*.yaml", Filter => Filter);

         --  Collect the files in the vector array.
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            Files.Append (Full_Name (Ent));
         end loop;

         Start_Search (Search, Directory => Path, Pattern => "*.zargo", Filter => Filter);

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
               H.Read_Model (File   => Util.Strings.Vectors.Element (Iter),
                             Silent => False);
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
            App.File.all   := Root.Get_Attribute (Context, "file");
            App.Mode.all   := Root.Get_Attribute (Context, "mode");
            App.Ignore.all := Root.Get_Attribute (Context, "ignore");
         end if;
      end;
   end Execute_Lifecycle;

   --  ------------------------------
   --  Prepare the model by checking, verifying and initializing it after it is completely known.
   --  ------------------------------
   procedure Prepare (H : in out Handler) is
   begin
      if H.XMI.Is_Initialized then
         H.XMI.Prepare (Model => H.Model, Project => H.Project, Context => H);
      end if;
      if H.Yaml.Is_Initialized then
         H.Yaml.Prepare (Model => H.Model, Project => H.Project, Context => H);
      end if;
      H.Model.Prepare;
      if H.Hibernate.Is_Initialized then
         H.Hibernate.Prepare (Model => H.Model, Project => H.Project, Context => H);
      end if;
      if H.Query.Is_Initialized then
         H.Query.Prepare (Model => H.Model, Project => H.Project, Context => H);
      end if;
      if H.Distrib.Is_Initialized then
         H.Distrib.Prepare (Model => H.Model, Project => H.Project, Context => H);
      end if;
      H.Model.Validate (H);
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
      if H.XMI.Is_Initialized then
         H.XMI.Finish (Model => H.Model, Project => H.Project, Context => H);
      end if;
   end Finish;

   --  ------------------------------
   --  Tell the generator to activate the generation of the given template name.
   --  The name is a property name that must be defined in generator.properties to
   --  indicate the template file.  Several artifacts can trigger the generation
   --  of a given template.  The template is generated only once.
   --  ------------------------------
   overriding
   procedure Add_Generation (H    : in out Handler;
                             Name : in String;
                             Mode : in Gen.Artifacts.Iteration_Mode;
                             Mapping : in String) is
      Value : constant String := H.Conf.Get (Name, "");
   begin
      Log.Debug ("Adding template {0} to the generation", Name);

      if Value'Length = 0 then
         H.Error ("Template '{0}' is not defined.", Name);
      else
         H.Templates.Include (To_UString (Value),
                              Template_Context '(Mode, To_UString (Mapping)));
      end if;
   end Add_Generation;

   --  ------------------------------
   --  Enable the generation of the Ada package given by the name.  By default all the Ada
   --  packages found in the model are generated.  When called, this enables the generation
   --  only for the Ada packages registered here.
   --  ------------------------------
   procedure Enable_Package_Generation (H    : in out Handler;
                                        Name : in String) is
   begin
      H.Model.Enable_Package_Generation (Name);
   end Enable_Package_Generation;

   --  ------------------------------
   --  Save the content generated by the template generator.
   --  ------------------------------
   procedure Save_Content (H       : in out Handler;
                           File    : in String;
                           Content : in UString) is
      Dir         : constant String := To_String (H.Output_Dir);
      Mode        : constant String := UBO.To_String (H.Mode.all);
      Path        : constant String := Util.Files.Compose (Dir, File);
      Exists      : constant Boolean := Ada.Directories.Exists (Path);
      Old_Content : UString;
   begin
      if Exists and then Mode = "once" then
         Log.Info ("File {0} exists, generation skipped.", Path);
      elsif Exists and then not (H.Force_Save or else Mode = "force") then
         H.Error ("Cannot generate file: '{0}' exists already.", Path);
      elsif not UBO.Is_Null (H.File.all) and
      then not UBO.To_Boolean (H.Ignore.all)
      then
         if Length (Content) = 0 and then Mode = "remove-empty" then
            Log.Debug ("File {0} skipped because it is empty", Path);
         else
            Log.Info ("Generating file '{0}'", Path);

            if Exists then
               Util.Files.Read_File (Path     => Path,
                                     Into     => Old_Content,
                                     Max_Size => Natural'Last);
            end if;
            if not Exists or else Content /= Old_Content then
               Util.Files.Write_File (Path => Path, Content => Content);
            end if;
         end if;
      end if;
   end Save_Content;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access;
                       Save  : not null access procedure (H       : in out Handler;
                                                          File    : in String;
                                                          Content : in UString)) is
      Req   : ASF.Requests.Mockup.Request;
      Reply : aliased ASF.Responses.Mockup.Response;
      Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := Model.all'Unchecked_Access;
      Bean  : constant UBO.Object := UBO.To_Object (Ptr, UBO.STATIC);

      Model_Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := H.Model'Unchecked_Access;
      Model_Bean  : constant UBO.Object := UBO.To_Object (Model_Ptr, UBO.STATIC);

      Prj_Ptr  : constant Util.Beans.Basic.Readonly_Bean_Access := H.Project'Unchecked_Access;
      Prj_Bean : constant UBO.Object := UBO.To_Object (Prj_Ptr, UBO.STATIC);
      Dispatcher : constant ASF.Servlets.Request_Dispatcher := H.Get_Request_Dispatcher (File);
   begin
      Log.Debug ("With template '{0}'", File);

      Current_Package := To_UString (Model.Get_Name);

      Req.Set_Method ("GET");
      Req.Set_Attribute (Name => "project", Value => Prj_Bean);
      Req.Set_Attribute (Name => "package", Value => Bean);
      Req.Set_Attribute (Name => "model", Value => Model_Bean);
      Req.Set_Attribute (Name => "genRevision", Value => UBO.To_Object (Configs.VERSION));
      Req.Set_Attribute (Name => "genURL", Value => UBO.To_Object (Configs.GIT_URL));
      Req.Set_Attribute (Name => "date",
                         Value => UBO.Time.To_Object (Ada.Calendar.Clock));

      Servlet.Core.Forward (Dispatcher, Req, Reply);

      declare
         Content  : UString;
         File     : constant String := UBO.To_String (H.File.all);
      begin
         Reply.Read_Content (Content);
         Save (H, File, Content);
      end;
   end Generate;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       Mode  : in Iteration_Mode;
                       File  : in String;
                       Save  : not null access
                         procedure (H       : in out Handler;
                                    File    : in String;
                                    Content : in UString)) is
   begin
      Log.Debug ("Generating with template {0} in mode {1}",
                 File, Iteration_Mode'Image (Mode));

      case Mode is
         when ITERATION_PACKAGE =>
            declare
               Pos : Gen.Model.Packages.Package_Cursor := H.Model.First;
            begin
               while Gen.Model.Packages.Has_Element (Pos) loop
                  declare
                     P          : constant Gen.Model.Packages.Package_Definition_Access
                       := Gen.Model.Packages.Element (Pos);
                     Name       : constant String := P.Get_Name;
                  begin
                     if H.Model.Is_Generation_Enabled (Name) then
                        Log.Debug ("  Generate for package {0}", Name);

                        H.Generate (File, Gen.Model.Definition_Access (P), Save);
                     else
                        Log.Debug ("Package {0} not generated", Name);
                     end if;
                  end;
                  Gen.Model.Packages.Next (Pos);
               end loop;
            end;

         when ITERATION_TABLE =>
            H.Generate (File, H.Model'Unchecked_Access, Save);

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
         declare
            T : constant Template_Context := Template_Map.Element (Iter);
         begin
            Gen.Model.Mappings.Set_Mapping_Name (To_String (T.Mapping));
            H.Generate (File => To_String (Template_Map.Key (Iter)),
                        Mode => T.Mode,
                        Save => Save_Content'Access);
         end;
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
      Base_Dir   : constant UString := H.Output_Dir;
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
            Content   : UString;
         begin
            if Ext = "xhtml" then
               H.Generate (Mode, File_Path, Save_Content'Access);
            elsif Util.Strings.Index (Base_Name, '~') = 0 then
               if Ada.Directories.Exists (Target) and then not H.Force_Save then
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
            if not (Dir_Name in "." | ".." | ".svn") then
               H.Output_Dir := To_UString (Dir);
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
   overriding
   procedure Scan_Directories (H : in Handler;
                               Process : not null access
                                 procedure (Dir : in String)) is
      Iter : Gen.Utils.String_List.Cursor := H.Project.Dynamo_Files.First;
   begin
      while Gen.Utils.String_List.Has_Element (Iter) loop
         declare
            use type Gen.Model.Projects.Project_Definition_Access;

            Name : constant String := Gen.Utils.String_List.Element (Iter);
            Prj  : constant Gen.Model.Projects.Project_Definition_Access
                 := H.Project.Find_Project (Name);
         begin
            if Prj /= null then
               Process (Prj.Get_Base_Dir);
            end if;
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
      Dirs : UString;
   begin
      while Gen.Utils.String_List.Has_Element (Iter) loop
         declare
            use type Gen.Model.Projects.Project_Definition_Access;

            Name : constant String := Gen.Utils.String_List.Element (Iter);
            Prj  : constant Gen.Model.Projects.Project_Definition_Access
                 := H.Project.Find_Project (Name);
         begin
            if Prj /= null then
               if Length (Dirs) > 0 then
                  Append (Dirs, ";");
               end if;
               Append (Dirs, Util.Files.Get_Relative_Path (Current_Dir, Prj.Get_Base_Dir));
            end if;
         end;
         Gen.Utils.String_List.Previous (Iter);
      end loop;
      return To_String (Dirs);
   end Get_Search_Directories;

end Gen.Generator;
