-----------------------------------------------------------------------
--  Gen -- Code Generator
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Components.Root;
with ASF.Components.Base;

with Util.Beans.Basic;
with EL.Functions;

with Gen.Model;
with Gen.Model.Tables;
with Gen.Model.Mappings;

with Util.Strings;
with Util.Files;
with Util.Log.Loggers;
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

--     function To_Column_Access is
--       new Util.Beans.Objects.To_Access (T => Gen.Model.Tables.Column_Definition,
--                                         T_Access => Gen.Model.Tables.Column_Definition_Access);

--     function To_Definition_Access is
--       new Util.Beans.Objects.To_Access (T => Gen.Model.Definition,
--                                         T_Access => Gen.Model.Definition_Access);

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

--        Def    : constant Definition_Access := To_Definition_Access (Value);
      Column : Column_Definition_Access := null; --  To_Definition_Access (Value);

      function To_Ada_Type (Value : in String) return Util.Beans.Objects.Object is
      begin
         if Value = "String" or Value = "java.lang.String" then
            return Util.Beans.Objects.To_Object (String '("Ada.Strings.Unbounded.Unbounded_String"));
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
   --  Register the generator EL functions
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
      URI : constant String := "http://code.google.com/p/ada-ado/generator";
   begin
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
      procedure Register_Funcs is
        new ASF.Applications.Main.Register_Functions (Set_Functions);

      Dir     : constant String := Ada.Strings.Unbounded.To_String (Config_Dir);
      Factory : ASF.Applications.Main.Application_Factory;
   begin
      H.Conf.Load_Properties (Path => Dir & "generator.properties");

      H.Conf.Set (ASF.Applications.VIEW_DIR, Config_Dir & "templates/");
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_WHITE_SPACES, "false");
      H.Conf.Set (ASF.Applications.VIEW_ESCAPE_UNKNOWN_TAGS, "false");
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_EMPTY_LINES, "true");
      H.Conf.Set (ASF.Applications.VIEW_FILE_EXT, "");
      H.Initialize (H.Conf, Factory);

      H.Output_Dir := To_Unbounded_String (H.Conf.Get (RESULT_DIR, "./"));

      Register_Funcs (H);
      H.File := new Util.Beans.Objects.Object;

      --  Read the type mappings
      Read_Model (H => H, File => Dir & H.Conf.Get ("generator.mapping", "AdaMappings.xml"));

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Cannot load configuration file {0}", Dir & "generator.properties");
   end Initialize;

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
   --  Get the exit status
   --  Returns 0 if the generation was successful
   --  Returns 1 if there was a generation error
   --  ------------------------------
   function Get_Status (H : in Handler) return Ada.Command_Line.Exit_Status is
   begin
      return H.Status;
   end Get_Status;

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
   --  Read the XML model file
   --  ------------------------------
   procedure Read_Model (H    : in out Handler;
                         File : in String) is
      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;

   begin
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

      H.Doc := DOM.Readers.Get_Tree (My_Tree_Reader);
      H.Root := DOM.Core.Documents.Get_Element (H.Doc);

      H.Mappings.Initialize (Path => File, Model => H.Model, Node => H.Root);
      H.Hibernate.Initialize (Path => File, Model => H.Model, Node => H.Root);
      H.Query.Initialize (Path => File, Model => H.Model, Node => H.Root);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Model file {0} does not exist", File);

   end Read_Model;

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
         Root    : constant access Components.Base.UIComponent'Class := Components.Root.Get_Root (View);
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
      Req   : ASF.Requests.Mockup.Request;
      Reply : ASF.Responses.Mockup.Response;
      Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := Model.all'Unchecked_Access;
      Bean  : constant Util.Beans.Objects.Object := Util.Beans.Objects.To_Object (Ptr);
   begin
      Log.Info ("With template '{0}'", File);

      Req.Set_Path_Info (File);
      Req.Set_Method ("GET");
      Req.Set_Attribute (Name => "model", Value => Bean);
      Req.Set_Attribute (Name => "genRevision", Value => Util.Beans.Objects.To_Object (SVN_REV));
      Req.Set_Attribute (Name => "genURL", Value => Util.Beans.Objects.To_Object (SVN_URL));

      H.Dispatch (Page     => File,
                  Request  => Req,
                  Response => Reply);

      declare
         Dir     : constant String := To_String (H.Output_Dir);
         Path    : constant String := Dir & Util.Beans.Objects.To_String (H.File.all);
         Content : Unbounded_String;
      begin
         Log.Info ("Generating file '{0}'", Path);

         Reply.Read_Content (Content);
         Util.Files.Write_File (Path => Path, Content => Content);
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
      Path       : constant String := Dir & Name;
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
               Util.Files.Write_File (Path => To_String (Base_Dir) & Base_Name,
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
               H.Output_Dir := Base_Dir & "/" & Dir_Name & "/";
               H.Generate_All (Mode, Name & "/" & Dir_Name);
            end if;
         end;
      end loop;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Template directory {0} does not exist", Path);
   end Generate_All;

end Gen.Generator;
