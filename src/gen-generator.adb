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

with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer.String;

with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Functions;
with EL.Variables;
with EL.Variables.Default;

with Gen.Model;
with Gen.Utils;

with Util.Files;
with Util.Log.Loggers;
package body Gen.Generator is

   use ASF;
   use Ada.Strings.Unbounded;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Generator");

   RESULT_DIR : constant String := "generator.output.dir";

   function To_Ada_Type (Name : EL.Objects.Object) return EL.Objects.Object;
   function Indent (Value : EL.Objects.Object) return EL.Objects.Object;

   --  ------------------------------
   --  EL Function to translate a model type to an Ada implementation type
   --  ------------------------------
   function To_Ada_Type (Name : EL.Objects.Object) return EL.Objects.Object is
      Value : constant String := EL.Objects.To_String (Name);
   begin
      if Value = "String" then
         return EL.Objects.To_Object (String '("Unbounded_String"));
      elsif Value = "Integer" or Value = "int" then
         return EL.Objects.To_Object (String '("Integer"));
      else
         return Name;
      end if;
   end To_Ada_Type;

   --  ------------------------------
   --  EL Function to check whether a type is an integer type
   --  ------------------------------
   function Is_Integer_Type (Name : EL.Objects.Object) return EL.Objects.Object is
      Value : constant String := EL.Objects.To_String (Name);
   begin
      if Value = "Integer" or Value = "int" then
         return EL.Objects.To_Object (True);
      else
         return EL.Objects.To_Object (False);
      end if;
   end Is_Integer_Type;

   --  ------------------------------
   --  EL function to indent the code
   --  ------------------------------
   function Indent (Value : EL.Objects.Object) return EL.Objects.Object is
      S      : constant String := EL.Objects.To_String (Value);
      Result : constant String (S'Range) := (others => ' ');
   begin
      return EL.Objects.To_Object (Result);
   end Indent;

   --  ------------------------------
   --  EL function to indent the code
   --  ------------------------------
   function To_Sql_Type (Value : EL.Objects.Object) return EL.Objects.Object is
      Name   : constant String := EL.Objects.To_String (Value);
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
      return EL.Objects.To_Object (Result);
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
      Mapper.Set_Function (Name      => "sqlType",
                           Namespace => URI,
                           Func      => To_Sql_Type'Access);
   end Set_Functions;

   --  ------------------------------
   --  Initialize the generator
   --  ------------------------------
   procedure Initialize (H : in out Handler) is
      procedure Register_Funcs is
        new ASF.Applications.Views.Register_Functions (Set_Functions);
   begin
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_WHITE_SPACES, "false");
      H.Conf.Set (ASF.Applications.VIEW_ESCAPE_UNKNOWN_TAGS, "false");
      H.Conf.Set (ASF.Applications.VIEW_IGNORE_EMPTY_LINES, "true");
      H.Conf.Set (ASF.Applications.VIEW_FILE_EXT, "");
      if not H.Conf.Exists (ASF.Applications.VIEW_DIR) then
         H.Set_Template_Directory ("templates/");
      end if;
      H.Initialize (H.Conf);

      Register_Funcs (H);
   end Initialize;

   --  ------------------------------
   --  Set the directory where template files are stored.
   --  ------------------------------
   procedure Set_Template_Directory (H    : in out Handler;
                                     Path : in String) is
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
   end Set_Result_Directory;

   --  ------------------------------
   --  Register a model mapping
   --  ------------------------------
   procedure Register_Mapping (H    : in out Handler;
                               Node : in DOM.Core.Node) is
   begin
      H.Model.Initialize (Node);
   end Register_Mapping;

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

      procedure Iterate is new Gen.Utils.Iterate_Nodes (T       => Handler,
                                                        Process => Register_Mapping);

   begin
      Log.Info ("Reading model file {0}", File);

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

      Iterate (H, H.Root, "hibernate-mapping");

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Model file {0} does not exist", File);

   end Read_Model;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       File  : in String;
                       Model : in Gen.Model.Definition_Access) is
      Writer    : aliased Contexts.Writer.String.String_Writer;
      Context   : aliased Contexts.Faces.Faces_Context;
      View      : Components.Core.UIViewRoot;
      ELContext : aliased EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Resolver  : aliased EL.Contexts.Default.Default_ELResolver;
   begin
      Log.Info ("Generating {0}", File);

      --  Model.Initialize (H.Model);
      Resolver.Register (To_Unbounded_String ("model"), Model.all'Unchecked_Access);
      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Context.Set_ELContext (ELContext'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);
      ELContext.Set_Resolver (Resolver'Unchecked_Access);
      Writer.Initialize ("text/plain", "UTF-8", 8192);

      H.Set_Context (Context'Unchecked_Access);
      H.Restore_View (File, Context, View);

      H.Render_View (Context, View);
      Writer.Flush;

      declare
         Dir     : constant String := H.Conf.Get (RESULT_DIR, "./");
         Result  : constant EL.Objects.Object := View.Get_Root.Get_Attribute (Context, "file");
         Path    : constant String := Dir & EL.Objects.To_String (Result);
      begin
         Log.Info ("Writing result file {0}", Path);
         Util.Files.Write_File (Path => Path, Content => Writer.Get_Response);
      end;

   end Generate;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H     : in out Handler;
                       Mode  : in Iteration_Mode;
                       File  : in String) is
   begin
      case Mode is
         when ITERATION_PACKAGE =>
            declare
               Pos : Gen.Model.Tables.Package_Cursor := H.Model.First;
            begin
               while Gen.Model.Tables.Has_Element (Pos) loop
                  H.Generate (File, Gen.Model.Tables.Element (Pos).all'Access);
                  Gen.Model.Tables.Next (Pos);
               end loop;
            end;

         when ITERATION_TABLE =>
            H.Generate (File, H.Model'Unchecked_Access);

      end case;
   end Generate;

   --  Generate all the code generation files stored in the directory
   procedure Generate_All (H    : in out Handler;
                           Mode : in Iteration_Mode;
                           Name : in String) is
      use Ada.Directories;

      Search  : Search_Type;
      Filter  : constant Filter_Type := (others => True);
      Ent     : Directory_Entry_Type;
      Dir     : constant String := H.Conf.Get (ASF.Applications.VIEW_DIR);
      Path    : constant String := Dir & Name;
   begin
      if Kind (Path) /= Directory then
         Ada.Text_IO.Put_Line ("Cannot read model directory: " & Path);
      end if;

      Start_Search (Search, Directory => Path, Pattern => "*.xhtml", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            File_Path : constant String := Full_Name (Ent);
         begin
            H.Generate (Mode, File_Path);
         end;
      end loop;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         H.Error ("Template directory {0} does not exist", Path);
   end Generate_All;

end Gen.Generator;
