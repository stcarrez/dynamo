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
with Input_Sources.File;
with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;
with DOM.Readers;
with Sax.Readers;

with ASF.Applications.Views;
with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer.String;

with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Functions;
with EL.Variables;
with EL.Variables.Default;

with Ada.Strings.Unbounded;

with Gen.Model;
with Gen.Model.Tables;

with Util.Files;
with Util.Log.Loggers;
package body Gen.Generator is

   use ASF;
   use Ada.Strings.Unbounded;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Generator");

   Template_Dir : Unbounded_String;

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
      elsif Value = "Integer" then
         return EL.Objects.To_Object (String '("Integer"));
      else
         return Name;
      end if;
   end To_Ada_Type;

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
   end Set_Functions;

   --  ------------------------------
   --  Initialize the generator
   --  ------------------------------
   procedure Initialize (H : in out Handler) is
      Conf : Applications.Config;

      procedure Register_Funcs is
        new ASF.Applications.Views.Register_Functions (Set_Functions);
   begin
      Conf.Set ("view.ignore_white_spaces", "false");
      Conf.Set ("view.escape_unknown_tags", "false");
      H.Initialize (Conf);

      Register_Funcs (H);
   end Initialize;

   function Get_Template_Path (H    : Handler;
                               Name : in String) return String is
   begin
      return To_String (Template_Dir & Name);
   end Get_Template_Path;

   --  Set the directory where template files are stored.
   procedure Set_Template_Directory (Path : in String) is
   begin
      Template_Dir := To_Unbounded_String (Path);
   end Set_Template_Directory;

   --  ------------------------------
   --  Read the XML model file
   --  ------------------------------
   procedure Read_Model (H    : in out Handler;
                         File : in String) is
      Read           : Input_Sources.File.File_Input;
      My_Tree_Reader : DOM.Readers.Tree_Reader;
      Name_Start     : Natural;
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
      declare
         Model_List : DOM.Core.Node_List :=
                        DOM.Core.Elements.Get_Elements_By_Tag_Name (H.Root, "model");
      begin
         H.Model :=  DOM.Core.Nodes.Item (Model_List, 0);
      end;
   end Read_Model;

   --  ------------------------------
   --  Generate the code using the template file
   --  ------------------------------
   procedure Generate (H    : in out Handler;
                       File : in String) is
      Writer    : aliased Contexts.Writer.String.String_Writer;
      Context   : aliased Contexts.Faces.Faces_Context;
      View      : Components.Core.UIViewRoot;
      ELContext : aliased EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Resolver  : aliased EL.Contexts.Default.Default_ELResolver;

      Model     : aliased Gen.Model.Tables.Model_Definition;
   begin
      Log.Info ("Generating {0}", File);

      Model.Initialize (H.Model);
      Resolver.Register (To_Unbounded_String ("model"), Model'Unchecked_Access);
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
         Result : constant EL.Objects.Object := View.Get_Root.Get_Attribute (Context, "file");
         Path   : constant String := To_String (Template_Dir) & EL.Objects.To_String (Result);
      begin
         Log.Info ("Writing result file {0}", Path);
         Util.Files.Write_File (Path => Path, Content => Writer.Get_Response);
      end;
   end Generate;

end Gen.Generator;
