-----------------------------------------------------------------------
--  gen-artifacts-yaml -- Query artifact for Code Generator
--  Copyright (C) 2018 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;

with Util.Strings;
with Util.Beans.Objects;

with Text;
with Yaml.Source.File;
with Yaml.Parser;

with Gen.Configs;
with Gen.Model.Tables;
with Gen.Model.Mappings;

with Util.Log.Loggers;
with Util.Stacks;

use Yaml;

package body Gen.Artifacts.Yaml is

   function To_String (S : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;
   use Gen.Configs;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.Yaml");

   type State_Type is (IN_ROOT,
                       IN_TABLE,
                       IN_COLUMNS,
                       IN_KEYS,
                       IN_COLUMN,
                       IN_KEY,
                       IN_UNKOWN);

   type Node_Info is record
      State    : State_Type := IN_UNKOWN;
      Name     : Text.Reference;
      Has_Name : Boolean := False;
      Table    : Gen.Model.Tables.Table_Definition_Access;
      Col      : Gen.Model.Tables.Column_Definition_Access;
   end record;

   type Node_Info_Access is access all Node_Info;

   package Node_Stack is new Util.Stacks (Element_Type        => Node_Info,
                                          Element_Type_Access => Node_Info_Access);

   procedure Process_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                              Stack : in out Node_Stack.Stack;
                              File  : in String;
                              Loc   : in Mark);

   procedure Read_Scalar (Node    : in Node_Info_Access;
                          Name    : in String;
                          Value   : in String;
                          Context : in out Generator'Class);

   procedure Read_Scalar (Node    : in Node_Info_Access;
                          Name    : in String;
                          Value   : in String;
                          Context : in out Generator'Class) is
   begin
      case Node.State is
         when IN_TABLE =>
            if Node.Table = null then
               return;
            end if;

            Log.Debug ("Set table {0} attribute {1}={2}", Node.Table.Name, Name, Value);
            if Name = "table" then
               Node.Table.Table_Name := To_Unbounded_String (Value);
            elsif Name = "description" or Name = "comment" then
               Node.Table.Set_Comment (Value);
            end if;

         when IN_COLUMN | IN_KEY =>
            if Node.Col = null then
               return;
            end if;

            Log.Debug ("Set table column {0} attribute {1}={2}", Node.Col.Name, Name, Value);
            if Name = "type" then
               Node.Col.Set_Type (Value);
            elsif Name = "length" then
               Node.Col.Set_Sql_Length (Value, Context);
            elsif Name = "column" then
               Node.Col.Sql_Name := To_Unbounded_String (Value);
            elsif Name = "unique" then
               Node.Col.Unique := Value = "true" or Value = "yes";
            elsif Name = "nullable" or Name = "optional" then
               Node.Col.Not_Null := Value = "false" or Value = "no";
            elsif Name = "not-null" or Name = "required" then
               Node.Col.Not_Null := Value = "true" or Value = "yes";
            elsif Name = "description" or Name = "comment" then
               Node.Col.Set_Comment (Value);
            end if;

         when others =>
            Log.Error ("Scalar {0}: {1} not handled", Name, Value);
      end case;
   end Read_Scalar;

   procedure Process_Mapping (Model    : in out Gen.Model.Packages.Model_Definition;
                              Stack    : in out Node_Stack.Stack;
                              File     : in String;
                              Loc      : in Mark) is

      function Location return String is
         (File & ":" & Util.Strings.Image (Loc.Line) & ":"
                     & Util.Strings.Image (Loc.Column));

      Node     : Node_Info_Access;
      New_Node : Node_Info_Access;
   begin
      Node := Node_Stack.Current (Stack);
      if Node.Has_Name then
         Node.Has_Name := False;
         case Node.State is
            when IN_ROOT =>
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Gen.Model.Tables.Create_Table (Node.Name);
               New_Node.Table.Set_Location (Location);
               New_Node.State := IN_TABLE;
               Model.Register_Table (New_Node.Table);

            when IN_TABLE =>
               if Node.Name = "fields" or Node.Name = "properties" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_COLUMNS;

               elsif Node.Name = "id" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_KEYS;

               else
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_TABLE;
               end if;

            when IN_COLUMNS =>
               Node.Table.Add_Column (Node.Name, Node.Col);
               Node.Col.Set_Location (Location);
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_COLUMN;

            when IN_KEYS =>
               Node.Table.Add_Column (Node.Name, Node.Col);
               Node.Col.Set_Location (Location);
               Node.Col.Is_Key := True;
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_KEY;

            when others =>
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.State := IN_UNKOWN;

         end case;
      else
         Node_Stack.Push (Stack);

      end if;
   end Process_Mapping;

   --  Read the UML/XMI model file.
   procedure Read_Model (Handler       : in out Artifact;
                         File          : in String;
                         Model         : in out Gen.Model.Packages.Model_Definition;
                         Context       : in out Generator'Class) is
      pragma Unreferenced (Handler);

      Input : Source.Pointer;
      P     : Parser.Instance;
      Cur   : Event;
      Stack : Node_Stack.Stack;
      Node  : Node_Info_Access;
      Loc   : Mark;
   begin
      Log.Info ("Reading YAML file {0}", File);

      Input := Source.File.As_Source (File);
      P.Set_Input (Input);
      loop
         Cur := P.Next;
         exit when Cur.Kind = Stream_End;
         case Cur.Kind is
            when Stream_Start | Document_Start =>
               Node_Stack.Push (Stack);
               Node := Node_Stack.Current (Stack);
               Node.State := IN_ROOT;

            when Stream_End | Document_End =>
               Node_Stack.Pop (Stack);

            when Alias =>
               null;

            when Scalar =>
               Node := Node_Stack.Current (Stack);
               if Node.Has_Name then
                  Read_Scalar (Node, To_String (Node.Name), To_String (Cur.Content), Context);
                  Node.Has_Name := False;
               else
                  Node.Name := Cur.Content;
                  Node.Has_Name := True;
               end if;

            when Sequence_Start =>
               Node_Stack.Push (Stack);

            when Sequence_End =>
               Node_Stack.Pop (Stack);

            when Mapping_Start =>
               Process_Mapping (Model, Stack, File, P.Current_Lexer_Token_Start);

            when Mapping_End =>
               Node_Stack.Pop (Stack);

            when Annotation_Start =>
               null;

            when Annotation_End =>
               null;

         end case;
      end loop;

   exception
      when E : others =>
         Loc := P.Current_Lexer_Token_Start;
         Context.Error ("{0}: {1}", Util.Strings.Image (Loc.Line) & ":"
                        & Util.Strings.Image (Loc.Column) & ": ",
                        Ada.Exceptions.Exception_Message (E));
   end Read_Model;

   --  ------------------------------
   --  Save the model in a YAML file.
   --  ------------------------------
   procedure Save_Model (Handler : in Artifact;
                         Path    : in String;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is

      procedure Write_Description (Comment : in Util.Beans.Objects.Object;
                                   Indent  : in Ada.Text_IO.Count);

      File : Ada.Text_IO.File_Type;

      --  Write a description field taking into account multi-lines.
      procedure Write_Description (Comment : in Util.Beans.Objects.Object;
                                   Indent  : in Ada.Text_IO.Count) is
         use type Ada.Text_IO.Count;

         Content     : constant String := Util.Beans.Objects.To_String (Comment);
         Pos, Start  : Positive := Content'First;
      begin
         Ada.Text_IO.Set_Col (File, Indent);
         Ada.Text_IO.Put (File, "description: ");
         if Util.Strings.Index (Content, ASCII.LF) > 0
           or Util.Strings.Index (Content, ASCII.CR) > 0
         then
            Start := Content'First;
            Pos   := Content'First;
            Ada.Text_IO.Put_Line (File, "|");
            while Pos <= Content'Last loop
               if Content (Pos) = ASCII.CR or Content (Pos) = ASCII.LF then
                  Ada.Text_IO.Set_Col (File, Indent + 2);
                  Ada.Text_IO.Put_Line (File, Content (Start .. Pos - 1));
                  Start := Pos + 1;
               end if;
               Pos := Pos + 1;
            end loop;
            if Start < Pos then
               Ada.Text_IO.Set_Col (File, Indent + 2);
               Ada.Text_IO.Put_Line (File, Content (Start .. Pos - 1));
            end if;
         else
            Ada.Text_IO.Put (File, Content);
         end if;
         Ada.Text_IO.New_Line (File);
      end Write_Description;

      procedure Write_Field (Item  : in Gen.Model.Definition'Class;
                             Name  : in String) is
         Value : constant Util.Beans.Objects.Object := Item.Get_Value (Name);
      begin
         Ada.Text_IO.Put_Line (File, Util.Beans.Objects.To_String (Value));
      end Write_Field;

      procedure Write_Column (Col : in Gen.Model.Tables.Column_Definition'Class) is
         use type Gen.Model.Mappings.Mapping_Definition_Access;

         Col_Type : Gen.Model.Mappings.Mapping_Definition_Access;
      begin
         Col_Type := Col.Get_Type_Mapping;
         Ada.Text_IO.Put (File, "    ");
         Ada.Text_IO.Put (File, Col.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         Ada.Text_IO.Put (File, "      type: ");
         if Col_Type /= null then
            Ada.Text_IO.Put (File, Col_Type.Get_Type_Name);
         end if;
         Ada.Text_IO.New_Line (File);
         if Col.Is_Variable_Length then
            Ada.Text_IO.Put (File, "      length:");
            Ada.Text_IO.Put_Line (File, Positive'Image (Col.Sql_Length));
         end if;
         Ada.Text_IO.Put (File, "      column: ");
         Write_Field (Col, "sqlName");
         if Col_Type /= null and then Col_Type.Nullable then
            Ada.Text_IO.Put_Line (File, "       nullable: true");
         end if;
         Ada.Text_IO.Put (File, "      not-null: ");
         Ada.Text_IO.Put_Line (File, (if Col.Not_Null then "true" else "false"));
         if Col.Is_Version then
            Ada.Text_IO.Put_Line (File, "      version: true");
         end if;
         if not Col.Is_Updated then
            Ada.Text_IO.Put_Line (File, "      readonly: true");
         end if;
         if Col.Is_Auditable then
            Ada.Text_IO.Put_Line (File, "      auditable: true");
         end if;
         Ada.Text_IO.Put (File, "      unique: ");
         Ada.Text_IO.Put_Line (File, (if Col.Unique then "true" else "false"));
         Write_Description (Col.Get_Comment, 7);
      end Write_Column;

      procedure Process_Table (Table : in out Gen.Model.Tables.Table_Definition) is

         Iter : Gen.Model.Tables.Column_List.Cursor := Table.Members.First;
         Col  : Gen.Model.Tables.Column_Definition_Access;
      begin

         Ada.Text_IO.Put (File, Table.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         Ada.Text_IO.Put_Line (File, "  type: entity");
         Ada.Text_IO.Put (File, "  table: ");
         Write_Field (Table, "sqlName");
         Write_Description (Table.Get_Comment, 3);
         Ada.Text_IO.Put (File, "  indexes: ");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put (File, "  id: ");
         Ada.Text_IO.New_Line (File);

         for Col of Table.Members loop
            if Col.Is_Key then
               Write_Column (Col.all);
            end if;
         end loop;

         Ada.Text_IO.Put (File, "  fields: ");
         Ada.Text_IO.New_Line (File);

         for Col of Table.Members loop
            if not Col.Is_Key then
               Write_Column (Col.all);
            end if;
         end loop;
      end Process_Table;

   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Model.Iterate_Tables (Process_Table'Access);
      Ada.Text_IO.Close (File);
   end Save_Model;

   --  ------------------------------
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Context : in out Generator'Class) is
      Iter : Gen.Model.Packages.Package_Cursor;
      Pkg  : Gen.Model.Packages.Package_Definition_Access;

   begin
      Log.Debug ("Preparing the model for query");

      Handler.Save_Model (Path    => "model.yaml",
                          Model   => Model,
                          Context => Context);
      --
--        Iter := Gen.Model.Packages.First (Model);
--        while Gen.Model.Packages.Has_Element (Iter) loop
--           Pkg := Gen.Model.Packages.Element (Iter);
--
--           Gen.Model.Packages.Next (Iter);
--        end loop;
   end Prepare;

end Gen.Artifacts.Yaml;
