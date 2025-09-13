-----------------------------------------------------------------------
--  gen-artifacts-yaml -- Query artifact for Code Generator
--  Copyright (C) 2018, 2019, 2021, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Exceptions;
with Ada.Text_IO;

with Util.Strings;
with Util.Beans.Objects;

with Text;
with Yaml.Source.File;
with Yaml.Parser;

with Gen.Model.Enums;
with Gen.Model.Tables;
with Gen.Model.Beans;
with Gen.Model.Mappings;
with Gen.Model.Operations;

with Util.Log.Loggers;
with Util.Stacks;

use Yaml;

package body Gen.Artifacts.Yaml is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;
   use Gen.Model.Beans;
   use Gen.Model.Operations;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Artifacts.Yaml");

   type State_Type is (IN_ROOT,
                       IN_TYPE,
                       IN_TABLE,
                       IN_BEAN,
                       IN_COLUMNS,
                       IN_ONE_TO_MANY,
                       IN_OPERATIONS,
                       IN_OPERATION,
                       IN_ENUM,
                       IN_ENUM_VALUES,
                       IN_KEYS,
                       IN_COLUMN,
                       IN_ASSOCIATION,
                       IN_KEY,
                       IN_GENERATOR,
                       IN_UNKNOWN);

   type Node_Info is record
      State     : State_Type := IN_UNKNOWN;
      Name      : Text.Reference;
      Tag       : Text.Reference;
      Has_Name  : Boolean := False;
      Enum      : Gen.Model.Enums.Enum_Definition_Access;
      Table     : Gen.Model.Tables.Table_Definition_Access;
      Bean      : Gen.Model.Beans.Bean_Definition_Access;
      Col       : Gen.Model.Tables.Column_Definition_Access;
      Assoc     : Gen.Model.Tables.Association_Definition_Access;
      Operation : Gen.Model.Operations.Operation_Definition_Access;
   end record;

   type Node_Info_Access is access all Node_Info;

   package Node_Stack is new Util.Stacks (Element_Type        => Node_Info,
                                          Element_Type_Access => Node_Info_Access);
   --  Read the UML/XMI model file.
   procedure Read_Model (Handler       : in out Artifact;
                         File          : in String;
                         Model         : in out Gen.Model.Packages.Model_Definition;
                         Context       : in out Generator'Class) is
      pragma Unreferenced (Handler);

      procedure Process_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                                 Stack : in out Node_Stack.Stack;
                                 File  : in String;
                                 Loc   : in Mark);

      procedure Read_Scalar (Node    : in Node_Info_Access;
                             Name    : in String;
                             Value   : in String;
                             Loc     : in Mark);

      procedure Read_Scalar (Node    : in Node_Info_Access;
                             Name    : in String;
                             Value   : in String;
                             Loc     : in Mark) is

         use type Gen.Model.Enums.Enum_Definition_Access;

         function Location return String is
           (File & ":" & Util.Strings.Image (Loc.Line) & ":"
            & Util.Strings.Image (Loc.Column));

         New_Value : Gen.Model.Enums.Value_Definition_Access;
      begin
         case Node.State is
         when IN_TYPE =>
            if Name = "type" then
               if Value = "enum" then
                  Node.Enum := Gen.Model.Enums.Create_Enum (Node.Tag);
                  Node.Enum.Set_Location (Location);
                  Node.State := IN_ENUM;
                  Model.Register_Enum (Node.Enum);
               elsif Value = "bean" or else Value = "limited_bean" then
                  Node.Bean := Gen.Model.Beans.Create_Bean (Node.Tag);
                  Node.Bean.Set_Location (Location);
                  Node.Bean.Is_Limited := Value = "limited_bean";
                  Node.Bean.Target := Node.Tag;
                  Node.State := IN_BEAN;
                  Model.Register_Bean (Node.Bean);
               else
                  Node.Table := Gen.Model.Tables.Create_Table (Node.Tag);
                  Node.Table.Set_Location (Location);
                  Node.State := IN_TABLE;
                  Model.Register_Table (Node.Table);
               end if;
            end if;

         when IN_TABLE =>
            if Node.Table = null then
               return;
            end if;

            Log.Debug ("Set table {0} attribute {1}={2}", Node.Table.Name, Name, Value);
            if Name = "table" then
               Node.Table.Table_Name := To_UString (Value);
            elsif Name = "description" or else Name = "comment" then
               Node.Table.Set_Comment (Value);
            elsif Name = "hasList" then
               Node.Table.Has_List := Value in "true" | "yes";
            elsif Name = "serializable" then
               Node.Table.Is_Serializable := Value in "true" | "yes";
            end if;

         when IN_BEAN =>
            if Node.Bean = null then
               return;
            end if;

            Log.Debug ("Set bean {0} attribute {1}={2}", Node.Bean.Name, Name, Value);
            if Name = "table" then
               Node.Bean.Table_Name := To_UString (Value);
            elsif Name = "description" or else Name = "comment" then
               Node.Bean.Set_Comment (Value);
            elsif Name = "hasList" then
               Node.Bean.Has_List := Value in "true" | "yes";
            elsif Name = "serializable" then
               Node.Bean.Is_Serializable := Value in "true" | "yes";
            elsif Name = "parent" then
               Node.Bean.Parent_Name := To_UString (Value);
            end if;

         when IN_COLUMN | IN_KEY | IN_ASSOCIATION =>
            if Node.Col = null then
               return;
            end if;

            Log.Debug ("Set table column {0} attribute {1}={2}", Node.Col.Name, Name, Value);
            if Name = "type" then
               Node.Col.Set_Type (Value);
            elsif Name = "length" then
               Node.Col.Set_Sql_Length (Value, Context);
            elsif Name = "column" then
               Node.Col.Sql_Name := To_UString (Value);
            elsif Name = "unique" then
               Node.Col.Unique := Value in "true" | "yes";
            elsif Name = "nullable" or else Name = "optional" then
               Node.Col.Not_Null := Value in "false" | "no";
            elsif Name = "not-null" or else Name = "required" then
               Node.Col.Not_Null := Value in "true" | "yes";
            elsif Name = "description" or else Name = "comment" then
               Node.Col.Set_Comment (Value);
            elsif Name = "version" then
               Node.Col.Is_Version := Value in "true" | "yes";
            elsif Name = "readonly" then
               Node.Col.Is_Updated := Value in "false" | "no";
            elsif Name = "auditable" then
               Node.Col.Is_Auditable := Value in "true" | "yes";
            elsif Name = "useForeignKey" and then Node.Assoc /= null then
               Node.Assoc.Use_Foreign_Key_Type := Value in "true" | "yes";
            end if;

         when IN_OPERATION =>
            if Node.Name = "type" then
               if Value = "action" then
                  Node.Operation.Set_Type (Operations.ASF_ACTION);
               elsif Value = "event" then
                  Node.Operation.Set_Type (Operations.AWA_EVENT);
               elsif Value = "upload" then
                  Node.Operation.Set_Type (Operations.ASF_UPLOAD);
               else
                  Log.Error ("Operation type {0} not recognized", Value);
               end if;
            end if;

         when IN_GENERATOR =>
            if Node.Col = null then
               return;
            end if;
            if Name = "strategy" then
               Node.Col.Generator := Util.Beans.Objects.To_Object (Value);
            end if;

         when IN_ENUM_VALUES =>
            if Node.Enum = null then
               return;
            end if;
            Node.Enum.Add_Value (Name, New_Value);
            New_Value.Set_Location (Location);
            New_Value.Number := Natural'Value (Value);

         when others =>
            Log.Error ("Scalar {0}: {1} not handled", Name, Value);
         end case;
      end Read_Scalar;

      procedure Process_Mapping (Model    : in out Gen.Model.Packages.Model_Definition;
                                 Stack    : in out Node_Stack.Stack;
                                 File     : in String;
                                 Loc      : in Mark) is
         pragma Unreferenced (Model);

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
               New_Node.Tag := Node.Name;
               New_Node.State := IN_TYPE;

            when IN_TABLE =>
               if Node.Name = "fields" or else Node.Name = "properties" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_COLUMNS;

               elsif Node.Name = "id" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_KEYS;

               elsif Node.Name = "oneToMany" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_ONE_TO_MANY;

               else
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_TABLE;
               end if;

            when IN_BEAN =>
               if Node.Name = "fields" or else Node.Name = "properties" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Bean := Node.Bean;
                  New_Node.State := IN_COLUMNS;
               elsif Node.Name = "operations" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Bean := Node.Bean;
                  New_Node.State := IN_OPERATIONS;
               end if;

            when IN_COLUMNS =>
               if Node.Bean /= null then
                  Node.Bean.Add_Column (Node.Name, Node.Col);
               else
                  Node.Table.Add_Column (Node.Name, Node.Col);
               end if;
               Node.Col.Set_Location (Location);
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_COLUMN;

            when IN_ONE_TO_MANY =>
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_ASSOCIATION;
               Node.Table.Add_Association (Node.Name, New_Node.Assoc);
               New_Node.Assoc.Set_Location (Location);
               New_Node.Col := New_Node.Assoc.all'Access;

            when IN_KEYS =>
               Node.Table.Add_Column (Node.Name, Node.Col);
               Node.Col.Set_Location (Location);
               Node.Col.Is_Key := True;
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_KEY;

            when IN_KEY =>
               if Node.Name = "generator" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.Col := Node.Col;
                  New_Node.State := IN_GENERATOR;
               end if;

            when IN_OPERATIONS =>
               if Node.Bean /= null then
                  Node.Bean.Add_Operation (Node.Name, Node.Operation);
               elsif Node.Table /= null then
                  Node.Table.Add_Operation (Node.Name, Node.Operation);
               else
                  return;
               end if;

               Node.Operation.Set_Location (Location);
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Operation := Node.Operation;
               New_Node.Bean := Node.Bean;
               New_Node.Table := Node.Table;
               New_Node.State := IN_OPERATION;

            when IN_ENUM =>
               if Node.Name = "values" then
                  Node_Stack.Push (Stack);
                  New_Node := Node_Stack.Current (Stack);
                  New_Node.Table := Node.Table;
                  New_Node.State := IN_ENUM_VALUES;
               end if;

            when others =>
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.State := IN_UNKNOWN;

            end case;
         else
            Node_Stack.Push (Stack);

         end if;
      end Process_Mapping;

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
                  Read_Scalar (Node, To_String (Node.Name), To_String (Cur.Content),
                               P.Current_Lexer_Token_Start);
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
      pragma Unreferenced (Handler, Context);

      procedure Write_Description (Comment : in Util.Beans.Objects.Object;
                                   Indent  : in Ada.Text_IO.Count);
      procedure Write_Field (Item  : in Gen.Model.Definition'Class;
                             Name  : in String);
      procedure Write_Column (Col : in Gen.Model.Tables.Column_Definition'Class;
                              Is_Bean : in Boolean := False);
      procedure Write_Association (Col : in Gen.Model.Tables.Association_Definition'Class);
      procedure Write_Operation (Op : in Gen.Model.Operations.Operation_Definition'Class);

      procedure Process_Table (Table : in out Gen.Model.Tables.Table_Definition);
      procedure Process_Bean (Bean : in out Gen.Model.Beans.Bean_Definition);
      procedure Process_Enum (Enum : in out Gen.Model.Enums.Enum_Definition);

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
           or else Util.Strings.Index (Content, ASCII.CR) > 0
         then
            Start := Content'First;
            Pos   := Content'First;
            Ada.Text_IO.Put_Line (File, "|");
            while Pos <= Content'Last loop
               if Content (Pos) in ASCII.CR | ASCII.LF then
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

      procedure Write_Column (Col : in Gen.Model.Tables.Column_Definition'Class;
                              Is_Bean : in Boolean := False) is
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
         if not Is_Bean then
            Ada.Text_IO.Put (File, "      column: ");
            Write_Field (Col, "sqlName");
         end if;
         if Col_Type /= null and then Col_Type.Nullable then
            Ada.Text_IO.Put_Line (File, "      nullable: true");
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
         if not Is_Bean then
            Ada.Text_IO.Put (File, "      unique: ");
            Ada.Text_IO.Put_Line (File, (if Col.Unique then "true" else "false"));
         end if;
         Write_Description (Col.Get_Comment, 7);
      end Write_Column;

      procedure Write_Association (Col : in Gen.Model.Tables.Association_Definition'Class) is
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
            Ada.Text_IO.Put_Line (File, "      nullable: true");
         end if;
         Ada.Text_IO.Put (File, "      not-null: ");
         Ada.Text_IO.Put_Line (File, (if Col.Not_Null then "true" else "false"));
         if not Col.Is_Updated then
            Ada.Text_IO.Put_Line (File, "      readonly: true");
         end if;
         if Col.Is_Auditable then
            Ada.Text_IO.Put_Line (File, "      auditable: true");
         end if;
         if Col.Use_Foreign_Key_Type then
            Ada.Text_IO.Put_Line (File, "      useForeignKey: true");
         end if;
         Ada.Text_IO.Put (File, "      unique: ");
         Ada.Text_IO.Put_Line (File, (if Col.Unique then "true" else "false"));
         Write_Description (Col.Get_Comment, 7);
      end Write_Association;

      procedure Write_Operation (Op : in Gen.Model.Operations.Operation_Definition'Class) is
      begin
         Ada.Text_IO.Put (File, "    ");
         Ada.Text_IO.Put (File, Op.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         Ada.Text_IO.Put (File, "      type: ");
         case Op.Get_Type is
            when ASF_ACTION =>
               Ada.Text_IO.Put_Line (File, "action");
            when ASF_UPLOAD =>
               Ada.Text_IO.Put_Line (File, "upload");
            when AWA_EVENT =>
               Ada.Text_IO.Put_Line (File, "event");
            when others =>
               Ada.Text_IO.Put_Line (File, "unknown");
         end case;
      end Write_Operation;

      procedure Process_Table (Table : in out Gen.Model.Tables.Table_Definition) is
      begin
         Ada.Text_IO.Put (File, Table.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         Ada.Text_IO.Put_Line (File, "  type: entity");
         Ada.Text_IO.Put (File, "  table: ");
         Write_Field (Table, "sqlName");
         Write_Description (Table.Get_Comment, 3);
         Ada.Text_IO.Put (File, "  hasList: ");
         Ada.Text_IO.Put_Line (File, (if Table.Has_List then "true" else "false"));
         Ada.Text_IO.Put (File, "  serializable: ");
         Ada.Text_IO.Put_Line (File, (if Table.Is_Serializable then "true" else "false"));
         Ada.Text_IO.Put (File, "  indexes: ");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put (File, "  id: ");
         Ada.Text_IO.New_Line (File);

         for Col of Table.Members loop
            if Col.Is_Key then
               Write_Column (Col.all);
               if not UBO.Is_Null (Col.Generator) then
                  Ada.Text_IO.Put_Line (File, "      generator:");
                  Ada.Text_IO.Put (File, "        strategy: ");
                  if UBO.Is_Empty (Col.Generator) then
                     Ada.Text_IO.Put_Line (File, "AUTO");
                  else
                     Ada.Text_IO.Put_Line (File, UBO.To_String (Col.Generator));
                  end if;
               end if;
            end if;
         end loop;

         Ada.Text_IO.Put (File, "  fields: ");
         Ada.Text_IO.New_Line (File);

         for Col of Table.Members loop
            if not Col.Is_Key and then
              not (Col.all in Gen.Model.Tables.Association_Definition'Class)
            then
               Write_Column (Col.all);
            end if;
         end loop;

         if Table.Has_Associations then
            Ada.Text_IO.Put_Line (File, "  oneToMany:");
            for Col of Table.Members loop
               if Col.all in Gen.Model.Tables.Association_Definition'Class then
                  Write_Association (Gen.Model.Tables.Association_Definition'Class (Col.all));
               end if;
            end loop;
         end if;
      end Process_Table;

      procedure Process_Bean (Bean : in out Gen.Model.Beans.Bean_Definition) is
      begin
         Ada.Text_IO.Put (File, Bean.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         if Bean.Is_Limited then
            Ada.Text_IO.Put_Line (File, "  type: limited_bean");
         else
            Ada.Text_IO.Put_Line (File, "  type: bean");
         end if;
         Write_Description (Bean.Get_Comment, 3);
         Ada.Text_IO.Put (File, "  hasList: ");
         Ada.Text_IO.Put_Line (File, (if Bean.Has_List then "true" else "false"));
         Ada.Text_IO.Put (File, "  serializable: ");
         Ada.Text_IO.Put_Line (File, (if Bean.Is_Serializable then "true" else "false"));

         Ada.Text_IO.Put (File, "  fields: ");
         Ada.Text_IO.New_Line (File);

         for Col of Bean.Members loop
            if not Col.Is_Key and then
              not (Col.all in Gen.Model.Tables.Association_Definition'Class)
            then
               Write_Column (Col.all, Is_Bean => True);
            end if;
         end loop;

         if Bean.Has_Associations then
            Ada.Text_IO.Put_Line (File, "  oneToMany:");
            for Col of Bean.Members loop
               if Col.all in Gen.Model.Tables.Association_Definition'Class then
                  Write_Association (Gen.Model.Tables.Association_Definition'Class (Col.all));
               end if;
            end loop;
         end if;
         if Bean.Operations.Get_Count > 0 then
            Ada.Text_IO.Put_Line (File, "  operations:");
            for Op of Bean.Operations loop
               Write_Operation (Op.all);
            end loop;
         end if;
      end Process_Bean;

      procedure Process_Enum (Enum : in out Gen.Model.Enums.Enum_Definition) is
      begin
         Ada.Text_IO.Put (File, Enum.Get_Name);
         Ada.Text_IO.Put_Line (File, ":");
         Ada.Text_IO.Put_Line (File, "  type: enum");
         Ada.Text_IO.Put (File, "  values: ");
         Ada.Text_IO.New_Line (File);

         for Value of Enum.Values loop
            Ada.Text_IO.Put (File, "    ");
            Ada.Text_IO.Put (File, Value.Get_Name);
            Ada.Text_IO.Put (File, ":");
            Ada.Text_IO.Put_Line (File, Natural'Image (Value.Number));
         end loop;
      end Process_Enum;

   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Model.Iterate_Enums (Process_Enum'Access);
      Model.Iterate_Tables (Process_Table'Access);
      Model.Iterate_Beans (Process_Bean'Access);
      Ada.Text_IO.Close (File);
   end Save_Model;

end Gen.Artifacts.Yaml;
