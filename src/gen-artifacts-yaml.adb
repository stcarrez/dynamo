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

with Text;
with Yaml.Source.File;
with Yaml.Parser;

with Gen.Configs;
with Gen.Model.Tables;
with Gen.Model.Queries;
with Gen.Model.Mappings;
with Gen.Model.Operations;

with Util.Log.Loggers;
with Util.Encoders.HMAC.SHA1;
with Util.Stacks;

use Yaml;

package body Gen.Artifacts.Yaml is

   use Ada.Strings.Unbounded;
   use Gen.Model;
   use Gen.Model.Tables;
   use Gen.Model.Queries;
   use Gen.Configs;

   use Util.Log;

   use type Text.Reference;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Yaml");

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

   procedure Read_Scalar (Node  : in Node_Info_Access;
                          Name  : in String;
                          Value : in String) is
   begin
      case Node.State is
         when IN_TABLE =>
            if Node.Table = null then
               return;
            end if;

            Log.Debug ("Set table {0} attribute {1}={2}", Node.Table.Name, Name, Value);
            if Name = "table" then
               Node.Table.Table_Name := To_Unbounded_String (Value);
            end if;

         when IN_COLUMN | IN_KEY =>
            if Node.Col = null then
               return;
            end if;

            Log.Debug ("Set table column {0} attribute {1}={2}", Node.Col.Name, Name, Value);
            if Name = "type" then
               Node.Col.Set_Type (Value);
            elsif Name = "length" then
               Node.Col.Sql_Length := Natural'Value (Value);
            elsif Name = "column" then
               Node.Col.Sql_Name := To_Unbounded_String (Value);
            elsif Name = "unique" then
               Node.Col.Unique := Value = "true" or Value = "yes";
            elsif Name = "nullable" then
               null;
            end if;

         when others =>
            Log.Error ("Scalar {0}: {1} not handled", Name, Value);
      end case;
   end Read_Scalar;

   procedure Process_Mapping (Model : in out Gen.Model.Packages.Model_Definition;
                              Stack : in out Node_Stack.Stack) is
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
               New_Node.Table := Gen.Model.Tables.Create_Table (To_Unbounded_String (Node.Name & ""));
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
               Node.Table.Add_Column (To_Unbounded_String (Node.Name & ""), Node.Col);
               Node_Stack.Push (Stack);
               New_Node := Node_Stack.Current (Stack);
               New_Node.Table := Node.Table;
               New_Node.State := IN_COLUMN;

            when IN_KEYS =>
               Node.Table.Add_Column (To_Unbounded_String (Node.Name & ""), Node.Col);
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
                         Context       : in out Generator'Class;
                         Is_Predefined : in Boolean := False) is
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
                  Read_Scalar (Node, Node.Name & "", Cur.Content & "");
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
               Process_Mapping (Model, Stack);

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
   --  Prepare the generation of the package:
   --  o identify the column types which are used
   --  o build a list of package for the with clauses.
   --  ------------------------------
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Context : in out Generator'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Debug ("Preparing the model for query");

      if Model.Has_Packages then
         Context.Add_Generation (Name => GEN_PACKAGE_SPEC, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
         Context.Add_Generation (Name => GEN_PACKAGE_BODY, Mode => ITERATION_PACKAGE,
                                 Mapping => Gen.Model.Mappings.ADA_MAPPING);
      end if;
   end Prepare;

end Gen.Artifacts.Yaml;
