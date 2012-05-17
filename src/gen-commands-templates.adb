-----------------------------------------------------------------------
--  gen-commands-templates -- Template based command
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Directories;

with GNAT.Command_Line;

with Gen.Artifacts;

with Util.Log.Loggers;
with Util.Files;
with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;
package body Gen.Commands.Templates is

   use Ada.Strings.Unbounded;
   use Util.Log;
   use GNAT.Command_Line;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Commands.Templates");

   --  ------------------------------
   --  Page Creation Command
   --  ------------------------------
   --  This command adds a XHTML page to the web application.

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is
      function Get_Output_Dir return String;

      function Get_Output_Dir return String is
         Dir        : constant String := Generator.Get_Result_Directory;
      begin
         if Length (Cmd.Base_Dir) = 0 then
            return Dir;
         else
            return Util.Files.Compose (Dir, To_String (Cmd.Base_Dir));
         end if;
      end Get_Output_Dir;

      Out_Dir : constant String := Get_Output_Dir;
      Iter    : Param_Vectors.Cursor := Cmd.Params.First;
   begin
      Generator.Read_Project ("dynamo.xml", False);
      while Param_Vectors.Has_Element (Iter) loop
         declare
            P     : constant Param := Param_Vectors.Element (Iter);
            Value : constant String := Get_Argument;
         begin
            if not P.Is_Optional and Value'Length = 0 then
               Generator.Error ("Missing argument for {0}", To_String (P.Argument));
               return;
            end if;
            Generator.Set_Global (To_String (P.Name), Value);
         end;
         Param_Vectors.Next (Iter);
      end loop;

      Generator.Set_Force_Save (False);
      Generator.Set_Result_Directory (Out_Dir);
      declare
         Iter : Util.Strings.Sets.Cursor := Cmd.Templates.First;
      begin
         while Util.Strings.Sets.Has_Element (Iter) loop
            Gen.Generator.Generate (Generator, Gen.Artifacts.ITERATION_TABLE,
                                    Util.Strings.Sets.Element (Iter));
            Util.Strings.Sets.Next (Iter);
         end loop;
      end;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   procedure Help (Cmd       : in Command;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Generator);

      use Ada.Text_IO;
   begin
      Put_Line (To_String (Cmd.Name) & ": " & To_String (Cmd.Title));
      Put_Line ("Usage: " & To_String (Cmd.Usage));
      New_Line;
      Put_Line (To_String (Cmd.Help_Msg));
   end Help;

   type Command_Fields is (FIELD_NAME,
                           FIELD_HELP,
                           FIELD_USAGE,
                           FIELD_TITLE,
                           FIELD_BASEDIR,
                           FIELD_PARAM,
                           FIELD_PARAM_NAME,
                           FIELD_PARAM_OPTIONAL,
                           FIELD_PARAM_ARG,
                           FIELD_TEMPLATE,
                           FIELD_COMMAND);

   type Command_Loader is record
      Command : Command_Access := null;
      P       : Param;
   end record;
   type Command_Loader_Access is access all Command_Loader;

   procedure Set_Member (Closure : in out Command_Loader;
                         Field   : in Command_Fields;
                         Value   : in Util.Beans.Objects.Object);

   function To_String (Value : in Util.Beans.Objects.Object) return Unbounded_String;

   --  ------------------------------
   --  Convert the object value into a string and trim any space/tab/newlines.
   --  ------------------------------
   function To_String (Value : in Util.Beans.Objects.Object) return Unbounded_String is
      Result    : constant String := Util.Beans.Objects.To_String (Value);
      First_Pos : Natural := Result'First;
      Last_Pos  : Natural := Result'Last;
      C : Character;
   begin
      while First_Pos <= Last_Pos loop
         C := Result (First_Pos);
         exit when C /= ' ' and C /= ASCII.LF and C /= ASCII.CR and C /= ASCII.HT;
         First_Pos := First_Pos + 1;
      end loop;
      while Last_Pos >= First_Pos loop
         C := Result (Last_Pos);
         exit when C /= ' ' and C /= ASCII.LF and C /= ASCII.CR and C /= ASCII.HT;
         Last_Pos := Last_Pos - 1;
      end loop;
      return To_Unbounded_String (Result (First_Pos .. Last_Pos));
   end To_String;

   procedure Set_Member (Closure : in out Command_Loader;
                         Field   : in Command_Fields;
                         Value   : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;
   begin
      if Field = FIELD_COMMAND then
         if Closure.Command /= null then
            Log.Info ("Adding command {0}", Closure.Command.Name);

            Add_Command (Name => To_String (Closure.Command.Name),
                         Cmd  => Closure.Command.all'Access);
            Closure.Command := null;
         end if;
      else
         if Closure.Command = null then
            Closure.Command := new Gen.Commands.Templates.Command;
         end if;
         case Field is
            when FIELD_NAME =>
               Closure.Command.Name := To_String (Value);

            when FIELD_TITLE =>
               Closure.Command.Title := To_String (Value);

            when FIELD_USAGE =>
               Closure.Command.Usage := To_String (Value);

            when FIELD_HELP =>
               Closure.Command.Help_Msg := To_String (Value);

            when FIELD_TEMPLATE =>
               Closure.Command.Templates.Include (To_String (Value));

            when FIELD_PARAM_NAME =>
               Closure.P.Name := To_Unbounded_String (Value);

            when FIELD_PARAM_OPTIONAL =>
               Closure.P.Is_Optional := To_Boolean (Value);

            when FIELD_PARAM_ARG =>
               Closure.P.Argument := To_Unbounded_String (Value);

            when FIELD_PARAM =>
               Closure.P.Value := To_Unbounded_String (Value);
               Closure.Command.Params.Append (Closure.P);
               Closure.P.Name        := Ada.Strings.Unbounded.Null_Unbounded_String;
               Closure.P.Argument    := Ada.Strings.Unbounded.Null_Unbounded_String;
               Closure.P.Is_Optional := False;

            when FIELD_BASEDIR =>
               Closure.Command.Base_Dir := To_Unbounded_String (Value);

            when FIELD_COMMAND =>
               null;

         end case;
      end if;
   end Set_Member;

   package Command_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Command_Loader,
                                               Element_Type_Access => Command_Loader_Access,
                                               Fields              => Command_Fields,
                                               Set_Member          => Set_Member);

   Cmd_Mapper : aliased Command_Mapper.Mapper;

   --  ------------------------------
   --  Read the template commands defined in dynamo configuration directory.
   --  ------------------------------
   procedure Read_Commands (Generator : in out Gen.Generator.Handler) is

      procedure Read_Command (Name      : in String;
                              File_Path : in String;
                              Done      : out Boolean);

      --  ------------------------------
      --  Read the XML command file.
      --  ------------------------------
      procedure Read_Command (Name      : in String;
                              File_Path : in String;
                              Done      : out Boolean) is
         pragma Unreferenced (Name);

         Loader : aliased Command_Loader;
         Reader : Util.Serialize.IO.XML.Parser;
      begin
         Log.Info ("Reading command file '{0}'", File_Path);
         Done := False;

         --  Create the mapping to load the XML command file.
         Reader.Add_Mapping ("commands", Cmd_Mapper'Access);

         --  Set the context for Set_Member.
         Command_Mapper.Set_Context (Reader, Loader'Unchecked_Access);

         --  Read the XML command file.
         Reader.Parse (File_Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Generator.Error ("Command file {0} does not exist", File_Path);
      end Read_Command;

      Config_Dir : constant String := Generator.Get_Config_Directory;
      Cmd_Dir    : constant String := Generator.Get_Parameter ("generator.commands.dir");
      Path       : constant String := Util.Files.Compose (Config_Dir, Cmd_Dir);
   begin
      Log.Debug ("Checking commands in {0}", Path);

      if Ada.Directories.Exists (Path) then
         Util.Files.Iterate_Files_Path (Path    => Path,
                                        Pattern => "*.xml",
                                        Process => Read_Command'Access);
      end if;
   end Read_Commands;

begin
   --  Setup the mapper to load XML commands.
   Cmd_Mapper.Add_Mapping ("command/name", FIELD_NAME);
   Cmd_Mapper.Add_Mapping ("command/title", FIELD_TITLE);
   Cmd_Mapper.Add_Mapping ("command/usage", FIELD_USAGE);
   Cmd_Mapper.Add_Mapping ("command/help", FIELD_HELP);
   Cmd_Mapper.Add_Mapping ("command/basedir", FIELD_BASEDIR);
   Cmd_Mapper.Add_Mapping ("command/param", FIELD_PARAM);
   Cmd_Mapper.Add_Mapping ("command/param/@name", FIELD_PARAM_NAME);
   Cmd_Mapper.Add_Mapping ("command/param/@optional", FIELD_PARAM_OPTIONAL);
   Cmd_Mapper.Add_Mapping ("command/param/@arg", FIELD_PARAM_ARG);
   Cmd_Mapper.Add_Mapping ("command/template", FIELD_TEMPLATE);
   Cmd_Mapper.Add_Mapping ("command", FIELD_COMMAND);
end Gen.Commands.Templates;
