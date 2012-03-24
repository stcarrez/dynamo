-----------------------------------------------------------------------
--  gen-artifacts-distribs-exec -- External command based distribution artifact
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Processes;
with Util.Beans.Objects;
with Util.Log.Loggers;

with EL.Variables.Default;
with EL.Contexts.Default;

with Gen.Utils;

--  The <b>Gen.Artifacts.Distribs.Exec</b> package provides distribution rules
--  to copy a file or a directory by using an external program.
package body Gen.Artifacts.Distribs.Exec is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Distribs.Exec");

   --  Create a distribution rule to copy a set of files or directories and
   --  execute an external command.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      Ctx     : EL.Contexts.Default.Default_Context;

      Result  : constant Exec_Rule_Access := new Exec_Rule;
      Command : constant String := Gen.Utils.Get_Data_Content (Node, "command");
   begin
      Result.Command := EL.Expressions.Create_Expression (Command, Ctx);
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------

   overriding
   procedure Install (Rule    : in Exec_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is

      Ctx       : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Source    : constant String := Get_First_Path (Files);
      Dir       : constant String := Ada.Directories.Containing_Directory (Path);
   begin
      Log.Info ("install {0} to {1}", Source, Path);

      Variables.Bind ("src", Util.Beans.Objects.To_Object (Source));
      Variables.Bind ("dst", Util.Beans.Objects.To_Object (Path));

      Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
      declare
         Cmd     : constant Util.Beans.Objects.Object := Rule.Command.Get_Value (Ctx);
         Command : constant String := Util.Beans.Objects.To_String (Cmd);
         Proc    : Util.Processes.Process;
      begin
         Ada.Directories.Create_Path (Dir);
         Util.Processes.Spawn (Proc, Command);
         Util.Processes.Wait (Proc);
         if Util.Processes.Get_Exit_Status (Proc) /= 0 then
            Context.Error ("Command {0} exited with status {1}", Command,
                           Integer'Image (Util.Processes.Get_Exit_Status (Proc)));
         end if;
      end;
   end Install;

end Gen.Artifacts.Distribs.Exec;
