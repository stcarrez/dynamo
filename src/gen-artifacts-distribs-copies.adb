-----------------------------------------------------------------------
--  gen-artifacts-distribs-copies -- Copy based distribution artifact
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

with Util.Log.Loggers;

--  The <b>Gen.Artifacts.Distribs.Copies</b> package provides distribution rules
--  to copy a file or a directory to the distribution area.
package body Gen.Artifacts.Distribs.Copies is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Distribs.Copies");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      pragma Unreferenced (Node);

      Result : constant Copy_Rule_Access := new Copy_Rule;
   begin
      return Result.all'Access;
   end Create_Rule;

   overriding
   procedure Install (Rule    : in Copy_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is
      pragma Unreferenced (Rule, Context);

      Source : constant String := Get_First_Path (Files);
      Dir    : constant String := Ada.Directories.Containing_Directory (Path);
   begin
      Log.Info ("copy {0} to {1}", Source, Path);

      Ada.Directories.Create_Path (Dir);
      Ada.Directories.Copy_File (Source_Name => Source,
                                 Target_Name => Path,
                                 Form        => "preserve=all_attributes, mode=overwrite");
   end Install;

end Gen.Artifacts.Distribs.Copies;
