-----------------------------------------------------------------------
--  gen-artifacts-distribs-bundles -- Merge bundles for distribution artifact
--  Copyright (C) 2013, 2017 Stephane Carrez
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
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with Util.Log.Loggers;
with Util.Properties;

package body Gen.Artifacts.Distribs.Bundles is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Artifacts.Distribs.Bundles");

   --  ------------------------------
   --  Create a distribution rule to copy a set of files or directories.
   --  ------------------------------
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access is
      pragma Unreferenced (Node);

      Result : constant Bundle_Rule_Access := new Bundle_Rule;
   begin
      return Result.all'Access;
   end Create_Rule;

   --  ------------------------------
   --  Get a name to qualify the installation rule (used for logs).
   --  ------------------------------
   overriding
   function Get_Install_Name (Rule : in Bundle_Rule) return String is
      pragma Unreferenced (Rule);
   begin
      return "bundle";
   end Get_Install_Name;

   --  ------------------------------
   --  Install the file <b>File</b> according to the distribution rule.
   --  Merge all the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   --  ------------------------------
   overriding
   procedure Install (Rule    : in Bundle_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class) is
      procedure Load_File (File : in File_Record);
      procedure Merge_Property (Name : in String;
                                Item : in Util.Properties.Value);
      procedure Save_Property (Name : in String;
                               Item : in Util.Properties.Value);

      Dir    : constant String := Ada.Directories.Containing_Directory (Path);
      Output : Ada.Text_IO.File_Type;
      Merge  : Util.Properties.Manager;

      --  ------------------------------
      --  Merge the property into the target property list.
      --  ------------------------------
      procedure Merge_Property (Name : in String;
                                Item : in Util.Properties.Value) is
      begin
         Merge.Set_Value (Name, Item);
      end Merge_Property;

      procedure Save_Property (Name : in String;
                               Item : in Util.Properties.Value) is
      begin
         Ada.Text_IO.Put (Output, Name);
         Ada.Text_IO.Put (Output, "=");
         Ada.Text_IO.Put_Line (Output, Util.Properties.To_String (Item));
      end Save_Property;

      --  ------------------------------
      --  Append the file to the output
      --  ------------------------------
      procedure Load_File (File : in File_Record) is
         File_Path : constant String := Rule.Get_Source_Path (File);
         Props     : Util.Properties.Manager;
      begin
         Log.Info ("loading {0}", File_Path);

         Props.Load_Properties (Path => File_Path);

         Props.Iterate (Process => Merge_Property'Access);
      exception
         when Ex : Ada.IO_Exceptions.Name_Error =>
            Context.Error ("Cannot read {0}: ", File_Path, Ada.Exceptions.Exception_Message (Ex));

      end Load_File;

      Iter   : File_Cursor := Files.First;
   begin
      Ada.Directories.Create_Path (Dir);

      while File_Record_Vectors.Has_Element (Iter) loop
         File_Record_Vectors.Query_Element (Iter, Load_File'Access);
         File_Record_Vectors.Next (Iter);
      end loop;
      Ada.Text_IO.Create (File => Output, Mode => Ada.Text_IO.Out_File, Name => Path);
      Merge.Iterate (Process => Save_Property'Access);
      Ada.Text_IO.Close (File => Output);
   end Install;

end Gen.Artifacts.Distribs.Bundles;
