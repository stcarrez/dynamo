-----------------------------------------------------------------------
--  gen-utils-gnat -- GNAT utilities
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Log.Loggers;

with Csets;
with Snames;
with Namet;
with Prj.Pars;
with Prj.Tree;
with Prj.Env;
with Makeutl;
with Output;
package body Gen.Utils.GNAT is
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Utils.GNAT");

   Project_Node_Tree : Prj.Tree.Project_Node_Tree_Ref;
   Main_Project      : Prj.Project_Id;

   --  ------------------------------
   --  Initialize the GNAT project runtime for reading the GNAT project tree.
   --  Configure it according to the dynamo configuration properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
      Project_Dirs : constant String := Config.Get ("generator.gnat.projects.dir",
                                                    DEFAULT_GNAT_PROJECT_DIR);
   begin
      Log.Info ("Initializing GNAT runtime to read projects from: {0}", Project_Dirs);

      Project_Node_Tree := new Prj.Tree.Project_Node_Tree_Data;
      Prj.Tree.Initialize (Project_Node_Tree);
      Output.Set_Standard_Error;
      Csets.Initialize;
      Snames.Initialize;
      Prj.Initialize (Makeutl.Project_Tree);

      Prj.Env.Add_Directories (Project_Node_Tree.Project_Path, Project_Dirs);
   end Initialize;

   --  ------------------------------
   --  Read the GNAT project file identified by <b>Project_File_Name</b> and get
   --  in <b>Project_List</b> an ordered list of absolute project paths used by
   --  the root project.
   --  ------------------------------
   procedure Read_GNAT_Project_List (Project_File_Name : in String;
                                     Project_List      : out String_List.Vector) is

      procedure Recursive_Add (Proj : in Prj.Project_Id;
                               Dummy : in out Boolean);

      --  ------------------------------
      --  Add the full path of the GNAT project in the project list.
      --  ------------------------------
      procedure Recursive_Add (Proj  : in Prj.Project_Id;
                               Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);

         Path : constant String := Namet.Get_Name_String (Proj.Path.Name);
      begin
         Log.Info ("Using GNAT project: {0}", Path);

         Project_List.Append (Path);
      end Recursive_Add;

      procedure For_All_Projects is
        new Prj.For_Every_Project_Imported (Boolean, Recursive_Add);

      use type Prj.Project_Id;
   begin
      Log.Info ("Reading GNAT project {0}", Project_File_Name);

      --  Parse the GNAT project files and build the tree.
      Prj.Pars.Parse (Project           => Main_Project,
                      In_Tree           => Makeutl.Project_Tree,
                      Project_File_Name => Project_File_Name,
                      Flags             => Prj.Gnatmake_Flags,
                      In_Node_Tree      => Project_Node_Tree);

      if Main_Project /= Prj.No_Project then
         declare
            Dummy : Boolean := False;
         begin
            --  Scan the tree to get the list of projects (in dependency order).
            For_All_Projects (Main_Project, Dummy, Imported_First => True);
         end;
      end if;
   end Read_GNAT_Project_List;

end Gen.Utils.GNAT;
