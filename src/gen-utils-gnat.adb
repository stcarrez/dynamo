-----------------------------------------------------------------------
--  gen-utils-gnat -- GNAT utilities
--  Copyright (C) 2011, 2012, 2014, 2015 Stephane Carrez
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

with Gen.Configs;

with Csets;
with Snames;
with Namet;
with Prj.Pars;
with Prj.Tree;
with Prj.Env;
with Prj.Util;
with Makeutl;
with Output;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Gen.Utils.GNAT is
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Utils.GNAT");

   Project_Node_Tree : Prj.Tree.Project_Node_Tree_Ref;
   Main_Project      : Prj.Project_Id;

   Project_Tree : constant Prj.Project_Tree_Ref :=
                    new Prj.Project_Tree_Data (Is_Root_Tree => True);

   --  The project tree
   --  ------------------------------
   --  Initialize the GNAT project runtime for reading the GNAT project tree.
   --  Configure it according to the dynamo configuration properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
      Project_Dirs : constant String := Config.Get (Gen.Configs.GEN_GNAT_PROJECT_DIRS,
                                                    DEFAULT_GNAT_PROJECT_DIR);
   begin
      Log.Info ("Initializing GNAT runtime to read projects from: {0}", Project_Dirs);

      Project_Node_Tree := new Prj.Tree.Project_Node_Tree_Data;
      Prj.Tree.Initialize (Project_Node_Tree);
      Output.Set_Standard_Error;
      Csets.Initialize;
      Snames.Initialize;
      Prj.Initialize (Project_Tree);

      Prj.Env.Add_Directories (Makeutl.Root_Environment.Project_Path, Project_Dirs);
   end Initialize;

   --  ------------------------------
   --  Read the GNAT project file identified by <b>Project_File_Name</b> and get
   --  in <b>Project_List</b> an ordered list of absolute project paths used by
   --  the root project.
   --  ------------------------------
   procedure Read_GNAT_Project_List (Project_File_Name : in String;
                                     Project_List      : out Project_Info_Vectors.Vector) is

      procedure Recursive_Add (Proj  : in Prj.Project_Id;
                               Tree  : in Prj.Project_Tree_Ref;
                               Dummy : in out Boolean);

      function Get_Variable_Value (Proj : in Prj.Project_Id;
                                   Name : in String) return String;
      function Get_Project_Name (Proj  : in Prj.Project_Id) return String;


      --  Get the variable value represented by the name <b>Name</b>.
      --  ??? There are probably other efficient ways to get this but I couldn't find them.
      function Get_Variable_Value (Proj : in Prj.Project_Id;
                                   Name : in String) return String is
         use type Prj.Variable_Id;

         Current      : Prj.Variable_Id;
         The_Variable : Prj.Variable;
      begin
         Current := Proj.Decl.Variables;
         while Current /= Prj.No_Variable loop
            The_Variable := Project_Tree.Shared.Variable_Elements.Table (Current);

            if Namet.Get_Name_String (The_Variable.Name) = Name then
               return Prj.Util.Value_Of (The_Variable.Value, "");
            end if;

            Current := The_Variable.Next;
         end loop;
         return "";
      end Get_Variable_Value;

      --  ------------------------------
      --  Get the project name from the GNAT project name or from the "name" project variable.
      --  ------------------------------
      function Get_Project_Name (Proj  : in Prj.Project_Id) return String is
         Name    : constant String := Get_Variable_Value (Proj, "name");
      begin
         if Name'Length > 0 then
            return Name;
         else
            return Namet.Get_Name_String (Proj.Name);
         end if;
      end Get_Project_Name;

      --  ------------------------------
      --  Add the full path of the GNAT project in the project list.
      --  ------------------------------
      procedure Recursive_Add (Proj  : in Prj.Project_Id;
                               Tree  : in Prj.Project_Tree_Ref;
                               Dummy : in out Boolean) is
         pragma Unreferenced (Tree, Dummy);
         use type Prj.Project_Qualifier;

         Path    : constant String := Namet.Get_Name_String (Proj.Path.Name);
         Name    : constant String := Get_Project_Name (Proj);
         Project : Project_Info;
      begin
         Log.Info ("Using GNAT project: {0} - {1}", Path, Name);

         Project.Path := To_Unbounded_String (Path);
         Project.Name := To_Unbounded_String (Name);
         Project.Is_Abstract := Proj.Qualifier = Prj.Abstract_Project;
         Project_List.Append (Project);
      end Recursive_Add;

      procedure For_All_Projects is
        new Prj.For_Every_Project_Imported (Boolean, Recursive_Add);

      use type Prj.Project_Id;

   begin
      Log.Info ("Reading GNAT project {0}", Project_File_Name);

      --  Parse the GNAT project files and build the tree.
      Prj.Pars.Parse (Project           => Main_Project,
                      In_Tree           => Project_Tree,
                      Project_File_Name => Project_File_Name,
                      Packages_To_Check => null,
                      Env               => Makeutl.Root_Environment,
                      In_Node_Tree      => Project_Node_Tree);

      if Main_Project /= Prj.No_Project then
         declare
            Dummy : Boolean := False;
         begin
            --  Scan the tree to get the list of projects (in dependency order).
            For_All_Projects (Main_Project, Project_Tree,
                              Imported_First => True, With_State => Dummy);
         end;
      end if;
   end Read_GNAT_Project_List;

end Gen.Utils.GNAT;
