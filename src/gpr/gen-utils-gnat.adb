-----------------------------------------------------------------------
--  gen-utils-gnat -- GNAT utilities
--  Copyright (C) 2011, 2012, 2014, 2015, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;

with Gen.Configs;

with GPR.Snames;
with GPR.Names;
with GPR.Tree;
with GPR.Env;
with GPR.Util;
with GPR.Conf;
with GPR.Err;
with Gpr_Build_Util;
with GPR.Output;
with GPR.Sdefault;
with GNAT.OS_Lib; use GNAT;

package body Gen.Utils.GNAT is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Utils.GNAT");

   Project_Node_Tree : GPR.Tree.Project_Node_Tree_Ref;
   Main_Project      : GPR.Project_Id;
   Project_Node      : GPR.Project_Node_Id;

   Project_Tree : constant GPR.Project_Tree_Ref :=
                    new GPR.Project_Tree_Data (Is_Root_Tree => True);

   --  ------------------------------
   --  Initialize the GNAT project runtime for reading the GNAT project tree.
   --  Configure it according to the dynamo configuration properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
      Project_Dirs : constant String := Config.Get (Gen.Configs.GEN_GNAT_PROJECT_DIRS,
                                                    DEFAULT_GNAT_PROJECT_DIR);
   begin
      Log.Info ("Initializing GNAT runtime to read projects from: {0}", Project_Dirs);

      Project_Node_Tree := new GPR.Project_Node_Tree_Data;
      GPR.Tree.Initialize (Project_Node_Tree);
      GPR.Output.Set_Standard_Error;
      GPR.Snames.Initialize;
      GPR.Initialize (Project_Tree);

      GPR.Env.Initialize_Default_Project_Path (Gpr_Build_Util.Root_Environment.Project_Path,
                                               GPR.Sdefault.Hostname);
      GPR.Env.Add_Directories (Gpr_Build_Util.Root_Environment.Project_Path, Project_Dirs);
      Log.Info ("Added directories {0}", GPR.Util.Executable_Prefix_Path);
   end Initialize;

   --  ------------------------------
   --  Read the GNAT project file identified by <b>Project_File_Name</b> and get
   --  in <b>Project_List</b> an ordered list of absolute project paths used by
   --  the root project.
   --  ------------------------------
   procedure Read_GNAT_Project_List (Project_File_Name : in String;
                                     Project_List      : out Project_Info_Vectors.Vector) is

      procedure Recursive_Add (Proj  : in GPR.Project_Id;
                               Tree  : in GPR.Project_Tree_Ref;
                               Dummy : in out Boolean);

      function Get_Variable_Value (Proj : in GPR.Project_Id;
                                   Name : in String) return String;
      function Get_Project_Name (Proj  : in GPR.Project_Id) return String;

      --  Get the variable value represented by the name <b>Name</b>.
      --  ??? There are probably other efficient ways to get this but I couldn't find them.
      function Get_Variable_Value (Proj : in GPR.Project_Id;
                                   Name : in String) return String is
         use type GPR.Variable_Id;

         Current      : GPR.Variable_Id;
         The_Variable : GPR.Variable;
      begin
         Current := Proj.Decl.Variables;
         while Current /= GPR.No_Variable loop
            The_Variable := Project_Tree.Shared.Variable_Elements.Table (Current);

            if GPR.Names.Get_Name_String (The_Variable.Name) = Name then
               return GPR.Util.Value_Of (The_Variable.Value, "");
            end if;

            Current := The_Variable.Next;
         end loop;
         return "";
      end Get_Variable_Value;

      --  ------------------------------
      --  Get the project name from the GNAT project name or from the "name" project variable.
      --  ------------------------------
      function Get_Project_Name (Proj  : in GPR.Project_Id) return String is
         Name    : constant String := Get_Variable_Value (Proj, "name");
      begin
         if Name'Length > 0 then
            return Name;
         else
            return GPR.Names.Get_Name_String (Proj.Name);
         end if;
      end Get_Project_Name;

      --  ------------------------------
      --  Add the full path of the GNAT project in the project list.
      --  ------------------------------
      procedure Recursive_Add (Proj  : in GPR.Project_Id;
                               Tree  : in GPR.Project_Tree_Ref;
                               Dummy : in out Boolean) is
         pragma Unreferenced (Tree, Dummy);
         use type GPR.Project_Qualifier;

         Path    : constant String := GPR.Names.Get_Name_String (Proj.Path.Name);
         Name    : constant String := Get_Project_Name (Proj);
         Project : Project_Info;
      begin
         Log.Info ("Using GNAT project: {0} - {1}", Path, Name);

         Project.Path := To_Unbounded_String (Path);
         Project.Name := To_Unbounded_String (Name);
         Project.Is_Abstract := Proj.Qualifier = GPR.Abstract_Project;
         Project_List.Append (Project);
      end Recursive_Add;

      procedure For_All_Projects is
        new GPR.For_Every_Project_Imported (Boolean, Recursive_Add);

      use type GPR.Project_Id;

      Dummy  : Boolean := False;
      Config : OS_Lib.String_Access;
   begin
      Log.Info ("Reading GNAT project {0}", Project_File_Name);

      --  Parse the GNAT project files and build the tree.
      Main_Project := GPR.No_Project;
      GPR.Conf.Parse_Project_And_Apply_Config
        (Main_Project => Main_Project,
         User_Project_Node => Project_Node,
         Config_File_Name => "",
         Autoconf_Specified => False,
         Project_File_Name => Project_File_Name,
         Project_Tree => Project_Tree,
         Project_Node_Tree => Project_Node_Tree,
         Env => Gpr_Build_Util.Root_Environment,
         Automatically_Generated => Dummy,
         Config_File_Path => Config,
         Packages_To_Check => null,
         Target_Name => "",
         Normalized_Hostname => "");
      if Main_Project = GPR.No_Project then
         GPR.Err.Finalize;
         return;
      end if;

      if not GPR.Tree.No (Project_Node) then
         --  Scan the tree to get the list of projects (in dependency order).
         For_All_Projects (Main_Project, Project_Tree, Include_Aggregated => True,
                           Imported_First => True, With_State => Dummy);
      end if;
   end Read_GNAT_Project_List;

end Gen.Utils.GNAT;
