-----------------------------------------------------------------------
--  gen-model-projects -- Projects meta data
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2018, 2021, 2022 Stephane Carrez
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

with Ada.Containers.Vectors;

with Util.Properties;

with Gen.Utils;
with Gen.Utils.GNAT;
package Gen.Model.Projects is

   type Project_Definition is tagged;
   type Project_Definition_Access is access all Project_Definition'Class;

   type Dependency_Type is (NONE, DIRECT, INDIRECT, BOTH);

   type Project_Reference is record
      Project : Project_Definition_Access := null;
      Name    : UString;
      Kind    : Dependency_Type := NONE;
   end record;

   package Project_Vectors is
     new Ada.Containers.Vectors (Element_Type => Project_Reference,
                                 Index_Type   => Natural);

   --  ------------------------------
   --  Project Definition
   --  ------------------------------
   type Project_Definition is new Definition with record
      Path    : UString;
      Props   : Util.Properties.Manager;
      Modules : Project_Vectors.Vector;

      --  The root project definition.
      Root          : Project_Definition_Access := null;

      --  The list of plugin names that this plugin or project depends on.
      Dependencies  : Project_Vectors.Vector;

      --  The list of GNAT project files used by the project.
      Project_Files : Gen.Utils.GNAT.Project_Info_Vectors.Vector;

      --  The list of 'dynamo.xml' files used by the project (gathered from GNAT files
      --  and by scanning the 'plugins' directory).
      Dynamo_Files  : Gen.Utils.String_List.Vector;

      --  Whether we did a recursive scan of GNAT project files.
      Recursive_Scan : Boolean := False;

      --  Whether we are doing a recursive operation on the project (prevent from cycles).
      Recursing      : Boolean := False;

      --  Whether we did a recursive scan of Dynamo dependencies.
      Depend_Scan    : Boolean := False;

      --  Whether this project is a plugin.
      Is_Plugin      : Boolean := False;

      --  Whether the project needs the generation for the different databases
      Use_Mysql      : Boolean := True;
      Use_Sqlite     : Boolean := True;
      Use_Postgresql : Boolean := True;

      --  The database model version for this project.
      Model_Version  : Positive := 1;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : Project_Definition;
                       Name : String) return UBO.Object;

   --  Get the project name.
   function Get_Project_Name (Project : in Project_Definition) return String;

   --  Get the GNAT project file name.  The default is to use the Dynamo project
   --  name and add the <b>.gpr</b> extension.  The <b>gnat_project</b> configuration
   --  property allows to override this default.
   function Get_GNAT_Project_Name (Project : in Project_Definition) return String;

   --  Get the directory path which holds application modules.
   --  This is controlled by the <b>modules_dir</b> configuration property.
   --  The default is <tt>plugins</tt>.
   function Get_Module_Dir (Project : in Project_Definition) return String;

   --  Get the directory path which holds database model files.
   --  This is controlled by the <b>db_dir</b> configuration property.
   --  The default is <tt>db</tt>.
   function Get_Database_Dir (Project : in Project_Definition) return String;

   --  Get the directory path which is the base dir for the 'web, 'config' and 'bundles'.
   --  This is controlled by the <b>base_dir</b> configuration property.
   --  The default is <tt>.</tt>.
   function Get_Base_Dir (Project : in Project_Definition) return String;

   --  Find the dependency for the <b>Name</b> plugin.
   --  Returns a null dependency if the project does not depend on that plugin.
   function Find_Dependency (From : in Project_Definition;
                             Name : in String) return Project_Reference;

   --  Add a dependency to the plugin identified by <b>Name</b>.
   procedure Add_Dependency (Into : in out Project_Definition;
                             Name : in String;
                             Kind : in Dependency_Type);

   --  Add a dependency to the plugin identified by <b>Project</b>.
   procedure Add_Dependency (Into    : in out Project_Definition;
                             Project : in Project_Definition_Access;
                             Kind    : in Dependency_Type);

   --  Create a project definition instance to record a project with the dynamo XML file path.
   procedure Create_Project (Into    : in out Project_Definition;
                             Name    : in String;
                             Path    : in String;
                             Project : out Project_Definition_Access);

   --  Add the project in the global project list on the root project instance.
   procedure Add_Project (Into    : in out Project_Definition;
                          Project : in Project_Definition_Access);

   --  Add the project <b>Name</b> as a module.
   procedure Add_Module (Into : in out Project_Definition;
                         Name : in String);

   --  Add the project represented by <b>Project</b> if it is not already part of the modules.
   procedure Add_Module (Into    : in out Project_Definition;
                         Project : in Project_Definition_Access);

   --  Find the project definition associated with the dynamo XML file <b>Path</b>.
   --  Returns null if there is no such project
   function Find_Project (From : in Project_Definition;
                          Path : in String) return Project_Definition_Access;

   --  Find the project definition having the name <b>Name</b>.
   --  Returns null if there is no such project
   function Find_Project_By_Name (From : in Project_Definition;
                                  Name : in String) return Project_Definition_Access;

   --  Save the project description and parameters.
   procedure Save (Project : in out Project_Definition;
                   Path    : in String);

   --  Read the XML project description into the project description.
   procedure Read_Project (Project : in out Project_Definition);

   --  Scan and read the possible modules used by the application.  Modules are stored in the
   --  <b>modules</b> directory.  Each module is stored in its own directory and has its own
   --  <b>dynamo.xml</b> file.
   procedure Read_Modules (Project  : in out Project_Definition);

   --  Update the project definition from the properties.
   procedure Update_From_Properties (Project : in out Project_Definition);

   --  ------------------------------
   --  Root Project Definition
   --  ------------------------------
   --  The root project is the project that is actually read by Dynamo.
   --  It contains the lists of all projects that are necessary and which are found either
   --  by scanning GNAT projects or by looking at plugin dependencies.
   type Root_Project_Definition is new Project_Definition with record
      Projects    : Project_Vectors.Vector;

      Install_Dir : UString;
   end record;

   --  Add the project in the global project list on the root project instance.
   overriding
   procedure Add_Project (Into    : in out Root_Project_Definition;
                          Project : in Project_Definition_Access);

   --  Find the project definition having the name <b>Name</b>.
   --  Returns null if there is no such project
   overriding
   function Find_Project_By_Name (From : in Root_Project_Definition;
                                  Name : in String) return Project_Definition_Access;

   --  Find the project definition associated with the dynamo XML file <b>Path</b>.
   --  Returns null if there is no such project
   overriding
   function Find_Project (From : in Root_Project_Definition;
                          Path : in String) return Project_Definition_Access;

   --  Read the XML project file.  When <b>Recursive</b> is set, read the GNAT project
   --  files used by the main project and load all the <b>dynamo.xml</b> files defined
   --  by these project.
   procedure Read_Project (Project   : in out Root_Project_Definition;
                           File      : in String;
                           Config    : in Util.Properties.Manager'Class;
                           Recursive : in Boolean := False);

private

   --  Update the project references after a project is found and initialized.
   procedure Update_References (Root    : in out Root_Project_Definition;
                                Project : in Project_Definition_Access);

   --  Iterate over the project referenced in the list and execute the <b>Process</b> procedure.
   procedure Iterate (List    : in out Project_Vectors.Vector;
                      Process : access procedure (Item : in out Project_Reference));

   --  Find a project from the list
   function Find_Project (List : in Project_Vectors.Vector;
                          Name : in String) return Project_Reference;

end Gen.Model.Projects;
