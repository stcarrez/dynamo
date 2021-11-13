-----------------------------------------------------------------------
--  gen-artifacts-distribs -- Artifact for distributions
--  Copyright (C) 2012, 2017, 2021 Stephane Carrez
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
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Regpat;
with DOM.Core;
with Gen.Model.Packages;
with Util.Log;

--  The <b>Gen.Artifacts.Distribs</b> package is an artifact for the generation of
--  application distributions.
--
--  1/ The package.xml file contains a set of rules which describe how to build the distribution.
--     This file is read and distribution rules are collected in the <b>Rules</b> artifact.
--
--  2/ The list of source paths to scan is obtained by looking at the GNAT project files
--     and looking at components which have a <b>dynamo.xml</b> configuration file.
--
--  3/ The source paths are then scanned and a complete tree of source files is created
--     in the <b>Trees</b> artifact member.
--
--  4/ The source paths are matched against the distribution rules and each distribution rule
--     is filled with the source files that they match.
--
--  5/ The distribution rules are executed in the order defined in the <b>package.xml</b> file.
--     Each distribution rule can have its own way to make the distribution for the set of
--     files that matched the rule definition.  A distribution rule can copy the file, another
--     can concatenate the source files, another can do some transformation on the source files
--     and prepare it for the distribution.
--
package Gen.Artifacts.Distribs is

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Artifact is new Gen.Artifacts.Artifact with private;

   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   overriding
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class);

   --  Prepare the model after all the configuration files have been read and before
   --  actually invoking the generation.
   overriding
   procedure Prepare (Handler : in out Artifact;
                      Model   : in out Gen.Model.Packages.Model_Definition'Class;
                      Project : in out Gen.Model.Projects.Project_Definition'Class;
                      Context : in out Generator'Class);

private

   type Directory_List;
   type Directory_List_Access is access all Directory_List;

   --  A <b>File_Record</b> refers to a source file that must be processed by a distribution
   --  rule.  It is linked to the directory which contains it through the <b>Dir</b> member.
   --  The <b>Name</b> refers to the file name part.
   type File_Record (Length : Natural) is record
      Dir  : Directory_List_Access;
      Name : String (1 .. Length);
   end record;

   package File_Record_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => File_Record);

   subtype File_Vector is File_Record_Vectors.Vector;
   subtype File_Cursor is File_Record_Vectors.Cursor;

   --  Get the first source path from the list.
   function Get_Source_Path (From           : in File_Vector;
                             Use_First_File : in Boolean := False) return String;

   --  The file tree represents the target distribution tree that must be built.
   --  Each key represent a target file and it is associated with a <b>File_Vector</b> which
   --  represents the list of source files that must be used to build the target.
   package File_Tree is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => File_Vector,
                                                 "<"          => "<",
                                                 "="          => File_Record_Vectors."=");

   package Directory_List_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Directory_List_Access);

   --  The <b>Directory_List<b> describes the content of a source directory.
   type Directory_List (Length      : Positive;
                        Path_Length : Natural)
   is record
      Files       : File_Record_Vectors.Vector;
      Directories : Directory_List_Vector.Vector;
      Rel_Pos     : Positive := 1;
      Name        : String (1 .. Length);
      Path        : String (1 .. Path_Length);
   end record;

   --  Get the relative path of the directory.
   function Get_Relative_Path (Dir : in Directory_List) return String;

   --  Strip the base part of the path
   function Get_Strip_Path (Base : in String;
                            Path : in String) return String;

   --  Build a regular expression pattern from a pattern string.
   function Make_Regexp (Pattern : in String) return String;

   --  Build a regular expression pattern from a pattern string.
   function Make_Regexp (Pattern : in String) return GNAT.Regpat.Pattern_Matcher;

   --  Scan the directory whose root path is <b>Path</b> and with the relative path
   --  <b>Rel_Path</b> and build in <b>Dir</b> the list of files and directories.
   procedure Scan (Path     : in String;
                   Rel_Path : in String;
                   Dir      : in Directory_List_Access);

   type Match_Rule is record
      Base_Dir : UString;
      Match    : UString;
   end record;

   package Match_Rule_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Match_Rule);

   --  ------------------------------
   --  Distribution rule
   --  ------------------------------
   --  The <b>Distrib_Rule</b> represents a distribution rule that must be executed on
   --  a given file or set of files.
   type Distrib_Rule is abstract tagged record
      Dir      : UString;
      Matches  : Match_Rule_Vector.Vector;
      Excludes : Match_Rule_Vector.Vector;
      Files    : File_Tree.Map;
      Level    : Util.Log.Level_Type := Util.Log.DEBUG_LEVEL;
   end record;
   type Distrib_Rule_Access is access all Distrib_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   function Get_Install_Name (Rule    : in Distrib_Rule) return String is abstract;

   --  Install the file <b>File</b> according to the distribution rule.
   procedure Install (Rule    : in Distrib_Rule;
                      Target  : in String;
                      File    : in File_Vector;
                      Context : in out Generator'Class) is abstract;

   --  Scan the directory tree whose root is defined by <b>Dir</b> and find the files
   --  that match the current rule.
   procedure Scan (Rule : in out Distrib_Rule;
                   Dir  : in Directory_List);

   procedure Scan (Rule     : in out Distrib_Rule;
                   Dir      : in Directory_List;
                   Base_Dir : in String;
                   Pattern  : in String;
                   Exclude  : in Boolean);

   procedure Execute (Rule    : in out Distrib_Rule;
                      Path    : in String;
                      Context : in out Generator'Class);

   --  Get the target path associate with the given source file for the distribution rule.
   function Get_Target_Path (Rule : in Distrib_Rule;
                             Base : in String;
                             File : in File_Record) return String;

   --  Get the source path of the file.
   function Get_Source_Path (Rule : in Distrib_Rule;
                             File : in File_Record) return String;

   --  Add the file to be processed by the distribution rule.  The file has a relative
   --  path represented by <b>Path</b>.  The path is relative from the base directory
   --  specified in <b>Base_Dir</b>.
   procedure Add_Source_File (Rule     : in out Distrib_Rule;
                              Path     : in String;
                              File     : in File_Record);

   --  Remove the file to be processed by the distribution rule.  This is the opposite of
   --  <tt>Add_Source_File</tt> and used for the <exclude name="xxx"/> rules.
   procedure Remove_Source_File (Rule     : in out Distrib_Rule;
                                 Path     : in String;
                                 File     : in File_Record);

   --  Create a distribution rule identified by <b>Kind</b>.
   --  The distribution rule is configured according to the DOM tree whose node is <b>Node</b>.
   function Create_Rule (Kind : in String;
                         Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  A list of rules that define how to build the distribution.
   package Distrib_Rule_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Distrib_Rule_Access);

   type Artifact is new Gen.Artifacts.Artifact with record
      Rules : Distrib_Rule_Vectors.Vector;
      Trees : Directory_List_Vector.Vector;
   end record;

end Gen.Artifacts.Distribs;
