-----------------------------------------------------------------------
--  gen-artifacts-distribs -- Artifact for distributions
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
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with DOM.Core;
with Gen.Model.Packages;

with Util.Strings;
with Util.Strings.Vectors;

--  The <b>Gen.Artifacts.Distribs</b> package is an artifact for the generation of
--  application distributions.
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
                      Context : in out Generator'Class);

private

   --  A <b>File_Info</b> record holds the information to build a distribution file.
   --
   type File_Info is record
      S : Util.Strings.Vectors.Vector;
   end record;

   --  Get the first source path from the list.
   function Get_First_Path (From : in File_Info) return String;

   function Get_Target_Path (From : in File_Info) return String;

   --  The file tree represents the target distribution tree that must be built.
   --  Each key represent a target file and it is associated with a <b>File_Info</b> which
   --  represents the list of source files that must be used to build the target.
   package File_Tree is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => File_Info,
                                                 "<"          => "<",
                                                 "="          => "=");

   type Directory_List;
   type Directory_List_Access is access all Directory_List;

   package Directory_List_Vector is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Directory_List_Access);

   type Directory_List (Length : Positive) is record
      Name        : String (1 .. Length);
      Files       : Util.Strings.Vectors.Vector;
      Directories : Directory_List_Vector.Vector;
   end record;

   function Is_Ignored (Name : in String) return Boolean;

   --  Scan the directory whose root path is <b>Path</b> and with the relative path
   --  <b>Rel_Path</b> and build in <b>Dir</b> the list of files and directories.
   procedure Scan (Path     : in String;
                   Rel_Path : in String;
                   Dir      : in out Directory_List);

   --  ------------------------------
   --  Distribution rule
   --  ------------------------------
   --  The <b>Distrib_Rule</b> represents a distribution rule that must be executed on
   --  a given file or set of files.
   type Distrib_Rule is abstract tagged record
      Dir      : Ada.Strings.Unbounded.Unbounded_String;
      Includes : Util.Strings.Vectors.Vector;
      Files    : File_Tree.Map;
   end record;
   type Distrib_Rule_Access is access all Distrib_Rule'Class;

   --  Install the file <b>File</b> according to the distribution rule.
   procedure Install (Rule : in Distrib_Rule;
                      File : in File_Info) is abstract;

   procedure Scan (Rule : in Distrib_Rule);
      --  foreach Search_Path
      --
   procedure Scan (Rule     : in out Distrib_Rule;
                   Dir      : in Directory_List;
                   Base_Dir : in String;
                   Pattern  : in String);

   --  Add the file to be processed by the distribution rule.  The file has a relative
   --  path represented by <b>Path</b>.  The path is relative from the base directory
   --  specified in <b>Base_Dir</b>.
   procedure Add_Source_File (Rule     : in out Distrib_Rule;
                              Base_Dir : in String;
                              Path     : in String);

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
      Tree  : Directory_List_Access;
   end record;

end Gen.Artifacts.Distribs;
