-----------------------------------------------------------------------
--  gen-utils-gnat -- GNAT utilities
--  Copyright (C) 2011, 2012, 2015, 2017, 2021 Stephane Carrez
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

--  This package encapsulate a set of high level operations on top of the GNAT project
--  files.  It uses the GNAT project reader which is packaged in the GNAT compiler (4.6.0).
--  A minimal subset of GNAT compiler was copied in <b>src/gnat</b> to ensure that a
--  compatible API is defined.  The GNAT files stored in <b>src/gnat</b> are licensed
--  under the GNU General Public License.
package Gen.Utils.GNAT is

   --  Directory which contains the GNAT project files installed on the system.
   --  This is overridden by the configuration property 'generator.gnat.projects.dir'.
   DEFAULT_GNAT_PROJECT_DIR : constant String := "/usr/lib/gnat";

   ADA_PROJECT_PATH_NAME    : constant String := "ADA_PROJECT_PATH";

   type Project_Info is record
      Path        : UString;
      Name        : UString;
      Is_Abstract : Boolean := False;
   end record;

   package Project_Info_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Project_Info);

   --  Initialize the GNAT project runtime for reading the GNAT project tree.
   --  Configure it according to the dynamo configuration properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

   --  Read the GNAT project file identified by <b>Project_File_Name</b> and get
   --  in <b>Project_List</b> an ordered list of absolute project paths used by
   --  the root project.
   procedure Read_GNAT_Project_List (Project_File_Name : in String;
                                     Project_List      : out Project_Info_Vectors.Vector);

end Gen.Utils.GNAT;
