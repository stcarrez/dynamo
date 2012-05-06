-----------------------------------------------------------------------
--  gen-artifacts-distribs-libs -- Unix shared library extraction and distribution
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
with EL.Expressions;
with Util.Strings.Vectors;

--  The <b>Gen.Artifacts.Distribs.Libs</b> package provides a specific
--  distribution rule which gather the shared libraries used by executables
--  and put them in a target distribution directory.
--
--  <install mode='libs' dir='lib'>
--    <library>libgnat</library>
--    <library>libmysql*</library>
--    <fileset dir="bin">
--        <include name="*-server"/>
--    </fileset>
--  </install>
--
--  Libraries to copy in the distribution are specified in the <b>library</b>
--  rule description.  The library name must match the pattern defined.
--
--  The command works by executing the Unix application <b>ldd</b> on the
--  executable.  It parses the <b>ldd</b> output and collects the libraries
--  that the program is using.  It then copies the selected libraries
--  in the target directory.
private package Gen.Artifacts.Distribs.Libs is

   --  Create a distribution rule to extract the shared libraries used by an executable
   --  and copy a selected subset in the target directory.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Libs_Rule is new Distrib_Rule with private;
   type Libs_Rule_Access is access all Libs_Rule'Class;

   --  Check if the library whose absolute path is defined in <b>Source</b> must be
   --  copied in the target directory and copy that library if needed.
   procedure Copy (Rule   : in Libs_Rule;
                   Dir    : in String;
                   Source : in String);

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Libs_Rule) return String;

   --  Get the target path associate with the given source file for the distribution rule.
   overriding
   function Get_Target_Path (Rule : in Libs_Rule;
                             Base : in String;
                             File : in File_Record) return String;

   overriding
   procedure Install (Rule    : in Libs_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Libs_Rule is new Distrib_Rule with record
      Command       : EL.Expressions.Expression;
      Libraries     : Util.Strings.Vectors.Vector;
   end record;

end Gen.Artifacts.Distribs.Libs;
