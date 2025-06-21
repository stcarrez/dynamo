-----------------------------------------------------------------------
--  gen-artifacts-distribs-bundles -- Merge bundles for distribution artifact
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>Gen.Artifacts.Distribs.Bundles</b> package provides distribution rules
--  to merge a list of bundles to the distribution area.  The rule is
--  created by using the following XML definition:
--
--  <install mode='bundles'>
--     <fileset dir='bundles'>
--        <include name="**/*.properties"/>
--     </fileset>
--  </install>
--
private package Gen.Artifacts.Distribs.Bundles is

   --  Create a distribution rule to concatenate a set of files.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Bundle_Rule is new Distrib_Rule with private;
   type Bundle_Rule_Access is access all Bundle_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Bundle_Rule) return String;

   --  Install the file <b>File</b> according to the distribution rule.
   --  Merge all the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   overriding
   procedure Install (Rule    : in Bundle_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Bundle_Rule is new Distrib_Rule with null record;

end Gen.Artifacts.Distribs.Bundles;
