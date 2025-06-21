-----------------------------------------------------------------------
--  gen-artifacts-distribs-concat -- Concatenate based distribution artifact
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>Gen.Artifacts.Distribs.Concat</b> package provides distribution rules
--  to concatenate a list of files to the distribution area.  The rule is
--  created by using the following XML definition:
--
--  <install mode='copy' file='NOTICE.txt'>
--     <include name="NOTICE.txt"/>
--  </install>
--
private package Gen.Artifacts.Distribs.Concat is

   --  Create a distribution rule to concatenate a set of files.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Concat_Rule is new Distrib_Rule with private;
   type Concat_Rule_Access is access all Concat_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Concat_Rule) return String;

   --  Install the file <b>File</b> according to the distribution rule.
   --  Concatenate the files listed in <b>Files</b> in the target path specified by <b>Path</b>.
   overriding
   procedure Install (Rule    : in Concat_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Concat_Rule is new Distrib_Rule with null record;

end Gen.Artifacts.Distribs.Concat;
