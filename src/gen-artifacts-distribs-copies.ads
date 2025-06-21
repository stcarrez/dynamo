-----------------------------------------------------------------------
--  gen-artifacts-distribs-copies -- Copy based distribution artifact
--  Copyright (C) 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>Gen.Artifacts.Distribs.Copies</b> package provides distribution rules
--  to copy a file or a directory to the distribution area.
private package Gen.Artifacts.Distribs.Copies is

   --  Create a distribution rule to copy a set of files or directories.
   function Create_Rule (Node : in DOM.Core.Node;
                         Copy_First_File : in Boolean) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Copy_Rule is new Distrib_Rule with private;
   type Copy_Rule_Access is access all Copy_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Copy_Rule) return String;

   overriding
   procedure Install (Rule    : in Copy_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Copy_Rule is new Distrib_Rule with record
      --  When True and there are several source files, use the first file.
      --  Otherwise, use the last file.
      Copy_First_File : Boolean := False;
   end record;

end Gen.Artifacts.Distribs.Copies;
