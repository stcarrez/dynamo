-----------------------------------------------------------------------
--  gen-artifacts -- Artifacts for Code Generator
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body Gen.Artifacts is

   --  ------------------------------
   --  After the configuration file is read, processes the node whose root
   --  is passed in <b>Node</b> and initializes the <b>Model</b> with the information.
   --  ------------------------------
   procedure Initialize (Handler : in out Artifact;
                         Path    : in String;
                         Node    : in DOM.Core.Node;
                         Model   : in out Gen.Model.Packages.Model_Definition'Class;
                         Context : in out Generator'Class) is
      pragma Unreferenced (Path, Node, Model, Context);
   begin
      Handler.Initialized := True;
   end Initialize;

   --  ------------------------------
   --  Check whether this artifact has been initialized.
   --  ------------------------------
   function Is_Initialized (Handler : in Artifact) return Boolean is
   begin
      return Handler.Initialized;
   end Is_Initialized;

end Gen.Artifacts;
