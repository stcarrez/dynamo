-----------------------------------------------------------------------
--  gen-artifacts-distribs-exec -- External command based distribution artifact
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;

--  The <b>Gen.Artifacts.Distribs.Exec</b> package provides distribution rules
--  to copy a file or a directory by using an external program.  The rule is
--  created by using the following XML definition:
--
--  <install mode='exec' dir='target'>
--    <command>cmd #{src} #{dst}</command>
--    <fileset dir="source">
--        <include name="**/*"/>
--    </fileset>
--  </install>
--
--  The command is a string which can contain EL expressions that are
--  evaluated before executing the command.  The command is executed for
--  each source file.  The following EL variables are defined:
--
--    src   defines the absolute source path
--    dst   defines the target destination path
--
private package Gen.Artifacts.Distribs.Exec is

   --  Create a distribution rule to copy a set of files or directories and
   --  execute an external command.
   function Create_Rule (Node : in DOM.Core.Node;
                         Copy : in Boolean) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Exec_Rule is new Distrib_Rule with private;
   type Exec_Rule_Access is access all Exec_Rule'Class;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule    : in Exec_Rule) return String;

   overriding
   procedure Install (Rule    : in Exec_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Exec_Rule is new Distrib_Rule with record
      Command       : EL.Expressions.Expression;
      Output        : EL.Expressions.Expression;
      Output_Append : Boolean;
      Copy_First    : Boolean := False;
      Slow_Flag     : Boolean := False;
   end record;

end Gen.Artifacts.Distribs.Exec;
