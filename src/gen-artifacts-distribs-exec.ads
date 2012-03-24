-----------------------------------------------------------------------
--  gen-artifacts-distribs-exec -- External command based distribution artifact
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

--  The <b>Gen.Artifacts.Distribs.Exec</b> package provides distribution rules
--  to copy a file or a directory by using an external program.
private package Gen.Artifacts.Distribs.Exec is

   --  Create a distribution rule to copy a set of files or directories and
   --  execute an external command.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Exec_Rule is new Distrib_Rule with private;
   type Exec_Rule_Access is access all Exec_Rule'Class;

   overriding
   procedure Install (Rule    : in Exec_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Exec_Rule is new Distrib_Rule with record
      Command : EL.Expressions.Expression;
   end record;

end Gen.Artifacts.Distribs.Exec;
