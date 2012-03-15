-----------------------------------------------------------------------
--  gen-artifacts-distribs-copies -- Copy based distribution artifact
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

--  The <b>Gen.Artifacts.Distribs.Copies</b> package provides distribution rules
--  to copy a file or a directory to the distribution area.
private package Gen.Artifacts.Distribs.Copies is

   --  Create a distribution rule to copy a set of files or directories.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Copy_Rule is new Distrib_Rule with private;
   type Copy_Rule_Access is access all Copy_Rule'Class;

   overriding
   procedure Install (Rule : in Copy_Rule;
                      Path : in String;
                      File : in File_Info);

private

   type Copy_Rule is new Distrib_Rule with null record;

end Gen.Artifacts.Distribs.Copies;
