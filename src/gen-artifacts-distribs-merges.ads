-----------------------------------------------------------------------
--  gen-artifacts-distribs-merges -- Web file merge
--  Copyright (C) 2020 Stephane Carrez
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
with EL.Variables.Default;
with EL.Contexts.Default;
with Util.Strings.Maps;
with Util.Beans.Objects.Maps;

--  === Distribution: webmerge ===
--  The `webmerge` distribution rule is intended to merge Javascript or CSS files
--  which are used by XHTML presentation files.  It requires some help from the
--  developer to describe what files must be merged.  The XHTML file must contain
--  well defined XML comments which are used to identify the merging areas.
--  The CSS file merge start section begins with:
--
--    <!-- DYNAMO-MERGE-START link=#{contextPath}/css/target-merge-1.css -->
--
--  and the Javascript merge start begins with:
--
--    <!-- DYNAMO-MERGE-START script=#{contextPath}/js/target-merge-1.js -->
--
--  The merge section is terminated by the following XML comment:
--
--    <!-- DYNAMO-MERGE-END -->
--
--  Everything withing these XML comments is then replaced either by a `link`
--  HTML tag or by a `script` HTML tag and a file described either by the
--  `link=` or `script=` markers is generated to include every `link` or `script`
--  that was defined within the XML comment markers.  For example, with the following
--  XHTML extract:
--
--    <!-- DYNAMO-MERGE-START link=#{contextPath}/css/merged.css -->
--    <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/awa.css"/>
--    <link media="screen" type="text/css" rel="stylesheet" href="#{jquery.uiCssPath}"/>
--    <link media="screen" type="text/css" rel="stylesheet" href="#{jquery.chosenCssPath}"/>
--    <!-- DYNAMO-MERGE-END -->
--
--  The generated file `css/merged.css` will include `awa.css`, `jquery-ui-1.12.1.css`,
--  `chosen.css` and the XHTML will be replaced to include `css/merge.css` only
--  by using the following XHTML:
--
--    <link media='screen' type='text/css' rel='stylesheet' href='#{contextPath}/css/merged.css'/>
--
--  To use the `webmerge`, the `package.xml` description file should contain
--  the following command:
--
--    <install mode='merge' dir='web'>
--       <property name="contextPath"></property>
--       <property name="jquery.path">/js/jquery-3.4.1.js</property>
--       <property name="jquery.uiCssPath">/css/redmond/jquery-ui-1.12.1.css</property>
--       <property name="jquery.chosenCssPath">/css/jquery-chosen-1.8.7/chosen.css</property>
--       <property name="jquery.uiPath">/js/jquery-ui-1.12.1</property>
--       <fileset dir="web">
--          <include name="WEB-INF/layouts/*.xhtml"/>
--       </fileset>
--    </install>
--
private package Gen.Artifacts.Distribs.Merges is

   --  Create a distribution rule to copy a set of files or directories.
   function Create_Rule (Node : in DOM.Core.Node) return Distrib_Rule_Access;

   --  ------------------------------
   --  Distribution artifact
   --  ------------------------------
   type Merge_Rule is new Distrib_Rule with private;
   type Merge_Rule_Access is access all Merge_Rule;

   --  Get a name to qualify the installation rule (used for logs).
   overriding
   function Get_Install_Name (Rule : in Merge_Rule) return String;

   overriding
   procedure Install (Rule    : in Merge_Rule;
                      Path    : in String;
                      Files   : in File_Vector;
                      Context : in out Generator'Class);

private

   type Merge_Rule is new Distrib_Rule with record
      Params    : Util.Beans.Objects.Maps.Map_Bean;
      Context   : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;
      Replace   : Util.Strings.Maps.Map;
   end record;

end Gen.Artifacts.Distribs.Merges;
