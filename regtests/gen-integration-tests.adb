-----------------------------------------------------------------------
--  gen-integration-tests -- Tests for integration
--  Copyright (C) 2012 - 2023 Stephane Carrez
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

with Ada.Directories;

with Util.Test_Caller;
with Util.Files;
with Util.Systems.Os;

with Gen.Testsuite;
package body Gen.Integration.Tests is

   --  Get the dynamo executable path.
   function Dynamo return String;

   --  Clean the directory if it exists.
   procedure Clean_Directory (Path : in String);

   package Caller is new Util.Test_Caller (Test, "Dynamo");
   --  ------------------------------
   --  Execute the command and get the output in a string.
   --  ------------------------------
   overriding
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Result  : out UString;
                      Status  : in Natural := 0;
                      Source  : String := GNAT.Source_Info.File;
                      Line    : Natural := GNAT.Source_Info.Line) is
      Test_Dir : constant String := Gen.Testsuite.Get_Test_Directory;
      Dir      : constant String := Util.Files.Compose (Test_Dir, "test-app");
   begin
      if Ada.Directories.Exists (Dir) then
         Util.Tests.Test (T).Execute (Command, "", "", Result, Dir, Status, Source, Line);
      else
         Util.Tests.Test (T).Execute (Command, "", "", Result, "", Status, Source, Line);
      end if;
   end Execute;

   --  ------------------------------
   --  Clean the directory if it exists.
   --  ------------------------------
   procedure Clean_Directory (Path : in String) is
   begin
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_Tree (Path);
      end if;
   end Clean_Directory;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Create AWA project",
                       Test_Create_Project'Access);
      Caller.Add_Test (Suite, "Create ADO project",
                       Test_Create_ADO_Project'Access);
      Caller.Add_Test (Suite, "Create GTK project",
                       Test_Create_GTK_Project'Access);
      Caller.Add_Test (Suite, "Create Lib project",
                       Test_Create_Lib_Project'Access);
      Caller.Add_Test (Suite, "Configure",
                       Test_Configure'Access);
      Caller.Add_Test (Suite, "Propset",
                       Test_Change_Property'Access);
      Caller.Add_Test (Suite, "Add Module",
                       Test_Add_Module'Access);
      Caller.Add_Test (Suite, "Add Model",
                       Test_Add_Model'Access);
      Caller.Add_Test (Suite, "Add Module Operation",
                       Test_Add_Module_Operation'Access);
      Caller.Add_Test (Suite, "Add Service",
                       Test_Add_Service'Access);
      Caller.Add_Test (Suite, "Add Query",
                       Test_Add_Query'Access);
      Caller.Add_Test (Suite, "Add Page",
                       Test_Add_Page'Access);
      Caller.Add_Test (Suite, "Add Layout",
                       Test_Add_Layout'Access);
      Caller.Add_Test (Suite, "Add Ajax Form",
                       Test_Add_Ajax_Form'Access);
      Caller.Add_Test (Suite, "Generate",
                       Test_Generate'Access);
      Caller.Add_Test (Suite, "Help",
                       Test_Help'Access);
      Caller.Add_Test (Suite, "Dist",
                       Test_Dist'Access);
      Caller.Add_Test (Suite, "Dist with exclude",
                       Test_Dist_Exclude'Access);
      Caller.Add_Test (Suite, "Info",
                       Test_Info'Access);
      Caller.Add_Test (Suite, "Build Doc",
                       Test_Build_Doc'Access);
      Caller.Add_Test (Suite, "Build Pandoc",
                       Test_Build_Pandoc'Access);
      Caller.Add_Test (Suite, "Generate from Hibernate XML model",
                       Test_Generate_Hibernate'Access);
      Caller.Add_Test (Suite, "Generate XMI Enum",
                       Test_Generate_XMI_Enum'Access);
      Caller.Add_Test (Suite, "Generate XMI Bean",
                       Test_Generate_XMI_Bean_Table'Access);
      Caller.Add_Test (Suite, "Generate XMI Bean with table inheritance",
                       Test_Generate_XMI_Bean'Access);
      Caller.Add_Test (Suite, "Generate XMI Table",
                       Test_Generate_XMI_Table'Access);
      Caller.Add_Test (Suite, "Generate XMI Association",
                       Test_Generate_XMI_Association'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML Association",
                       Test_Generate_Zargo_Association'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML Association (dependencies)",
                       Test_Generate_Zargo_Dependencies'Access);
      Caller.Add_Test (Suite, "Generate XMI Datatypes",
                       Test_Generate_XMI_Datatype'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML several packages",
                       Test_Generate_Zargo_Packages'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML serialization",
                       Test_Generate_Zargo_Serialization'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML with several UML errors",
                       Test_Generate_Zargo_Errors'Access);
      Caller.Add_Test (Suite, "Build generated project",
                       Test_Build'Access);
      Caller.Add_Test (Suite, "Build generated model files (UML)",
                       Test_Build_Model'Access);

      --  Delete the previous test application if it exists.
      Clean_Directory ("test-app");
   end Add_Tests;

   --  ------------------------------
   --  Get the dynamo executable path.
   --  ------------------------------
   function Dynamo return String is
   begin
      return Util.Files.Compose (Gen.Testsuite.Get_Test_Directory, "bin/dynamo");
   end Dynamo;

   --  ------------------------------
   --  Test dynamo create-project command.
   --  ------------------------------
   procedure Test_Create_Project (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " -o test-app create-project -l apache test", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test.properties", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*Generating file.*src/test.ads", Result,
                                 "Invalid generation");
   end Test_Create_Project;

   --  ------------------------------
   --  Test dynamo create-project command --ado.
   --  ------------------------------
   procedure Test_Create_ADO_Project (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " -o test-ado create-project -l apache --ado test", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test.properties", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*Generating file.*src/test.ads", Result,
                                 "Invalid generation");
   end Test_Create_ADO_Project;

   --  ------------------------------
   --  Test dynamo create-project command --gtk.
   --  ------------------------------
   procedure Test_Create_GTK_Project (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " -o test-gtk create-project -l gpl3 --gtk test", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*COPYING3", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*Generating file.*src/test-main.adb", Result,
                                 "Invalid generation");
   end Test_Create_GTK_Project;

   --  ------------------------------
   --  Test dynamo create-project command --lib.
   --  ------------------------------
   procedure Test_Create_Lib_Project (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " -o test-lib create-project -l gpl3 --lib test", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*Makefile", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*Generating file.*src/test.ads", Result,
                                 "Invalid generation");
   end Test_Create_Lib_Project;

   --  ------------------------------
   --  Test project configure.
   --  ------------------------------
   procedure Test_Configure (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("./configure --enable-coverage", Result);
      Util.Tests.Assert_Matches (T, ".*checking build system.*", Result,
                                 "Invalid configure");
      Util.Tests.Assert_Matches (T, ".*config.status: creating Makefile.*", Result,
                                 "Invalid configure");
   end Test_Configure;

   --  ------------------------------
   --  Test propset command.
   --  ------------------------------
   procedure Test_Change_Property (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " propset author Druss", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid propset command");

      T.Execute (Dynamo & " propset author_email Druss@drenai.com", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid propset command");

      T.Execute (Dynamo & " propset license Apache", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid propset command");
   end Test_Change_Property;

   --  ------------------------------
   --  Test add-module command.
   --  ------------------------------
   procedure Test_Add_Module (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-module tblog", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tblog-beans.ads.*", Result,
                                 "Invalid add-module");

      T.Execute (Dynamo & " add-module tuser", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tuser-beans.ads.*", Result,
                                 "Invalid add-module");
   end Test_Add_Module;

   --  ------------------------------
   --  Test add-model command.
   --  ------------------------------
   procedure Test_Add_Model (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-model tblog test_model", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tblog-models.ads.*", Result,
                                 "Invalid add-model");

      T.Execute (Dynamo & " add-model tblog test_second_model", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tblog-models.ads.*", Result,
                                 "Invalid add-model");
   end Test_Add_Model;

   --  ------------------------------
   --  Test add-module-operation command.
   --  ------------------------------
   procedure Test_Add_Module_Operation (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-module-operation tblog test_model Save", Result);
      Util.Tests.Assert_Matches (T, ".*Patching file.*tblog-modules.ads.*", Result,
                                 "Invalid add-module-operation");
      Util.Tests.Assert_Matches (T, ".*Patching file.*tblog.*procedure implementation.*", Result,
                                 "Invalid add-module-operation");

      T.Execute (Dynamo & " add-module-operation tblog test_model Delete", Result);
      Util.Tests.Assert_Matches (T, ".*Patching file.*test-tblog-modules.adb.*", Result,
                                 "Invalid add-module-operation");

      T.Execute (Dynamo & " add-module-operation tblog test_second_model Delete", Result);
      Util.Tests.Assert_Matches (T, ".*Patching file.*test-tblog-modules.adb.*", Result,
                                 "Invalid add-module-operation");
   end Test_Add_Module_Operation;

   --  ------------------------------
   --  Test add-service command.
   --  ------------------------------
   procedure Test_Add_Service (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-service tblog blogging", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tblog-services.ads.*", Result,
                                 "Invalid add-module");

      T.Execute (Dynamo & " add-service tuser admin", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-tuser-services.ads.*", Result,
                                 "Invalid add-module");
   end Test_Add_Service;

   --  ------------------------------
   --  Test add-query command.
   --  ------------------------------
   procedure Test_Add_Query (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-query tuser user_query", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*db/tuser-user_query.xml*", Result,
                                 "Invalid add-query");

      T.Execute (Dynamo & " add-query tblog blog_query", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*db/tblog-blog_query.xml.*", Result,
                                 "Invalid add-query");
   end Test_Add_Query;

   --  ------------------------------
   --  Test add-page command.
   --  ------------------------------
   procedure Test_Add_Page (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-page main_page", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*web/main_page.xhtml.*", Result,
                                 "Invalid add-page");
   end Test_Add_Page;

   --  ------------------------------
   --  Test add-layout command.
   --  ------------------------------
   procedure Test_Add_Layout (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-layout my-layout", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*web/WEB-INF/layouts/my-layout.xhtml.*",
                                 Result,
                                 "Invalid add-layout");
   end Test_Add_Layout;

   --  ------------------------------
   --  Test add-ajax-form command.
   --  ------------------------------
   procedure Test_Add_Ajax_Form (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " add-ajax-form tuser create-user", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*web/tuser/forms/create-user-response.xhtml.*",
                                 Result,
                                 "Invalid add-ajax-form");
   end Test_Add_Ajax_Form;

   --  ------------------------------
   --  Test generate command.
   --  ------------------------------
   procedure Test_Generate (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " propset is_plugin true", Result);
      T.Execute (Dynamo & " generate db", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Reading model file stored in .db.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*src/model/test-tuser-models.*",
                                 Result,
                                 "Invalid generate");

      T.Execute (Dynamo & " propset is_plugin false", Result);
      T.Execute (Dynamo & " generate db", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Reading model file stored in .db.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*src/model/test-tuser-models.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*db/mysql/test-mysql.sql.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating mysql.*db/mysql/create-test-mysql.sql.*",
                                 Result,
                                 "Invalid generate");
   end Test_Generate;

   --  ------------------------------
   --  Test help command.
   --  ------------------------------
   procedure Test_Help (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " help", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Usage.*dynamo.*",
                                 Result,
                                 "Invalid help");
      Util.Tests.Assert_Matches (T,
                                 ".*add.*module.*",
                                 Result,
                                 "Invalid help");
   end Test_Help;

   --  ------------------------------
   --  Test dist command.
   --  ------------------------------
   procedure Test_Dist (T : in out Test) is
      Result : UString;
   begin
      T.Execute ("unzip ../regtests/files/dist-web.zip", Result);
      Clean_Directory ("test-dist");
      T.Execute (Dynamo & " dist ../test-dist", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*files with copy.*",
                                 Result,
                                 "Invalid dist");
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*compressor.*",
                                 Result,
                                 "Invalid dist");
      Util.Tests.Assert_Exists (T, "test-dist/bundles/test.properties");
      Util.Tests.Assert_Exists (T, "test-dist/bundles/tuser.properties");
   end Test_Dist;

   --  ------------------------------
   --  Test dist with exclude support command.
   --  ------------------------------
   procedure Test_Dist_Exclude (T : in out Test) is
      Result : UString;
   begin
      Clean_Directory ("test-dist");
      T.Execute (Dynamo & " dist ../test-dist ../regtests/files/package.xml", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*files with copy.*",
                                 Result,
                                 "Invalid dist");
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*compressor.*",
                                 Result,
                                 "Invalid dist");
      Util.Tests.Assert_Exists (T, "test-dist/bundles/test.properties");
      T.Assert (not Ada.Directories.Exists ("test-dir/bundles/tuser.properties"),
                "File should not be copied");
   end Test_Dist_Exclude;

   --  ------------------------------
   --  Test dist command.
   --  ------------------------------
   procedure Test_Info (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " info", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*GNAT project files.*",
                                 Result,
                                 "Invalid info");
      Util.Tests.Assert_Matches (T,
                                 ".*Dynamo project files.*",
                                 Result,
                                 "Invalid info");
   end Test_Info;

   --  ------------------------------
   --  Test build-doc command.
   --  ------------------------------
   procedure Test_Build_Doc (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " build-doc wiki", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid build-doc command");

      Util.Tests.Assert_Exists (T, "test-app/wiki/tblog.wiki");
      Util.Tests.Assert_Exists (T, "test-app/wiki/tuser-user_query.wiki");
   end Test_Build_Doc;

   --  ------------------------------
   --  Test build-doc command with -pandoc.
   --  ------------------------------
   procedure Test_Build_Pandoc (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " build-doc -pandoc docs", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid build-doc -pandoc command");

      Util.Tests.Assert_Exists (T, "test-app/docs/tblog.md");
      Util.Tests.Assert_Exists (T, "test-app/docs/tuser-user_query.md");
   end Test_Build_Pandoc;

   --  ------------------------------
   --  Test generate command with Hibernate XML mapping files.
   --  ------------------------------
   procedure Test_Generate_Hibernate (T : in out Test) is
      Result : UString;
   begin
      Ada.Directories.Copy_File (Source_Name => "regtests/files/User.hbm.xml",
                                 Target_Name => "test-app/db/User.hbm.xml");
      Ada.Directories.Copy_File (Source_Name => "regtests/files/Queues.hbm.xml",
                                 Target_Name => "test-app/db/Queues.hbm.xml");
      Ada.Directories.Copy_File (Source_Name => "regtests/files/Permission.hbm.xml",
                                 Target_Name => "test-app/db/Permission.hbm.xml");
      Ada.Directories.Copy_File (Source_Name => "regtests/files/permissions.xml",
                                 Target_Name => "test-app/db/permissions.xml");
      Ada.Directories.Copy_File (Source_Name => "regtests/files/queue-messages.xml",
                                 Target_Name => "test-app/db/queue-messages.xml");
      Ada.Directories.Copy_File (Source_Name => "regtests/files/serialize.xml",
                                 Target_Name => "test-app/db/serialize.xml");
      T.Execute (Dynamo & " generate db", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Reading model file stored in .db.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*src/model/test-tuser-models.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*db/mysql/test-mysql.sql.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-permissions-models.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-permissions-models.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-users-models.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-users-models.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-events-models.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-events-models.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-events-serialize.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-events-serialize.adb");
   end Test_Generate_Hibernate;

   --  ------------------------------
   --  Test generate command (XMI enum).
   --  ------------------------------
   procedure Test_Generate_XMI_Enum (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-enum.xmi", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-enums.ads");
   end Test_Generate_XMI_Enum;

   --  ------------------------------
   --  Test generate command (XMI Ada Bean).
   --  ------------------------------
   procedure Test_Generate_XMI_Bean (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-beans.xmi", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-beans.ads");
   end Test_Generate_XMI_Bean;

   --  ------------------------------
   --  Test generate command (XMI Ada Bean with inheritance).
   --  ------------------------------
   procedure Test_Generate_XMI_Bean_Table (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-table-beans.zargo", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-table_beans.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-table_beans.adb");
   end Test_Generate_XMI_Bean_Table;

   --  ------------------------------
   --  Test generate command (XMI Ada Table).
   --  ------------------------------
   procedure Test_Generate_XMI_Table (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-table.xmi", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-tables.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-tables.adb");
   end Test_Generate_XMI_Table;

   --  ------------------------------
   --  Test generate command (XMI Associations between Tables).
   --  ------------------------------
   procedure Test_Generate_XMI_Association (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-associations.xmi", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-associations.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-associations.adb");
   end Test_Generate_XMI_Association;

   --  ------------------------------
   --  Test generate command using the ArgoUML file directly (runs unzip -cq and parse the output).
   --  ------------------------------
   procedure Test_Generate_Zargo_Association (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-associations.zargo", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-associations.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-associations.adb");
   end Test_Generate_Zargo_Association;

   --  ------------------------------
   --  Test generate command (XMI Datatype).
   --  ------------------------------
   procedure Test_Generate_XMI_Datatype (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-types.xmi", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-types.ads");
   end Test_Generate_XMI_Datatype;

   --  ------------------------------
   --  Test UML with several tables that have dependencies between each of them (non circular).
   --  ------------------------------
   procedure Test_Generate_Zargo_Dependencies (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-dependencies.zargo", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-dependencies.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-dependencies.adb");
   end Test_Generate_Zargo_Dependencies;

   --  ------------------------------
   --  Test UML with several tables in several packages (non circular).
   --  ------------------------------
   procedure Test_Generate_Zargo_Packages (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-packages.zargo", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-a.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-a.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-b.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-b.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-c.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-c.adb");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-e.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-packages-e.adb");
   end Test_Generate_Zargo_Packages;

   --  ------------------------------
   --  Test UML with serialization code.
   --  ------------------------------
   procedure Test_Generate_Zargo_Serialization (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-serialize.zargo", Result);

      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-serialize.ads");
      Util.Tests.Assert_Exists (T, "test-app/src/model/gen-tests-serialize.adb");
   end Test_Generate_Zargo_Serialization;

   --  ------------------------------
   --  Test UML with several errors in the UML model.
   --  ------------------------------
   procedure Test_Generate_Zargo_Errors (T : in out Test) is
      Result : UString;
   begin
      T.Execute (Dynamo & " -o tmp generate ../regtests/uml/dynamo-test-errors.zargo", Result, 1);

      Util.Tests.Assert_Matches (T, ".*attribute .name. in table .Table_A. has no type.*",
                                 Result,
                                 "attribute with no type error not detected");
      Util.Tests.Assert_Matches (T, ".*attribute .count. in table .Table_A. has no type.*",
                                 Result,
                                 "attribute with no type error not detected");
      Util.Tests.Assert_Matches (T, ".*attribute .name. in table .Table_C. has no type.*",
                                 Result,
                                 "attribute with no type error not detected");
      Util.Tests.Assert_Matches (T, ".*the enum 'Test_Import_Enum' is empty.*",
                                 Result,
                                 "empty enum error not detected");
      Util.Tests.Assert_Matches (T, ".*multiple association 'properties' for table.*",
                                 Result,
                                 "multiple association not detected");
   end Test_Generate_Zargo_Errors;

   --  ------------------------------
   --  Test GNAT compilation of the final project.
   --  ------------------------------
   procedure Test_Build (T : in out Test) is

      Result : UString;
   begin
      T.Test_Configure;
      T.Execute ("make", Result);

      pragma Warnings (Off, "condition is always False");
      if Util.Systems.Os.Directory_Separator = '\' then
         Util.Tests.Assert_Exists (T, "test-app/bin/test-server.exe");
      else
         Util.Tests.Assert_Exists (T, "test-app/bin/test-server");
      end if;
      pragma Warnings (On, "condition is always False");

   end Test_Build;

   --  ------------------------------
   --  Test GNAT compilation of the generated model files.
   --  ------------------------------
   procedure Test_Build_Model (T : in out Test) is

      Result : UString;
   begin
      Ada.Directories.Copy_File (Source_Name => "regtests/check_build/check_build.gpr",
                                 Target_Name => "test-app//check_build.gpr");

      T.Execute ("gprbuild -p -Pcheck_build", Result);

      pragma Warnings (Off, "condition is always False");
      if Util.Systems.Os.Directory_Separator = '\' then
         Util.Tests.Assert_Exists (T, "test-app/bin/test-server.exe");
      else
         Util.Tests.Assert_Exists (T, "test-app/bin/test-server");
      end if;
      pragma Warnings (On, "condition is always False");

   end Test_Build_Model;

end Gen.Integration.Tests;
