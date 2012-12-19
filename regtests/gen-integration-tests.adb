-----------------------------------------------------------------------
--  gen-integration-tests -- Tests for integration
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

with Ada.Directories;

with Util.Log.Loggers;
with Util.Test_Caller;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Processes;
with Util.Files;
with Util.Systems.Os;

with Gen.Testsuite;
package body Gen.Integration.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Integration.Tests");

   --  Get the dynamo executable path.
   function Dynamo return String;

   package Caller is new Util.Test_Caller (Test, "Dynamo");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Create_Project",
                       Test_Create_Project'Access);
      Caller.Add_Test (Suite, "Create_ADO_Project",
                       Test_Create_ADO_Project'Access);
      Caller.Add_Test (Suite, "Configure",
                       Test_Configure'Access);
      Caller.Add_Test (Suite, "Propset",
                       Test_Change_Property'Access);
      Caller.Add_Test (Suite, "Add Module",
                       Test_Add_Module'Access);
      Caller.Add_Test (Suite, "Add Service",
                       Test_Add_Service'Access);
      Caller.Add_Test (Suite, "Add Query",
                       Test_Add_Query'Access);
      Caller.Add_Test (Suite, "Add Page",
                       Test_Add_Page'Access);
      Caller.Add_Test (Suite, "Add Ajax Form",
                       Test_Add_Ajax_Form'Access);
      Caller.Add_Test (Suite, "Generate",
                       Test_Generate'Access);
      Caller.Add_Test (Suite, "Help",
                       Test_Help'Access);
      Caller.Add_Test (Suite, "Dist",
                       Test_Dist'Access);
      Caller.Add_Test (Suite, "Info",
                       Test_Info'Access);
      Caller.Add_Test (Suite, "Build Doc",
                       Test_Build_Doc'Access);
      Caller.Add_Test (Suite, "Generate XMI Enum",
                       Test_Generate_XMI_Enum'Access);
      Caller.Add_Test (Suite, "Generate XMI Bean",
                       Test_Generate_XMI_Bean'Access);
      Caller.Add_Test (Suite, "Generate XMI Table",
                       Test_Generate_XMI_Table'Access);
      Caller.Add_Test (Suite, "Generate XMI Association",
                       Test_Generate_XMI_Association'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML Association",
                       Test_Generate_Zargo_Association'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML Association (dependencies)",
                       Test_Generate_Zargo_Dependencies'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML several packages",
                       Test_Generate_Zargo_Packages'Access);
      Caller.Add_Test (Suite, "Generate ArgoUML with several UML errors",
                       Test_Generate_Zargo_Errors'Access);
      Caller.Add_Test (Suite, "Build generated project",
                       Test_Build'Access);
      Caller.Add_Test (Suite, "Build generated model files (UML)",
                       Test_Build_Model'Access);

      --  Delete the previous test application if it exists.
      if Ada.Directories.Exists ("test-app") then
         Ada.Directories.Delete_Tree ("test-app");
      end if;
   end Add_Tests;

   --  ------------------------------
   --  Change the working directory before running dynamo.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);

      Dir : constant String := Util.Files.Compose (Gen.Testsuite.Get_Test_Directory, "test-app");
   begin
      Log.Debug ("Change {0}", Dir);
      if Ada.Directories.Exists (Dir) then
         Ada.Directories.Set_Directory (Dir);
      else
         Ada.Directories.Set_Directory (Gen.Testsuite.Get_Test_Directory);
      end if;
   end Set_Up;

   --  ------------------------------
   --  Restore the working directory after running dynamo.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Ada.Directories.Set_Directory (Gen.Testsuite.Get_Test_Directory);
   end Tear_Down;

   --  ------------------------------
   --  Get the dynamo executable path.
   --  ------------------------------
   function Dynamo return String is
   begin
      return Util.Files.Compose (Gen.Testsuite.Get_Test_Directory, "bin/dynamo");
   end Dynamo;

   --  ------------------------------
   --  Execute the command and get the output in a string.
   --  ------------------------------
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Result  : out Ada.Strings.Unbounded.Unbounded_String;
                      Status  : in Natural := 0) is
      P       : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer  : Util.Streams.Buffered.Buffered_Stream;
   begin
      P.Open (Command, Util.Processes.READ);

      --  Write on the process input stream.
      Buffer.Initialize (null, P'Unchecked_Access, 8192);
      Buffer.Read (Result);
      P.Close;
      Log.Info ("Command result: {0}", Result);
      Util.Tests.Assert_Equals (T, Status, P.Get_Exit_Status, "Command '" & Command & "' failed");
   end Execute;

   --  ------------------------------
   --  Test dynamo create-project command.
   --  ------------------------------
   procedure Test_Create_Project (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
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
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " -o test-ado create-project -l apache --ado test", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test.properties", Result,
                                 "Invalid generation");
      Util.Tests.Assert_Matches (T, ".*Generating file.*src/test.ads", Result,
                                 "Invalid generation");
   end Test_Create_ADO_Project;

   --  ------------------------------
   --  Test project configure.
   --  ------------------------------
   procedure Test_Configure (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("./configure", Result);
      Util.Tests.Assert_Matches (T, ".*checking build system.*", Result,
                                 "Invalid configure");
      Util.Tests.Assert_Matches (T, ".*config.status: creating Makefile.*", Result,
                                 "Invalid configure");
   end Test_Configure;

   --  ------------------------------
   --  Test propset command.
   --  ------------------------------
   procedure Test_Change_Property (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
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
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " add-module blog", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-blog-beans.ads.*", Result,
                                 "Invalid add-module");

      T.Execute (Dynamo & " add-module user", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-user-beans.ads.*", Result,
                                 "Invalid add-module");
   end Test_Add_Module;

   --  ------------------------------
   --  Test add-service command.
   --  ------------------------------
   procedure Test_Add_Service (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " add-service blog blogging", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-blog-services.ads.*", Result,
                                 "Invalid add-module");

      T.Execute (Dynamo & " add-service user admin", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test-user-services.ads.*", Result,
                                 "Invalid add-module");
   end Test_Add_Service;

   --  ------------------------------
   --  Test add-query command.
   --  ------------------------------
   procedure Test_Add_Query (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " add-query user user_query", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*db/user-user_query.xml*", Result,
                                 "Invalid add-query");

      T.Execute (Dynamo & " add-query blog blog_query", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*db/blog-blog_query.xml.*", Result,
                                 "Invalid add-query");
   end Test_Add_Query;

   --  ------------------------------
   --  Test add-page command.
   --  ------------------------------
   procedure Test_Add_Page (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " add-page main_page", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*web/main_page.xhtml.*", Result,
                                 "Invalid add-page");
   end Test_Add_Page;

   --  ------------------------------
   --  Test add-ajax-form command.
   --  ------------------------------
   procedure Test_Add_Ajax_Form (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " add-ajax-form user create-user", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*web/user/forms/create-user-response.xhtml.*",
                                 Result,
                                 "Invalid add-ajax-form");
   end Test_Add_Ajax_Form;

   --  ------------------------------
   --  Test generate command.
   --  ------------------------------
   procedure Test_Generate (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate db", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Reading model file stored in .db.*",
                                 Result,
                                 "Invalid generate");
      Util.Tests.Assert_Matches (T,
                                 ".*Generating file.*src/model/test-user-models.*",
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
      Result : Ada.Strings.Unbounded.Unbounded_String;
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
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " dist ../test-dist", Result);
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*files with copy.*",
                                 Result,
                                 "Invalid dist");
      Util.Tests.Assert_Matches (T,
                                 ".*Installing.*compressor.*",
                                 Result,
                                 "Invalid dist");
   end Test_Dist;

   --  ------------------------------
   --  Test dist command.
   --  ------------------------------
   procedure Test_Info (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
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
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " build-doc wiki", Result);
      Util.Tests.Assert_Equals (T, "", Result, "Invalid build-doc command");

      Util.Tests.Assert_Exists (T, "wiki/blog.wiki");
      Util.Tests.Assert_Exists (T, "wiki/user-user_query.wiki");
   end Test_Build_Doc;

   --  ------------------------------
   --  Test generate command (XMI enum).
   --  ------------------------------
   procedure Test_Generate_XMI_Enum (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-enum.xmi", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-enums.ads");
   end Test_Generate_XMI_Enum;

   --  ------------------------------
   --  Test generate command (XMI Ada Bean).
   --  ------------------------------
   procedure Test_Generate_XMI_Bean (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-beans.xmi", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-beans.ads");
   end Test_Generate_XMI_Bean;

   --  ------------------------------
   --  Test generate command (XMI Ada Table).
   --  ------------------------------
   procedure Test_Generate_XMI_Table (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-table.xmi", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-tables.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-tables.adb");
   end Test_Generate_XMI_Table;

   --  ------------------------------
   --  Test generate command (XMI Associations between Tables).
   --  ------------------------------
   procedure Test_Generate_XMI_Association (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-associations.xmi", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-associations.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-associations.adb");
   end Test_Generate_XMI_Association;

   --  ------------------------------
   --  Test generate command using the ArgoUML file directly (runs unzip -cq and parse the output).
   --  ------------------------------
   procedure Test_Generate_Zargo_Association (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-associations.zargo", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-associations.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-associations.adb");
   end Test_Generate_Zargo_Association;

   --  ------------------------------
   --  Test UML with several tables that have dependencies between each of them (non circular).
   --  ------------------------------
   procedure Test_Generate_Zargo_Dependencies (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-dependencies.zargo", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-dependencies.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-dependencies.adb");
   end Test_Generate_Zargo_Dependencies;

   --  ------------------------------
   --  Test UML with several tables in several packages (non circular).
   --  ------------------------------
   procedure Test_Generate_Zargo_Packages (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute (Dynamo & " generate ../regtests/uml/dynamo-test-packages.zargo", Result);

      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-a.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-a.adb");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-b.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-b.adb");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-c.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-c.adb");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-e.ads");
      Util.Tests.Assert_Exists (T, "src/model/gen-tests-packages-e.adb");
   end Test_Generate_Zargo_Packages;

   --  ------------------------------
   --  Test UML with several errors in the UML model.
   --  ------------------------------
   procedure Test_Generate_Zargo_Errors (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
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

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("make", Result);

      pragma Warnings (Off, "condition is always False");
      if Util.Systems.Os.Directory_Separator = '\' then
         Util.Tests.Assert_Exists (T, "bin/test-server.exe");
      else
         Util.Tests.Assert_Exists (T, "bin/test-server");
      end if;
      pragma Warnings (On, "condition is always False");

   end Test_Build;

   --  ------------------------------
   --  Test GNAT compilation of the generated model files.
   --  ------------------------------
   procedure Test_Build_Model (T : in out Test) is

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Directories.Copy_File (Source_Name => "../regtests/check_build/check_build.gpr",
                                 Target_Name => "check_build.gpr");

      T.Execute ("gnatmake -p -Pcheck_build", Result);

      pragma Warnings (Off, "condition is always False");
      if Util.Systems.Os.Directory_Separator = '\' then
         Util.Tests.Assert_Exists (T, "bin/test-server.exe");
      else
         Util.Tests.Assert_Exists (T, "bin/test-server");
      end if;
      pragma Warnings (On, "condition is always False");

   end Test_Build_Model;

end Gen.Integration.Tests;
