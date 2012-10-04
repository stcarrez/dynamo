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
                      Result  : out Ada.Strings.Unbounded.Unbounded_String) is
      P       : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer  : Util.Streams.Buffered.Buffered_Stream;
   begin
      P.Open (Command, Util.Processes.READ);

      --  Write on the process input stream.
      Buffer.Initialize (null, P'Unchecked_Access, 8192);
      Buffer.Read (Result);
      P.Close;
      Log.Info ("Command result: {0}", Result);
      Util.Tests.Assert_Equals (T, 0, P.Get_Exit_Status, "Command '" & Command & "' failed");
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

end Gen.Integration.Tests;
