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

with Util.Log.Loggers;
with Util.Tests;
with Util.Test_Caller;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Processes;
package body Gen.Integration.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Gen.Integration.Tests");

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
   end Add_Tests;

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
      Ada.Directories.Delete_Tree ("test-app");

      T.Execute ("bin/dynamo -o test-app create-project -l apache test", Result);
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
      Util.Tests.Assert_Matches (T, ".*Generating file.*test.properties", Result,
                                 "Invalid configure");
   end Test_Configure;

   --  ------------------------------
   --  Test propset command.
   --  ------------------------------
   procedure Test_Change_Property (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/dynamo -o test-app propset author Druss", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*test.properties", Result,
                                 "Invalid configure");
      T.Execute ("bin/dynamo -o test-app propset author_email Druss@drenai.com", Result);
      T.Execute ("bin/dynamo -o test-app propset license Apache", Result);
   end Test_Change_Property;

   --  ------------------------------
   --  Test add-module command.
   --  ------------------------------
   procedure Test_Add_Module (T : in out Test) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/dynamo -o test-app add-module blog", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*model/test-blog-model.ads", Result,
                                 "Invalid add-module");

      T.Execute ("bin/dynamo -o test-app add-module user", Result);
      Util.Tests.Assert_Matches (T, ".*Generating file.*model/test-user-model.ads", Result,
                                 "Invalid add-module");
   end Test_Add_Module;

end Gen.Integration.Tests;
