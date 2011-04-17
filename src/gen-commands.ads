-----------------------------------------------------------------------
--  gen-commands -- Commands for dynamo
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

with Gen.Generator;
package Gen.Commands is

   --  ------------------------------
   --  Command
   --  ------------------------------
   type Command is abstract tagged private;
   type Command_Access is access all Command'Class;

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Command;
                      Generator : in out Gen.Generator.Handler) is abstract;

   --  Write the help associated with the command.
   procedure Help (Cmd : in Command) is abstract;

   --  Write the command usage.
   procedure Usage (Cmd : in Command);

   --  ------------------------------
   --  Help Command
   --  ------------------------------
   type Help_Command is new Command with private;

   --  Execute the command with the arguments.
   procedure Execute (Cmd       : in Help_Command;
                      Generator : in out Gen.Generator.Handler);

   --  Write the help associated with the command.
   procedure Help (Cmd : in Help_Command);

   --  Register the command under the given  name.
   procedure Add_Command (Cmd  : in Command_Access;
                          Name : in String);

   --  Find the command having the given name.
   function Find_Command (Name : in String) return Command_Access;

   --  Print dynamo usage
   procedure Usage;

private

   package Command_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
                                      Element_Type => Command_Access,
                                      "<"          => Ada.Strings.Unbounded."<");

   type Command is abstract tagged null record;

   type Help_Command is new Command with null record;

end Gen.Commands;
