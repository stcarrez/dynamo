-----------------------------------------------------------------------
--  gen-commands-propset -- Set a property on dynamo project
--  Copyright (C) 2011, 2017, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package Gen.Commands.Propset is

   --  ------------------------------
   --  Propset Command
   --  ------------------------------
   --  This command sets a property in the dynamo project configuration.
   type Command is new Gen.Commands.Command with null record;

   --  Execute the command with the arguments.
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler);

   --  Write the help associated with the command.
   overriding
   procedure Help (Cmd       : in out Command;
                   Name      : in String;
                   Generator : in out Gen.Generator.Handler);

end Gen.Commands.Propset;
