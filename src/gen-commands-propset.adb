-----------------------------------------------------------------------
--  gen-commands-propset -- Set a property on dynamo project
--  Copyright (C) 2011, 2012, 2017, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;

package body Gen.Commands.Propset is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
   begin
      if Args.Get_Count /= 2 then
         Cmd.Usage (Name, Generator);
         return;
      end if;

      Generator.Read_Project ("dynamo.xml", True);
      Generator.Set_Project_Property (Args.Get_Argument (1), Args.Get_Argument (2));
      Generator.Save_Project;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Cmd       : in out Command;
                   Name      : in String;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name, Generator);
      use Ada.Text_IO;
   begin
      Put_Line ("propset: Set the value of a property in the dynamo project file");
      Put_Line ("Usage: propset NAME VALUE");
      New_Line;
   end Help;

end Gen.Commands.Propset;
