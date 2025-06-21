-----------------------------------------------------------------------
--  gen-commands-project -- Project creation command for dynamo
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2018, 2019, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Directories;
with Ada.Text_IO;
with Gen.Artifacts;
with GNAT.Command_Line;

with Util.Log.Loggers;
with Util.Strings.Transforms;
package body Gen.Commands.Project is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Gen.Commands.Project");

   --  ------------------------------
   --  Generator Command
   --  ------------------------------
   function Get_Name_From_Email (Email : in String) return String;

   --  ------------------------------
   --  Get the user name from the email address.
   --  Returns the possible user name from his email address.
   --  ------------------------------
   function Get_Name_From_Email (Email : in String) return String is
      Pos : Natural := Util.Strings.Index (Email, '<');
   begin
      if Pos > 0 then
         return Email (Email'First .. Pos - 1);
      end if;
      Pos := Util.Strings.Index (Email, '@');
      if Pos > 0 then
         return Email (Email'First .. Pos - 1);
      else
         return Email;
      end if;
   end Get_Name_From_Email;

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Name, Args);
      use GNAT.Command_Line;

      Web_Flag  : aliased Boolean := False;
      Tool_Flag : aliased Boolean := False;
      Ado_Flag  : aliased Boolean := False;
      Gtk_Flag  : aliased Boolean := False;
      Lib_Flag  : aliased Boolean := False;
   begin
      --  If a dynamo.xml file exists, read it.
      if Ada.Directories.Exists ("dynamo.xml") then
         Generator.Read_Project ("dynamo.xml");
      else
         Generator.Set_Project_Property ("license", "apache");
         Generator.Set_Project_Property ("author", "unknown");
         Generator.Set_Project_Property ("author_email", "unknown@company.com");
      end if;
      --  Parse the command line
      loop
         case Getopt ("l: ? -lib -web -tool -ado -gtk") is
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-web" then
               Web_Flag := True;
            elsif Full_Switch = "-tool" then
               Tool_Flag := True;
            elsif Full_Switch = "-ado" then
               Ado_Flag := True;
            elsif Full_Switch = "-lib" then
               Lib_Flag := True;
            elsif Full_Switch = "-gtk" then
               Gtk_Flag := True;
            end if;

         when 'l' =>
            declare
               L : constant String := Util.Strings.Transforms.To_Lower_Case (Parameter);
            begin
               Log.Info ("License {0}", L);

               if L = "apache" then
                  Generator.Set_Project_Property ("license", "apache");
               elsif L = "gpl" then
                  Generator.Set_Project_Property ("license", "gpl");
               elsif L = "gpl3" then
                  Generator.Set_Project_Property ("license", "gpl3");
               elsif L = "mit" then
                  Generator.Set_Project_Property ("license", "mit");
               elsif L = "bsd3" then
                  Generator.Set_Project_Property ("license", "bsd3");
               elsif L = "proprietary" then
                  Generator.Set_Project_Property ("license", "proprietary");
               else
                  Generator.Error ("Invalid license: {0}", L);
                  Generator.Error ("Valid licenses: apache, gpl, gpl3, mit, bsd3, proprietary");
                  return;
               end if;
            end;

         when others =>
            null;
         end case;
      end loop;
      if not Web_Flag and not Ado_Flag and not Tool_Flag and not Gtk_Flag then
         Web_Flag := True;
      end if;
      declare
         Name  : constant String := Get_Argument;
         Arg2  : constant String := Get_Argument;
         Arg3  : constant String := Get_Argument;
      begin
         if Name'Length = 0 then
            Generator.Error ("Missing project name");
            Cmd.Usage (Name, Generator);
            return;
         end if;

         if Util.Strings.Index (Arg2, '@') > Arg2'First then
            Generator.Set_Project_Property ("author_email", Arg2);
            if Arg3'Length = 0 then
               Generator.Set_Project_Property ("author", Get_Name_From_Email (Arg2));
            else
               Generator.Set_Project_Property ("author", Arg3);
            end if;

         elsif Util.Strings.Index (Arg3, '@') > Arg3'First then
            Generator.Set_Project_Property ("author", Arg2);
            Generator.Set_Project_Property ("author_email", Arg3);

         elsif Arg3'Length > 0 then
            Generator.Error ("The last argument should be the author's email address.");
            Cmd.Usage (Name, Generator);
            return;
         end if;

         Generator.Set_Project_Property ("is_web", Boolean'Image (Web_Flag));
         Generator.Set_Project_Property ("is_tool", Boolean'Image (Tool_Flag));
         Generator.Set_Project_Property ("is_ado", Boolean'Image (Ado_Flag));
         Generator.Set_Project_Property ("is_gtk", Boolean'Image (Gtk_Flag));
         Generator.Set_Project_Property ("is_lib", Boolean'Image (Lib_Flag));
         Generator.Set_Project_Name (Name);
         Generator.Set_Force_Save (False);
         if Ado_Flag then
            Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project-ado");
         elsif Gtk_Flag then
            Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project-gtk");
         elsif Tool_Flag then
            Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project-tool");
         elsif Lib_Flag then
            Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project-lib");
         else
            Gen.Generator.Generate_All (Generator, Gen.Artifacts.ITERATION_TABLE, "project");
         end if;

         Generator.Save_Project;
         Ada.Text_IO.Put_Line ("Your project is now created.");
         Ada.Text_IO.Put_Line ("  make generate build");
      end;
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
      Put_Line ("create-project: Create a new Ada Web Application project");
      Put_Line ("Usage: create-project [-l apache|gpl|gpl3|mit|bsd3|proprietary] [--web] [--tool]"
                & " [--lib] [--ado] [--gtk] NAME [AUTHOR] [EMAIL]");
      New_Line;
      Put_Line ("  Creates a new AWA application with the name passed in NAME.");
      Put_Line ("  The application license is controlled with the -l option. ");
      Put_Line ("  License headers can use either the Apache, the MIT license, the BSD 3 clauses");
      Put_Line ("  license, the GNU licenses or a proprietary license.");
      Put_Line ("  The author's name and email addresses are also reported in generated files.");
      New_Line;
      Put_Line ("  --web   Generate a Web application (the default)");
      Put_Line ("  --tool  Generate a command line tool");
      Put_Line ("  --ado   Generate a database tool operation for ADO");
      Put_Line ("  --gtk   Generate a GtkAda project");
      Put_Line ("  --lib   Generate a library project");
   end Help;

end Gen.Commands.Project;
