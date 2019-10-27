-----------------------------------------------------------------------
--  gen-commands-info -- Collect and give information about the project
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2017, 2018, 2019 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Directories;

with Gen.Utils;
with Gen.Utils.GNAT;
with Gen.Model.Projects;

with Util.Strings.Sets;
with Util.Files;
package body Gen.Commands.Info is

   --  ------------------------------
   --  Execute the command with the arguments.
   --  ------------------------------
   overriding
   procedure Execute (Cmd       : in out Command;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name, Args);

      procedure Collect_Directories (List   : in Gen.Utils.String_List.Vector;
                                     Result : out Gen.Utils.String_List.Vector);

      procedure Print_Model_File (Name : in String;
                                  File : in String;
                                  Done : out Boolean);

      procedure Print_GNAT_Projects (Project : in out Gen.Model.Projects.Project_Definition);

      procedure Print_Dynamo_Projects (Project : in out Model.Projects.Root_Project_Definition);

      procedure Print_Modules (Project : in out Gen.Model.Projects.Project_Definition'Class;
                               Indent  : in Ada.Text_IO.Positive_Count);

      procedure Print_Project (Project : in out Gen.Model.Projects.Root_Project_Definition);

      procedure Print_Project_List (Indent  : in Ada.Text_IO.Positive_Count;
                                    List    : in Gen.Model.Projects.Project_Vectors.Vector);

      List  : Gen.Utils.String_List.Vector;
      Names : Util.Strings.Sets.Set;

      procedure Collect_Directories (List : in Gen.Utils.String_List.Vector;
                                     Result : out Gen.Utils.String_List.Vector) is

         procedure Add_Model_Dir (Base_Dir : in String;
                                  Dir : in String);

         procedure Add_Model_Dir (Base_Dir : in String;
                                  Dir : in String) is
            Path : constant String := Util.Files.Compose (Base_Dir, Dir);
         begin
            if not Result.Contains (Path) and then Ada.Directories.Exists (Path) then
               Result.Append (Path);
            end if;
         end Add_Model_Dir;

         Iter : Gen.Utils.String_List.Cursor := List.First;
      begin
         while Gen.Utils.String_List.Has_Element (Iter) loop
            declare
               Path : constant String := Gen.Utils.String_List.Element (Iter);
               Dir  : constant String := Ada.Directories.Containing_Directory (Path);
            begin
               Add_Model_Dir (Dir, "db");
               Add_Model_Dir (Dir, "db/regtests");
               Add_Model_Dir (Dir, "db/samples");
            end;
            Gen.Utils.String_List.Next (Iter);
         end loop;
      end Collect_Directories;

      procedure Print_Model_File (Name : in String;
                                  File : in String;
                                  Done : out Boolean) is
         pragma Unreferenced (Name);
      begin
         Ada.Text_IO.Put ("    ");
         Ada.Text_IO.Put_Line (File);
         Done := False;
      end Print_Model_File;

      --  ------------------------------
      --  Print the list of GNAT projects used by the main project.
      --  ------------------------------
      procedure Print_GNAT_Projects (Project : in out Gen.Model.Projects.Project_Definition) is
         use Gen.Utils.GNAT;

         Iter : Project_Info_Vectors.Cursor := Project.Project_Files.First;
         Info : Project_Info;
      begin
         if Project_Info_Vectors.Has_Element (Iter) then
            Ada.Text_IO.Put_Line ("GNAT project files:");
            while Project_Info_Vectors.Has_Element (Iter) loop
               Ada.Text_IO.Put ("   ");
               Info := Project_Info_Vectors.Element (Iter);
               Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Info.Path));
               Project_Info_Vectors.Next (Iter);
            end loop;
         end if;
      end Print_GNAT_Projects;

      --  ------------------------------
      --  Print the list of Dynamo modules
      --  ------------------------------
      procedure Print_Project_List (Indent  : in Ada.Text_IO.Positive_Count;
                                    List    : in Gen.Model.Projects.Project_Vectors.Vector) is
         use Gen.Model.Projects;
         use type Ada.Text_IO.Positive_Count;

         Iter : Project_Vectors.Cursor := List.First;
         Ref  : Model.Projects.Project_Reference;
      begin
         while Project_Vectors.Has_Element (Iter) loop
            Ref := Project_Vectors.Element (Iter);
            Ada.Text_IO.Set_Col (Indent);
            Ada.Text_IO.Put ("   ");
            Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Ref.Name));
            Ada.Text_IO.Set_Col (Indent + 30);
            if Ref.Project /= null then
               Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Ref.Project.Path));
            else
               Ada.Text_IO.Put_Line ("?");
            end if;
            Project_Vectors.Next (Iter);
         end loop;
      end Print_Project_List;

      --  ------------------------------
      --  Print the list of Dynamo modules
      --  ------------------------------
      procedure Print_Modules (Project : in out Gen.Model.Projects.Project_Definition'Class;
                               Indent  : in Ada.Text_IO.Positive_Count) is
         use Gen.Model.Projects;
         use type Ada.Text_IO.Positive_Count;

         Iter : Project_Vectors.Cursor := Project.Modules.First;
         Ref  : Model.Projects.Project_Reference;
      begin
         if not Project.Modules.Is_Empty then
            Ada.Text_IO.Set_Col (Indent);
            Ada.Text_IO.Put_Line ("Dynamo plugins:");
            Print_Project_List (Indent, Project.Modules);

            Print_Project_List (Indent, Project.Dependencies);
            Iter := Project.Modules.First;
            while Project_Vectors.Has_Element (Iter) loop
               Ref := Project_Vectors.Element (Iter);
               if Ref.Project /= null and then not Ref.Project.Modules.Is_Empty then
                  declare
                     Name : constant String := Ada.Strings.Unbounded.To_String (Ref.Name);
                  begin
                     Ada.Text_IO.Set_Col (Indent);
                     if Names.Contains (Name) then
                        Ada.Text_IO.Put_Line ("!! " & Name);
                     else
                        Names.Insert (Name);
                        Ada.Text_IO.Put_Line ("== " & Name);
                        Print_Modules (Ref.Project.all, Indent + 4);
                        Names.Delete (Name);
                     end if;
                  end;
               end if;
               Project_Vectors.Next (Iter);
            end loop;
         end if;
      end Print_Modules;

      --  ------------------------------
      --  Print the list of Dynamo projects used by the main project.
      --  ------------------------------
      procedure Print_Dynamo_Projects (Project : in out Model.Projects.Root_Project_Definition) is
         Iter : Gen.Utils.String_List.Cursor := Project.Dynamo_Files.First;
      begin
         if Gen.Utils.String_List.Has_Element (Iter) then
            Ada.Text_IO.Put_Line ("Dynamo project files:");
            while Gen.Utils.String_List.Has_Element (Iter) loop
               Ada.Text_IO.Put ("   ");
               Ada.Text_IO.Put_Line (Gen.Utils.String_List.Element (Iter));
               Gen.Utils.String_List.Next (Iter);
            end loop;
         end if;
      end Print_Dynamo_Projects;

      procedure Print_Project (Project : in out Gen.Model.Projects.Root_Project_Definition) is
      begin
         Print_GNAT_Projects (Gen.Model.Projects.Project_Definition (Project));
         Print_Dynamo_Projects (Project);

         Print_Modules (Project, 1);
         declare
            Model_Dirs : Gen.Utils.String_List.Vector;
         begin
            Collect_Directories (List, Model_Dirs);
            declare
               Iter : Gen.Utils.String_List.Cursor := Model_Dirs.First;
            begin
               Ada.Text_IO.Put_Line ("ADO model files:");
               while Gen.Utils.String_List.Has_Element (Iter) loop
                  Ada.Text_IO.Put ("   ");
                  Ada.Text_IO.Put_Line (Gen.Utils.String_List.Element (Iter));
                  Util.Files.Iterate_Files_Path (Pattern => "*.xml",
                                                 Path    => Gen.Utils.String_List.Element (Iter),
                                                 Process => Print_Model_File'Access);
                  Gen.Utils.String_List.Next (Iter);
               end loop;
            end;
         end;
      end Print_Project;

   begin
      Generator.Read_Project ("dynamo.xml", True);

      Generator.Update_Project (Print_Project'Access);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Cmd       : in out Command;
                   Name      : in String;
                   Generator : in out Gen.Generator.Handler) is
      pragma Unreferenced (Cmd, Name, Generator);
   begin
      Ada.Text_IO.Put_Line ("info: Print information about the current project");
      Ada.Text_IO.Put_Line ("Usage: info");
      Ada.Text_IO.New_Line;
   end Help;

end Gen.Commands.Info;
