------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                A S I S . C O M P I L A T I O N _ U N I T S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2009, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Asis.Errors;             use Asis.Errors;
with Asis.Exceptions;         use Asis.Exceptions;
with Asis.Extensions;         use Asis.Extensions;
with Asis.Implementation;     use Asis.Implementation;

with Asis.Set_Get;            use  Asis.Set_Get;

with A4G.A_Opt;               use A4G.A_Opt;
with A4G.A_Output;            use A4G.A_Output;
with A4G.Vcheck;              use A4G.Vcheck;
with A4G.Get_Unit;            use A4G.Get_Unit;
with A4G.Contt;               use A4G.Contt;
with A4G.Contt.UT;            use A4G.Contt.UT;

with Lib;                     use Lib;

package body Asis.Compilation_Units is

   Package_Name : constant String := "Asis.Compilation_Units.";
   LT           : Wide_String renames A4G.A_Types.Asis_Wide_Line_Terminator;

   function "=" (Left, Right : Compilation_Unit) return Boolean
      renames Asis.Set_Get."=";

   ----------------------
   -- Attribute_Values --
   ----------------------

   function Attribute_Values
     (Compilation_Unit : Asis.Compilation_Unit;
      Attribute        : Wide_String)
      return             Wide_String
   is
   begin
      pragma Unreferenced (Attribute);
      Check_Validity (Compilation_Unit, Package_Name & "Attribute_Values");

      return Nil_Asis_Wide_String;
   end Attribute_Values;

   -------------------------------
   -- Attribute_Value_Delimiter --
   -------------------------------

   function Attribute_Value_Delimiter return Wide_String is
   begin
      return Asis_Wide_Line_Terminator;
   end Attribute_Value_Delimiter;

   -------------------------
   -- Can_Be_Main_Program --
   -------------------------

   function Can_Be_Main_Program
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Boolean
   is
      Result : Boolean := False;
   begin

      Check_Validity (Compilation_Unit, Package_Name & "Can_Be_Main_Program");

      Reset_Context (Encl_Cont_Id (Compilation_Unit));

      case  Kind (Compilation_Unit) is

         when A_Procedure          |
              A_Function           |
              A_Procedure_Body     |
              A_Function_Body      |
              A_Procedure_Renaming |
              A_Function_Renaming  |
             A_Procedure_Instance  |
             A_Function_Instance  =>

            Result := Is_Main_Unit (Compilation_Unit);

         when others =>
            null;
      end case;

      return Result;
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Can_Be_Main_Program");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Can_Be_Main_Program",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Can_Be_Main_Program;

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   function Compilation_Command_Line_Options
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Arg_Cont_Id    : Context_Id;

      Arg_Len   : Natural := 0; -- total length of compilation switches
      Arg_Count : Nat     := 0; -- total number of compilation switches
      Arg_Ptr   : String_Ptr;

      Corr_Main_Unit_Id       : Unit_Id               := Nil_Unit;
      Corresponding_Main_Unit : Asis.Compilation_Unit := Nil_Compilation_Unit;
   begin
      Check_Validity
        (Compilation_Unit, Package_Name & "Compilation_Command_Line_Options");

      Arg_Kind := Kind (Compilation_Unit);

      if Arg_Kind not in A_Procedure .. A_Protected_Body_Subunit then
         return Nil_Asis_Wide_String;
      end if;

      Arg_Cont_Id := Encl_Cont_Id (Compilation_Unit);
      Reset_Context (Arg_Cont_Id);

      if Is_Main_Unit_In_Tree (Compilation_Unit) then
         Corresponding_Main_Unit := Compilation_Unit;
      else
         Arg_Unit_Id       := Get_Unit_Id  (Compilation_Unit);

         --  Here we have to check if the argument unit should to
         --  inherit command line options from some main unit:

         if Arg_Kind in A_Procedure .. A_Package then
            --  Here we have to check if the corresponding body is a
            --  main unit of some compilation:
            Corr_Main_Unit_Id := Get_Body (Arg_Cont_Id, Arg_Unit_Id);

         elsif Arg_Kind in A_Procedure_Body_Subunit ..
               A_Protected_Body_Subunit
         then
            --  We have to go to ancestor body and to check if it is a main
            --  unit of some compilation
            Corr_Main_Unit_Id :=
               Get_Subunit_Parent_Body (Arg_Cont_Id, Arg_Unit_Id);

            while Class (Arg_Cont_Id, Corr_Main_Unit_Id) = A_Separate_Body
            loop

               Corr_Main_Unit_Id :=
                  Get_Subunit_Parent_Body (Arg_Cont_Id, Corr_Main_Unit_Id);

            end loop;

         end if;

         Corresponding_Main_Unit :=
            Get_Comp_Unit (Corr_Main_Unit_Id, Arg_Cont_Id);

         if not Is_Main_Unit_In_Tree (Corresponding_Main_Unit) then
            Corresponding_Main_Unit := Nil_Compilation_Unit;
         end if;

      end if;

      if Is_Nil (Corresponding_Main_Unit) then
         return Nil_Asis_Wide_String;
      else

         Reset_Main_Tree (Corresponding_Main_Unit);

         --  First, find the length of the string to return
         Find_Arguments_Length : loop
            Arg_Ptr := Get_Compilation_Switch (Arg_Count + 1);

            exit Find_Arguments_Length when Arg_Ptr = null;

            Arg_Count := Arg_Count + 1;
            Arg_Len := Arg_Len + Arg_Ptr'Length + 1;
         end loop Find_Arguments_Length;

         if Arg_Count > 0 then
            Arg_Len := Arg_Len - 1;
         end if;

         declare
            Result       : String (1 .. Arg_Len);
            Next_Pos     : Natural := 1;
            Next_Arg_Len : Natural;
         begin

            --  Should be rewritten on the base of ASIS string buffer???
            for Next_Arg in 1 .. Arg_Count loop
               Arg_Ptr := Get_Compilation_Switch (Next_Arg);
               Next_Arg_Len := Arg_Ptr'Length;

               Result (Next_Pos .. Next_Pos + Next_Arg_Len - 1) := Arg_Ptr.all;

               Next_Pos := Next_Pos + Next_Arg_Len;

               if Next_Arg < Arg_Count then
                  Result (Next_Pos) := ' ';

                  Next_Pos := Next_Pos + 1;
               end if;

            end loop;

            return To_Program_Text (Result);
         end;
      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Compilation_Command_Line_Options");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Compilation_Command_Line_Options",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Compilation_Command_Line_Options;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (The_Context : Asis.Context)
      return        Asis.Compilation_Unit_List
   is
      Res_Cont_Id    : constant Context_Id := Get_Cont_Id (The_Context);
      Cont_Tree_Mode : Tree_Mode;
   begin
      Check_Validity (The_Context, Package_Name & "Compilation_Units");

      Cont_Tree_Mode := Tree_Processing_Mode (Res_Cont_Id);

      if not (Cont_Tree_Mode = Pre_Created or else
              Cont_Tree_Mode = Incremental)
      then
         Set_Status
           (Status    => Use_Error,
            Diagnosis =>
              "Asis.Compilation_Units.Compilation_Units can not be used " &
              LT & "for dynamic ASIS Context");

         raise ASIS_Failed;
      end if;

      Reset_Context (Res_Cont_Id);

      declare
         Result_Len : constant Natural :=
           Lib_Unit_Decls (Res_Cont_Id) + Comp_Unit_Bodies (Res_Cont_Id);

         Result : Compilation_Unit_List (1 .. Result_Len);
      begin
         --  We have to skip A_Configuration_Compilation unit, it is the second
         --  unit in the table

         Result (1) := Get_Comp_Unit (Standard_Id, Res_Cont_Id);

         for I in 2 .. Result_Len loop
            Result (I) :=
              Get_Comp_Unit (First_Unit_Id + Unit_Id (I), Res_Cont_Id);
         end loop;

         return Result;
      end;

   exception
      when ASIS_Inappropriate_Context =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Units");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Compilation_Units",
            Ex         => Ex);
   end Compilation_Units;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   function Compilation_Unit_Bodies
     (The_Context : Asis.Context)
      return        Asis.Compilation_Unit_List
   is
      Res_Cont_Id    : constant Context_Id := Get_Cont_Id (The_Context);
      Cont_Tree_Mode : Tree_Mode;
   begin
      Check_Validity (The_Context, Package_Name & "Compilation_Unit_Bodies");

      Cont_Tree_Mode := Tree_Processing_Mode (Res_Cont_Id);

      if not (Cont_Tree_Mode = Pre_Created or else
              Cont_Tree_Mode = Incremental)
      then
         Set_Status
           (Status    => Use_Error,
            Diagnosis =>
              "Asis.Compilation_Units.Compilation_Unit_Bodies can not be used "
              & LT & "for dynamic ASIS Context");

         raise ASIS_Failed;

      end if;

      Reset_Context (Res_Cont_Id);

      declare
         Result_Len : constant Natural := Comp_Unit_Bodies (Res_Cont_Id);
         Result     : Compilation_Unit_List (1 .. Result_Len);
         L_U_Body   : Unit_Id := First_Body;
      begin
         for I in 1 .. Result_Len loop
            Result (I) := Get_Comp_Unit (L_U_Body,  Res_Cont_Id);
            L_U_Body   := Next_Body (L_U_Body);
         end loop;

         return Result;
      end;
   exception
      when ASIS_Inappropriate_Context =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Unit_Bodies");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Compilation_Unit_Bodies",
            Ex         => Ex);
   end Compilation_Unit_Bodies;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   function Compilation_Unit_Body
     (Name        : Wide_String;
      The_Context : Asis.Context)
      return        Asis.Compilation_Unit
   is
      Result_Id   : Unit_Id;
      Result_Cont : Context_Id;
   begin

      Check_Validity (The_Context, Package_Name & "Compilation_Unit_Body");

      Result_Cont := Get_Cont_Id (The_Context);
      Reset_Context (Result_Cont);
      Result_Id := Get_One_Unit (Name, Result_Cont, Spec => False);

      return Get_Comp_Unit (Result_Id, Result_Cont);

   exception
      when ASIS_Inappropriate_Context =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Unit_Body");
         end if;

         raise;

      when Ex : others      =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Compilation_Unit_Body (" &
                          To_String (Name) & ")",
            Ex         => Ex);
   end Compilation_Unit_Body;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Library_Item : Asis.Compilation_Unit)
      return         Asis.Compilation_Unit
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Result_Unit_Id : Unit_Id;
      Result_Cont_Id : Context_Id;
      Cont_Tree_Mode : Tree_Mode;
   begin
      Check_Validity (Library_Item, Package_Name & "Corresponding_Body");

      Result_Cont_Id := Encl_Cont_Id (Library_Item);
      Reset_Context (Result_Cont_Id);
      Arg_Kind := Kind (Library_Item);

      if not (Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = An_Unknown_Unit              or else
              Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body               or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body_Subunit     or else
              Arg_Kind = A_Function_Body_Subunit      or else
              Arg_Kind = A_Package_Body_Subunit       or else
              Arg_Kind = A_Task_Body_Subunit          or else
              Arg_Kind = A_Protected_Body_Subunit     or else
              Arg_Kind = A_Nonexistent_Declaration    or else
              Arg_Kind = A_Nonexistent_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Body");
      end if;

      if Arg_Kind = A_Procedure_Body             or else
         Arg_Kind = A_Function_Body              or else
         Arg_Kind = A_Package_Body               or else
         Arg_Kind = A_Procedure_Instance         or else
         Arg_Kind = A_Function_Instance          or else
         Arg_Kind = A_Package_Instance           or else
         Arg_Kind = A_Procedure_Renaming         or else
         Arg_Kind = A_Function_Renaming          or else
         Arg_Kind = A_Package_Renaming           or else
         Arg_Kind = A_Generic_Procedure_Renaming or else
         Arg_Kind = A_Generic_Function_Renaming  or else
         Arg_Kind = A_Generic_Package_Renaming   or else
         Arg_Kind = A_Procedure_Body_Subunit     or else
         Arg_Kind = A_Function_Body_Subunit      or else
         Arg_Kind = A_Package_Body_Subunit       or else
         Arg_Kind = A_Task_Body_Subunit          or else
         Arg_Kind = A_Protected_Body_Subunit     or else
         Arg_Kind = A_Nonexistent_Declaration    or else
         Arg_Kind = A_Nonexistent_Body
      then
         return Library_Item;
      end if;

      if  (Arg_Kind = A_Package or else
           Arg_Kind = A_Generic_Package)
         and then
           not Asis.Set_Get.Is_Body_Required (Library_Item)
      then
         return Nil_Compilation_Unit;
      end if;

      Arg_Unit_Id    := Get_Unit_Id  (Library_Item);
      Cont_Tree_Mode := Tree_Processing_Mode (Result_Cont_Id);
      Result_Unit_Id := Get_Body (Result_Cont_Id, Arg_Unit_Id);

      if No (Result_Unit_Id) and then
         (Cont_Tree_Mode = On_The_Fly or else
          Cont_Tree_Mode = Mixed      or else
          Cont_Tree_Mode = Incremental)
      then
         --  as a last escape, we try to create the result body by
         --  compiling on the fly:
         Result_Unit_Id :=
           Get_One_Unit (Name    => Unit_Full_Name (Library_Item),
                         Context => Result_Cont_Id,
                         Spec    => False);
      end if;

      if No (Result_Unit_Id) then
         Result_Unit_Id := Get_Nonexistent_Unit (Result_Cont_Id);
      end if;

      return Get_Comp_Unit (Result_Unit_Id, Result_Cont_Id);

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Body");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Corresponding_Body",
            Ex         => Ex,
            Arg_CU     => Library_Item);
   end Corresponding_Body;

   function Corresponding_Body
     (Library_Item   : Asis.Compilation_Unit;
      The_Context    : Asis.Context)
      return           Asis.Compilation_Unit
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
   begin
      Check_Validity (The_Context, Package_Name & "Corresponding_Body");

      Check_Validity (Library_Item, Package_Name & "Corresponding_Body");

      Arg_Cont_Id := Encl_Cont_Id  (Library_Item);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Library_Item);

      if not (Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = An_Unknown_Unit              or else
              Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body               or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body_Subunit     or else
              Arg_Kind = A_Function_Body_Subunit      or else
              Arg_Kind = A_Package_Body_Subunit       or else
              Arg_Kind = A_Task_Body_Subunit          or else
              Arg_Kind = A_Protected_Body_Subunit     or else
              Arg_Kind = A_Nonexistent_Declaration    or else
              Arg_Kind = A_Nonexistent_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Body");
      end if;

      Arg_Unit_Id     := Get_Unit_Id (Library_Item);
      Result_Cont_Id  := Get_Cont_Id (The_Context);

      New_Arg_Unit_Id :=
        Get_Same_Unit (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then
         return Corresponding_Body
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else
         return Nil_Compilation_Unit;
      end if;

   exception
      when   ASIS_Inappropriate_Compilation_Unit
           | ASIS_Inappropriate_Context          =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Body");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Body",
            Ex          => Ex,
            Arg_CU      => Library_Item,
            Context_Par => True);
   end Corresponding_Body;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   function Corresponding_Children
     (Library_Unit : Asis.Compilation_Unit)
      return         Asis.Compilation_Unit_List
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Res_Cont_Id    : Context_Id;
      Cont_Tree_Mode : Tree_Mode;
   begin
      Check_Validity (Library_Unit, Package_Name & "Corresponding_Children");

      Res_Cont_Id := Encl_Cont_Id (Library_Unit);
      Reset_Context (Res_Cont_Id);
      Arg_Kind := Kind (Library_Unit);

      if not (Arg_Kind = A_Package               or else
              Arg_Kind = A_Generic_Package       or else
              Arg_Kind = A_Package_Instance)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Children");
      end if;

      Cont_Tree_Mode := Tree_Processing_Mode (Res_Cont_Id);

      if not (Cont_Tree_Mode = Pre_Created or else
              Cont_Tree_Mode = Incremental)
      then
         Set_Status
           (Status    => Use_Error,
            Diagnosis =>
              "Asis.Compilation_Units.Corresponding_Children can not be used "
             & LT & "for dynamic ASIS Context");

         raise ASIS_Failed;
      end if;

      Arg_Unit_Id := Get_Unit_Id  (Library_Unit);

      declare
         Result_Id_List : constant Unit_Id_List := Children (Arg_Unit_Id);
         Result_List    : constant Compilation_Unit_List :=
           Get_Comp_Unit_List (Result_Id_List, Res_Cont_Id);
      begin
         return Result_List;
      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Children");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Corresponding_Children",
            Ex         => Ex,
            Arg_CU     => Library_Unit);
   end Corresponding_Children;

   function Corresponding_Children
     (Library_Unit : Asis.Compilation_Unit;
      The_Context  : Asis.Context)
      return         Asis.Compilation_Unit_List
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
      Cont_Tree_Mode  : Tree_Mode;
   begin
      Check_Validity (The_Context,  Package_Name & "Corresponding_Children");
      Check_Validity (Library_Unit, Package_Name & "Corresponding_Children");

      Arg_Cont_Id := Encl_Cont_Id (Library_Unit);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Library_Unit);

      if not (Arg_Kind = A_Package         or else
              Arg_Kind = A_Generic_Package or else
              Arg_Kind = A_Package_Instance)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Children");
      end if;

      Result_Cont_Id := Get_Cont_Id  (The_Context);
      Cont_Tree_Mode := Tree_Processing_Mode (Result_Cont_Id);

      if not (Cont_Tree_Mode = Pre_Created or else
              Cont_Tree_Mode = Incremental)
      then
         Set_Status
           (Status    => Use_Error,
            Diagnosis =>
              "Asis.Compilation_Units.Corresponding_Children can not be used "
              & LT & "for dynamic ASIS Context");

         raise ASIS_Failed;
      end if;

      Arg_Unit_Id := Get_Unit_Id  (Library_Unit);

      New_Arg_Unit_Id :=
        Get_Same_Unit (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then
         return Corresponding_Children
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else
         return Nil_Compilation_Unit_List;
      end if;

   exception
      when   ASIS_Inappropriate_Compilation_Unit
           | ASIS_Inappropriate_Context          =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Children");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Children",
            Ex          => Ex,
            Arg_CU      => Library_Unit,
            Context_Par => True);
   end Corresponding_Children;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : Asis.Compilation_Unit)
      return         Asis.Compilation_Unit
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Result_Unit_Id : Unit_Id;
      Result_Cont_Id : Context_Id;
   begin
      Check_Validity
        (Library_Item, Package_Name & "Corresponding_Declaration");

      Result_Cont_Id := Encl_Cont_Id (Library_Item);
      Reset_Context (Result_Cont_Id);
      Arg_Kind := Kind (Library_Item);

      if not (Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body               or else
              Arg_Kind = An_Unknown_Unit              or else
              Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body_Subunit     or else
              Arg_Kind = A_Function_Body_Subunit      or else
              Arg_Kind = A_Package_Body_Subunit       or else
              Arg_Kind = A_Task_Body_Subunit          or else
              Arg_Kind = A_Protected_Body_Subunit     or else
              Arg_Kind = A_Nonexistent_Declaration    or else
              Arg_Kind = A_Nonexistent_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Declaration");
      end if;

      if Arg_Kind = A_Procedure                  or else
         Arg_Kind = A_Function                   or else
         Arg_Kind = A_Package                    or else
         Arg_Kind = A_Generic_Procedure          or else
         Arg_Kind = A_Generic_Function           or else
         Arg_Kind = A_Generic_Package            or else
         Arg_Kind = A_Procedure_Instance         or else
         Arg_Kind = A_Function_Instance          or else
         Arg_Kind = A_Package_Instance           or else
         Arg_Kind = A_Procedure_Renaming         or else
         Arg_Kind = A_Function_Renaming          or else
         Arg_Kind = A_Package_Renaming           or else
         Arg_Kind = A_Generic_Procedure_Renaming or else
         Arg_Kind = A_Generic_Function_Renaming  or else
         Arg_Kind = A_Generic_Package_Renaming   or else
         Arg_Kind = A_Procedure_Body_Subunit     or else --  ???
         Arg_Kind = A_Function_Body_Subunit      or else --  ???
         Arg_Kind = A_Package_Body_Subunit       or else --  ???
         Arg_Kind = A_Task_Body_Subunit          or else --  ???
         Arg_Kind = A_Protected_Body_Subunit     or else --  ???
         Arg_Kind = A_Nonexistent_Declaration    or else
         Arg_Kind = A_Nonexistent_Body                   --  ???
      then
         return Library_Item;
      end if;

      if (Arg_Kind = A_Procedure_Body or else
          Arg_Kind = A_Function_Body)
         and then
          Class (Library_Item) = A_Public_Declaration_And_Body
      then
         return Nil_Compilation_Unit;
      end if;

      Arg_Unit_Id    := Get_Unit_Id  (Library_Item);
      Result_Unit_Id := Get_Declaration (Result_Cont_Id, Arg_Unit_Id);

      return Get_Comp_Unit (Result_Unit_Id, Result_Cont_Id);

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Declaration");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Corresponding_Declaration",
            Ex         => Ex,
            Arg_CU     => Library_Item);
   end Corresponding_Declaration;

   function Corresponding_Declaration
    (Library_Item   : Asis.Compilation_Unit;
     The_Context    : Asis.Context)
     return           Asis.Compilation_Unit
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
   begin
      Check_Validity (The_Context, Package_Name & "Corresponding_Declaration");

      Check_Validity
        (Library_Item, Package_Name & "Corresponding_Declaration");

      Arg_Cont_Id     := Encl_Cont_Id  (Library_Item);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Library_Item);

      if not (Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body               or else
              Arg_Kind = An_Unknown_Unit              or else
              Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body_Subunit     or else
              Arg_Kind = A_Function_Body_Subunit      or else
              Arg_Kind = A_Package_Body_Subunit       or else
              Arg_Kind = A_Task_Body_Subunit          or else
              Arg_Kind = A_Protected_Body_Subunit     or else
              Arg_Kind = A_Nonexistent_Declaration    or else
              Arg_Kind = A_Nonexistent_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Declaration");
      end if;

      Arg_Unit_Id     := Get_Unit_Id   (Library_Item);
      Result_Cont_Id  := Get_Cont_Id   (The_Context);

      New_Arg_Unit_Id := Get_Same_Unit
                           (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then
         return Corresponding_Declaration
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else
         return Nil_Compilation_Unit;
      end if;

   exception
      when   ASIS_Inappropriate_Compilation_Unit
           | ASIS_Inappropriate_Context          =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Corresponding_Declaration");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Declaration",
            Ex          => Ex,
            Arg_CU      => Library_Item,
            Context_Par => True);
   end Corresponding_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   function Corresponding_Parent_Declaration
     (Library_Unit : Asis.Compilation_Unit)
      return         Asis.Compilation_Unit
   is
      Arg_Kind    : Asis.Unit_Kinds;
      Arg_Unit_Id : Unit_Id;
      Res_Cont_Id : Context_Id;
      Result_Id   : Unit_Id;
   begin

      Check_Validity
        (Library_Unit, Package_Name & "Corresponding_Parent_Declaration");

      Res_Cont_Id := Encl_Cont_Id (Library_Unit);
      Reset_Context (Res_Cont_Id);
      Arg_Kind := Kind (Library_Unit);

      if not (Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Parent_Declaration");
      end if;

      Arg_Unit_Id := Get_Unit_Id  (Library_Unit);
      Result_Id   := Get_Parent_Unit (Res_Cont_Id, Arg_Unit_Id);

      --  Result_Id cannot be Nil_Unit here

      return Get_Comp_Unit (Result_Id, Res_Cont_Id);

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Corresponding_Parent_Declaration");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Corresponding_Parent_Declaration",
            Ex         => Ex,
            Arg_CU     => Library_Unit);
   end Corresponding_Parent_Declaration;

   function Corresponding_Parent_Declaration
     (Library_Unit : Asis.Compilation_Unit;
      The_Context  : Asis.Context)
      return         Asis.Compilation_Unit
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
   begin
      Check_Validity
        (The_Context, Package_Name & "Corresponding_Parent_Declaration");

      Check_Validity
        (Library_Unit, Package_Name & "Corresponding_Parent_Declaration");

      Arg_Cont_Id := Encl_Cont_Id (Library_Unit);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Library_Unit);

      if not (Arg_Kind = A_Procedure                  or else
              Arg_Kind = A_Function                   or else
              Arg_Kind = A_Package                    or else
              Arg_Kind = A_Generic_Procedure          or else
              Arg_Kind = A_Generic_Function           or else
              Arg_Kind = A_Generic_Package            or else
              Arg_Kind = A_Procedure_Instance         or else
              Arg_Kind = A_Function_Instance          or else
              Arg_Kind = A_Package_Instance           or else
              Arg_Kind = A_Procedure_Renaming         or else
              Arg_Kind = A_Function_Renaming          or else
              Arg_Kind = A_Package_Renaming           or else
              Arg_Kind = A_Generic_Procedure_Renaming or else
              Arg_Kind = A_Generic_Function_Renaming  or else
              Arg_Kind = A_Generic_Package_Renaming   or else
              Arg_Kind = A_Procedure_Body             or else
              Arg_Kind = A_Function_Body              or else
              Arg_Kind = A_Package_Body)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Parent_Declaration");
      end if;

      Arg_Unit_Id     := Get_Unit_Id  (Library_Unit);
      Result_Cont_Id  := Get_Cont_Id  (The_Context);
      New_Arg_Unit_Id :=
        Get_Same_Unit (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then

         return Corresponding_Parent_Declaration
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else

         return Nil_Compilation_Unit;
      end if;

   exception
      when   ASIS_Inappropriate_Compilation_Unit
           | ASIS_Inappropriate_Context          =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Corresponding_Parent_Declaration");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Parent_Declaration",
            Ex          => Ex,
            Arg_CU      => Library_Unit,
            Context_Par => True);
   end Corresponding_Parent_Declaration;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   function Corresponding_Subunit_Parent_Body
     (Subunit : Asis.Compilation_Unit)
      return    Asis.Compilation_Unit
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Result_Unit_Id : Unit_Id;
      Result_Cont_Id : Context_Id;
   begin
      Check_Validity
        (Subunit, Package_Name & "Corresponding_Subunit_Parent_Body");

      Result_Cont_Id := Encl_Cont_Id (Subunit);
      Reset_Context (Result_Cont_Id);
      Arg_Kind := Kind (Subunit);

      if not (Arg_Kind = A_Procedure_Body_Subunit or else
              Arg_Kind = A_Function_Body_Subunit  or else
              Arg_Kind = A_Package_Body_Subunit   or else
              Arg_Kind = A_Task_Body_Subunit      or else
              Arg_Kind = A_Protected_Body_Subunit)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Subunit_Parent_Body");
      end if;

      Arg_Unit_Id    := Get_Unit_Id  (Subunit);
      Result_Unit_Id := Get_Subunit_Parent_Body (Result_Cont_Id, Arg_Unit_Id);

      return Get_Comp_Unit (Result_Unit_Id, Result_Cont_Id);

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Corresponding_Subunit_Parent_Body");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Corresponding_Subunit_Parent_Body",
            Ex         => Ex,
            Arg_CU     => Subunit);
   end Corresponding_Subunit_Parent_Body;

   function Corresponding_Subunit_Parent_Body
     (Subunit     : Asis.Compilation_Unit;
      The_Context : Asis.Context)
      return        Asis.Compilation_Unit
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
   begin
      Check_Validity
        (The_Context, Package_Name & "Corresponding_Subunit_Parent_Body");
      Check_Validity
        (Subunit, Package_Name & "Corresponding_Subunit_Parent_Body");

      Arg_Cont_Id := Encl_Cont_Id (Subunit);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Subunit);

      if not (Arg_Kind = A_Procedure_Body_Subunit or else
              Arg_Kind = A_Function_Body_Subunit  or else
              Arg_Kind = A_Package_Body_Subunit   or else
              Arg_Kind = A_Task_Body_Subunit      or else
              Arg_Kind = A_Protected_Body_Subunit)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Corresponding_Subunit_Parent_Body");
      end if;

      Arg_Unit_Id     := Get_Unit_Id  (Subunit);
      Result_Cont_Id  := Get_Cont_Id  (The_Context);
      New_Arg_Unit_Id := Get_Same_Unit
                           (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then

         return Corresponding_Subunit_Parent_Body
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else

         return Nil_Compilation_Unit;
      end if;
   exception
      when  ASIS_Inappropriate_Compilation_Unit
          | ASIS_Inappropriate_Context            =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call =>
              Package_Name & "Corresponding_Subunit_Parent_Body");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Corresponding_Subunit_Parent_Body",
            Ex          => Ex,
            Arg_CU      => Subunit,
            Context_Par => True);
   end Corresponding_Subunit_Parent_Body;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
      LT : String renames A4G.A_Types.ASIS_Line_Terminator;
   begin
      return To_Wide_String (LT & "Compilation Unit Debug_Image: "
             & Debug_String (Compilation_Unit));
   exception
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Debug_Image",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Debug_Image;

   -------------------------
   -- Enclosing_Container --
   -------------------------

   function Enclosing_Container
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Ada_Environments.Containers.Container
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Enclosing_Container");

      if Is_Nil (Compilation_Unit) then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Enclosing_Container");
      else
         --  For the currently implemented trivial Container model we have:
         return Asis.Ada_Environments.Containers.Defining_Containers
                  (Enclosing_Context (Compilation_Unit)) (1);
      end if;

   exception
      when  ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Enclosing_Container");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Enclosing_Container",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Enclosing_Container;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Context
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Enclosing_Context");

      if Is_Nil (Compilation_Unit) then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Enclosing_Context");
      else
         return Encl_Cont (Compilation_Unit);
      end if;

   exception
      when  ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Enclosing_Context");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Enclosing_Context",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Enclosing_Context;

   ------------
   -- Exists --
   ------------

   function Exists
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Boolean
   is
      Unit_Kind : Asis.Unit_Kinds;
   begin

      Check_Validity (Compilation_Unit, Package_Name & "Exists");

      Reset_Context (Encl_Cont_Id (Compilation_Unit));
      Unit_Kind := Kind (Compilation_Unit);

      return not (Unit_Kind = Not_A_Unit                 or else
                  Unit_Kind = A_Nonexistent_Declaration  or else
                  Unit_Kind = A_Nonexistent_Body);
   exception
      when  ASIS_Inappropriate_Compilation_Unit =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Exists");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Exists",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Exists;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Compilation_Unit : Asis.Compilation_Unit;
      Attribute        : Wide_String)
   return                Boolean
   is
   begin
      pragma Unreferenced (Attribute);

      Check_Validity (Compilation_Unit, Package_Name & "Has_Attribute");
      return False;
   end Has_Attribute;

   ----------------------
   -- Is_Body_Required --
   ----------------------

   function Is_Body_Required
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Boolean
   is
      Unit_Kind : constant Asis.Unit_Kinds := Kind (Compilation_Unit);
      Result    :          Boolean         := False;
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Is_Body_Required");
      Reset_Context (Encl_Cont_Id (Compilation_Unit));

      case Unit_Kind is
         when A_Package         |
              A_Generic_Package =>

            Result := Asis.Set_Get.Is_Body_Required (Compilation_Unit);
         when others =>
            null;
      end case;

      return Result;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Body_Required");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Is_Body_Required",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Is_Body_Required;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : Asis.Compilation_Unit;
      Right : Asis.Compilation_Unit)
      return  Boolean
   is
      Left_Unit_Id  : Unit_Id;
      Right_Unit_Id : Unit_Id;
      Left_Cont_Id  : Context_Id;
      Right_Cont_Id : Context_Id;
   begin
      Check_Validity (Left,  Package_Name & "Is_Equal");
      Check_Validity (Right, Package_Name & "Is_Equal");

      Left_Unit_Id  := Get_Unit_Id (Left);
      Right_Unit_Id := Get_Unit_Id (Right);

      if Left_Unit_Id = Nil_Unit and then Right_Unit_Id = Nil_Unit then
         return True;
      elsif (Right_Unit_Id = Nil_Unit and then Left_Unit_Id /= Nil_Unit)
            or else
            (Right_Unit_Id /= Nil_Unit and then Left_Unit_Id = Nil_Unit)
      then
         return False;
      end if;

      Left_Cont_Id  := Encl_Cont_Id (Left);
      Right_Cont_Id := Encl_Cont_Id (Right);

      if Left_Cont_Id = Right_Cont_Id then
         return Left_Unit_Id = Right_Unit_Id;
      else
         return Right_Unit_Id =
           Get_Same_Unit (Left_Cont_Id, Left_Unit_Id, Right_Cont_Id);
      end if;
   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Equal");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Is_Equal",
            Ex         => Ex,
            Arg_CU     => Left,
            Arg_CU_2   => Right);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : Asis.Compilation_Unit;
      Right : Asis.Compilation_Unit)
      return  Boolean
   is
      Left_Cont_Id  : Context_Id;
      Right_Cont_Id : Context_Id;
   begin

      Check_Validity (Left,  Package_Name & "Is_Identical");
      Check_Validity (Right, Package_Name & "Is_Identical");

      Left_Cont_Id  := Encl_Cont_Id (Left);
      Right_Cont_Id := Encl_Cont_Id (Right);

      return Left_Cont_Id = Right_Cont_Id and then Is_Equal (Left, Right);

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Is_Identical");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Is_Identical",
            Ex         => Ex,
            Arg_CU     => Left,
            Arg_CU_2   => Right);
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : Asis.Compilation_Unit) return Boolean is
   begin
      Check_Validity (Right, Package_Name & "Is_Nil");
      return Right = Nil_Compilation_Unit;
   end Is_Nil;

   function Is_Nil (Right : Asis.Compilation_Unit_List) return Boolean is
   begin
      return Right = Nil_Compilation_Unit_List;
   end Is_Nil;

   ------------------------------
   -- Library_Unit_Declaration --
   ------------------------------

   function Library_Unit_Declaration
     (Name        : Wide_String;
      The_Context : Asis.Context)
      return        Asis.Compilation_Unit
   is
      Result_Id   : Unit_Id;
      Result_Cont : Context_Id;
   begin

      Check_Validity (The_Context, Package_Name & "Library_Unit_Declaration");

      Result_Cont := Get_Cont_Id (The_Context);
      Reset_Context (Result_Cont);
      Result_Id := Get_One_Unit (Name, Result_Cont, Spec => True);

      return Get_Comp_Unit (Result_Id, Result_Cont);

   exception
      when ASIS_Inappropriate_Context =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Library_Unit_Declaration");
         end if;

         raise;

      when Ex : others      =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Library_Unit_Declaration (" &
                          To_String (Name) & ")",
            Ex         => Ex);
   end Library_Unit_Declaration;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Context : Asis.Context)
      return        Asis.Compilation_Unit_List
   is
      Res_Cont_Id    : constant Context_Id := Get_Cont_Id (The_Context);
      Cont_Tree_Mode : Tree_Mode;
   begin
      Check_Validity (The_Context, Package_Name & "Library_Unit_Declarations");

      Cont_Tree_Mode := Tree_Processing_Mode (Res_Cont_Id);

      if not (Cont_Tree_Mode = Pre_Created or else
              Cont_Tree_Mode = Incremental)
      then
         Set_Status
           (Status    => Use_Error,
            Diagnosis =>
              "Asis.Compilation_Units.Library_Unit_Declarations "
              & "can not be used "
              & LT & "for dynamic ASIS Context");

         raise ASIS_Failed;
      end if;

      Reset_Context (Res_Cont_Id);

      declare
         Result_Len : constant Natural := Lib_Unit_Decls (Res_Cont_Id);
         Result     : Compilation_Unit_List (1 .. Result_Len);
         L_U_Decl   : Unit_Id := First_Unit_Id; --  Standard
      begin
         for I in 1 .. Result_Len loop
            Result (I) := Get_Comp_Unit (L_U_Decl,  Res_Cont_Id);
            L_U_Decl   := Next_Decl (L_U_Decl);
         end loop;
         return Result;
      end;
   exception
      when ASIS_Inappropriate_Context =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Library_Unit_Declarations");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Library_Unit_Declarations",
            Ex         => Ex);
   end Library_Unit_Declarations;

   -----------------
   -- Object_Form --
   -----------------

   function Object_Form
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Object_Form");

      return Nil_Asis_Wide_String;

   end Object_Form;

   -----------------
   -- Object_Name --
   -----------------

   function Object_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Object_Name");

      return Nil_Asis_Wide_String;

   end Object_Name;

   --------------
   -- Subunits --
   --------------

   function Subunits
     (Parent_Body : Asis.Compilation_Unit)
      return        Asis.Compilation_Unit_List
   is
      Arg_Kind       : Asis.Unit_Kinds;
      Arg_Unit_Id    : Unit_Id;
      Res_Cont_Id    : Context_Id;
   begin
      Check_Validity (Parent_Body, Package_Name & "Subunits");

      Res_Cont_Id := Encl_Cont_Id (Parent_Body);
      Reset_Context (Res_Cont_Id);
      Arg_Kind := Kind (Parent_Body);

      if not (Arg_Kind = A_Procedure_Body         or else
              Arg_Kind = A_Function_Body          or else
              Arg_Kind = A_Package_Body           or else
              Arg_Kind = A_Procedure_Body_Subunit or else
              Arg_Kind = A_Function_Body_Subunit  or else
              Arg_Kind = A_Package_Body_Subunit   or else
              Arg_Kind = A_Task_Body_Subunit      or else
              Arg_Kind = A_Protected_Body_Subunit)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Subunits");
      end if;

      Arg_Unit_Id := Get_Unit_Id  (Parent_Body);

      declare
         Result_Id_List : constant Unit_Id_List :=
           Subunits (Res_Cont_Id, Arg_Unit_Id);
         Result_List : constant Compilation_Unit_List :=
           Get_Comp_Unit_List (Result_Id_List, Res_Cont_Id);
      begin
         return Result_List;
      end;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Subunits");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Subunits",
            Ex         => Ex,
            Arg_CU     => Parent_Body);
   end Subunits;

   function Subunits
     (Parent_Body : Asis.Compilation_Unit;
      The_Context : Asis.Context)
      return        Asis.Compilation_Unit_List
   is
      Arg_Kind        : Asis.Unit_Kinds;
      Arg_Unit_Id     : Unit_Id;
      Arg_Cont_Id     : Context_Id;
      Result_Cont_Id  : Context_Id;
      New_Arg_Unit_Id : Unit_Id;
   begin
      Check_Validity (The_Context, Package_Name & "Subunits");
      Check_Validity (Parent_Body, Package_Name & "Subunits");

      Arg_Cont_Id := Encl_Cont_Id (Parent_Body);
      Reset_Context (Arg_Cont_Id);
      Arg_Kind := Kind (Parent_Body);

      if not (Arg_Kind = A_Procedure_Body         or else
              Arg_Kind = A_Function_Body          or else
              Arg_Kind = A_Package_Body           or else
              Arg_Kind = A_Procedure_Body_Subunit or else
              Arg_Kind = A_Function_Body_Subunit  or else
              Arg_Kind = A_Package_Body_Subunit   or else
              Arg_Kind = A_Task_Body_Subunit      or else
              Arg_Kind = A_Protected_Body_Subunit)
      then
         Raise_ASIS_Inappropriate_Compilation_Unit
           (Diagnosis => Package_Name & "Subunits");
      end if;

      Result_Cont_Id  := Get_Cont_Id  (The_Context);

      Arg_Unit_Id     := Get_Unit_Id  (Parent_Body);
      New_Arg_Unit_Id := Get_Same_Unit
                           (Arg_Cont_Id, Arg_Unit_Id, Result_Cont_Id);

      if Present (New_Arg_Unit_Id) then
         return Subunits
                  (Get_Comp_Unit (New_Arg_Unit_Id, Result_Cont_Id));
      else
         return Nil_Compilation_Unit_List;
      end if;

   exception
      when  ASIS_Inappropriate_Compilation_Unit
          | ASIS_Inappropriate_Context            =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Subunits");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Subunits",
            Ex          => Ex,
            Arg_CU      => Parent_Body,
            Context_Par => True);
   end Subunits;

   ---------------
   -- Text_Form --
   ---------------

   function Text_Form
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Text_Form");

      return Nil_Asis_Wide_String;
   end Text_Form;

   ---------------
   -- Text_Name --
   ---------------

   function Text_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Text_Name");

      if not Exists (Compilation_Unit) then
         return Nil_Asis_Wide_String;
      else
         --  Exists resets the Context!
         return To_Program_Text (Source_File (Compilation_Unit));
      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Text_Name");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Text_Name",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Text_Name;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
      Arg_Kind : Unit_Kinds;
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Unique_Name");

      if Is_Nil (Compilation_Unit) then
         return Nil_Asis_Wide_String;
      else
         Reset_Context (Encl_Cont_Id (Compilation_Unit));
         Arg_Kind := Unit_Kind (Compilation_Unit);

         --  ???!! Diagnosis_Buffer and Diagnosis_Len should not be used here!

         Diagnosis_Len := 0;
         A4G.Vcheck.Add (Context_Info (Compilation_Unit));
         A4G.Vcheck.Add (": ");
         A4G.Vcheck.Add (Unit_Name (Compilation_Unit));

         case Arg_Kind is
            when Asis.A_Library_Unit_Body =>
               A4G.Vcheck.Add (" (body)");
            when Asis.A_Subunit =>
               A4G.Vcheck.Add (" (subunit)");
            when others =>
               A4G.Vcheck.Add (" (spec)");
         end case;

         return To_Program_Text (Diagnosis_Buffer (1 .. Diagnosis_Len));
      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Unique_Name",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Unique_Name;

   ----------------
   -- Unit_Class --
   ----------------

   function Unit_Class
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Unit_Classes
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Unit_Class");
      Reset_Context (Encl_Cont_Id (Compilation_Unit));
      return Class (Compilation_Unit);
   end Unit_Class;

   --------------------
   -- Unit_Full_Name --
   --------------------

   function Unit_Full_Name
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Wide_String
   is
   begin

      Check_Validity (Compilation_Unit, Package_Name & "Unit_Full_Name");

      if Is_Nil (Compilation_Unit) or else
         Unit_Kind (Compilation_Unit) = A_Configuration_Compilation
      then
         return Nil_Asis_Wide_String;
      else
         Reset_Context (Encl_Cont_Id (Compilation_Unit));

         return To_Program_Text (Unit_Name (Compilation_Unit));

      end if;

   exception
      when ASIS_Inappropriate_Compilation_Unit =>
         raise;

      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Unit_Full_Name");
         end if;

         raise;

      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name => Package_Name & "Unit_Full_Name",
            Ex         => Ex,
            Arg_CU     => Compilation_Unit);
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   function Unit_Kind
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Unit_Kinds
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Unit_Kind");
      return Kind (Compilation_Unit);
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   function Unit_Origin
     (Compilation_Unit : Asis.Compilation_Unit)
      return             Asis.Unit_Origins
   is
   begin
      Check_Validity (Compilation_Unit, Package_Name & "Unit_Origin");
      Reset_Context (Encl_Cont_Id (Compilation_Unit));
      return Origin (Compilation_Unit);
   end Unit_Origin;

end Asis.Compilation_Units;
