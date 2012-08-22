------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . C U _ I N F O 2                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore.                  --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with A4G.A_Opt;
with A4G.A_Sinput;    use A4G.A_Sinput;
with A4G.Int_Knds;    use A4G.Int_Knds;
with A4G.Mapping;     use A4G.Mapping;
with A4G.Contt;       use A4G.Contt;
with A4G.Contt.Dp;    use A4G.Contt.Dp;
with A4G.Contt.UT;    use A4G.Contt.UT;

with Atree;           use Atree;
with Einfo;           use Einfo;
with Fname;           use Fname;
with Namet;           use Namet;
with Nlists;          use Nlists;
with Sinfo;           use Sinfo;
with Sinput;          use Sinput;

package body A4G.CU_Info2 is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Mentioned_In_Annex_A return Boolean;
   --  This function assumes, that a normalized Ada name of a unit is set
   --  in the ASIS name buffer. It determines if this name is one of the
   --  names mentioned in the list of the names of predefined units given
   --  in RM 95 Annex A (2)

   function Is_Obsolescent_Renaming  return Boolean;
   --  This function assumes, that a normalized Ada name of a unit is set
   --  in the ASIS name buffer. It determines if this name is one of the
   --  names mentioned in RM95 J.1 as Ada-83-style renaming of an Ada
   --  predefined unit

   ------------------
   -- Get_Ada_Name --
   ------------------

   procedure Get_Ada_Name (Top : Node_Id) is
      Temp_Node     : Node_Id;
      Unit_Name     : Node_Id;
      Parent_Prefix : Node_Id; -- only for handling a subunit!
      Is_Subunit    : Boolean := False;
   begin
      Temp_Node := Original_Node (Unit (Top));

      if Nkind (Temp_Node) = N_Subunit then

         Is_Subunit    := True;
         Parent_Prefix := Sinfo.Name (Temp_Node);
         Temp_Node     := Proper_Body (Temp_Node);

         if Nkind (Temp_Node) = N_Subprogram_Body then
            Unit_Name := Defining_Unit_Name  (Specification (Temp_Node));
         elsif Nkind (Temp_Node) = N_Package_Body then
            Unit_Name := Defining_Unit_Name (Temp_Node);
         else --  N_Task_Body or N_Protected_Body
            Unit_Name := Defining_Identifier (Temp_Node);
         end if;
         --  in case of a subunit Unit_Name may be only of
         --  N_Defining_Identifier kind
      else

         case Nkind (Temp_Node) is

            when   N_Subprogram_Declaration
                 | N_Subprogram_Body
                 | N_Package_Declaration
                 | N_Generic_Package_Declaration
                 | N_Generic_Subprogram_Declaration
                 | N_Subprogram_Renaming_Declaration =>

               Unit_Name := Defining_Unit_Name (Specification (Temp_Node));

            when   N_Package_Body
                 | N_Package_Renaming_Declaration
                 | N_Generic_Package_Renaming_Declaration
                 | N_Generic_Procedure_Renaming_Declaration
                 | N_Generic_Function_Renaming_Declaration
                 | N_Package_Instantiation
                 | N_Function_Instantiation
                 | N_Procedure_Instantiation =>

               Unit_Name := Defining_Unit_Name (Temp_Node);

            when others =>
               pragma Assert (False);
               null;
         end case;

      end if;

      if Is_Subunit then
         Set_Name_String (Exp_Name_Image (Parent_Prefix)
                        & '.'
                        & Identifier_Image (Unit_Name));
      else
         Set_Name_String (Exp_Name_Image (Unit_Name));
      end if;

   end Get_Ada_Name;

   -------------
   -- Is_Main --
   -------------

   function Is_Main (Top : Node_Id; Kind : Unit_Kinds) return Boolean is
      Unit_Node : Node_Id := Empty;
      Result    : Boolean := False;
   begin

      case Kind is
         when A_Procedure          |
              A_Procedure_Body     |
              A_Function           |
              A_Function_Body      |
              A_Procedure_Renaming |
              A_Function_Renaming  =>

            Unit_Node := Specification (Unit (Top));

         when A_Function_Instance  |
              A_Procedure_Instance =>

            Unit_Node :=  Unit (Top);

            if Nkind (Unit_Node) = N_Package_Body then
               Unit_Node := Corresponding_Spec (Unit_Node);

               while Nkind (Unit_Node) /= N_Package_Declaration loop
                  Unit_Node := Parent (Unit_Node);
               end loop;

            end if;

            Unit_Node :=
              Last_Non_Pragma
                (Visible_Declarations (Specification (Unit_Node)));

            Unit_Node := Specification (Unit_Node);

         when others =>
            null;
      end case;

      if Present (Unit_Node) then
         Result := not Present (Parameter_Specifications (Unit_Node));
      end if;

      if Result then

         case Kind is
            when A_Function          |
                 A_Function_Body     |
                 A_Function_Instance |
                 A_Function_Renaming =>
               Result :=
                 Is_Integer_Type
                   (Entity (Sinfo.Result_Definition (Unit_Node)));

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Main;

   -----------------------------
   -- Is_Mentioned_In_Annex_A --
   -----------------------------

   function Is_Mentioned_In_Annex_A return Boolean is
      Result : Boolean := False;
      Ind    : Positive := 1;

      Max_Child_Nlen : constant Integer := 36;
      subtype Child_Name is String (1 .. Max_Child_Nlen);

      type Child_List is array (Integer range <>) of Child_Name;

      Ada_Childs : constant Child_List := (
      --  Contains names of child units of the Ada package that are predefined
      --  units in Ada 95
         "asynchronous_task_control           ",
         "calendar                            ",
         "characters                          ",
         "command_line                        ",
         "decimal                             ",
         "direct_io                           ",
         "dynamic_priorities                  ",
         "exceptions                          ",
         "finalization                        ",
         "interrupts                          ",
         "io_exceptions                       ",
         "numerics                            ",
         "real_time                           ",
         "sequential_io                       ",
         "storage_io                          ",
         "streams                             ",
         "strings                             ",
         "synchronous_task_control            ",
         "tags                                ",
         "task_attributes                     ",
         "task_identification                 ",
         "text_io                             ",
         "unchecked_conversion                ",
         "unchecked_deallocation              ",
         "wide_text_io                        ");

      Ada_Childs_2005 : constant Child_List := (
      --  Contains names of child units of the Ada package that are predefined
      --  units in Ada 2005 (but not in Ada 95)
         "assertions                          ",
         "complex_text_io                     ",
         "containers                          ",
         "directories                         ",
         "dispatching                         ",
         "environment_variables               ",
         "execution_time                      ",
         "float_text_io                       ",
         "float_wide_text_io                  ",
         "float_wide_wide_text_io             ",
         "integer_text_io                     ",
         "integer_wide_text_io                ",
         "integer_wide_wide_text_io           ",
         "task_termination                    ",
         "wide_characters                     ",
         "wide_wide_characters                ",
         "wide_wide_text_io                   ");

      Ada_Execution_Time_Childs : constant Child_List := (
      --  Contains names of child units of the Ada.Execution_Time package,
      --  these units are predefined in Ada 2005 only
         "group_budgets                       ",
         "timers                              ");

      Ada_Characters_Childs : constant Child_List := (
         "handling                            ",
         "latin_1                             ");

      Ada_Numerics_Childs : constant Child_List := (
      --  Contains names of child units of Ada.Numerics, that are defined in
      --  Ada 95
         "complex_elementary_functions        ",
         "complex_types                       ",
         "discrete_random                     ",
         "elementary_functions                ",
         "float_random                        ",
         "generic_complex_elementary_functions",
         "generic_complex_types               ",
         "generic_elementary_functions        ");

      Ada_Numerics_Childs_2005 : constant Child_List := (
      --  Contains names of child units of Ada.Numerics, that are defined in
      --  Ada 2005 (but not in Ada 95)
         "complex_arrays                      ",
         "generic_complex_arrays              ",
         "generic_real_arrays                 ",
         "real_arrays                         ");

      Ada_Strings_Childs : constant Child_List := (
      --  Contains names of child units of Ada.Strings, that are defined in
      --  Ada 95
         "bounded                             ",
         "fixed                               ",
         "maps                                ",
         "unbounded                           ",
         "wide_bounded                        ",
         "wide_fixed                          ",
         "wide_maps                           ",
         "wide_unbounded                      ");

      Ada_Strings_Childs_2005 : constant Child_List := (
      --  Contains names of child units of Ada.Strings, that are defined in
      --  Ada 2005 (but not in Ada 95)
         "hash                                ",
         "wide_hash                           ",
         "wide_wide_bounded                   ",
         "wide_wide_fixed                     ",
         "wide_wide_hash                      ",
         "wide_wide_maps                      ",
         "wide_wide_unbounded                 ");

      Ada_Text_IO_Childs : constant Child_List := (
      --  Contains names of child units of Ada.Text_IO, that are defined in
      --  Ada 95
         "complex_io                          ",
         "editing                             ",
         "text_streams                        ");

      Ada_Text_IO_Childs_2005 : constant Child_List := (
      --  Contains names of child units of Ada.Text_IO, that are defined in
      --  Ada 2005 (but not in Ada 95)
         "bounded_io                          ",
         "unbounded_io                        ");

      Ada_Wide_Text_IO_Childs : constant Child_List := (
      --  Contains names of child units of Ada.Wide_Text_IO, that are defined
      --  in Ada 2005 and that are different from the defined in Ada 95
      --  children of Ada.Text_IO
         "wide_bounded_io                     ",
         "wide_unbounded_io                   ");

      Ada_Wide_Wide_Text_IO_Childs : constant Child_List := (
      --  Contains names of child units of Ada.Wide_Wide_Text_IO, that are
      --  defined in Ada 2005 and that are different from the defined in Ada 95
      --  children of Ada.Text_IO
         "wide_wide_bounded_io                ",
         "wide_wide_unbounded_io              ");

      Interfaces_Childs : constant Child_List := (
         "c                                   ",
         "cobol                               ",
         "fortran                             ");

      Interfaces_C_Childs : constant Child_List := (
         "pointers                            ",
         "strings                             ");

      System_Childs : constant Child_List := (
         "address_to_access_conversions       ",
         "machine_code                        ",
         "rpc                                 ",
         "storage_elements                    ",
         "storage_pools                       ");

      function In_Child_List (Ind : Positive; L : Child_List) return Boolean;
      --  Checks if a part of a unit name which is stored in
      --  A_Name_Buffer (Ind .. A_Name_Len) belongs to a list of predefined
      --  child unit for a given predefined root unit

      function In_Child_List (Ind : Positive; L : Child_List) return Boolean
      is
         Padded_Unit_Name : String (1 .. Max_Child_Nlen);
         Last_Dot : Positive := 1;
      begin
         if A_Name_Len >= Ind + Max_Child_Nlen then
            return False;
         end if;

         --  checking if the argument name is not a name of grandchild:
         for I in reverse 1 .. A_Name_Len loop
            if A_Name_Buffer (I) = '.' then
               Last_Dot := I;
               exit;
            end if;
         end loop;

         if Last_Dot > Ind then

            return False;

         end if;

         Padded_Unit_Name (1 .. (A_Name_Len - Ind + 1)) :=
            A_Name_Buffer (Ind .. A_Name_Len);

         Padded_Unit_Name ((A_Name_Len - Ind + 1) + 1 .. Max_Child_Nlen) :=
           (others => ' ');

         for I in L'Range loop

            if Padded_Unit_Name = L (I) then
               return True;
            end if;

         end loop;

         return False;

      end In_Child_List;

   begin
      --  no need to analyze the suffix of a normalized name:
      A_Name_Len := A_Name_Len - 2;

      --  No need to check the Standard package - its origin is set when
      --  the corresponding unit entry is created in a special way

      if A_Name_Len >= 3 and then
         A_Name_Buffer (1 .. 3) = "ada"
      then

         if A_Name_Len = 3 then
            Result := True;
         else
            Ind := 5;

            Result := In_Child_List (Ind, Ada_Childs)
                     or else
                      (A4G.A_Opt.ASIS_2005_Mode_Internal
                      and then
                       In_Child_List (Ind, Ada_Childs_2005));

            if Result = False then

               --  Checking grandchildren of Ada:
               if A_Name_Buffer (Ind .. Ind + 10) = "characters." then

                  Ind := Ind + 11;

                  Result := In_Child_List (Ind, Ada_Characters_Childs)
                           or else
                            (A4G.A_Opt.ASIS_2005_Mode_Internal
                            and then
                             A_Name_Buffer (Ind .. A_Name_Len) =
                             "conversions");

               elsif A_Name_Buffer (Ind .. Ind + 8) = "numerics." then

                  Ind := Ind + 9;

                  Result := In_Child_List (Ind, Ada_Numerics_Childs)
                           or else
                            (A4G.A_Opt.ASIS_2005_Mode_Internal
                            and then
                             In_Child_List (Ind, Ada_Numerics_Childs_2005));

               elsif A_Name_Buffer (Ind .. A_Name_Len) = "streams.stream_io"
                  or else
                     A_Name_Buffer (Ind .. A_Name_Len) = "interrupts.names"
                  or else
                     (A4G.A_Opt.ASIS_2005_Mode_Internal
                     and then
                      (A_Name_Buffer (Ind .. A_Name_Len) =
                       "real_time.timing_events"
                      or else
                       A_Name_Buffer (Ind .. A_Name_Len) =
                       "tags.generic_dispatching_constructor"))
               then
                  --  only one grandchild is possible, no need for searching
                  Result := True;

               elsif A_Name_Buffer (Ind .. Ind + 7) = "strings." then

                  Ind := Ind + 8;

                  Result := In_Child_List (Ind, Ada_Strings_Childs)
                           or else
                            (A4G.A_Opt.ASIS_2005_Mode_Internal
                            and then
                             In_Child_List (Ind, Ada_Strings_Childs_2005));

                  if Result = False then
                     if A_Name_Buffer (Ind .. A_Name_Len) = "maps.constants"
                       or else
                        A_Name_Buffer (Ind .. A_Name_Len) =
                        "wide_maps.wide_constants"
                       or else
                        (A4G.A_Opt.ASIS_2005_Mode_Internal
                        and then
                          (A_Name_Buffer (Ind .. A_Name_Len) =
                           "bounded.hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "fixed.hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "unbounded.hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_bounded.wide_hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_fixed.wide_hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_unbounded.wide_hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_wide_bounded.wide_wide_hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_wide_fixed.wide_wide_hash"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_wide_maps.wide_wide_constants"
                          or else
                           A_Name_Buffer (Ind .. A_Name_Len) =
                           "wide_wide_unbounded.wide_wide_hash"))
                     then
                        Result := True;
                     end if;
                  end if;

               elsif A_Name_Buffer (Ind .. Ind + 7) = "text_io." then

                  Ind := Ind + 8;

                  Result := In_Child_List (Ind, Ada_Text_IO_Childs)
                           or else
                            (A4G.A_Opt.ASIS_2005_Mode_Internal
                            and then
                             In_Child_List (Ind, Ada_Text_IO_Childs_2005));

               elsif A_Name_Buffer (Ind .. Ind + 12) = "wide_text_io." then

                  Ind := Ind + 13;

                  Result := In_Child_List (Ind, Ada_Text_IO_Childs)
                           or else
                            (A4G.A_Opt.ASIS_2005_Mode_Internal
                            and then
                             (In_Child_List (Ind, Ada_Text_IO_Childs_2005)
                              or else
                              In_Child_List (Ind, Ada_Wide_Text_IO_Childs)));

               --  Ada 2005 stuff

               elsif A4G.A_Opt.ASIS_2005_Mode_Internal
                    and then
                     A_Name_Buffer (Ind .. Ind + 14) = "execution_time."
               then
                  Ind := Ind + 15;
                  Result := In_Child_List (Ind, Ada_Execution_Time_Childs);

               elsif A4G.A_Opt.ASIS_2005_Mode_Internal
                    and then
                     A_Name_Buffer (Ind .. Ind + 17) = "wide_wide_text_io."
               then
                  Result := In_Child_List (Ind, Ada_Text_IO_Childs)
                           or else
                            In_Child_List (Ind, Ada_Text_IO_Childs_2005)
                           or else
                            In_Child_List (Ind, Ada_Wide_Wide_Text_IO_Childs);
               end if;

            end if;

         end if;

      elsif A_Name_Len >= 10 and then
         A_Name_Buffer (1 .. 10) = "interfaces"
      then

         if A_Name_Len = 10 then
            Result := True;
         else

            Ind := 12;
            Result := In_Child_List (Ind, Interfaces_Childs);

            if Result = False then
               --  Checking grandchildren of Interfaces:
               if A_Name_Buffer (Ind .. Ind + 1) = "c." then

                  Ind := Ind + 2;
                  Result := In_Child_List (Ind, Interfaces_C_Childs);

               end if;
            end if;

         end if;

      elsif A_Name_Len >= 6 and then
         A_Name_Buffer (1 .. 6) = "system"
      then
         if A_Name_Len = 6 then
            Result := True;
         else

            Ind := 8;
            Result := In_Child_List (Ind, System_Childs);

         end if;

      end if;

      return Result;

   end Is_Mentioned_In_Annex_A;

   -----------------------------
   -- Is_Obsolescent_Renaming --
   -----------------------------

   function Is_Obsolescent_Renaming  return Boolean
   is
      Result : Boolean := False;
   begin

      --  This function is called after Is_Mentioned_In_Annex_A, so A_Name_Len
      --  is already moved two positions left to skip the suffix

      if A_Name_Buffer (A_Name_Len + 1 .. A_Name_Len + 2) = "%s" then

         case A_Name_Len is
            when 7 =>
               Result := A_Name_Buffer (1 .. A_Name_Len) = "text_io";
            when 8 =>
               Result := A_Name_Buffer (1 .. A_Name_Len) = "calendar";
            when 9 =>
               Result := A_Name_Buffer (1 .. A_Name_Len) = "direct_io";
            when 12 =>
               Result := A_Name_Buffer (1 .. A_Name_Len) = "machine_code";
            when 13 =>
               Result := A_Name_Buffer (1 .. A_Name_Len) = "sequential_io"
                       or else
                         A_Name_Buffer (1 .. A_Name_Len) = "io_exceptions";
            when 20 =>
               Result :=
                 A_Name_Buffer (1 .. A_Name_Len) = "unchecked_conversion";
            when 22 =>
               Result :=
                 A_Name_Buffer (1 .. A_Name_Len) = "unchecked_deallocation";
            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Obsolescent_Renaming;

   ----------------------
   -- Set_Dependencies --
   ----------------------

   procedure Set_Dependencies
     (C   : Context_Id;
      U   : Unit_Id;
      Top : Node_Id)
   is
      Unit_Kind : constant Unit_Kinds := Kind (C, U);
   begin
      Set_Supporters (C, U, Top);

      if Unit_Kind = A_Procedure                  or else
         Unit_Kind = A_Function                   or else
         Unit_Kind = A_Package                    or else
         Unit_Kind = A_Generic_Procedure          or else
         Unit_Kind = A_Generic_Function           or else
         Unit_Kind = A_Generic_Package            or else
         Unit_Kind = A_Procedure_Instance         or else
         Unit_Kind = A_Function_Instance          or else
         Unit_Kind = A_Package_Instance           or else
         Unit_Kind = A_Procedure_Renaming         or else
         Unit_Kind = A_Function_Renaming          or else
         Unit_Kind = A_Package_Renaming           or else
         Unit_Kind = A_Generic_Procedure_Renaming or else
         Unit_Kind = A_Generic_Function_Renaming  or else
         Unit_Kind = A_Generic_Package_Renaming   or else
         Unit_Kind = A_Procedure_Body             or else
         Unit_Kind = A_Function_Body              or else
         Unit_Kind = A_Package_Body               or else
         Unit_Kind = A_Procedure_Body_Subunit     or else
         Unit_Kind = A_Function_Body_Subunit      or else
         Unit_Kind = A_Package_Body_Subunit       or else
         Unit_Kind = A_Task_Body_Subunit          or else
         Unit_Kind = A_Protected_Body_Subunit     or else
         Unit_Kind = An_Unknown_Unit
      then
         Add_To_Parent (C, U);
      end if;

   end Set_Dependencies;

   ------------------------
   -- Set_Kind_and_Class --
   -------------------------

   procedure Set_Kind_and_Class
     (C   : Context_Id;
      U   : Unit_Id;
      Top : Node_Id)
   is
      Is_Private     : Boolean;
      Unit_Node      : Node_Id;
      Unit_Node_Kind : Node_Kind;

      Kind_To_Set    : Unit_Kinds   := Not_A_Unit;
      Class_To_Set   : Unit_Classes := Not_A_Class;
   begin
      Is_Private := Private_Present (Top);
      Unit_Node  := Unit (Top); --  Original_Node???

      if Is_Rewrite_Substitution (Unit_Node) then  -- For Generic
         Unit_Node := Original_Node (Unit_Node);   -- Instantiations
      end if;

      Unit_Node_Kind := Nkind (Unit_Node);

      case Unit_Node_Kind is

         when N_Subprogram_Declaration =>

            if Asis_Internal_Element_Kind (Unit_Node) =
               A_Procedure_Declaration
            then
               Kind_To_Set := A_Procedure;
            else
               Kind_To_Set := A_Function;
            end if;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Package_Declaration =>

            Kind_To_Set := A_Package;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Generic_Declaration =>

            if Unit_Node_Kind = N_Generic_Package_Declaration then
               Kind_To_Set :=  A_Generic_Package;
            else -- two possibilities: generic procedure or generic function
               if Asis_Internal_Element_Kind (Unit_Node) =
                  A_Generic_Procedure_Declaration
               then
                  Kind_To_Set := A_Generic_Procedure;
               else
                  Kind_To_Set := A_Generic_Function;
               end if;
            end if;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Generic_Instantiation =>

            if Unit_Node_Kind = N_Package_Instantiation then
               Kind_To_Set := A_Package_Instance;
            elsif Unit_Node_Kind = N_Procedure_Instantiation then
               Kind_To_Set := A_Procedure_Instance;
            else
               Kind_To_Set := A_Function_Instance;
            end if;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Subprogram_Renaming_Declaration =>

            if Asis_Internal_Element_Kind (Unit_Node) =
               A_Procedure_Renaming_Declaration
            then
               Kind_To_Set := A_Procedure_Renaming;
            else
               Kind_To_Set := A_Function_Renaming;
            end if;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Package_Renaming_Declaration =>

            Kind_To_Set := A_Package_Renaming;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Generic_Renaming_Declaration =>

            if Unit_Node_Kind = N_Generic_Procedure_Renaming_Declaration then
               Kind_To_Set := A_Generic_Procedure_Renaming;
            elsif Unit_Node_Kind = N_Generic_Function_Renaming_Declaration then
               Kind_To_Set := A_Generic_Function_Renaming;
            else
               Kind_To_Set := A_Generic_Package_Renaming;
            end if;

            if Is_Private then
               Class_To_Set := A_Private_Declaration;
            else
               Class_To_Set := A_Public_Declaration;
            end if;

         when N_Subprogram_Body =>

            if Asis_Internal_Element_Kind (Unit_Node) =
               A_Procedure_Body_Declaration
            then
               Kind_To_Set := A_Procedure_Body;
            else
               Kind_To_Set := A_Function_Body;
            end if;

            if Acts_As_Spec (Top)
              or else
               not Comes_From_Source (Corresponding_Spec (Unit (Top)))
               --  This part of the condition covers an artificial spec created
               --  for a child subprogram
            then
               Class_To_Set := A_Public_Declaration_And_Body;
            else
               if Private_Present (Library_Unit (Top)) then
                  Class_To_Set := A_Private_Body;
               else
                  Class_To_Set := A_Public_Body;
               end if;
            end if;

         when N_Package_Body    =>

            Kind_To_Set := A_Package_Body;

            if Private_Present (Library_Unit (Top)) then
               Class_To_Set := A_Private_Body;
            else
               Class_To_Set := A_Public_Body;
            end if;

         when N_Subunit         =>

            Unit_Node    := Proper_Body (Unit_Node);

            case Nkind (Unit_Node) is

               when N_Subprogram_Body =>

                  if Asis_Internal_Element_Kind (Unit_Node) =
                     A_Procedure_Body_Declaration
                  then
                     Kind_To_Set := A_Procedure_Body_Subunit;
                  else
                     Kind_To_Set := A_Function_Body_Subunit;
                  end if;

               when N_Package_Body =>
                  Kind_To_Set := A_Package_Body_Subunit;

               when N_Task_Body =>
                  Kind_To_Set := A_Task_Body_Subunit;

               when N_Protected_Body =>
                  Kind_To_Set := A_Protected_Body_Subunit;

               when others =>
                  null;
            end case;

            Class_To_Set := A_Separate_Body;

         when others =>
            pragma Assert (False);
            null;
      end case;

      Set_Kind  (C, U, Kind_To_Set);
      Set_Class (C, U, Class_To_Set);
   end Set_Kind_and_Class;

   -----------------------------
   -- Set_S_F_Name_and_Origin --
   -----------------------------

   procedure Set_S_F_Name_and_Origin
     (Context : Context_Id;
      Unit    : Unit_Id;
      Top     : Node_Id)
   is
      Fname     : File_Name_Type;
      Ref_Fname : File_Name_Type;

      Origin    : Unit_Origins;
   begin
      --  Setting the (full) source file name in the Unit table.
      --  The source file is the file which has been compiled

      Fname := Full_File_Name (Get_Source_File_Index (Sloc (Top)));
      Namet.Get_Name_String (Fname);

      Set_Name_String
        (Normalize_Pathname (Namet.Name_Buffer (1 .. Namet.Name_Len),
                             Resolve_Links => False));
      Set_Source_File_Name (Unit);

      --  Setting the (full) reference file name

      Ref_Fname := Full_Ref_Name (Get_Source_File_Index (Sloc (Top)));

      if Ref_Fname = Fname then
         Set_Ref_File_As_Source_File (Unit);
      else
         Namet.Get_Name_String (Ref_Fname);
         Set_Ref_File_Name_String (Unit);
         Set_Source_File_Name (Unit, Ref => True);
      end if;
      --  to define the unit origin, we have to reset Fname to the short
      --  (that is, containing no directory information) file name

      Fname := File_Name (Get_Source_File_Index (Sloc (Top)));

      if Is_Predefined_File_Name (Fname) then

         Get_Name_String (Unit, Norm_Ada_Name);

         if Is_Mentioned_In_Annex_A then
            Origin := A_Predefined_Unit;

         elsif Is_Obsolescent_Renaming then
            --  We use a separate elsif path here to stress that this case may
            --  need more processing: we may want to check if such a unit is
            --  not redefined by a user, see RM95 J.1(10) and the discussion
            --  for B612-002
            Origin := A_Predefined_Unit;
         else
            Origin := An_Implementation_Unit;
         end if;

      elsif Is_Internal_File_Name (Fname) then
         Origin := An_Implementation_Unit;
      else
         Origin := An_Application_Unit;
      end if;

      Set_Origin (Context, Unit, Origin);

   end Set_S_F_Name_and_Origin;

end A4G.CU_Info2;
