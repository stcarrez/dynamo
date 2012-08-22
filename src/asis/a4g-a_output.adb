------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A 4 G . A _ O U T P U T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  AdaCore                   --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

--  with Asis.Text;         use Asis.Text;
with Asis.Elements;     use Asis.Elements;

with A4G.A_Debug;       use A4G.A_Debug;
with A4G.A_Types;       use A4G.A_Types;
with A4G.Int_Knds;      use A4G.Int_Knds;
with A4G.Contt;         use A4G.Contt;
with A4G.Contt.UT;      use A4G.Contt.UT;
with A4G.Contt.TT;      use A4G.Contt.TT;
with A4G.A_Opt;         use A4G.A_Opt;
with A4G.Vcheck;        use A4G.Vcheck;

with Asis.Set_Get;      use  Asis.Set_Get;

with Atree;             use Atree;
with Namet;             use Namet;
with Output;            use Output;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;

package body A4G.A_Output is

   LT : String renames A4G.A_Types.ASIS_Line_Terminator;

   ---------
   -- Add --
   ---------
   procedure Add (Phrase : String) is
   begin
      if Debug_Buffer_Len = Max_Debug_Buffer_Len then
         return;
      end if;
      for I in Phrase'Range loop
         Debug_Buffer_Len := Debug_Buffer_Len + 1;
         Debug_Buffer (Debug_Buffer_Len) := Phrase (I);
         if Debug_Buffer_Len = Max_Debug_Buffer_Len then
            exit;
         end if;
      end loop;
   end Add;

   ------------------
   -- ASIS_Warning --
   ------------------

   procedure ASIS_Warning
     (Message : String;
      Error   : Asis.Errors.Error_Kinds := Not_An_Error)
   is
   begin
      case ASIS_Warning_Mode is
         when Suppress =>
            null;
         when Normal =>
            Set_Standard_Error;
            Write_Str ("ASIS warning: ");
            Write_Eol;
            Write_Str (Message);
            Write_Eol;
            Set_Standard_Output;
         when Treat_As_Error =>
            --  ??? Raise_ASIS_Failed should be revised to use like that
            Raise_ASIS_Failed (
               Argument  => Nil_Element,
               Diagnosis => Message,
               Stat      => Error);
      end case;
   end ASIS_Warning;

   --------------------------------------
   --  Debug_String (Compilation Unit) --
   --------------------------------------

   --  SHOULD BE REVISED USING Debug_Buffer!!!

   function Debug_String (CUnit : Compilation_Unit) return String is
      LT : String renames A4G.A_Types.ASIS_Line_Terminator;
      U  : Unit_Id;
      C  : Context_Id;
   begin
      U := Get_Unit_Id  (CUnit);
      C := Encl_Cont_Id (CUnit);

      if No (U) then
         return "This is a Nil Compilation Unit";
      else
         Reset_Context (C);
         return LT &
             "Unit Id: " & Unit_Id'Image (U) & LT
             &
             "   Unit name: " & Unit_Name (CUnit) & LT
             &
             "   Kind:      " & Asis.Unit_Kinds'Image (Kind (C, U)) & LT
             &
             "   Class:     " & Asis.Unit_Classes'Image (Class (C, U)) & LT
             &
             "   Origin:    " & Asis.Unit_Origins'Image (Origin (C, U)) & LT
             &
             "   Enclosing Context Id: " & Context_Id'Image (C) & LT
             &
             "   Is consistent: " & Boolean'Image (Is_Consistent (C, U)) & LT
             &
             "-------------------------------------------------";
      end if;

   end Debug_String;

   procedure  Debug_String
     (CUnit    : Compilation_Unit;
      No_Abort : Boolean := False)
   is
      LT : String renames A4G.A_Types.ASIS_Line_Terminator;
      U  : Unit_Id;
      C  : Context_Id;
   begin
      Debug_Buffer_Len := 0;
      U                := Get_Unit_Id  (CUnit);
      C                := Encl_Cont_Id (CUnit);

      if No (U) then
         Add ("This is a Nil Compilation Unit");
      else
         Reset_Context (C);
         Add (LT);
         Add ("Unit Id: " & Unit_Id'Image (U) & LT);
         Add ("   Unit name: " & Unit_Name (CUnit) & LT);
         Add ("   Kind:      " & Asis.Unit_Kinds'Image (Kind (C, U)) & LT);
         Add ("   Class:     " & Asis.Unit_Classes'Image (Class (C, U)) & LT);
         Add ("   Origin:    " & Asis.Unit_Origins'Image (Origin (C, U)) & LT);
         Add ("   Enclosing Context Id: " & Context_Id'Image (C) & LT);
         Add ("   Is consistent: " &
                  Boolean'Image (Is_Consistent (C, U)) & LT);
         Add ("-------------------------------------------------");
      end if;

   exception
      when Ex : others =>

         if No_Abort then
            Add (LT & "Can not complete the unit debug image because of" & LT);
            Add (Exception_Information (Ex));
         else
            raise;
         end if;

   end Debug_String;

   -----------------------------
   --  Debug_String (Context) --
   -----------------------------

   --  SHOULD BE REVISED USING Debug_Buffer!!!

   function Debug_String (Cont : Context) return String is
      LT : String renames A4G.A_Types.ASIS_Line_Terminator;
      C  : constant Context_Id := Get_Cont_Id (Cont);

      Debug_String_Prefix : constant String :=
         "Context Id: " & Context_Id'Image (C) & LT;
   begin
      if C = Non_Associated then
         return Debug_String_Prefix
            &  "   This Context has never been associated";

      elsif not Is_Associated (C) and then
            not Is_Opened (C)
      then
         return Debug_String_Prefix
            &  "   This Context is dissociated at the moment";
      elsif not Is_Opened (C) then
      --  here Is_Associated (C)
         return Debug_String_Prefix
            &  "   This Context has associations," & LT
            &  "   but it is closed at the moment";
      else  -- here Is_Associated (C) and Is_Opened (C)
         return Debug_String_Prefix
            &
            "   This Context is opened at the moment" & LT
            &
            "      All tree files:   "
            & Tree_Id'Image (Last_Tree (C) - First_Tree_Id + 1) & LT
            &
            "      All units:        "
            & Unit_Id'Image (Last_Unit - First_Unit_Id + 1) & LT
            &
            "      Existing specs :  "
            & Natural'Image (Lib_Unit_Decls (C)) & LT
            &
            "      Existing bodies:  "
            & Natural'Image (Comp_Unit_Bodies (C)) & LT
            &
            "      Nonexistent units:"
            & Natural'Image (Natural (Last_Unit - First_Unit_Id + 1) -
                (Lib_Unit_Decls (C) + Comp_Unit_Bodies (C)))
            & LT
            & "=================";
      end if;
   end Debug_String;

   -----------------------------
   --  Debug_String (Element) --
   -----------------------------

   procedure Debug_String
     (E        : Element;
      No_Abort : Boolean := False)
   is
      E_Kind       : constant Internal_Element_Kinds := Int_Kind (E);
      E_Kind_Image : constant String := Internal_Element_Kinds'Image (E_Kind);
      E_Unit       : constant Asis.Compilation_Unit := Encl_Unit (E);
      E_Unit_Class : constant Unit_Classes := Class (E_Unit);

      N     : constant Node_Id := Node         (E);
      R_N   : constant Node_Id := R_Node       (E);
      N_F_1 : constant Node_Id := Node_Field_1 (E);
      N_F_2 : constant Node_Id := Node_Field_2 (E);

      C  : constant Context_Id := Encl_Cont_Id (E);
      T  : constant Tree_Id    := Encl_Tree    (E);
   begin

      Debug_Buffer_Len := 0;

      if Is_Nil (E) then
         Add ("This is a Nil Element");
      else
         Add (E_Kind_Image);
         Add (LT & "located in ");
         Add (Unit_Name (E_Unit));

         if E_Unit_Class = A_Separate_Body then
            Add (" (subunit, Unit_Id =");
         elsif E_Unit_Class = A_Public_Declaration or else
               E_Unit_Class =  A_Private_Declaration
         then
            Add (" (spec, Unit_Id =");
         else
            Add (" (body, Unit_Id =");
         end if;

         Add (Unit_Id'Image (Encl_Unit_Id (E)));
         Add (", Context_Id =");
         Add (Context_Id'Image (C));
         Add (")" & LT);

         if not (Debug_Flag_I) then
            Add ("text position :");

--            if not Is_Text_Available (E) then

               --  Creating the source location from the element node. We
               --  cannot safely use Element_Span here because in case of a
               --  bug in a structural query this may result in curcling of
               --  query blow-ups.

               if Sloc (N) <= No_Location then
                  Add (" not available");
                  Add (LT);
               else

                  Add (" ");

                  declare
                     use Ada.Strings;

                     P              : Source_Ptr;
                     Sindex         : Source_File_Index;
                     Instance_Depth : Natural := 0;

                     procedure Enter_Sloc;
                     --  For the current value of P, adds to the debug string
                     --  the string of the form file_name:line_number. Also
                     --  computes Sindex as the Id of the sourse file of P.

                     procedure Enter_Sloc is
                        F_Name : File_Name_Type;
                     begin
                        Sindex := Get_Source_File_Index (P);
                        F_Name := File_Name (Sindex);

                        Get_Name_String (F_Name);

                        Add (Name_Buffer (1 .. Name_Len) & ":");
                        Add (Trim (Get_Physical_Line_Number (P)'Img, Both));
                        Add (":");
                        Add (Trim (Get_Column_Number (P)'Img, Both));
                     end Enter_Sloc;

                  begin

                     P := Sloc (N);
                     Enter_Sloc;
                     P := Instantiation (Sindex);

                     while P /= No_Location loop
                        Add ("[");
                        Instance_Depth := Instance_Depth + 1;
                        Enter_Sloc;
                        P := Instantiation (Sindex);

                     end loop;

                     for J in 1 .. Instance_Depth loop
                        Add ("]");
                     end loop;

                     Add (LT);
                  end;

               end if;

--            else
--               declare
--                  Arg_Span : Span;
--                  FL : String_Ptr;
--                  LL : String_Ptr;
--                  FC : String_Ptr;
--                  LC : String_Ptr;
--               begin
--                  --  this operation is potentially dangerous - it may
--                  --  change the tree (In fact, it should not, if we
--                  --  take into account the typical conditions when
--                  --  this routine is called
--                  Arg_Span := Element_Span (E);
--                  FL := new String'(Line_Number'Image (Arg_Span.First_Line));
--                  LL := new String'(Line_Number'Image (Arg_Span.Last_Line));
--                  FC := new String'(Character_Position'Image
--                              (Arg_Span.First_Column));
--                  LC := new String'(Character_Position'Image
--                              (Arg_Span.Last_Column));
--                  Add (FL.all);
--                  Add (" :");
--                  Add (FC.all);
--                  Add (" -");
--                  Add (LL.all);
--                  Add (" :");
--                  Add (LC.all);
--                  Add (LT);
--               end;
--            end if;

         end if;

         Add ("   Nodes:" & LT);
         Add ("      Node            :" & Node_Id'Image (N));
         Add (" - " & Node_Kind'Image (Nkind (N)) & LT);

         Add ("      R_Node          :" & Node_Id'Image (R_N));
         Add (" - " & Node_Kind'Image (Nkind (R_N)) & LT);

         Add ("      Node_Field_1    :" & Node_Id'Image (N_F_1));
         Add (" - " & Node_Kind'Image (Nkind (N_F_1)) & LT);

         Add ("      Node_Field_2    :" & Node_Id'Image (N_F_2));
         Add (" - " & Node_Kind'Image (Nkind (N_F_2)) & LT);

         Add ("   Rel_Sloc           :");
         Add (Source_Ptr'Image (Rel_Sloc (E)) & LT);

         if Special_Case (E) /= Not_A_Special_Case then
            Add ("   Special Case       : ");
            Add (Special_Cases'Image (Special_Case (E)) & LT);

         end if;

         if Special_Case (E) = Stand_Char_Literal or else
            Character_Code (E) /= 0
         then
            Add ("   Character_Code     :");
            Add (Char_Code'Image (Character_Code (E)) & LT);
         end if;

         case Normalization_Case (E) is
            when Is_Normalized =>
               Add ("   Normalized" & LT);
            when Is_Normalized_Defaulted =>
               Add ("   Normalized (with default value)" & LT);
            when Is_Normalized_Defaulted_For_Box =>
               Add ("   Normalized (with default value computed for box)"
                    & LT);
            when Is_Not_Normalized =>
               null;
         end case;

         if Parenth_Count (E) > 0 then
            Add ("   Parenth_Count      :");
            Add (Nat'Image (Parenth_Count (E)) & LT);
         end if;

         if Is_From_Implicit (E) then
            Add ("   Is implicit" & LT);
         end if;

         if Is_From_Inherited (E) then
            Add ("   Is inherited" & LT);
         end if;

         if Is_From_Instance (E) then
            Add ("   Is from instance" & LT);
         end if;

         Add ("   obtained from the tree ");

         if Present (T) then
            Get_Name_String (C, T);
            Add (A_Name_Buffer (1 ..  A_Name_Len));
            Add (" (Tree_Id =" & Tree_Id'Image (T) & ")");
         else
            Add (" <nil tree>");
         end if;

      end if;

   exception
      when Ex : others =>

         if No_Abort then
            Add (LT & "Can not complete the unit debug image because of" & LT);
            Add (Exception_Information (Ex));
         else
            raise;
         end if;

   end Debug_String;

   ----------------
   -- Write_Node --
   ----------------

   procedure Write_Node (N : Node_Id; Prefix : String := "") is
   begin
      Write_Str (Prefix);
      Write_Str ("Node_Id =  ");
      Write_Int (Int (N));
      Write_Eol;
      Write_Str (Prefix);
      Write_Str ("Nkind   =  ");
      Write_Str (Node_Kind'Image (Nkind (N)));
      Write_Eol;
      Write_Str (Prefix);
      Write_Str ("Rewrite_Sub       value : ");
      Write_Str (Boolean'Image (Is_Rewrite_Substitution (N)));
      Write_Eol;
      Write_Str (Prefix);
      Write_Str ("Rewrite_Ins       value : ");
      Write_Str (Boolean'Image (Is_Rewrite_Insertion (N)));
      Write_Eol;
      Write_Str (Prefix);
      Write_Str ("Comes_From_Source value : ");
      Write_Str (Boolean'Image (Comes_From_Source (N)));
      Write_Eol;

      if Original_Node (N) = N then
         Write_Str (Prefix);
         Write_Str ("  Node is unchanged");
         Write_Eol;
      elsif Original_Node (N) = Empty then
         Write_Str (Prefix);
         Write_Str ("  Node has been inserted");
         Write_Eol;
      else
         Write_Str (Prefix);
         Write_Str ("  Node has been rewritten");
         Write_Eol;
         Write_Node (N      => Original_Node (N),
                     Prefix => Write_Node.Prefix & " Original node -> ");
      end if;

      Write_Eol;

   end Write_Node;

end A4G.A_Output;
