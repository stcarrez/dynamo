------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                                                                          --
--     A S I S . A D A _ E N V I R O N M E N T S . C O N T A I N E R S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 1995-2006, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License  distributed with ASIS-for-GNAT; see   file --
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
-- (http://www.adaccore.com).                                               --
--                                                                          --
------------------------------------------------------------------------------

with Asis.Exceptions;        use Asis.Exceptions;
with Asis.Errors;            use Asis.Errors;
with Asis.Compilation_Units; use Asis.Compilation_Units;

with Asis.Set_Get;           use  Asis.Set_Get;

with A4G.Contt;              use A4G.Contt;
with A4G.Vcheck;             use A4G.Vcheck;

package body Asis.Ada_Environments.Containers is

   Package_Name : constant String := "Asis.Ada_Environments.Containers.";

   --  At the moment, we implement the Container abstraction in a simplest
   --  possible way pointed out in section 9.1: any Context is mapped to
   --  a list of one Container
   --
   --  We think that the definition of the Container abstraction in ASIS
   --  contains a hole. The Container remains valid only till the Context
   --  from which it has been obtained remains opened (the same rule as for
   --  other ASIS abstractions - Compilation_Unit, Element, Line,
   --  Record_Component, Array_Component). We failed to find the corresponding
   --  rule in the ASIS Standard.

   -----------------------
   -- Local subprograms --
   -----------------------

   --  We can not put the access and update routines for the components of the
   --  Container type in Asis.Set_Get package where the access/update routines
   --  for other ASIS abstractions are defined, because the Container
   --  abstraction is defined not in Asis, but in
   --  Asis.Ada_Environments.Containers. If we decide to implement Containers
   --  in some non-trivial way, we may need to put low-level routines
   --  operating on Containers in some separate package

   function Set_Container
     (Cntnr : Container_Id;
      Cntxt : Context_Id)
      return Container;
   --  Creates the new value of the Container type, the Obtained field is set
   --  equal to the current ASIS OS time

   function Cntnr_Id (The_Container : Container) return Container_Id;
   function Encl_Cont_Id (The_Container : Container) return Context_Id;
   function Obtained     (The_Container : Container) return ASIS_OS_Time;
   --  These functions return components of the internal Contaier structure

   procedure Check_Validity
     (The_Container : Container;
      Query         : String);
   --  Checks if the argument does not belong to the closed (and possibly then
   --  reopened) Context. If this check fails, raises
   --  ASIS_Inappropriate_Container with Value_Error error status and forms
   --  the corresponding Diagnosis string

   function Is_Nil (The_Container : Container) return Boolean;
   --  Returns True if the container is a Nil_Container.
   --  This is really strange that we do not have this function in the spec.
   --  And the whole notion of Nil_Container seems to be ill-defined and
   --  as a result - useless.

   ------------------------------
   -- Local subprograms bodies --
   ------------------------------

   --------------------
   -- Check_Validity --
   --------------------

   procedure Check_Validity
     (The_Container : Container;
      Query         : String)
   is
   begin

      if not Is_Nil (The_Container) then

         if not Is_Opened (Encl_Cont_Id (The_Container)) or else
            not Later
                  (Opened_At (Encl_Cont_Id (The_Container)),
                   Obtained (The_Container))
         then
            Set_Error_Status
              (Status    => Value_Error,
               Diagnosis => "Invalid Container value in " & Query);

            raise ASIS_Inappropriate_Container;
         end if;

      end if;

   end Check_Validity;

   --------------
   -- Cntnr_Id --
   --------------

   function Cntnr_Id (The_Container : Container) return Container_Id is
   begin
      return The_Container.Id;
   end Cntnr_Id;

   ------------------
   -- Encl_Cont_Id --
   ------------------

   function Encl_Cont_Id (The_Container : Container) return Context_Id is
   begin
      return The_Container.Cont_Id;
   end Encl_Cont_Id;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (The_Container : Container) return Boolean is
   begin
      return Cntnr_Id (The_Container) = Nil_Container_Id;
   end Is_Nil;

   --------------
   -- Obtained --
   --------------

   function Obtained (The_Container : Container) return ASIS_OS_Time is
   begin
      return The_Container.Obtained;
   end Obtained;

   -------------------
   -- Set_Container --
   -------------------

   function Set_Container
     (Cntnr : Container_Id;
      Cntxt : Context_Id)
      return  Container
   is
      Result_Container : Container := Nil_Container;
   begin

      if Cntnr /= Nil_Container_Id then
         Result_Container :=
           (Id       => Cntnr,
            Cont_Id  => Cntxt,
            Obtained => A_OS_Time);
      end if;

      return Result_Container;

   end Set_Container;

   -----------------------------------------
   -- End of the local subprograms bodies --
   -----------------------------------------

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (The_Container : Container)
      return          Asis.Compilation_Unit_List
   is
   begin

      Check_Validity (The_Container, Package_Name & "Compilation_Units");

      if Is_Nil (The_Container) then
         return Nil_Compilation_Unit_List;
      end if;

      return Compilation_Units (Enclosing_Context (The_Container));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Units");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Compilation_Units",
            Ex          => Ex);
   end Compilation_Units;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   function Compilation_Unit_Bodies
     (The_Container : Container)
      return          Asis.Compilation_Unit_List
   is
   begin

      Check_Validity (The_Container, Package_Name & "Compilation_Unit_Bodies");

      if Is_Nil (The_Container) then
         return Nil_Compilation_Unit_List;
      end if;

      return Compilation_Unit_Bodies (Enclosing_Context (The_Container));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Compilation_Unit_Bodies");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Compilation_Unit_Bodies",
            Ex          => Ex);
   end Compilation_Unit_Bodies;

   -------------------------
   -- Defining_Containers --
   -------------------------

   function Defining_Containers
     (The_Context : Asis.Context)
      return        Container_List
   is
   begin

      Check_Validity (The_Context, Package_Name & "Defining_Containers");

      return
         (1 => Set_Container (First_Container_Id, Get_Cont_Id (The_Context)));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Defining_Containers");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Defining_Containers",
            Ex          => Ex);
   end Defining_Containers;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (The_Container : Container)
      return          Asis.Context
   is
   begin

      Check_Validity (The_Container, Package_Name & "Enclosing_Context");

      if Is_Nil (The_Container) then
         Set_Error_Status (Status    => Value_Error,
                           Diagnosis => "Nil_Contaier in " & Package_Name
                                      & "Enclosing_Context");

         raise ASIS_Inappropriate_Container;
      end if;

      return Get_Cont (Encl_Cont_Id (The_Container));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Enclosing_Context");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Enclosing_Context",
            Ex          => Ex);
   end Enclosing_Context;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : Container;
      Right : Container)
      return  Boolean
   is
   begin

      Check_Validity (Left,  Package_Name & "Is_Equal");
      Check_Validity (Right, Package_Name & "Is_Equal");

      return Is_Equal (Enclosing_Context (Left), Enclosing_Context (Right));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Is_Equal");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Equal",
            Ex          => Ex);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : Container;
      Right : Container)
      return  Boolean
   is
   begin

      Check_Validity (Left,  Package_Name & "Is_Identical");
      Check_Validity (Right, Package_Name & "Is_Identical");

      return Is_Equal (Left, Right) and then
             Is_Equal (Enclosing_Context (Left), Enclosing_Context (Right));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Is_Identical");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Is_Identical",
            Ex          => Ex);
   end Is_Identical;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Container : Container)
      return          Asis.Compilation_Unit_List
   is
   begin

      Check_Validity
        (The_Container, Package_Name & "Library_Unit_Declarations");

      if Is_Nil (The_Container) then
         return Nil_Compilation_Unit_List;
      end if;

      return Library_Unit_Declarations (Enclosing_Context (The_Container));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information
              (Outer_Call => Package_Name & "Library_Unit_Declarations");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Library_Unit_Declarations",
            Ex          => Ex);
   end Library_Unit_Declarations;

   ----------
   -- Name --
   ----------

   function Name
     (The_Container : Container)
      return          Wide_String
   is
   begin

      Check_Validity (The_Container, Package_Name & "Name");

      if Is_Nil (The_Container) then
         return Nil_Asis_Wide_String;
      end if;

      return Name (Enclosing_Context (The_Container));

   exception
      when ASIS_Inappropriate_Container =>
         raise;
      when ASIS_Failed =>

         if Status_Indicator = Unhandled_Exception_Error then
            Add_Call_Information (Outer_Call => Package_Name & "Name");
         end if;

         raise;
      when Ex : others =>
         Report_ASIS_Bug
           (Query_Name  => Package_Name & "Name",
            Ex          => Ex);
   end Name;

end Asis.Ada_Environments.Containers;
