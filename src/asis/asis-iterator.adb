------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                          A S I S . I T E R A T O R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1995-2010, Free Software Foundation, Inc.       --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with ASIS-for-GNAT;  see file --
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

with Asis.Elements;
with Asis.Exceptions; use Asis.Exceptions;

with A4G.Vcheck; use A4G.Vcheck;

with A4G.Queries; use A4G.Queries;

-----------------------------------------------------------------
--                                                             --
-- Process_Children is the function that gets all the children --
-- and calls Recursive_Traversal on them. To get the children  --
-- it uses a function that takes an element and returns all    --
-- the queries that can obtain children from this element.     --
-- (see asis_elements-queries.ads)                           --
--                                                             --
-- This way, the generic body to instanciate doesn't contain   --
-- the procedures that obtain the children, the code is not    --
-- duplicated, and so we have a gain in performance (time &   --
-- memory).                                                   --
--                                                             --
-- Concerning the Control, all Pre and Post-conditions have    --
-- been put at the begining and end of the procedures and      --
-- blocks that deal with them.                                 --
--                                                             --
-- The (Control = Terminate_Immediatly) has been handled by    --
-- returning from all the recursive calls ...                  --
--                                                             --
-----------------------------------------------------------------

package body Asis.Iterator is

   procedure Traverse_Element
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out State_Information)
   is
      procedure Recursive_Traversal
        (Element :        Asis.Element;
         Control : in out Traverse_Control);
      --  This procedure does the main job

      procedure Traverse_Children
        (Element :        Asis.Element;
         Control : in out Traverse_Control);
      --  Traverses children of a given construct

      ------------------------------------------------------
      -- Pre-condition: any value of Control is possible  --
      ------------------------------------------------------

      procedure Traverse_Children
        (Element :        Asis.Element;
         Control : in out Traverse_Control)
      is
         --  The value of Control has been set by Pre_Operation

         --  Child access is an array containing access to the functions
         --  needed to access element's children
         Child_Access : constant Query_Array := Appropriate_Queries (Element);

         function Do_Return return Boolean;
            --  Check and reset the Control value on return from the traverse.
            --  the boolean returned says wether or not the program should
            --  return

         function Do_Return return Boolean is
         begin
         --------------------------------------------------------
         -- Post-condition:   Control  = Continue              --
         --                or Control  = Abandon_Siblings      --
         --                or Control  = Terminate_Immediately --
         --------------------------------------------------------
            case Control is
               when Terminate_Immediately =>
                  return True;
               when Continue              =>
                  return False;
               when Abandon_Siblings      =>
                  Control := Continue;
                  --  to continue the traversal of the parent
                  --  of the Each_Child (that is, Element) with
                  --  its Post_Operation
                  return True;  -- to prevent traversal of Each_Child siblings
               when Abandon_Children =>
                  --  this choice could never been chosen!!!
                  return False;
            end case;
         ---------------------------------------------------------------
         -- Post-Condition : Control = Continue (True or False)     --
         --               or Control = Terminate_Immediately (True) --
         ---------------------------------------------------------------
         end Do_Return;

      begin  --  Traverse_Children

         --  Validity Check has already been done

         ------------------------------------------
         -- Pre-condition:   Control  = Continue --
         ------------------------------------------
         --  Classify the Element using the various kinds queries.
         --  Query for all children of the Element in left-to-right order.
         --  Perform a depth-first traversal on each child.

         --  The only possibility for Control is to be equal to Continue here!
         --  If the current Element has no children, Control remains to be
         --  equal to Continue

         for Each_Query in Child_Access'Range loop
            case Child_Access (Each_Query).Query_Kind is
               when Bug =>
                  raise Internal_Implementation_Error;
               when Single_Element_Query =>
                  declare
                     Child : constant Asis.Element :=
                       Child_Access (Each_Query).Func_Simple (Element);
                  begin

                     if Asis.Elements.Element_Kind (Child) /=
                        Not_An_Element
                     then
                        Recursive_Traversal (Child, Control);

                        if Do_Return then
                           return;
                        end if;

                     end if;

                  end;

               when Element_List_Query =>
                  declare
                     Child_List : constant Asis.Element_List :=
                       Child_Access (Each_Query).Func_List (Element);
                  begin
                     --  If the list is empty, it's ok ... nothing is processed
                     for Each_Element in Child_List'Range loop

                        Recursive_Traversal
                          (Child_List (Each_Element), Control);

                        if Do_Return then
                           return;
                        end if;

                     end loop;
                  end;

               when Element_List_Query_With_Boolean =>
                  declare
                     Child_List : constant Asis.Element_List :=
                       Child_Access (Each_Query).Func_List_Boolean
                         (Element, Child_Access (Each_Query).Bool);
                  begin
                     --  If the list is empty, it's ok ... nothing is processed
                     for Each_Element in Child_List'Range loop

                        Recursive_Traversal
                          (Child_List (Each_Element), Control);

                        if Do_Return then
                           return;
                        end if;

                     end loop;
                  end;

            end case;
         end loop;
         -------------------------------------------
         -- Post-condition:   Control  = Continue --
         -------------------------------------------
         -- if Terminate_Immediately was set, we  --
         -- just do not entry this procedure ...  --
         -------------------------------------------

      end Traverse_Children;
      --------------------------------------------------------
      -- Post-condition: any value of Control is possible,  --
      --------------------------------------------------------

      -------------------------
      -- Recursive_Traversal --
      -------------------------

      ----------------------------------------
      -- Pre-condition: Control = Continue  --
      ----------------------------------------
      procedure Recursive_Traversal
        (Element :        Asis.Element;
         Control : in out Traverse_Control) is
      begin

         ----------------------------------------
         -- Pre-condition: Control = Continue  --
         ----------------------------------------

         begin
            Pre_Operation (Element, Control, State); -- Visit the Element.
         exception
            when ASIS_Inappropriate_Context          |
                 ASIS_Inappropriate_Container        |
                 ASIS_Inappropriate_Compilation_Unit |
                 ASIS_Inappropriate_Element          |
                 ASIS_Inappropriate_Line             |
                 ASIS_Inappropriate_Line_Number      |
                 ASIS_Failed                         =>

               Add_Call_Information (
                  Argument   => Element,
                  Outer_Call => "Actual procedure for Pre_Operation");

               raise;
         end;

         --------------------------------------------------------
         -- Post-condition: any value of Control is possible   --
         --------------------------------------------------------

         if Control = Continue then
            Traverse_Children (Element, Control);
         end if;

         --------------------------------------------------------
         -- Pre-condition: any value of Control is possible,  --
         --------------------------------------------------------

         case Control is
            when Terminate_Immediately =>
               return;
            when Continue =>

               begin
                  --  Revisit the Element
                  Post_Operation (Element, Control, State);
               exception
                  when ASIS_Inappropriate_Context          |
                       ASIS_Inappropriate_Container        |
                       ASIS_Inappropriate_Compilation_Unit |
                       ASIS_Inappropriate_Element          |
                       ASIS_Inappropriate_Line             |
                       ASIS_Inappropriate_Line_Number      |
                       ASIS_Failed                         =>

                     Add_Call_Information (
                        Argument   => Element,
                        Outer_Call => "Actual procedure for Post_Operation");

                     raise;
               end;

               --  reset the Control set by Post_Operation:
               case Control is
                  when Terminate_Immediately =>
                     return;
                  when Continue         =>
                     null;
                  when Abandon_Children =>
                     Control := Continue;
                     --  the current Element has no children to traverse
                     --  anymore!
                  when Abandon_Siblings =>
                     null;
               end case;

            when Abandon_Children =>
               --  OK, we abandonned the children, now we go up and continue
               Control := Continue;
            when Abandon_Siblings =>
               null;
         end case;
         ---------------------------------------------------------
         -- Post-condition:   Control  = Continue               --
         --                or Control  = Abandon_Siblings       --
         --                or Control = Terminate_Immediately   --
         ---------------------------------------------------------
      end Recursive_Traversal;
      ---------------------------------------------------------
      -- Post-condition:   Control  = Continue               --
      --                or Control  = Abandon_Siblings       --
      --                or Control = Terminate_Immediately   --
      ---------------------------------------------------------

   ---------------------------------
   -- Traversal_Element Main body --
   ---------------------------------

   begin
      Check_Validity (Element, "Asis.Elements.Traverse_Element");

      if Asis.Elements.Is_Nil (Element) then
         Raise_ASIS_Inappropriate_Element
           ("Asis.Iterator.Traverse_Element",
            Wrong_Kind => Not_An_Element);
      elsif Control /= Continue then
         return;
      end if;

      ----------------------------------------
      -- Pre-condition: Control = Continue  --
      ----------------------------------------
      Recursive_Traversal (Element => Element,
                           Control => Control);
   exception
      when ASIS_Inappropriate_Element          |
           ASIS_Inappropriate_Context          |
           ASIS_Inappropriate_Container        |
           ASIS_Inappropriate_Compilation_Unit |
           ASIS_Inappropriate_Line             |
           ASIS_Inappropriate_Line_Number      |
           ASIS_Failed                         =>

         Add_Call_Information
           (Argument   => Element,
            Outer_Call => "Asis.Iterator.Traverse_Element");

         raise;
      --  when others =>
      --  Actual Pre- and Postoperations can raise whatever they want, and
      --  at the level of Traverse_Element we can (and should) do nothing
      --  with this. So we just let this exception go ahead
      --   raise;
   end Traverse_Element;

end Asis.Iterator;
