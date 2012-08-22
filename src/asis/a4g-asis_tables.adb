------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                      A 4 G . A S I S _ T A B L E S                       --
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

with Asis.Elements; use Asis.Elements;

with Atree;         use Atree;
with Sinput;        use Sinput;
with Einfo;         use Einfo;
with Nlists;        use Nlists;

package body A4G.Asis_Tables is

   ---------------------
   -- Add_New_Element --
   ---------------------

   procedure Add_New_Element (Element : Asis.Element) is
      Found : Boolean := False;
   begin
      for J in 1 .. Asis_Element_Table.Last loop
         if Is_Equal (Element, Asis_Element_Table.Table (J)) then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Asis_Element_Table.Append (Element);
      end if;
   end Add_New_Element;

   -----------------------
   -- Create_Node_Trace --
   -----------------------

   procedure Create_Node_Trace (N : Node_Id) is
      Next_Node     : Node_Id;
      Next_Sloc     : Source_Ptr;
      Next_Node_Rec : Node_Trace_Rec;
   begin
      Node_Trace.Init;

      Next_Node := N;

      while Present (Next_Node) loop
         Next_Sloc := Sloc (Next_Node);

         Next_Node_Rec.Kind      := Nkind                    (Next_Node);
         Next_Node_Rec.Node_Line := Get_Physical_Line_Number (Next_Sloc);
         Next_Node_Rec.Node_Col  := Get_Column_Number        (Next_Sloc);

         Node_Trace.Append (Next_Node_Rec);

         Next_Node := Enclosing_Scope (Next_Node);
      end loop;

   end Create_Node_Trace;

   ---------------------
   -- Enclosing_Scope --
   ---------------------

   function Enclosing_Scope (N : Node_Id) return Node_Id is
      Result      : Node_Id := N;
      Entity_Node : Entity_Id := Empty;
   begin

      if Nkind (Result) = N_Package_Declaration then
         Entity_Node := Defining_Unit_Name (Sinfo.Specification (Result));
      elsif Nkind (Result) = N_Package_Body then
         Entity_Node := Defining_Unit_Name (Result);
      end if;

      if Nkind (Entity_Node) = N_Defining_Program_Unit_Name then
         Entity_Node := Sinfo.Defining_Identifier (Entity_Node);
      end if;

      if Present (Entity_Node) and then
         Is_Generic_Instance (Entity_Node)
      then
         --  going to the corresponding instantiation

         if Nkind (Parent (Result)) = N_Compilation_Unit then
            --  We are at the top/ and we do not need a library-level
            --  instantiation - it is always unique in the compilation
            --  unit
            Result := Empty;
         else
            --  "local" instantiation, therefore - one or two steps down the
            --  declaration list to get in the instantiation node:
            Result := Next_Non_Pragma (Result);

            if Nkind (Result) = N_Package_Body then
               --  This is an expanded generic body
               Result := Next_Non_Pragma (Result);
            end if;

         end if;

      else
         --  One step up to the enclosing scope
         Result := Parent (Result);

         while not (Nkind (Result) = N_Package_Specification or else
                    Nkind (Result) = N_Package_Body          or else
                    Nkind (Result) = N_Compilation_Unit      or else
                    Nkind (Result) = N_Subprogram_Body       or else
                    Nkind (Result) = N_Block_Statement)
         loop
            Result := Parent (Result);
         end loop;

         if Nkind (Result) = N_Package_Specification then
            Result := Parent (Result);

         elsif Nkind (Result) = N_Compilation_Unit then
            Result := Empty;
         end if;

      end if;

      return Result;
   end Enclosing_Scope;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (N         : Node_Id;
      Trace_Rec : Node_Trace_Rec)
      return      Boolean
   is
   begin

      return Nkind (N)                           = Trace_Rec.Kind      and then
             Get_Physical_Line_Number (Sloc (N)) = Trace_Rec.Node_Line and then
             Get_Column_Number (Sloc (N))        = Trace_Rec.Node_Col;

   end Is_Equal;

end A4G.Asis_Tables;
