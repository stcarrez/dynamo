------------------------------------------------------------------------------
--                                                                          --
--                 ASIS-for-GNAT IMPLEMENTATION COMPONENTS                  --
--                                                                          --
--                             A S I S . I D S                              --
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
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License  distributed with ASIS-for-GNAT; see file     --
-- COPYING. If not, write to the Free Software Foundation,  59 Temple Place --
-- - Suite 330,  Boston, MA 02111-1307, USA.                                --
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
-- Sciences.  ASIS-for-GNAT is now maintained by  Ada Core Technologies Inc --
-- (http://www.gnat.com).                                                   --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Characters.Handling; use Ada.Characters.Handling;

with A4G.Vcheck; use A4G.Vcheck;

package body Asis.Ids is
------------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Hash (The_Id : Id) return Asis.ASIS_Integer is
   begin
      pragma Unreferenced (The_Id);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.Hash");
      return 0;
   end Hash;
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function "<" (Left  : Id;
                 Right : Id) return Boolean is
   begin
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.""<""");
      return True;
   end "<";
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function ">" (Left  : Id;
                 Right : Id) return Boolean is
   begin
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids."">""");
      return False;
   end ">";
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Is_Nil (Right : Id) return Boolean is
   begin
      pragma Unreferenced (Right);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.Is_Nil");
      return True;
   end Is_Nil;
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Is_Equal
     (Left  : Id;
      Right : Id)
      return Boolean
   is
   begin
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.Is_Equal");
      return True;
   end Is_Equal;
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Create_Id (Element : Asis.Element) return Id is
   begin
      pragma Unreferenced (Element);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.Create_Id");
      return Nil_Id;
   end Create_Id;
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Create_Element
     (The_Id      : Id;
      The_Context : Asis.Context)
      return Asis.Element
   is
   begin
      pragma Unreferenced (The_Id);
      pragma Unreferenced (The_Context);
      Raise_ASIS_Failed (Diagnosis => "Asis.Ids.Create_Element");
      return Nil_Element;
   end Create_Element;
-----------------------------------------------------------------------------
--  NOT IMPLEMENTED

   function Debug_Image (The_Id : Id) return Wide_String is
   begin

      if Is_Nil (The_Id) then
         return Nil_Asis_Wide_String;
      else
         return To_Wide_String (The_Id.all);
      end if;

   end Debug_Image;
-----------------------------------------------------------------------------
end Asis.Ids;
