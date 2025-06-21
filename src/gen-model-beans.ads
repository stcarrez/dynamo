-----------------------------------------------------------------------
--  gen-model-beans -- Ada Bean declarations
--  Copyright (C) 2012, 2013, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Gen.Model.Tables;
package Gen.Model.Beans is

   --  ------------------------------
   --  Bean Definition
   --  ------------------------------
   type Bean_Definition is new Tables.Table_Definition with null record;
   type Bean_Definition_Access is access all Bean_Definition'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Bean_Definition;
                       Name : in String) return UBO.Object;

   --  Create an attribute with the given name and add it to the bean.
   procedure Add_Attribute (Bean   : in out Bean_Definition;
                            Name   : in UString;
                            Column : out Gen.Model.Tables.Column_Definition_Access);

   --  Create a bean with the given name.
   function Create_Bean (Name : in UString) return Bean_Definition_Access;

end Gen.Model.Beans;
