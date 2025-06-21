-----------------------------------------------------------------------
--  gen-model -- Model for Code Generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2018, 2019, 2020, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with DOM.Core.Nodes;
with Gen.Utils;
package body Gen.Model is

   Trim_Chars : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & ASCII.HT & ASCII.LF & ASCII.CR);

   --  ------------------------------
   --  Get the object unique name.
   --  ------------------------------
   function Get_Name (From : in Definition) return String is
   begin
      return To_String (From.Def_Name);
   end Get_Name;

   function Name (From : in Definition) return UString is
   begin
      return From.Def_Name;
   end Name;

   --  ------------------------------
   --  Set the object unique name.
   --  ------------------------------
   procedure Set_Name (Def  : in out Definition;
                       Name : in String) is
   begin
      Def.Def_Name := To_UString (Name);
   end Set_Name;

   procedure Set_Name (Def  : in out Definition;
                       Name : in UString) is
   begin
      Def.Def_Name := Name;
   end Set_Name;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Definition;
                       Name : in String) return UBO.Object is
   begin
      if Name = "comment" then
         return From.Comment;

      elsif Name = "rowIndex" then
         return UBO.To_Object (From.Row_Index);

      elsif Name = "name" then
         return UBO.To_Object (From.Def_Name);
      else
         return From.Attrs.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : in Definition;
                           Name : in String) return String is
      V : constant UBO.Object := From.Get_Value (Name);
   begin
      return UBO.To_String (V);
   end Get_Attribute;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Attribute (From : in Definition;
                           Name : in String) return UString is
   begin
      return To_UString (From.Get_Attribute (Name));
   end Get_Attribute;

   --  ------------------------------
   --  Set the comment associated with the element.
   --  ------------------------------
   procedure Set_Comment (Def     : in out Definition;
                          Comment : in String) is
      Trimmed_Comment : constant String
        := Ada.Strings.Fixed.Trim (Comment, Trim_Chars, Trim_Chars);
   begin
      Def.Comment := UBO.To_Object (Trimmed_Comment);
   end Set_Comment;

   --  ------------------------------
   --  Get the comment associated with the element.
   --  ------------------------------
   function Get_Comment (Def : in Definition) return UBO.Object is
   begin
      return Def.Comment;
   end Get_Comment;

   --  ------------------------------
   --  Set the location (file and line) where the model element is defined in the XMI file.
   --  ------------------------------
   procedure Set_Location (Node     : in out Definition;
                           Location : in String) is
   begin
      Node.Location := To_UString (Location);
   end Set_Location;

   --  ------------------------------
   --  Get the location file and line where the model element is defined.
   --  ------------------------------
   function Get_Location (Node : in Definition) return String is
   begin
      return To_String (Node.Location);
   end Get_Location;

   --  ------------------------------
   --  Initialize the definition from the DOM node attributes.
   --  ------------------------------
   procedure Initialize (Def  : in out Definition;
                         Name : in UString;
                         Node : in DOM.Core.Node) is
      use type DOM.Core.Node;

      Attrs : constant DOM.Core.Named_Node_Map := DOM.Core.Nodes.Attributes (Node);
   begin
      Def.Def_Name := Name;
      Def.Comment := UBO.To_Object (Gen.Utils.Get_Comment (Node));

      for I in 0 .. DOM.Core.Nodes.Length (Attrs) loop
         declare
            A : constant DOM.Core.Node := DOM.Core.Nodes.Item (Attrs, I);
         begin
            if A /= null then
               declare
                  Name  : constant DOM.Core.DOM_String := DOM.Core.Nodes.Node_Name (A);
                  Value : constant DOM.Core.DOM_String := DOM.Core.Nodes.Node_Value (A);
               begin
                  Def.Attrs.Include (Name, UBO.To_Object (Value));
               end;
            end if;
         end;
      end loop;
   end Initialize;

   --  ------------------------------
   --  Validate the definition by checking and reporting problems to the logger interface.
   --  ------------------------------
   procedure Validate (Def : in out Definition;
                       Log : in out Util.Log.Logging'Class) is
   begin
      if Length (Def.Def_Name) = 0 then
         Log.Error (Def.Get_Location & ": name is empty");
      end if;
   end Validate;

   procedure Set_Index (Def   : in out Definition;
                        Index : in Natural) is
   begin
      Def.Row_Index := Index;
   end Set_Index;

end Gen.Model;
