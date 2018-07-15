--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Strings.Hash;

package body Text is

   function "&" (Left, Right : Reference) return String is
       (Ada.Strings.Unbounded.To_String (Left) & Ada.Strings.Unbounded.To_String (Right));

   function Hash (Object : Reference) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Ada.Strings.Unbounded.To_String (Object)));

end Text;
