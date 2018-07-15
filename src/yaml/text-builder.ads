--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Text.Pool;

package Text.Builder is
   type Reference is tagged private;

   procedure Init (Object : in out Reference; Pool : Text.Pool.Reference;
                   Initial_Size : Positive := 255);

   function Create (Pool : Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference;

   function Initialized (Object : Reference) return Boolean;

   procedure Append (Object : in out Reference; Value : String)
     with Pre => Object.Initialized;

   procedure Append (Object : in out Reference; Value : Character)
     with Pre => Object.Initialized;

   procedure Append (Object : in out Reference; Value : Text.Reference)
     with Pre => Object.Initialized;

   function Lock (Object : in out Reference) return Text.Reference;

   function Length (Object : Reference) return Natural;
private
   type Reference is tagged record
      Buffer : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Text.Builder;
