--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Builder is

   procedure Init (Object : in out Reference; Pool : Text.Pool.Reference;
                   Initial_Size : Positive := 255) is
   begin
      null;
   end Init;

   function Create (Pool : Text.Pool.Reference;
                    Initial_Size : Positive := 255) return Reference is
   begin
      return Ret : Reference do
         Init (Ret, Pool, Initial_Size);
      end return;
   end Create;

   function Initialized (Object : Reference) return Boolean is
     (True);

   procedure Append (Object : in out Reference; Value : String) is
   begin
      Ada.Strings.Unbounded.Append (Object.Buffer, Value);
   end Append;

   procedure Append (Object : in out Reference; Value : Character) is
   begin
      Ada.Strings.Unbounded.Append (Object.Buffer, Value);
   end Append;

   procedure Append (Object : in out Reference; Value : Text.Reference) is
   begin
      Ada.Strings.Unbounded.Append (Object.Buffer, Value);
   end Append;

   function Lock (Object : in out Reference) return Text.Reference is
   begin
      return Object.Buffer;
   end Lock;

   function Length (Object : Reference) return Natural is
     (Ada.Strings.Unbounded.Length (Object.Buffer));

end Text.Builder;
