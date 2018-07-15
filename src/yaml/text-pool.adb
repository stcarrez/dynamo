--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Pool is
   procedure Create (P : in out Reference'Class;
                     Initial_Size : Pool_Offset := Default_Size)
   is
      Initial_Chunk : constant Chunk := new Pool_Array
        (Pool_Offset (1) .. Round_To_Header_Size (Initial_Size));
   begin
      P.Data := new Pool_Data;
      P.Data.Chunks (1) := Initial_Chunk;
      P.Data.Pos := 1;
      declare
         H : Header with Import;
         for H'Address use Initial_Chunk.all (1)'Address;
      begin
         H.Refcount := 0;
         H.Last := Pool_Offset (Initial_Chunk'Last) - Header_Size;
      end;
   end Create;

   function With_Capacity (Size : Pool_Offset) return Reference is
   begin
      return Ret : Reference do
         Create (Ret, Size);
      end return;
   end With_Capacity;

   function From_String (P : Reference'Class; Data : String)
                         return Text.Reference is
      pragma Unreferenced (P);
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Data);
   end From_String;

end Text.Pool;
