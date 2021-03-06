--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Text.Pool is
   type Reference is tagged private;

   Default_Size : constant Pool_Offset;

   --  must be called once before the string pool can be used. if called again,
   --  the string pool re-initializes itself with new memory, and the old memory
   --  lives on only in References that have already been generated. the
   --  old memory is reclaimed once all string references to it vanish.
   procedure Create (P : in out Reference'Class;
                     Initial_Size : Pool_Offset := Default_Size);

   --  constructor that calls Create with the given Size parameter
   function With_Capacity (Size : Pool_Offset) return Reference;

   --  create a new string from the given data. the string will be allocated
   --  within the pool.
   function From_String (P : Reference'Class; Data : String)
                         return Text.Reference;

private
   type Reference is tagged record
      Data : Pool_Data_Access;
   end record with Type_Invariant =>
     (Reference.Data = null or else Reference.Data.Pos mod Header_Size = 1);

   Default_Size : constant Pool_Offset := 8192;
end Text.Pool;
