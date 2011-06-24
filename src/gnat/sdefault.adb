pragma Style_Checks (Off);
with Osint; use Osint;
package body Sdefault is
   S0 : constant String := "/usr/local/";
   S1 : constant String := "/usr/local/lib/gcc/i686-pc-linux-gnu/4.4.5/adainclude/";
   S2 : constant String := "/usr/local/lib/gcc/i686-pc-linux-gnu/4.4.5/adalib/";
   S3 : constant String := "i686-pc-linux-gnu/";
   S4 : constant String := "/usr/local/lib/gcc/i686-pc-linux-gnu/4.4.5/";
   function Include_Dir_Default_Name return String_Ptr is
   begin
      return Relocate_Path (S0, S1);
   end Include_Dir_Default_Name;
   function Object_Dir_Default_Name return String_Ptr is
   begin
      return Relocate_Path (S0, S2);
   end Object_Dir_Default_Name;
   function Target_Name return String_Ptr is
   begin
      return new String'(S3);
   end Target_Name;
   function Search_Dir_Prefix return String_Ptr is
   begin
      return Relocate_Path (S0, S4);
   end Search_Dir_Prefix;
end Sdefault;
