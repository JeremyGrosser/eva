--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Hash;

package body Eva.Strings is

   function Hash
      (Key : Unicode)
      return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (String (Encode (Key))));

   function Trim
      (Str : Unicode;
       Ch  : Wide_Wide_Character)
       return Unicode
   is
      First : Natural := Str'First;
      Last  : Natural := Str'Last;
   begin
      while First <= Str'Last and then Str (First) = Ch loop
         First := First + 1;
      end loop;

      while Last >= Str'First and then Str (Last) = Ch loop
         Last := Last - 1;
      end loop;
      return Str (First .. Last);
   end Trim;

   function Replace
      (Str   : Unicode;
       Match : Unicode;
       Subst : Unicode)
       return Unicode
   is
      First : constant Natural := Eva.Strings.Index (Str, Match, 1);
   begin
      if First = 0 then
         return Str;
      else
         declare
            Head : constant Unicode := Str (Str'First .. First - 1);
            Tail : constant Unicode := Str (First + Match'Length .. Str'Last);
         begin
            return Head & Subst & Tail;
         end;
      end if;
   end Replace;

   function Index
      (Source : Unicode;
       Pattern : Unicode;
       From : Positive)
       return Natural
   is
      First : Positive := From;
   begin
      while First <= Source'Last - (Pattern'Length - 1) loop
         if Source (First .. First + (Pattern'Length - 1)) = Pattern then
            return First;
         else
            First := First + 1;
         end if;
      end loop;

      return 0;
   end Index;

   function Starts_With
      (Str    : Unicode;
       Prefix : Unicode)
       return Boolean
   is (Str'Length >= Prefix'Length and then Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix);

   function Ends_With
      (Str    : Unicode;
       Suffix : Unicode)
       return Boolean
   is (Str'Length >= Suffix'Length and then Str (Str'Last - Suffix'Length + 1 .. Str'Last) = Suffix);

   function Remove_Prefix
      (Str    : Unicode;
       Prefix : Unicode)
       return Unicode
   is
   begin
      if Starts_With (Str, Prefix) then
         return Str (Str'First + Prefix'Length .. Str'Last);
      else
         return Str;
      end if;
   end Remove_Prefix;

   function Remove_Suffix
      (Str    : Unicode;
       Suffix : Unicode)
       return Unicode
   is
   begin
      if Ends_With (Str, Suffix) then
         return Str (Str'First .. Str'Last - Suffix'Length);
      else
         return Str;
      end if;
   end Remove_Suffix;

   function Decode
      (Str : UTF8)
      return Unicode
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (String (Str)));

   function Encode
      (Str : Unicode)
      return UTF8
   is
      WWS : constant Wide_Wide_String := Wide_Wide_String (Str);
      U8  : constant Ada.Strings.UTF_Encoding.UTF_8_String := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (WWS);
   begin
      return UTF8 (U8);
   end Encode;

   function Encode
      (Str : Unbounded_Unicode)
      return UTF8
   is
      WWS : constant Wide_Wide_String := To_Unicode (Str);
      U8  : constant Ada.Strings.UTF_Encoding.UTF_8_String := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (WWS);
   begin
      return UTF8 (U8);
   end Encode;

   function LF
      return Unicode
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (String'("" & ASCII.LF)));

   function To_String
      (N : Natural)
      return String
   is
      Num : constant array (0 .. 9) of Character := "0123456789";
      S : String (1 .. 10);
      I : Natural := S'Last;
      J : Natural := N;
   begin
      loop
         exit when J = 0 or else I = 0;
         S (I) := Num (J mod 10);
         J := J / 10;
         I := I - 1;
      end loop;
      return S (I + 1 .. S'Last);
   end To_String;
end Eva.Strings;
