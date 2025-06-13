--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers;

package Eva.Strings
   with Preelaborate
is

   type UTF8 is new Ada.Strings.UTF_Encoding.UTF_8_String;
   subtype Unicode is Wide_Wide_String;
   subtype Unbounded_Unicode is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function Hash
      (Key : Unicode)
      return Ada.Containers.Hash_Type;

   package Unicode_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => Unicode,
       Element_Type    => Unicode,
       Hash            => Hash,
       Equivalent_Keys => "=");

   function Trim
      (Str : Unicode;
       Ch  : Wide_Wide_Character)
      return Unicode;

   function Index
      (Source  : Unicode;
       Pattern : Unicode;
       From    : Positive)
       return Natural;

   function Starts_With
      (Str    : Unicode;
       Prefix : Unicode)
       return Boolean;

   function Ends_With
      (Str    : Unicode;
       Suffix : Unicode)
       return Boolean;

   function Remove_Prefix
      (Str : Unicode;
       Prefix : Unicode)
       return Unicode;

   function Remove_Suffix
      (Str    : Unicode;
       Suffix : Unicode)
       return Unicode;

   function Replace
      (Str, Match, Subst : Unicode)
       return Unicode;

   procedure Append
      (Str : in out Unbounded_Unicode;
       Ch  : Wide_Wide_Character)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   procedure Append
      (Left  : in out Unbounded_Unicode;
       Right : Unbounded_Unicode)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   procedure Append
      (Left  : in out Unbounded_Unicode;
       Right : Unicode)
   renames Ada.Strings.Wide_Wide_Unbounded.Append;

   function Length
      (Str : Unbounded_Unicode)
      return Natural
   renames Ada.Strings.Wide_Wide_Unbounded.Length;

   function To_Unbounded
      (Str : Unicode)
      return Unbounded_Unicode
   renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function To_Unicode
      (Str : Unbounded_Unicode)
      return Unicode
   renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   function Decode
      (Str : UTF8)
      return Unicode;

   function Encode
      (Str : Unicode)
      return UTF8;

   function Encode
      (Str : Unbounded_Unicode)
      return UTF8;

   function LF
      return Unicode;

   function To_String
      (N : Natural)
      return String;

end Eva.Strings;
