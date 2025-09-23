--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Hex_Format_8;
with Interfaces;

package body Eva is
   function To_String
      (Item : Bytes)
      return String
   is
      Result : String (Item'Range);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Item (I));
      end loop;
      return Result;
   end To_String;

   function To_Bytes
      (Item : String)
      return Bytes
   is
      Result : Bytes (Item'Range);
   begin
      for I in Result'Range loop
         Result (I) := Character'Pos (Item (I));
      end loop;
      return Result;
   end To_Bytes;

   function To_Hex
      (Item : Bytes)
      return String
   is
      use Hex_Format_8;
      Result : String (1 .. Item'Length * 3);
      I : Positive := Result'First;
   begin
      for J in Item'Range loop
         Result (I .. I + 1) := Hex (Interfaces.Unsigned_8 (Item (J)));
         Result (I + 2) := ' ';
         I := I + 3;
      end loop;
      return Result (Result'First .. Result'Last - 1);
   end To_Hex;

   procedure Append
      (This : in out Byte_Buffer;
       Item : Bytes)
   is
      First : constant Positive := This.Last + 1;
      Last  : constant Natural := First + Item'Length - 1;
   begin
      if Item'Length = 0 then
         return;
      elsif First in This.Item'Range and then Last in This.Item'Range then
         This.Item (First .. Last) := Item;
         This.Last := Last;
      else
         raise Constraint_Error with "Not enough space in buffer";
      end if;
   end Append;

   procedure Delete
      (This : in out Byte_Buffer;
       Count : Natural)
   is
   begin
      This.First := This.First + Count;
   end Delete;

   procedure Clear
      (This : in out Byte_Buffer)
   is
   begin
      This.Last := 0;
   end Clear;

   function Length
      (This : Byte_Buffer)
      return Natural
   is ((This.Last + 1) - This.First);

   function Available
      (This : Byte_Buffer)
      return Natural
   is (This.Capacity - This.Last);

   function Offset
      (This  : Byte_Buffer;
       Count : Positive)
       return Natural
   is (This.First + (Count - 1));

   function Slice
      (This : Byte_Buffer;
       First, Last : Positive)
       return Bytes
   is (This.Item (Offset (This, First) .. Offset (This, Last)));

   function To_Bytes
      (This : Byte_Buffer)
      return Bytes
   is (This.Item (This.First .. This.Last));

   function To_String
      (This : Byte_Buffer)
      return String
   is (To_String (To_Bytes (This)));

end Eva;
