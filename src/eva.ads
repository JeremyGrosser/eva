--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Eva
   with Preelaborate, SPARK_Mode => On
is
   type Byte is mod 2 ** 8 with Size => 8;
   type Bytes is array (Positive range <>) of Byte
      with Component_Size => 8;

   function To_String
      (Item : Bytes)
      return String;

   function To_Bytes
      (Item : String)
      return Bytes;

   function To_Hex
      (Item : Bytes)
      return String;

   type Byte_Buffer
      (Capacity : Positive)
   is private;

   procedure Append
      (This : in out Byte_Buffer;
       Item : Bytes);

   procedure Delete
      (This  : in out Byte_Buffer;
       Count : Natural);

   procedure Clear
      (This : in out Byte_Buffer);

   function Length
      (This : Byte_Buffer)
      return Natural;

   function Available
      (This : Byte_Buffer)
      return Natural;

   function Slice
      (This : Byte_Buffer;
       First, Last : Positive)
       return Bytes;

   function To_Bytes
      (This : Byte_Buffer)
      return Bytes;

   function To_String
      (This : Byte_Buffer)
      return String;

private

   type Byte_Buffer
      (Capacity : Positive)
   is record
      Item  : Bytes (1 .. Capacity) := (others => 0);
      First : Positive := 1;
      Last  : Natural := 0;
   end record;

end Eva;
