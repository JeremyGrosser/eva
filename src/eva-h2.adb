--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
pragma Ada_2022;
with Ada.Text_IO;

package body Eva.H2 is

   type UInt24 is mod 2 ** 24;
   type UInt32 is mod 2 ** 32;

   generic
      type Modular_Type is mod <>;
   function To_Modular
      (Item : Bytes)
      return Modular_Type;

   function To_Modular
      (Item : Bytes)
      return Modular_Type
   is
      Result : Modular_Type := 0;
   begin
      pragma Assert (Item'Length = Modular_Type'Size / 8);
      for I of Item loop
         Result := (Result * 2 ** 8) or Modular_Type (I);
      end loop;
      return Result;
   end To_Modular;

   function To_UInt24 is new To_Modular (UInt24);
   function To_UInt32 is new To_Modular (UInt32);

   type H2_Frame
      (Length : Natural)
   is record
      Typ         : Byte;
      Flags       : Byte;
      Stream_Id   : UInt32;
      Payload     : Bytes (1 .. Length);
   end record;

   function Read_Length
      (Buffer : in out Byte_Buffer)
      return Natural
   is
      Result : Natural;
   begin
      if Length (Buffer) < 3 then
         return 0;
      else
         Result := Natural (To_UInt24 (Slice (Buffer, 1, 3)));
         Delete (Buffer, 3);
         return Result;
      end if;
   end Read_Length;

   function Read_Frame
      (Buffer : in out Byte_Buffer;
       Length : Natural)
      return H2_Frame
   is
      Frame     : H2_Frame (Length => Length);
      Typ       : constant Bytes := Slice (Buffer, 1, 1);
      Flags     : constant Bytes := Slice (Buffer, 2, 2);
      Stream_Id : constant Bytes := Slice (Buffer, 3, 6);
   begin
      Frame.Typ := Typ (Typ'First);
      Frame.Flags := Flags (Flags'First);
      Frame.Stream_Id := To_UInt32 (Stream_Id) and 16#7FFF_FFFF#;
      Delete (Buffer, 6);

      if Frame.Length > 0 then
         Frame.Payload := Slice (Buffer, 1, Frame.Length);
         Delete (Buffer, Frame.Length);
      end if;
      return Frame;
   end Read_Frame;

   procedure On_Frame
      (This  : in out H2_Session;
       Frame : H2_Frame)
   is
      pragma Unreferenced (This);
   begin
      Ada.Text_IO.Put_Line (Frame'Image);
   end On_Frame;

   procedure Poll
      (Session : in out H2_Session;
       Read    : in out Byte_Buffer;
       Write   : in out Byte_Buffer)
   is
      CRLFCRLF       : constant String := ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
      Client_Preface : constant Bytes := To_Bytes ("PRI * HTTP/2.0" & CRLFCRLF & "SM" & CRLFCRLF);
      Server_Preface : constant Bytes := (16#00#, 16#00#, 16#00#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#);
   begin
      Ada.Text_IO.Put_Line ("Before poll: " & Session.State'Image);
      Ada.Text_IO.Put_Line ("Read= " & To_Hex (To_Bytes (Read)));

      if Session.State = Initial then
         Append (Write, Server_Preface);
         Ada.Text_IO.Put_Line ("Write= " & To_Hex (To_Bytes (Write)));
         Session.State := Preface_Sent;
      end if;

      if Session.State = Preface_Sent and then Length (Read) >= Client_Preface'Length then
         if Slice (Read, 1, Client_Preface'Length) = Client_Preface then
            Session.State := Preface_Received;
            Delete (Read, Client_Preface'Length);
         else
            --  wrong preface, not http2
            Session.State := Closed;
         end if;
      end if;

      if Session.State = Preface_Received then
         declare
            Frame_Length : constant Natural := Read_Length (Read);
         begin
            if Frame_Length > Session.SETTINGS_MAX_FRAME_SIZE then
               Session.State := Closed;
               Ada.Text_IO.Put_Line ("Frame length too big:" & Frame_Length'Image);
            elsif Length (Read) >= Frame_Length + 6 then
               declare
                  Frame : constant H2_Frame := Read_Frame (Read, Frame_Length);
               begin
                  On_Frame (Session, Frame);
               end;
            end if;
         end;
      end if;

      Ada.Text_IO.Put_Line ("After poll: " & Session.State'Image);
   end Poll;

end Eva.H2;
