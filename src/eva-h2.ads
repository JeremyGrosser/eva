--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Eva.H2 is

   type H2_State is (Initial, Preface_Sent, Preface_Received, Closed);

   type H2_Session is record
      State : H2_State := Initial;
      SETTINGS_MAX_FRAME_SIZE : Natural := 16_384;
   end record;

   procedure Poll
      (Session : in out H2_Session;
       Read    : in out Byte_Buffer;
       Write   : in out Byte_Buffer);

end Eva.H2;
