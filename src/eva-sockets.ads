--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces.C;

package Eva.Sockets
   with Preelaborate
is
   --  This package provides a stripped down version of GNAT.Sockets. This
   --  package only supports IPv6 TCP sockets. It adds support for the
   --  SO_REUSEPORT socket option, which is missing from GNAT.Sockets.
   --
   --  There's a lot of questionable C interfacing in the body of this package.
   --  It is not compatible with systems other than recent Linux on x86_64.

   Socket_Error : exception;

   subtype Socket_Type is Interfaces.C.int;

   type UInt8 is mod 2 ** 8
      with Size => 8;
   type UInt16 is mod 2 ** 16
      with Size => 16;
   type UInt32 is mod 2 ** 32
      with Size => 32;

   type Inet_Addr is array (1 .. 16) of UInt8
      with Size => 128;
   type Inet_Port is mod 2 ** 16
      with Size => 16;

   AF_INET6 : constant := 10;

   type Sock_Addr is record
      Family   : UInt16 := AF_INET6;
      Port     : Inet_Port := 0;
      Flowinfo : UInt32 := 0;
      Addr     : Inet_Addr := (others => 0);
      Scope_Id : UInt32 := 0;
   end record
      with Convention => C;

   type Socket_Option is
      (Reuse_Address, Reuse_Port);
   for Socket_Option use
      (Reuse_Address => 2,
       Reuse_Port => 15);

   procedure Create_Socket
      (Sock : out Socket_Type);

   procedure Set_Socket_Option
      (Sock   : Socket_Type;
       Option : Socket_Option);

   procedure Bind_Socket
      (Sock : Socket_Type;
       Addr : Sock_Addr);

   procedure Listen_Socket
      (Sock : Socket_Type;
       Backlog : Natural);

   procedure Accept_Socket
      (Server : Socket_Type;
       Sock   : out Socket_Type;
       Addr   : out Sock_Addr);

   procedure Close_Socket
      (Sock : Socket_Type);

   procedure Receive_Socket
      (Sock : Socket_Type;
       Data : out String;
       Last : out Natural);

   procedure Send_Socket
      (Sock : Socket_Type;
       Data : String;
       Last : out Natural);

end Eva.Sockets;
