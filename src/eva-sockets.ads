--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces.C;
with Interfaces;

package Eva.Sockets
   with Preelaborate
is
   Socket_Error : exception;

   subtype Socket_Type is Interfaces.C.int;
   type Inet_Port is new Interfaces.Unsigned_16;

   function Bind
      (Port : Inet_Port)
      return Socket_Type;

   procedure Accept_Socket
      (Listen_Sock : Socket_Type;
       Client_Sock : out Socket_Type);

   procedure Close_Socket
      (Sock : Socket_Type);

end Eva.Sockets;
