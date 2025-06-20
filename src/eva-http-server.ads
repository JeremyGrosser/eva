--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Eva.Sockets;

generic
   with procedure Handle_Request
      (Req  : Eva.HTTP.Request;
       Resp : in out Eva.HTTP.Response);
package Eva.HTTP.Server
   with Elaborate_Body
is
   procedure Run
      (Port : Eva.Sockets.Inet_Port := 9999);
   procedure Stop;
end Eva.HTTP.Server;
