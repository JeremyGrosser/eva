--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Eva.Sockets;

generic
   type Context_Type is private;
   type Any_Context_Type is access Context_Type;
   with procedure Handle_Request
      (Context : Any_Context_Type;
       Req     : Eva.HTTP.Request;
       Resp    : in out Eva.HTTP.Response);
package Eva.HTTP.Server
   with Elaborate_Body
is
   procedure Run
      (Context   : Any_Context_Type;
       Port      : Eva.Sockets.Inet_Port := 9999;
       Cert_File : String := "server.crt";
       Key_File  : String := "server.key");
   procedure Stop;
end Eva.HTTP.Server;
