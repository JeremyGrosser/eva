--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Eva.HTTP.Errors is

   procedure Bad_Request
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 400, "Bad Request");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "400 Bad Request" & ASCII.LF);
   end Bad_Request;

   procedure Not_Found
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 404, "Not Found");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "404 Not Found" & ASCII.LF);
   end Not_Found;

   procedure Method_Not_Allowed
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 405, "Method Not Allowed");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "405 Method Not Allowed" & ASCII.LF);
   end Method_Not_Allowed;

end Eva.HTTP.Errors;
