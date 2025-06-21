with Eva.Strings; use Eva.Strings;

package body Eva.HTTP.Errors is

   procedure Bad_Request
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 400, "Bad Request");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "400 Bad Request" & LF);
   end Bad_Request;

   procedure Not_Found
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 404, "Not Found");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "404 Not Found" & LF);
   end Not_Found;

   procedure Method_Not_Allowed
      (Resp : in out Eva.HTTP.Response)
   is
   begin
      Set_Status (Resp, 405, "Method Not Allowed");
      Set_Header (Resp, "Content-Type", "text/plain; charset=utf-8");
      Put (Resp, "405 Method Not Allowed" & LF);
   end Method_Not_Allowed;

end Eva.HTTP.Errors;
