with Eva.HTTP.Server;
with Eva.HTTP;

package Workers is

   procedure Test_Handler
      (Req  : Eva.HTTP.Request;
       Resp : in out Eva.HTTP.Response);

   package Test_Server is new Eva.HTTP.Server
      (Handle_Request => Test_Handler);

   procedure Wait;
end Workers;
