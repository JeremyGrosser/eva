package Eva.HTTP.Errors is

   procedure Bad_Request
      (Resp : in out Eva.HTTP.Response);

   procedure Not_Found
      (Resp : in out Eva.HTTP.Response);

   procedure Method_Not_Allowed
      (Resp : in out Eva.HTTP.Response);

end Eva.HTTP.Errors;
