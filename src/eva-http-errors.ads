--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Eva.HTTP.Errors is

   procedure Bad_Request
      (Resp : in out Eva.HTTP.Response);

   procedure Not_Found
      (Resp : in out Eva.HTTP.Response);

   procedure Method_Not_Allowed
      (Resp : in out Eva.HTTP.Response);

end Eva.HTTP.Errors;
