--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with GNAT.OS_Lib;

package body Eva.Sockets is
   use Interfaces.C;

   function Bind
      (Port : Inet_Port)
      return Socket_Type
   is
      function C_eva_bind
         (Port : Inet_Port)
         return Socket_Type
      with Import, Convention => C, External_Name => "eva_bind";

      Sock : Socket_Type;
   begin
      Sock := C_eva_bind (Port);
      if Sock < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      else
         return Sock;
      end if;
   end Bind;

   procedure Accept_Socket
      (Listen_Sock : Socket_Type;
       Client_Sock : out Socket_Type)
   is
      function C_eva_accept
         (Listen_Sock : Socket_Type)
         return Socket_Type
      with Import, Convention => C, External_Name => "eva_accept";
   begin
      Client_Sock := C_eva_accept (Listen_Sock);
      if Client_Sock < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Accept_Socket;

   procedure Close_Socket
      (Sock : Socket_Type)
   is
      function C_close
         (fd : Socket_Type)
         return int
      with Import, Convention => C, External_Name => "close";

      Result : int with Unreferenced;
   begin
      Result := C_close (Sock);
   end Close_Socket;

end Eva.Sockets;
