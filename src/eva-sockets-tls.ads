--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Eva.Sockets.TLS is

   TLS_Error : exception;

   type TLS_Socket is private;
   type TLS_Config is private;

   function New_Config
      return TLS_Config
      with Import, Convention => C, External_Name => "tls_config_new";

   procedure Set_Cert_File
      (Config   : TLS_Config;
       Filename : String);

   procedure Set_Key_File
      (Config   : TLS_Config;
       Filename : String);

   procedure Set_Protocols
      (Config    : TLS_Config;
       Protocols : String := "secure");

   procedure Set_Ciphers
      (Config  : TLS_Config;
       Ciphers : String := "secure");

   procedure Set_ALPN
      (Config : TLS_Config;
       ALPN   : String);

   function New_Server_Socket
      return TLS_Socket
   with Import, Convention => C, External_Name => "tls_server";

   procedure Configure
      (Server : TLS_Socket;
       Config : TLS_Config);

   procedure Accept_Socket
      (Server : TLS_Socket;
       Client : out TLS_Socket;
       Plain  : Eva.Sockets.Socket_Type);

   function ALPN_Selected
      (Sock : TLS_Socket)
      return String;

   procedure Send_Socket
      (Sock : TLS_Socket;
       Data : Bytes;
       Last : out Natural;
       POLLIN, POLLOUT : out Boolean);

   procedure Receive_Socket
      (Sock : TLS_Socket;
       Data : out Bytes;
       Last : out Natural;
       POLLIN, POLLOUT : out Boolean);

   procedure Close
      (Sock : TLS_Socket);

   procedure Reset
      (Sock : TLS_Socket);

   procedure Free
      (Sock : in out TLS_Socket);

   procedure Free
      (Config : in out TLS_Config);

   function Version
      return String;

private

   type TLS_Context_Internal is null record;
   type TLS_Socket is access all TLS_Context_Internal;

   type TLS_Config_Internal is null record;
   type TLS_Config is access all TLS_Config_Internal;

end Eva.Sockets.TLS;
