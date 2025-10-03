--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces; use Interfaces;
pragma Warnings (Off, "-gnatwi");
with System.CRTL;
with System;

package body Eva.Sockets.TLS is

   TLS_WANT_POLLIN  : constant := -2;
   TLS_WANT_POLLOUT : constant := -3;

   function Config_Error
      (Config : TLS_Config)
      return String
   is
      function C_tls_config_error
         (Config : TLS_Config)
          return chars_ptr
      with Import, Convention => C, External_Name => "tls_config_error";
   begin
      return Value (C_tls_config_error (Config));
   end Config_Error;

   function Socket_Error
      (Sock : TLS_Socket)
      return String
   is
      function C_tls_error
         (Context : TLS_Socket)
          return chars_ptr
      with Import, Convention => C, External_Name => "tls_error";
   begin
      return Value (C_tls_error (Sock));
   end Socket_Error;

   procedure Set_Cert_File
      (Config   : TLS_Config;
       Filename : String)
   is
      function C_tls_config_set_cert_file
         (Config    : TLS_Config;
          Cert_File : chars_ptr)
          return int
      with Import, Convention => C, External_Name => "tls_config_set_cert_file";
      F   : chars_ptr := New_String (Filename);
      Err : int;
   begin
      Err := C_tls_config_set_cert_file (Config, F);
      if Err = -1 then
         raise TLS_Error with Config_Error (Config);
      end if;
      Free (F);
   end Set_Cert_File;

   procedure Set_Key_File
      (Config   : TLS_Config;
       Filename : String)
   is
      function C_tls_config_set_key_file
         (Config   : TLS_Config;
          Key_File : chars_ptr)
          return int
      with Import, Convention => C, External_Name => "tls_config_set_key_file";
      F   : chars_ptr := New_String (Filename);
      Err : int;
   begin
      Err := C_tls_config_set_key_file (Config, F);
      if Err = -1 then
         raise TLS_Error with Config_Error (Config);
      end if;
      Free (F);
   end Set_Key_File;

   procedure Set_Protocols
      (Config    : TLS_Config;
       Protocols : String := "secure")
   is
      function C_tls_config_parse_protocols
         (Flags    : access Unsigned_32;
          Protostr : chars_ptr)
          return int
      with Import, Convention => C, External_Name => "tls_config_parse_protocols";

      function C_tls_config_set_protocols
         (Config : TLS_Config;
          Flags  : Unsigned_32)
          return int
      with Import, Convention => C, External_Name => "tls_config_set_protocols";

      P : chars_ptr := New_String (Protocols);
      Flags : aliased Unsigned_32;
   begin
      if C_tls_config_parse_protocols (Flags'Access, P) = -1 then
         raise TLS_Error;
      end if;
      Free (P);

      if C_tls_config_set_protocols (Config, Flags) = -1 then
         raise TLS_Error with Config_Error (Config);
      end if;
   end Set_Protocols;

   procedure Set_Ciphers
      (Config  : TLS_Config;
       Ciphers : String := "secure")
   is
      function C_tls_config_set_ciphers
         (Config  : TLS_Config;
          Ciphers : chars_ptr)
          return int
      with Import, Convention => C, External_Name => "tls_config_set_ciphers";

      C : chars_ptr := New_String (Ciphers);
   begin
      if C_tls_config_set_ciphers (Config, C) = -1 then
         raise TLS_Error with Config_Error (Config);
      end if;
      Free (C);
   end Set_Ciphers;

   procedure Set_ALPN
      (Config  : TLS_Config;
       ALPN    : String)
   is
      function C_tls_config_set_alpn
         (Config  : TLS_Config;
          ALPN    : chars_ptr)
          return int
      with Import, Convention => C, External_Name => "tls_config_set_alpn";

      C : chars_ptr := New_String (ALPN);
   begin
      if C_tls_config_set_alpn (Config, C) = -1 then
         raise TLS_Error with Config_Error (Config);
      end if;
      Free (C);
   end Set_ALPN;

   procedure Configure
      (Server : TLS_Socket;
       Config : TLS_Config)
   is
      function C_tls_configure
         (Context : TLS_Socket;
          Config  : TLS_Config)
          return int
      with Import, Convention => C, External_Name => "tls_configure";
   begin
      if C_tls_configure (Server, Config) = -1 then
         raise TLS_Error with Socket_Error (Server);
      end if;
   end Configure;

   procedure Accept_Socket
      (Server : TLS_Socket;
       Client : out TLS_Socket;
       Plain  : Eva.Sockets.Socket_Type)
   is
      function C_tls_accept_socket
         (tls    : TLS_Socket;
          cctx   : access TLS_Socket;
          socket : Eva.Sockets.Socket_Type)
          return int
      with Import, Convention => C, External_Name => "tls_accept_socket";

      Ctx : aliased TLS_Socket;
   begin
      if C_tls_accept_socket (Server, Ctx'Access, Plain) = -1 then
         raise TLS_Error with Socket_Error (Server);
      else
         Client := Ctx;
      end if;
   end Accept_Socket;

   function ALPN_Selected
      (Sock : TLS_Socket)
      return String
   is
      function C_tls_conn_alpn_selected
         (ctx : TLS_Socket)
         return chars_ptr
      with Import, Convention => C, External_Name => "tls_conn_alpn_selected";

      ALPN : chars_ptr;
   begin
      ALPN := C_tls_conn_alpn_selected (Sock);
      if ALPN /= Null_Ptr then
         return Value (ALPN);
      else
         return "";
      end if;
   end ALPN_Selected;

   procedure Send_Socket
      (Sock : TLS_Socket;
       Data : Bytes;
       Last : out Natural;
       POLLIN, POLLOUT : out Boolean)
   is
      function C_tls_write
         (ctx : TLS_Socket;
          buf : System.Address;
          len : size_t)
          return System.CRTL.ssize_t
      with Import, Convention => C, External_Name => "tls_write";

      use type System.CRTL.ssize_t;
      Ret : System.CRTL.ssize_t;
   begin
      POLLIN := False;
      POLLOUT := False;
      Last := 0;
      Ret := C_tls_write
         (ctx => Sock,
          buf => Data'Address,
          len => size_t (Data'Length));
      if Ret = -1 then
         raise TLS_Error with Socket_Error (Sock);
      elsif Ret = TLS_WANT_POLLIN then
         POLLIN := True;
      elsif Ret = TLS_WANT_POLLOUT then
         POLLOUT := True;
      else
         Last := Natural (Ret);
      end if;
   end Send_Socket;

   procedure Receive_Socket
      (Sock : TLS_Socket;
       Data : out Bytes;
       Last : out Natural;
       POLLIN, POLLOUT : out Boolean)
   is
      function C_tls_read
         (ctx : TLS_Socket;
          buf : System.Address;
          len : size_t)
          return System.CRTL.ssize_t
      with Import, Convention => C, External_Name => "tls_read";

      use type System.CRTL.ssize_t;
      Ret : System.CRTL.ssize_t;
   begin
      POLLIN := False;
      POLLOUT := False;
      Last := 0;
      Ret := C_tls_read (Sock, Data'Address, size_t (Data'Length));
      if Ret = -1 then
         raise TLS_Error with Socket_Error (Sock);
      elsif Ret = TLS_WANT_POLLIN then
         POLLIN := True;
      elsif Ret = TLS_WANT_POLLOUT then
         POLLOUT := True;
      else
         Last := Natural (Ret);
      end if;
   end Receive_Socket;

   procedure Close
      (Sock : TLS_Socket)
   is
      function C_tls_close
         (ctx : TLS_Socket)
         return int
      with Import, Convention => C, External_Name => "tls_close";

      Ret : int with Unreferenced; --  don't care about errors during close
   begin
      Ret := C_tls_close (Sock);
   end Close;

   procedure Reset
      (Sock : TLS_Socket)
   is
      procedure C_tls_reset
         (ctx : TLS_Socket)
      with Import, Convention => C, External_Name => "tls_reset";
   begin
      C_tls_reset (Sock);
   end Reset;

   procedure Free
      (Config : in out TLS_Config)
   is
      procedure C_tls_config_free
         (Config : TLS_Config)
      with Import, Convention => C, External_Name => "tls_config_free";
   begin
      C_tls_config_free (Config);
      Config := null;
   end Free;

   procedure Free
      (Sock : in out TLS_Socket)
   is
      procedure C_tls_free
         (Sock : TLS_Socket)
      with Import, Convention => C, External_Name => "tls_free";
   begin
      C_tls_free (Sock);
      Sock := null;
   end Free;

   function Version
      return String
   is
      function C_eva_get_tls_version
         return chars_ptr
      with Import, Convention => C, External_Name => "eva_get_tls_version";

      V : chars_ptr := C_eva_get_tls_version;
   begin
      if V = Null_Ptr then
         return "";
      else
         declare
            S : constant String := Value (V);
         begin
            Free (V);
            return S;
         end;
      end if;
   end Version;

end Eva.Sockets.TLS;
