--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with System;
with GNAT.OS_Lib;

package body Eva.Sockets is
   use Interfaces.C;

   EPIPE : constant := 32;

   subtype ssize_t is int;

   function htons
      (Host_Short : Inet_Port)
      return Inet_Port
   with Import, Convention => C, External_Name => "htons";

   SOL_SOCKET  : constant := 1;
   IPPROTO_TCP : constant := 6;

   function C_setsockopt
      (sockfd  : Socket_Type;
       level   : int;
       optname : int;
       optval  : System.Address;
       optlen  : size_t)
       return int
   with Import, Convention => C, External_Name => "setsockopt";

   procedure Create_Socket
      (Sock : out Socket_Type)
   is
      SOCK_STREAM : constant := 1;
      function C_socket
         (domain, typ, protocol : int)
         return int
      with Import, Convention => C, External_Name => "socket";
   begin
      Sock := Socket_Type (C_socket (AF_INET6, SOCK_STREAM, 0));
   end Create_Socket;

   procedure Set_Socket_Option
      (Sock   : Socket_Type;
       Option : Socket_Option)
   is
      Val : aliased constant int := 1;
      Result : int;
   begin
      Result := C_setsockopt
         (sockfd  => Sock,
          level   => SOL_SOCKET,
          optname => int (Socket_Option'Enum_Rep (Option)),
          optval  => Val'Address,
          optlen  => size_t (Val'Size / 8));
      if Result < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Set_Socket_Option;

   procedure Set_TCP_Option
      (Sock   : Socket_Type;
       Option : TCP_Option)
   is
      Val : aliased constant int := 1;
      Result : int;
   begin
      Result := C_setsockopt
         (sockfd  => Sock,
          level   => IPPROTO_TCP,
          optname => int (TCP_Option'Enum_Rep (Option)),
          optval  => Val'Address,
          optlen  => size_t (Val'Size / 8));
      if Result < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Set_TCP_Option;

   procedure Bind_Socket
      (Sock : Socket_Type;
       Addr : Sock_Addr)
   is
      function C_bind
         (sockfd   : Socket_Type;
          sockaddr : access Sock_Addr;
          addrlen  : UInt32)
          return int
      with Import, Convention => C, External_Name => "bind";

      Copy : aliased Sock_Addr := Addr;
      Result : int;
   begin
      Copy.Port := htons (Copy.Port);
      Result := C_bind (Sock, Copy'Access, Copy'Size / 8);
      if Result /= 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Bind_Socket;

   procedure Listen_Socket
      (Sock : Socket_Type;
       Backlog : Natural)
   is
      function C_listen
         (sockfd  : Socket_Type;
          backlog : int)
          return int
      with Import, Convention => C, External_Name => "listen";

      Result : int;
   begin
      Result := C_listen (Sock, int (Backlog));
      if Result /= 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      end if;
   end Listen_Socket;

   procedure Accept_Socket
      (Server : Socket_Type;
       Sock   : out Socket_Type;
       Addr   : out Sock_Addr)
   is
      function C_accept
         (sockfd  : Socket_Type;
          addr    : System.Address;
          addrlen : System.Address)
          return int
      with Import, Convention => C, External_Name => "accept";

      Copy : aliased Sock_Addr;
      Len  : aliased int;
      Result : int;
   begin
      Result := C_accept (Server, Copy'Address, Len'Address);
      if Result < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      else
         Sock := Socket_Type (Result);
         Addr := Copy;
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
      --  We don't actually care if close failed.
      --
      --  if Result /= 0 then
      --     raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      --  end if;
   end Close_Socket;

   procedure Receive_Socket
      (Sock : Socket_Type;
       Data : out String;
       Last : out Natural)
   is
      function C_recv
         (sockfd : Socket_Type;
          buf    : System.Address;
          len    : size_t;
          flags  : int)
          return ssize_t
      with Import, Convention => C, External_Name => "recv";

      Result : int;
   begin
      Result := C_recv (Sock, Data'Address, size_t (Data'Length), 0);
      if Result < 0 then
         raise Socket_Error with GNAT.OS_Lib.Errno_Message;
      else
         Last := Data'First + Natural (Result) - 1;
      end if;
   end Receive_Socket;

   procedure Send_Socket
      (Sock : Socket_Type;
       Data : String;
       Last : out Natural)
   is
      MSG_NOSIGNAL : constant := 16#4000#;
      --  no SIGPIPE if connection is closed

      function C_send
         (sockfd : Socket_Type;
          buf    : System.Address;
          len    : size_t;
          flags  : int)
          return int
      with Import, Convention => C, External_Name => "send";

      Result : int;
   begin
      Result := C_send
         (sockfd => Sock,
          buf    => Data'Address,
          len    => size_t (Data'Length),
          flags  => MSG_NOSIGNAL);
      if Result < 0 then
         Last := Data'First - 1;
         if GNAT.OS_Lib.Errno = EPIPE then
            --  Client closed connection
            return;
         else
            raise Socket_Error with GNAT.OS_Lib.Errno_Message;
         end if;
      else
         Last := Data'First + Natural (Result);
      end if;
   end Send_Socket;

end Eva.Sockets;
