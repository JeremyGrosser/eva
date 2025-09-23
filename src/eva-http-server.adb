--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;
with Eva.Sockets; use Eva.Sockets;
with Eva.Sockets.TLS;
with Eva.Epoll;
with Eva.H2;
with Eva_Config;
with Interfaces;

package body Eva.HTTP.Server is

   Request_Timeout : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Seconds
      (Eva_Config.Request_Timeout_Seconds);

   Running : Boolean := True;

   function To_C
      (Socket : Eva.Sockets.Socket_Type)
      return Interfaces.Unsigned_64
   is (Interfaces.Unsigned_64 (Socket));

   function To_Ada
      (Item : Interfaces.Unsigned_64)
      return Eva.Sockets.Socket_Type
   is (Eva.Sockets.Socket_Type (Item));

   type Session_Type is record
      TSock    : Eva.Sockets.TLS.TLS_Socket;
      POLLIN   : Boolean := True;
      POLLOUT  : Boolean := False;
      Closed   : Boolean := False;
      Request  : Eva.HTTP.Request := (others => <>);
      Response : Eva.HTTP.Response := (others => <>);
      Response_Buffered : Boolean := False;
      Write    : Byte_Buffer (Capacity => 65536);
      Deadline : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      H2       : Eva.H2.H2_Session := (others => <>);
   end record;

   use type Eva.Sockets.Socket_Type;
   package Socket_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Eva.Sockets.Socket_Type,
       Element_Type  => Session_Type);

   type Server_Type is record
      EP             : Eva.Epoll.Epoll_Descriptor := Eva.Epoll.Create;
      Sock           : Eva.Sockets.Socket_Type;
      TC             : Eva.Sockets.TLS.TLS_Config;
      TS             : Eva.Sockets.TLS.TLS_Socket;
      User_Context   : Any_Context_Type;
      Sessions       : Socket_Maps.Map;
   end record;

   procedure On_Connect
      (This : in out Server_Type)
   is
      use Ada.Real_Time;
      Client_Sock : Socket_Type;
      Session : Session_Type;
      Event : aliased Epoll.Epoll_Event;
   begin
      Eva.Sockets.Accept_Socket (This.Sock, Client_Sock);
      Eva.Sockets.TLS.Accept_Socket (This.TS, Session.TSock, Client_Sock);

      if Socket_Maps.Contains (This.Sessions, Client_Sock) then
         Eva.Sockets.TLS.Free (Socket_Maps.Reference (This.Sessions, Client_Sock).TSock);
         Socket_Maps.Delete (This.Sessions, Client_Sock);
      end if;

      Session.Deadline := Clock + Request_Timeout;
      Socket_Maps.Insert (This.Sessions, Client_Sock, Session);

      Event.Data := To_C (Client_Sock);
      Event.Flags.Readable := True;
      Event.Flags.Writable := True;
      --  Event.Flags.Edge_Triggered := True;
      Event.Flags.Hang_Up := True;
      Event.Flags.Error := True;
      Eva.Epoll.Control
         (This    => This.EP,
          Socket  => Client_Sock,
          Op      => Eva.Epoll.Add,
          Event   => Event'Unchecked_Access);
   exception
      when E : Eva.Sockets.Socket_Error =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end On_Connect;

   procedure Close
      (This : in out Server_Type;
       Sock : Socket_Type)
   is
      use Socket_Maps;
      Session : constant Reference_Type := Reference (This.Sessions, Sock);
   begin
      Session.Closed := True;
      Eva.Epoll.Control
         (This    => This.EP,
          Socket  => Sock,
          Op      => Eva.Epoll.Delete,
          Event   => null);
      Close_Socket (Sock);
   exception
      when Eva.Epoll.Epoll_Error =>
         null;
   end Close;

   procedure On_Data
      (This : in out Server_Type;
       Sock : Socket_Type;
       Readable, Writable : Boolean)
   is
      Session : constant Socket_Maps.Reference_Type := Socket_Maps.Reference (This.Sessions, Sock);
      Data : Bytes (1 .. 4096);
      Last : Natural;
   begin
      if Readable then
         Eva.Sockets.TLS.Receive_Socket (Session.TSock, Data, Last, Session.POLLIN, Session.POLLOUT);
         if Available (Session.Request.Buffer) >= Last then
            Append (Session.Request.Buffer, Data (1 .. Last));
         else
            Ada.Text_IO.Put_Line ("Request too large! Closing connection.");
            Close (This, Sock);
         end if;

         if Last = 0 and then not Session.POLLIN and then not Session.POLLOUT then
            Close (This, Sock);
         end if;
      end if;

      if Writable then
         Eva.Sockets.TLS.Send_Socket
            (Session.TSock, To_Bytes (Session.Write), Last,
             Session.POLLIN, Session.POLLOUT);
         Delete (Session.Write, Last);
         if Length (Session.Write) > 0 then
            Session.POLLOUT := True;
         end if;
      end if;
   exception
      when Eva.Sockets.TLS.TLS_Error =>
         Close (This, Sock);
   end On_Data;

   procedure Poll_HTTP1
      (This    : in out Server_Type;
       Sock    : Socket_Type;
       Session : in out Session_Type;
       Now     : Ada.Real_Time.Time)
   is
      use Ada.Real_Time;
   begin
      Eva.HTTP.Parse_Request (Session.Request);
      if not Session.Response_Buffered and then Eva.HTTP.Is_Complete (Session.Request) then
         Handle_Request (This.User_Context, Session.Request, Session.Response);
         Set_Complete (Session.Response);
      end if;

      if not Session.Response_Buffered and then Session.Response.Complete then
         Append (Session.Write, To_Bytes (To_String (Session.Response.Buffer)));
         Session.Response_Buffered := True;
         Session.POLLOUT := True;
      end if;

      if Session.Response_Buffered and then Length (Session.Write) = 0 then
         Eva.HTTP.Reset (Session.Request);
         Eva.HTTP.Reset (Session.Response);
         Session.Response_Buffered := False;
         Session.Deadline := Now + Request_Timeout;
      end if;

      if not Session.Closed and then Now >= Session.Deadline then
         Close (This, Sock);
      end if;
   exception
      when Eva.HTTP.Parse_Error =>
         Close (This, Sock);
   end Poll_HTTP1;

   procedure Poll_HTTP2
      (This    : in out Server_Type;
       Sock    : Socket_Type;
       Session : in out Session_Type;
       Now     : Ada.Real_Time.Time)
   is
      pragma Unreferenced (Now);
      use type Eva.H2.H2_State;
   begin
      Eva.H2.Poll (Session.H2, Session.Request.Buffer, Session.Write);
      if Session.H2.State = Eva.H2.Closed then
         Close (This, Sock);
      end if;
   end Poll_HTTP2;

   procedure Poll
      (This : in out Server_Type)
   is
      use Ada.Real_Time;
      Now : Time;
   begin
      for Event of Eva.Epoll.Wait (This.EP, Max_Events => 256, Timeout => 1000) loop
         if Event.Flags.Readable and then To_Ada (Event.Data) = This.Sock then
            On_Connect (This);
         else
            if Event.Flags.Hang_Up or else Event.Flags.Error then
               Close (This, To_Ada (Event.Data));
            elsif Event.Flags.Readable or else Event.Flags.Writable then
               On_Data (This, To_Ada (Event.Data), Event.Flags.Readable, Event.Flags.Writable);
            end if;
         end if;
      end loop;

      Now := Clock;

      for Cursor in Socket_Maps.Iterate (This.Sessions) loop
         declare
            use Socket_Maps;
            Session : constant Reference_Type := Reference (This.Sessions, Cursor);
         begin
            if not Session.Closed then
               if Eva.Sockets.TLS.ALPN_Selected (Session.TSock) = "h2" then
                  Poll_HTTP2
                     (This    => This,
                      Sock    => Key (Cursor),
                      Session => Session,
                      Now     => Now);
               else
                  Poll_HTTP1
                     (This    => This,
                      Sock    => Key (Cursor),
                      Session => Session,
                      Now     => Now);
               end if;
            end if;
         end;
      end loop;

      for Cursor in Socket_Maps.Iterate (This.Sessions) loop
         declare
            use Socket_Maps;
            Sock : constant Socket_Type := Key (Cursor);
            Session : constant Reference_Type := Reference (This.Sessions, Cursor);
            Event : aliased Epoll.Epoll_Event;
         begin
            if not Session.Closed then
               Event.Data := To_C (Sock);
               Event.Flags.Readable := True;
               Event.Flags.Writable := Session.POLLOUT;
               --  Event.Flags.Edge_Triggered := True;
               Event.Flags.Hang_Up := True;
               Event.Flags.Error := True;
               Eva.Epoll.Control
                  (This    => This.EP,
                   Socket  => Sock,
                   Op      => Eva.Epoll.Modify,
                   Event   => Event'Unchecked_Access);
            end if;
         end;
      end loop;
   end Poll;

   procedure Run
      (Context   : Any_Context_Type;
       Port      : Eva.Sockets.Inet_Port := 9999;
       Cert_File : String := "server.crt";
       Key_File  : String := "server.key")
   is
      use Eva.Sockets.TLS;
      Server : aliased Server_Type;
      Event  : aliased Eva.Epoll.Epoll_Event;
   begin
      Server.User_Context := Context;
      Server.TC := New_Config;
      Set_Cert_File (Server.TC, Cert_File);
      Set_Key_File (Server.TC, Key_File);
      Set_Protocols (Server.TC, "secure");
      Set_Ciphers (Server.TC, "secure");
      --  Set_ALPN (Server.TC, "h2,http/1.1");
      Set_ALPN (Server.TC, "http/1.1");
      Server.TS := New_Server_Socket;
      Configure (Server.TS, Server.TC);

      Server.Sock := Eva.Sockets.Bind (Port);
      Event.Data := To_C (Server.Sock);
      Event.Flags.Readable := True;
      Eva.Epoll.Control
         (This    => Server.EP,
          Socket  => Server.Sock,
          Op      => Eva.Epoll.Add,
          Event   => Event'Unchecked_Access);

      loop
         Poll (Server);
         exit when not Running;
      end loop;

      Close (Server.TS);
      Free (Server.TS);
      Free (Server.TC);
      Close_Socket (Server.Sock);
   end Run;

   procedure Stop is
   begin
      Running := False;
   end Stop;

end Eva.HTTP.Server;
