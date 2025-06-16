--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Containers.Ordered_Maps;
with Eva.IO;
with Eva.Timers;
with Eva.Sockets; use Eva.Sockets;
with Eva.Epoll;

package body Eva.HTTP.Server is
   type Server_Context;
   type Any_Server_Context is access all Server_Context;

   Running : Boolean := True;

   procedure On_Timeout
      (Sock : Socket_Type);

   package Socket_Timers is new Eva.Timers
      (Context_Type => Eva.Sockets.Socket_Type,
       On_Timeout   => On_Timeout);

   procedure On_Readable
      (Sock    : Socket_Type;
       Context : Any_Server_Context);

   procedure On_Client_Writable
      (Sock    : Socket_Type;
       Context : Any_Server_Context);

   procedure On_Error
      (Sock    : Socket_Type;
       Context : Any_Server_Context);

   package Socket_IO is new Eva.IO
      (Context_Type => Any_Server_Context,
       On_Writable  => On_Client_Writable,
       On_Readable  => On_Readable,
       On_Error     => On_Error);

   Request_Timeout : constant := 5;

   type Session_Type is record
      Req    : Request := (others => <>);
      Resp   : Response := (others => <>);
   end record;

   use type Eva.Sockets.Socket_Type;
   package Session_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Socket_Type,
       Element_Type => Session_Type);

   type Server_Context is record
      Sessions    : Session_Maps.Map := Session_Maps.Empty_Map;
      IOC         : Socket_IO.IO_Context;
      Timers      : Socket_Timers.Timer_Wheel;
      Listen_Sock : Socket_Type;
   end record;

   procedure On_Timeout
      (Sock : Socket_Type)
   is
   begin
      Close_Socket (Sock);
   end On_Timeout;

   procedure On_Error
      (Sock    : Socket_Type;
       Context : Any_Server_Context)
   is
      pragma Unreferenced (Context);
   begin
      Ada.Text_IO.Put_Line ("Socket Error event");
      Close_Socket (Sock);
   end On_Error;

   procedure On_Connect
      (Listen_Sock : Socket_Type;
       Context     : Any_Server_Context)
   is
      Sock : Socket_Type;
      Addr : Sock_Addr;
   begin
      Accept_Socket (Listen_Sock, Sock, Addr);

      if not Session_Maps.Contains (Context.Sessions, Sock) then
         declare
            New_Session : constant Session_Type := (others => <>);
         begin
            Session_Maps.Insert (Context.Sessions, Sock, New_Session);
         end;
      end if;

      declare
         Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Context.Sessions, Sock);
      begin
         Reset (Session.Req);
         Reset (Session.Resp);
         Socket_Timers.Set_Timeout
            (Wheel         => Context.Timers,
             Timeout_Sec   => Request_Timeout,
             Context       => Sock);
      end;

      Socket_IO.Register
         (This       => Context.IOC,
          Desc       => Sock,
          Context    => Context,
          Readable   => True,
          Writable   => False,
          One_Shot   => True);
   exception
      when E : Eva.Epoll.Epoll_Error =>
         Ada.Text_IO.Put ("On_Connect Epoll_Error: ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
         Close_Socket (Sock);
   end On_Connect;

   procedure On_Request
      (Session : in out Session_Type;
       Sock    : Socket_Type;
       Context : Any_Server_Context)
   is
   begin
      Session.Resp.Socket := Sock;

      Handle_Request (Session.Req, Session.Resp);

      Set_Header (Session.Resp,
         Key   => "Content-Length",
         Value => Eva.Strings.To_String
            (Payload_Length (Session.Resp)));

      if not Is_Empty (Session.Resp) then
         Socket_IO.Set_Triggers
            (This       => Context.IOC,
             Desc       => Sock,
             Readable   => False,
             Writable   => True,
             One_Shot   => True);
      else
         Socket_IO.Set_Triggers
            (This       => Context.IOC,
             Desc       => Sock,
             Readable   => True,
             Writable   => False,
             One_Shot   => True);
      end if;
   end On_Request;

   procedure On_Client_Readable
      (Sock : Socket_Type;
       Context : Any_Server_Context)
   is
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Context.Sessions, Sock);
      Last : Natural := 0;
   begin
      Receive_Socket (Sock, Session.Req.Item (1 .. Session.Req.Item'Last), Last);
      if Last = 0 then
         Close_Socket (Sock);
      else
         Session.Req.Last := Session.Req.Last + Last;
         Parse_Request (Session.Req);
         if Session.Req.End_Headers > 0 then
            On_Request (Session, Sock, Context);
         end if;
      end if;
   exception
      when E : Socket_Error =>
         Ada.Text_IO.Put ("On_Client_Readable Socket_Error: ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
         Close_Socket (Sock);
      when E : Parse_Error =>
         Ada.Text_IO.Put ("On_Client_Readable Parse_Error: ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
         Close_Socket (Sock);
   end On_Client_Readable;

   procedure On_Readable
      (Sock    : Socket_Type;
       Context : Any_Server_Context)
   is
   begin
      if Sock = Context.Listen_Sock then
         On_Connect (Sock, Context);
      else
         On_Client_Readable (Sock, Context);
      end if;
   end On_Readable;

   procedure On_Client_Writable
      (Sock    : Socket_Type;
       Context : Any_Server_Context)
   is
      Session : constant Session_Maps.Reference_Type := Session_Maps.Reference (Context.Sessions, Sock);
      Str  : constant String := Response_Buffers.To_String (Session.Resp.Buffer);
      Last : Natural := 0;
   begin
      if Str'Length = 0 then
         return;
      end if;
      Send_Socket (Sock, Str, Last);
      if Last = 0 then
         --  Client closed connection
         Close_Socket (Sock);
      else
         Response_Buffers.Delete (Session.Resp.Buffer, 1, Last);
         if Is_Empty (Session.Resp) then
            --  No more data to send, wait for another request
            Reset (Session.Req);
            Reset (Session.Resp);
            Socket_IO.Set_Triggers
               (This       => Context.IOC,
                Desc       => Sock,
                Readable   => True,
                Writable   => False,
                One_Shot   => True);
         else
            Socket_IO.Set_Triggers
               (This       => Context.IOC,
                Desc       => Sock,
                Readable   => False,
                Writable   => True,
                One_Shot   => True);
         end if;
      end if;
   exception
      when E : Socket_Error =>
         Ada.Text_IO.Put ("On_Client_Writable Socket_Error: ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
         Close_Socket (Sock);
   end On_Client_Writable;

   procedure Bind
      (Server : Any_Server_Context;
       Port   : Inet_Port)
   is
      Addr : constant Sock_Addr :=
         (Port   => Port,
          others => <>);
   begin
      Create_Socket (Server.Listen_Sock);
      Set_Socket_Option (Server.Listen_Sock, Reuse_Address);
      Set_Socket_Option (Server.Listen_Sock, Reuse_Port);
      Bind_Socket (Server.Listen_Sock, Addr);
      Listen_Socket (Server.Listen_Sock, 256);
      Socket_IO.Register
         (This       => Server.IOC,
          Desc       => Server.Listen_Sock,
          Context    => Server,
          Readable   => True,
          Writable   => False,
          One_Shot   => False);
   end Bind;

   procedure Stop is
   begin
      Running := False;
   end Stop;

   procedure Run
      (Port : Eva.Sockets.Inet_Port := 9999)
   is
      use Ada.Real_Time;
      Next_Tick : Time := Clock;
      Server : aliased Server_Context;
   begin
      Socket_IO.Initialize (Server.IOC);
      Bind (Server'Unrestricted_Access, Port);

      while Running loop
         Socket_IO.Poll (Server.IOC);
         if Clock >= Next_Tick then
            Socket_Timers.Tick (Server.Timers);
            Next_Tick := Next_Tick + Seconds (1);
         end if;
      end loop;

      Close_Socket (Server.Listen_Sock);
   end Run;
end Eva.HTTP.Server;
