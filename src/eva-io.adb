--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces;

package body Eva.IO is

   procedure Initialize
      (This : out IO_Context)
   is
   begin
      This.EP := Epoll.Create;
   end Initialize;

   procedure Register
      (This    : in out IO_Context;
       Desc    : Descriptor;
       Context : Context_Type;
       Readable, Writable, One_Shot : Boolean)
   is
      use Descriptor_Maps;
      Event : aliased Epoll.Epoll_Event;
   begin
      Event.Flags :=
         (Readable   => Readable,
          Writable   => Writable,
          One_Shot   => One_Shot,
          Error      => True,
          Hang_Up    => True,
          others     => False);
      Event.Data := Interfaces.Unsigned_64 (Desc);
      Epoll.Control (This.EP, Desc, Epoll.Add, Event'Access);

      if Contains (This.Descriptors, Desc) then
         Reference (This.Descriptors, Desc) := Context;
      else
         Insert (This.Descriptors, Desc, Context);
      end if;
   end Register;

   procedure Set_Triggers
      (This       : in out IO_Context;
       Desc       : Descriptor;
       Readable   : Boolean;
       Writable   : Boolean;
       One_Shot   : Boolean)
   is
      Event : aliased Epoll.Epoll_Event :=
         (Flags =>
            (Readable   => Readable,
             Writable   => Writable,
             One_Shot   => One_Shot,
             Error      => True,
             Hang_Up    => True,
             others     => False),
          Data => Interfaces.Unsigned_64 (Desc));
   begin
      Epoll.Control (This.EP, Desc, Epoll.Modify, Event'Access);
   exception
      when Epoll.Epoll_Error =>
         null;
   end Set_Triggers;

   procedure Unregister
      (This : in out IO_Context;
       Desc : Descriptor)
   is
   begin
      Epoll.Control (This.EP, Desc, Epoll.Delete, null);
   exception
      when Epoll.Epoll_Error =>
         null;
   end Unregister;

   procedure Poll
      (This : in out IO_Context)
   is
      use Descriptor_Maps;
   begin
      for Event of Epoll.Wait (This.EP, Timeout => 1000, Max_Events => 64) loop
         declare
            Desc : constant Descriptor := Descriptor (Integer (Event.Data));
            Context : Context_Type;
         begin
            Context := Element (This.Descriptors, Desc);
            if Event.Flags.Error or else Event.Flags.Hang_Up then
               On_Error (Desc, Context);
            end if;
            if Event.Flags.Readable then
               On_Readable (Desc, Context);
            end if;
            if Event.Flags.Writable then
               On_Writable (Desc, Context);
            end if;
         end;
      end loop;
   end Poll;

end Eva.IO;
