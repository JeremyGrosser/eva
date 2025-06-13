--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private with Ada.Containers.Ordered_Maps;
private with Eva.Epoll;
with Eva.Sockets;

generic
   type Context_Type is private;
   with procedure On_Readable (Desc : Eva.Sockets.Socket_Type; Context : Context_Type);
   with procedure On_Writable (Desc : Eva.Sockets.Socket_Type; Context : Context_Type);
   with procedure On_Error    (Desc : Eva.Sockets.Socket_Type; Context : Context_Type);
package Eva.IO
   with Preelaborate
is

   type IO_Context is private;

   subtype Descriptor is Eva.Sockets.Socket_Type;

   procedure Initialize
      (This : out IO_Context);

   procedure Register
      (This       : in out IO_Context;
       Desc       : Descriptor;
       Context    : Context_Type;
       Readable   : Boolean;
       Writable   : Boolean;
       One_Shot   : Boolean);

   procedure Set_Triggers
      (This       : in out IO_Context;
       Desc       : Descriptor;
       Readable   : Boolean;
       Writable   : Boolean;
       One_Shot   : Boolean);

   procedure Unregister
      (This : in out IO_Context;
       Desc : Descriptor);

   procedure Poll
      (This : in out IO_Context);

private

   use type Eva.Sockets.Socket_Type;
   package Descriptor_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Descriptor,
       Element_Type => Context_Type);

   package Epoll renames Eva.Epoll;

   type IO_Context is record
      EP          : Epoll.Epoll_Descriptor;
      Descriptors : Descriptor_Maps.Map;
   end record;

end Eva.IO;
