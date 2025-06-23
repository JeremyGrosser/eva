--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Eva.Strings;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Eva.Sockets;

package Eva.HTTP
   with Elaborate_Body
is

   type Request is private;

   --  Request
   function Method
      (This : Request)
      return String;

   function Path
      (This : Request)
      return String;

   function Header
      (This : Request;
       Name : String)
       return String;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String;

   function Data
      (This : Request)
      return String;

   type Response is private;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String);

   type HTTP_Status is new Integer range 100 .. 599;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String);

   procedure Put
      (This : in out Response;
       Data : Eva.Strings.Unicode);

   procedure Put
      (This : in out Response;
       Ch   : Wide_Wide_Character);

   procedure Put_Raw
      (This : in out Response;
       Data : String);

private

   Max_Request_Length   : constant := 4096;

   type Span is record
      First, Last : Natural := 0;
   end record;

   package Request_Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type         => String,
       Element_Type     => Span,
       Equivalent_Keys  => Ada.Strings.Equal_Case_Insensitive,
       Hash             => Ada.Strings.Hash_Case_Insensitive);

   package Response_Buffers renames Ada.Strings.Unbounded;

   type Request is record
      Item        : String (1 .. Max_Request_Length);
      Last        : Natural := 0;
      End_Headers : Natural := 0;
      Start_Data  : Natural := 0;
      Method      : Span := (0, 0);
      Target      : Span := (0, 0);
      Protocol    : Span := (0, 0);
      Headers     : Request_Header_Maps.Map := Request_Header_Maps.Empty_Map;
   end record;

   type Response is record
      Socket   : Eva.Sockets.Socket_Type := 0;
      Started  : Boolean := False; --  Set True after status and headers are sent
      Buffer   : Response_Buffers.Unbounded_String := Response_Buffers.Null_Unbounded_String;
   end record;

   function Is_Empty
      (Resp : Response)
      return Boolean;

   function Get_String
      (Req : Request;
       Sp  : Span)
       return String;

   procedure Reset
      (Req : in out Request);

   procedure Reset
      (Resp : in out Response);

   Parse_Error : exception;

   procedure Parse_Request
      (Req : in out Request);

   function End_Headers
      (Resp : Response)
      return Natural;

   function Payload_Length
      (Resp : Response)
      return Natural;

end Eva.HTTP;
