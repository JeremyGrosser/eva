--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Strings.Fixed;
with URI;

package body Eva.HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   function Method
      (This : Request)
      return String
   is (Get_String (This, This.Method));

   function Path
      (This : Request)
      return String
   is
      use Ada.Strings.Fixed;
      Target : constant String := Get_String (This, This.Target);
      Query : constant Natural := Index
         (Source  => Target,
          Pattern => "?",
          From    => Target'First);
      Fragment : constant Natural := Index
         (Source  => Target,
          Pattern => "#",
          From    => Target'First);
      Last : Natural := Target'Last;
   begin
      if Query in Target'First .. Last then
         Last := Query - 1;
      end if;
      if Fragment in Target'First .. Last then
         Last := Fragment - 1;
      end if;
      return URI.Normalize_Path (Target (Target'First .. Last));
   end Path;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String
   is
      use Ada.Strings.Fixed;
      Target : constant String := Get_String (This, This.Target);
      Query  : constant Natural := Index
         (Source  => Target,
          Pattern => "?",
          From    => Target'First);
      First : Natural;
   begin
      if Query = 0 then
         return Default;
      end if;
      for I in Query + 1 .. Target'Last loop
         exit when Target (I) = '#';
         First := Index (Target (I .. Target'Last), Key & '=');
         if First > 0 then
            First := First + Key'Length + 1;
            for J in First .. Target'Last loop
               if Target (J) = '&' or else Target (J) = '#' then
                  return Target (First .. J - 1);
               end if;
            end loop;
            return Target (First .. Target'Last);
         end if;
      end loop;
      return Default;
   end Query_Parameter;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String)
   is
      use Response_Buffers;
   begin
      if This.Started then
         Insert (This.Buffer,
            New_Item => CRLF & Key & ": " & Value,
            Before => End_Headers (This));
      else
         Append (This.Buffer, Key);
         Append (This.Buffer, ": ");
         Append (This.Buffer, Value);
         Append (This.Buffer, CRLF);
      end if;
   end Set_Header;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String)
   is
      use Response_Buffers;
   begin
      if This.Started or else Length (This.Buffer) /= 0 then
         raise Program_Error with "Set_Status cannot be called at this time";
      end if;
      --  This.Started := False;
      Append (This.Buffer, "HTTP/1.1");
      Append (This.Buffer, Code'Image);
      Append (This.Buffer, ' ');
      Append (This.Buffer, Message);
      Append (This.Buffer, CRLF);
   end Set_Status;

   procedure Put_Raw
      (This : in out Response;
       Data : String)
   is
   begin
      if not This.Started then
         Response_Buffers.Append (This.Buffer, CRLF);
         This.Started := True;
      end if;
      Response_Buffers.Append (This.Buffer, Data);
   end Put_Raw;

   procedure Put
      (This : in out Response;
       Data : Eva.Strings.UTF8)
   is
   begin
      Put_Raw (This, String (Data));
   end Put;

   procedure Put
      (This : in out Response;
       Data : Eva.Strings.Unicode)
   is
   begin
      Put (This, Eva.Strings.Encode (Data));
   end Put;

   procedure Put
      (This : in out Response;
       Ch   : Wide_Wide_Character)
   is
   begin
      Put (This, Eva.Strings.Encode (Ch & ""));
   end Put;

   function Is_Empty
      (Resp : Response)
      return Boolean
   is (Response_Buffers.Length (Resp.Buffer) = 0);

   function Get_String
      (Req : Request;
       Sp  : Span)
       return String
   is
   begin
      if Sp.Last = 0 then
         return "";
      else
         return Req.Item (Sp.First .. Sp.Last);
      end if;
   end Get_String;

   procedure Reset
      (Req : in out Request)
   is
   begin
      Req.Last := 0;
      Req.End_Headers := 0;
      Request_Header_Maps.Clear (Req.Headers);
   end Reset;

   procedure Reset
      (Resp : in out Response)
   is
   begin
      Resp.Started := False;
      Response_Buffers.Delete (Resp.Buffer, 1, Response_Buffers.Length (Resp.Buffer));
   end Reset;

   procedure Parse_Request
      (Req : in out Request)
   is separate;

   function End_Headers
      (Resp : Response)
      return Natural
   is (Response_Buffers.Index (Resp.Buffer, CRLF & CRLF, 1));

   function Payload_Length
      (Resp : Response)
      return Natural
   is
      Buffer_Length : constant Natural := Response_Buffers.Length (Resp.Buffer);
   begin
      if Buffer_Length < 4 then
         return 0;
      else
         return Buffer_Length - End_Headers (Resp) - 3;
      end if;
   end Payload_Length;

end Eva.HTTP;
