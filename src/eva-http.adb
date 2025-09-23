--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Strings.Fixed;
with URI;

package body Eva.HTTP is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   function Get_String
      (Req : Request;
       Sp  : Span)
       return String
   is
   begin
      if Sp.Last = 0 then
         return "";
      else
         return To_String (Slice (Req.Buffer, Sp.First, Sp.Last));
      end if;
   end Get_String;

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

   function Header
      (This : Request;
       Name : String)
       return String
   is
      use Request_Header_Maps;
   begin
      if Contains (This.Headers, Name) then
         return Get_String (This, Element (This.Headers, Name));
      else
         return "";
      end if;
   end Header;

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

   function Data
      (This : Request)
      return String
   is (To_String (Slice (This.Buffer, This.Start_Data, Length (This.Buffer))));

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String)
   is
   begin
      This.Status := Code;
      This.Status_Message := To_Unbounded_String (Message);
   end Set_Status;

   procedure Set_Header
      (This : in out Response;
       Key, Value : String)
   is
   begin
      Response_Header_Maps.Include (This.Headers, Key, To_Unbounded_String (Value));
   end Set_Header;

   function Status
      (This : Response)
      return HTTP_Status
   is (This.Status);

   procedure Put
      (This : in out Response;
       Data : String)
   is
   begin
      Append (This.Data, Data);
   end Put;

   function Is_Empty
      (Resp : Response)
      return Boolean
   is (Length (Resp.Buffer) = 0);

   procedure Reset
      (Req : in out Request)
   is
   begin
      Req := (others => <>);
   end Reset;

   procedure Reset
      (Resp : in out Response)
   is
   begin
      Resp := (others => <>);
   end Reset;

   procedure Parse_Request
      (Req : in out Request)
   is separate;

   function Content_Length
      (This : Response)
      return Natural
   is (Length (This.Data));

   procedure Set_Complete
      (This : in out Response)
   is
      use Response_Header_Maps;
      Length : constant Natural := Content_Length (This);
   begin
      if Length > 0 and then not Contains (This.Headers, "Content-Length") then
         Insert
            (This.Headers, "Content-Length", To_Unbounded_String
               (Ada.Strings.Fixed.Trim (Length'Image, Ada.Strings.Both)));
      end if;

      This.Buffer := Null_Unbounded_String;
      Append (This.Buffer, "HTTP/1.1");
      Append (This.Buffer, This.Status'Image);
      Append (This.Buffer, " ");
      Append (This.Buffer, To_String (This.Status_Message));
      Append (This.Buffer, CRLF);

      for Cursor in Iterate (This.Headers) loop
         Append (This.Buffer, Key (Cursor));
         Append (This.Buffer, ": ");
         Append (This.Buffer, To_String (Element (Cursor)));
         Append (This.Buffer, CRLF);
      end loop;
      Append (This.Buffer, CRLF);
      Append (This.Buffer, To_String (This.Data));

      This.Complete := True;
   end Set_Complete;

   function Is_Complete
      (This : Request)
      return Boolean
   is
   begin
      if This.End_Headers = 0 then
         return False;
      end if;

      if Request_Header_Maps.Contains (This.Headers, "Content-Length") then
         declare
            Content_Length : constant Natural := Natural'Value (Header (This, "Content-Length"));
         begin
            return ((Length (This.Buffer) + 1) - This.Start_Data) >= Content_Length;
         end;
      else
         return True;
      end if;
   exception
      when Constraint_Error =>
         return False;
   end Is_Complete;

   function Image
      (This : Request)
      return String
   is (To_String (This.Buffer));

   function Available
      (This : Request_Buffers.Bounded_String)
      return Natural
   is
      use Request_Buffers;
   begin
      return Request_Buffers.Max_Length - Length (This);
   end Available;

end Eva.HTTP;
