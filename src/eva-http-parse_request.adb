--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--

separate (Eva.HTTP) procedure Parse_Request
   (Req : in out Request)
is
   function Index
      (Req   : Request;
       First : Positive;
       Str   : String)
       return Natural
   is
   begin
      if Str'Length > (Req.End_Headers - First + 1) then
         return 0;
      end if;

      for I in First .. Req.End_Headers - Str'Length + 1 loop
         if Req.Item (I .. I + Str'Length - 1) = Str then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   CRLF : constant String := ASCII.CR & ASCII.LF;
   I, Next : Natural;
begin
   --  Find the end of headers (double CRLF)
   Req.End_Headers := Req.Item'Last;
   Req.End_Headers := Index (Req, Req.Item'First, CRLF & CRLF);
   if Req.End_Headers = 0 then
      --  Don't start parsing the request until we have all the headers
      return;
   else
      Req.End_Headers := Req.End_Headers + 2;
      --  Leave one CRLF in the buffer to make header parsing easier
   end if;

   --  Parse request line (METHOD TARGET PROTOCOL)
   Req.Method.First := Req.Item'First;
   I := Index (Req, Req.Method.First, " ");
   if I = 0 then
      raise Parse_Error with "Invalid request line: missing space after method";
   end if;
   Req.Method.Last := I - 1;

   --  Parse target
   if I + 1 > Req.End_Headers then
      raise Parse_Error with "Invalid request line: unexpected end after method";
   end if;
   Req.Target.First := I + 1;
   I := Index (Req, Req.Target.First, " ");
   if I = 0 then
      raise Parse_Error with "Invalid request line: missing space after target";
   end if;
   Req.Target.Last := I - 1;

   --  Parse protocol
   if I + 1 > Req.End_Headers then
      raise Parse_Error with "Invalid request line: unexpected end after target";
   end if;
   Req.Protocol.First := I + 1;
   I := Index (Req, Req.Protocol.First, CRLF);
   if I = 0 then
      raise Parse_Error with "Invalid request line: missing CRLF after protocol";
   end if;
   Req.Protocol.Last := I - 1;

   --  Parse headers
   I := Req.Protocol.Last + CRLF'Length + 1;
   while I <= Req.End_Headers - CRLF'Length loop
      --  Check if we've reached the end of headers (empty line)
      if I + CRLF'Length - 1 <= Req.End_Headers and then
         Req.Item (I .. I + CRLF'Length - 1) = CRLF
      then
         exit;
      end if;

      --  Parse header name
      declare
         Name, Value : Span;
      begin
         Name.First := I;
         Next := Index (Req, Name.First, ":");
         if Next = 0 or else Next >= Req.End_Headers then
            raise Parse_Error with "Invalid header: missing colon";
         end if;
         Name.Last := Next - 1;

         --  Parse header value (skipping optional whitespace)
         Value.First := Next + 1;
         if Value.First <= Req.End_Headers and then
            Req.Item (Value.First) = ' '
         then
            Value.First := Value.First + 1;
         end if;

         Next := Index (Req, Value.First, CRLF);
         if Next = 0 or else Next > Req.End_Headers then
            raise Parse_Error with "Invalid header: missing CRLF after value";
         end if;
         Value.Last := Next - 1;

         --  Only add the header if both name and value are valid
         if Name.First <= Name.Last and then
            Value.First <= Value.Last and then
            Name.Last < Req.Item'Last and then
            Value.Last < Req.Item'Last
         then
            Request_Header_Maps.Include (
               Req.Headers,
               Get_String (Req, Name),
               Value
            );
         end if;

         --  Move to the next header
         I := Next + CRLF'Length;
      end;
   end loop;
exception
   when Constraint_Error =>
      raise Parse_Error with "Index out of bounds during request parsing";
end Parse_Request;
