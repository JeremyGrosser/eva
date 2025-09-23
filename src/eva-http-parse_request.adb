--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
separate (Eva.HTTP) procedure Parse_Request
   (Req : in out Request)
is
   Data  : constant String := To_String (Req.Buffer);
   First : Natural := Data'First;
   I     : Natural := Data'First;

   function Index
      (Str : String;
       Sub : String)
      return Natural
   is
   begin
      for J in Str'First .. (Str'Last - Sub'Length) + 1 loop
         if Str (J .. J + (Sub'Length - 1)) = Sub then
            return J;
         end if;
      end loop;
      return Str'First - 1;
   end Index;

   procedure Find_End is
      CRLFCRLF : constant String := ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
      LFLF : constant String := ASCII.LF & ASCII.LF;
   begin
      Req.End_Headers := Index (Data, CRLFCRLF);
      if Req.End_Headers = 0 then
         Req.End_Headers := Index (Data, LFLF);
         if Req.End_Headers > 0 then
            Req.Start_Data := Req.End_Headers + 2;
         end if;
      else
         Req.Start_Data := Req.End_Headers + 4;
      end if;
   end Find_End;

   function End_Of_Headers
      return Boolean
   is (I > Req.End_Headers);

   procedure Scan_To
      (Ch : Character)
   is
   begin
      while not End_Of_Headers loop
         if Data (I) = Ch then
            return;
         else
            I := I + 1;
         end if;
      end loop;
      I := 0;
   end Scan_To;

   procedure Strip
      (Sp : in out Span;
       Ch : Character)
   is
   begin
      while Data (Sp.First) = Ch and then Sp.First < Sp.Last loop
         Sp.First := Sp.First + 1;
      end loop;

      while Data (Sp.Last) = Ch and then Sp.Last > Sp.First loop
         Sp.Last := Sp.Last - 1;
      end loop;
   end Strip;
begin
   Find_End;
   if Req.End_Headers = 0 then
      return;
   end if;

   Scan_To (' ');
   if I = 0 then
      raise Parse_Error with "No space after method";
   end if;
   Req.Method := (First, I - 1);
   First := I + 1;
   I := First;

   if not End_Of_Headers then
      Scan_To (' ');
      if I = 0 then
         raise Parse_Error with "No space after target";
      end if;
      Req.Target := (First, I - 1);
      First := I + 1;
      I := First;
   end if;

   if not End_Of_Headers then
      Scan_To (ASCII.LF);
      if I = 0 then
         raise Parse_Error with "No LF after protocol";
      end if;
      Req.Protocol := (First, I - 1);
      Strip (Req.Protocol, ASCII.CR);
      First := I + 1;
      I := First;
   end if;

   while not End_Of_Headers loop
      declare
         Key, Value : Span;
      begin
         Scan_To (':');
         if I = 0 then
            raise Parse_Error with "No ':' after header name";
         end if;
         Key := (First, I - 1);
         First := I + 1;
         I := First;

         Scan_To (ASCII.LF);
         if I = 0 then
            I := Req.End_Headers + 1;
         end if;
         Value := (First, I - 1);
         First := I + 1;
         I := First;

         Strip (Value, ' ');
         Strip (Value, ASCII.CR);
         Request_Header_Maps.Include (Req.Headers, Get_String (Req, Key), Value);
      end;
   end loop;
--  exception
--     when Constraint_Error =>
--        raise Parse_Error with "Index out of bounds during request parsing";
end Parse_Request;
