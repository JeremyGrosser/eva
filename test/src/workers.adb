--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
pragma Ada_2022;
with Eva.HTTP.Errors;
with Eva.HTTP.Server;
with Eva.HTTP;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Interrupts.Names;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.UTF_Encoding;

package body Workers is
   LF : constant String := "" & ASCII.LF;

   function To_String
      (Ch : Wide_Wide_Character)
      return String
   is
      Str  : constant Wide_Wide_String := "" & Ch;
      UStr : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
         Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Str);
   begin
      return String (UStr);
   end To_String;

   protected Signals is
      procedure SIGPIPE
         with Attach_Handler => Ada.Interrupts.Names.SIGPIPE;
   end Signals;

   protected body Signals is
      procedure SIGPIPE is null;
   end Signals;

   pragma Unreferenced (Signals);

   type Context_Type is record
      Request_Count : Natural := 0;
   end record;

   type Any_Context_Type is access Context_Type;

   procedure Test_Handler
      (Context : Any_Context_Type;
       Req     : Eva.HTTP.Request;
       Resp    : in out Eva.HTTP.Response)
   is
      use Eva.HTTP;
   begin
      Context.Request_Count := Context.Request_Count + 1;

      if Method (Req) = "GET" then
         if Path (Req) = "/version" then
            Set_Status (Resp, 200, "OK");
            Set_Header (Resp, "Content-Type", "text/plain;charset=utf-8");
            if Query_Parameter (Req, "q") = "test" then
               Put (Resp, To_String (Wide_Wide_Character'Val (16#1F60A#)));
            else
               Put (Resp, "1.0.0");
               Put (Resp, LF);
               Put (Resp, Header (Req, "User-Agent"));
               Put (Resp, LF);
            end if;

            Put (Resp, LF);
            return;
         elsif Path (Req) = "/fault1" then
            Set_Status (Resp, 200, "OK");
            Set_Status (Resp, 404, "Not Found");
            return;
         elsif Path (Req) = "/fault2" then
            Put (Resp, "data before status");
            Set_Status (Resp, 200, "OK");
            return;
         end if;
      elsif Method (Req) = "POST" then
         declare
            Content_Length : Natural;
         begin
            Content_Length := Natural'Value (Header (Req, "Content-Length"));
            if Data (Req)'Length = Content_Length then
               Set_Status (Resp, 200, "OK");
               Set_Header (Resp, "Content-Type", "text/plain;charset=utf-8");
               Put (Resp, Data (Req));
               return;
            else
               return;
            end if;
         exception
            when others =>
               Eva.HTTP.Errors.Bad_Request (Resp);
               return;
         end;
      else
         Eva.HTTP.Errors.Method_Not_Allowed (Resp);
         return;
      end if;

      Eva.HTTP.Errors.Not_Found (Resp);
   end Test_Handler;

   package Test_Server is new Eva.HTTP.Server
      (Context_Type     => Context_Type,
       Any_Context_Type => Any_Context_Type,
       Handle_Request   => Test_Handler);

   procedure Wait is
      task Runner;

      task body Runner is
         Context : aliased Context_Type;
      begin
         Test_Server.Run (Context'Unrestricted_Access);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            Ada.Command_Line.Set_Exit_Status (1);
      end Runner;

      package Env renames Ada.Environment_Variables;
      Test_Duration : Duration;
   begin
      if Env.Exists ("TEST_DURATION") then
         Test_Duration := Duration'Value (Env.Value ("TEST_DURATION"));
      else
         Test_Duration := Duration'Last;
      end if;
      delay Test_Duration;
      Test_Server.Stop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Ada.Command_Line.Set_Exit_Status (1);
         Test_Server.Stop;
   end Wait;
end Workers;
