with Eva.Strings;
with Eva.HTTP.Errors;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Assertions;

package body Workers is
   procedure Test_Handler
      (Req  : Eva.HTTP.Request;
       Resp : in out Eva.HTTP.Response)
   is
      use Eva.HTTP;
      use Eva.Strings;
   begin
      if Method (Req) = "GET" then
         if Path (Req) = "/version" then
            Set_Status (Resp, 200, "OK");
            Set_Header (Resp, "Content-Type", "text/plain;charset=utf-8");
            if Query_Parameter (Req, "q") = "test" then
               Put (Resp, Wide_Wide_Character'Val (16#1F60A#));
            else
               Put (Resp, "1.0.0");
               Put (Resp, LF);
               Put (Resp, Decode (UTF8 (Header (Req, "User-Agent"))));
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
               Put (Resp, Decode (UTF8 (Data (Req))));
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

   procedure Test_Strings is
      use Ada.Assertions;
      use Eva.Strings;
      use type Ada.Containers.Hash_Type;
      A : constant Unicode := "test";
   begin
      Assert (Hash (A) /= 0);
      Assert (Trim (A, 't') = "es");
      Assert (Trim ("", 't') = "");
      Assert (Trim (A, 'x') = "test");
      Assert (Replace (A, "es", "tt") = "tttt");
      Assert (Index (A, "st", 1) = 3);
      Assert (Index (A, "abc", 1) = 0);
      Assert (Starts_With (A, "te"));
      Assert (not Starts_With ("", "te"));
      Assert (not Starts_With (A, "testing"));
      Assert (Ends_With (A, "st"));
      Assert (Remove_Prefix (A, "te") = "st");
      Assert (Remove_Prefix (A, "foo") = "test");
      Assert (Remove_Suffix (A, "st") = "te");
      Assert (Remove_Suffix (A, "foo") = "test");
      Assert (Decode ("ABCD") = Unicode'("ABCD"));
      Assert (Encode (To_Unbounded ("ABCD")) = UTF8'("ABCD"));
      Assert (To_String (123) = "123");
   end Test_Strings;

   procedure Wait is
      task Runner;

      task body Runner is
      begin
         Test_Server.Run;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            Ada.Command_Line.Set_Exit_Status (1);
      end Runner;

      package Env renames Ada.Environment_Variables;
      Test_Duration : Duration;
   begin
      Test_Strings;
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
