--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Bounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Eva_Config;

package Eva.HTTP
   with Elaborate_Body, SPARK_Mode => On
is
   type Request is private;

   Parse_Error : exception;

   function Method
      (This : Request)
      return String;

   function Path
      (This : Request)
      return String
   with SPARK_Mode => Off;

   function Header
      (This : Request;
       Name : String)
       return String
   with SPARK_Mode => Off;

   function Query_Parameter
      (This    : Request;
       Key     : String;
       Default : String := "")
       return String
   with SPARK_Mode => Off;

   function Data
      (This : Request)
      return String;

   function Is_Complete
      (This : Request)
      return Boolean
   with SPARK_Mode => Off;

   function Image
      (This : Request)
      return String;

   type Response is private;

   type HTTP_Status is new Integer range 100 .. 599;

   procedure Set_Status
      (This    : in out Response;
       Code    : HTTP_Status;
       Message : String);

   procedure Set_Header
      (This : in out Response;
       Key, Value : String);

   procedure Put
      (This : in out Response;
       Data : String);

   function Status
      (This : Response)
      return HTTP_Status;

   function Content_Length
      (This : Response)
      return Natural;

   procedure Reset
      (Resp : in out Response);

private

   type Span is record
      First, Last : Natural := 0;
   end record;

   package Request_Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type         => String,
       Element_Type     => Span,
       Equivalent_Keys  => Ada.Strings.Equal_Case_Insensitive,
       Hash             => Ada.Strings.Hash_Case_Insensitive);

   package Request_Buffers is new Ada.Strings.Bounded.Generic_Bounded_Length
      (Max => Eva_Config.Max_Request_Length);

   use Ada.Strings.Unbounded;

   package Response_Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type         => String,
       Element_Type     => Ada.Strings.Unbounded.Unbounded_String,
       Equivalent_Keys  => Ada.Strings.Equal_Case_Insensitive,
       Hash             => Ada.Strings.Hash_Case_Insensitive);

   type Request is record
      Buffer      : Byte_Buffer (Capacity => Eva_Config.Max_Request_Length);
      End_Headers : Natural := 0;
      Start_Data  : Natural := 0;
      Method      : Span := (0, 0);
      Target      : Span := (0, 0);
      Protocol    : Span := (0, 0);
      Headers     : Request_Header_Maps.Map := Request_Header_Maps.Empty_Map;
   end record;

   type Response is record
      Buffer         : Unbounded_String := Null_Unbounded_String;
      Status         : HTTP_Status := 500;
      Status_Message : Unbounded_String := Null_Unbounded_String;
      Headers        : Response_Header_Maps.Map := Response_Header_Maps.Empty_Map;
      Data           : Unbounded_String := Null_Unbounded_String;
      Complete       : Boolean := False;
   end record;

   function Is_Empty
      (Resp : Response)
      return Boolean;

   function Available
      (This : Request_Buffers.Bounded_String)
      return Natural;

   procedure Reset
      (Req : in out Request);

   procedure Parse_Request
      (Req  : in out Request);

   procedure Set_Complete
      (This : in out Response);

end Eva.HTTP;
