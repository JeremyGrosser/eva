--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private with Ada.Containers.Vectors;
private with Ada.Real_Time;

generic
   type Context_Type is private;
   with procedure On_Timeout (Context : Context_Type);
package Eva.Timers
   with Elaborate_Body
is

   Max_Timeout : constant := 64;

   type Timer_Wheel is private;

   procedure Set_Timeout
     (Wheel       : in out Timer_Wheel;
      Timeout_Sec : Natural;
      Context     : Context_Type)
   with Pre => Timeout_Sec <= Max_Timeout;

   procedure Tick
      (Wheel : in out Timer_Wheel);

private

   type Timer is record
      Context : Context_Type;
   end record;

   type Timer_Id is new Positive;

   package Timer_Vectors is new Ada.Containers.Vectors
      (Timer_Id, Timer);

   type Slot_Index is mod Max_Timeout;
   type Slots_Array is array (Slot_Index) of Timer_Vectors.Vector;

   type Timer_Wheel is record
      Slots : Slots_Array := (others => Timer_Vectors.Empty_Vector);
      Current_Slot : Slot_Index := 0;
      Start_Time : Ada.Real_Time.Time;
   end record;
end Eva.Timers;
