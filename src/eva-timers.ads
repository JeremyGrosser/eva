private with Ada.Containers.Vectors;
private with Ada.Containers.Ordered_Maps;
with Ada.Real_Time;

generic
   type Context_Type is private;
   with procedure On_Timeout (Context : Context_Type);
package Eva.Timers
   with Elaborate_Body
is
   type Timers is private;

   procedure Set_Timeout
      (This     : in out Timers;
       Deadline : Ada.Real_Time.Time;
       Context  : Context_Type);

   procedure Poll
      (This : in out Timers);

private

   package Context_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Context_Type);
   use Context_Vectors;

   use Ada.Real_Time;
   package Time_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type      => Time,
       Element_Type  => Context_Vectors.Vector);
   use Time_Maps;

   type Timers is record
      M : Time_Maps.Map := Time_Maps.Empty_Map;
   end record;

end Eva.Timers;
