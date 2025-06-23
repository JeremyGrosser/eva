package body Eva.Timers is

   procedure Set_Timeout
      (This     : in out Timers;
       Deadline : Ada.Real_Time.Time;
       Context  : Context_Type)
   is
   begin
      if not Contains (This.M, Deadline) then
         Include (This.M, Deadline, Context_Vectors.Empty_Vector);
      end if;
      Append (Reference (This.M, Deadline), Context);
   end Set_Timeout;

   procedure Poll
      (This : in out Timers)
   is
      Now : constant Time := Clock;
   begin
      loop
         exit when Is_Empty (This.M) or else First_Key (This.M) > Now;
         for Context of First_Element (This.M) loop
            On_Timeout (Context);
         end loop;
         Delete_First (This.M);
      end loop;
   end Poll;

end Eva.Timers;
