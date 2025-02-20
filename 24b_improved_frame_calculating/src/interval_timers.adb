package body Interval_Timers is

   function Has_Elapsed (Self          : in out Interval_Timer;
                         Ticks_Elapsed : in out SDL.Timers.Milliseconds_Long)
                         return Boolean is
      use type SDL.Timers.Milliseconds_Long;
   begin
      if Self.Is_Started and then
        Self.Get_Ticks >= Self.Interval
      then
         Ticks_Elapsed := Self.Get_Ticks;
         --  Reset the start ticks for the next interval
         Self.Start;
         return True;
      else
         Ticks_Elapsed := 0;
         return False;
      end if;
   end Has_Elapsed;

end Interval_Timers;
