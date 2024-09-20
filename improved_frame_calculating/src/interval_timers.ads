with Timers;
with SDL.Timers;

package Interval_Timers is

   type Interval_Timer  (Interval : SDL.Timers.Milliseconds_Long) is
     new Timers.Timer with null record;

   function Has_Elapsed (Self          : in out Interval_Timer;
                         Ticks_Elapsed : in out SDL.Timers.Milliseconds_Long)
                         return Boolean;
end Interval_Timers;
