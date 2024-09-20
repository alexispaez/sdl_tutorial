with SDL.Timers;

package Timers is

   type Timer is tagged private;

   --  Starts the timer
   procedure Start (Self : in out Timer);
   --  Stops the timer
   procedure Stop (Self : in out Timer);
   --  Pauses the timer
   procedure Pause (Self : in out Timer);
   --  Unpauses the timer
   procedure Unpause (Self : in out Timer);

   --  Returns the ticks since the timer was started
   function Get_Ticks (Self : Timer) return SDL.Timers.Milliseconds_Long;

   --  Indicates if the timer is started
   function Is_Started (Self : Timer) return Boolean;
   --  Indicates if the timer is paused
   function Is_Paused (Self : Timer) return Boolean;

private

   type Timer is tagged record
      Paused        : Boolean := False;
      Started       : Boolean := False;
      Started_Ticks : SDL.Timers.Milliseconds_Long := 0;
      Paused_Ticks  : SDL.Timers.Milliseconds_Long := 0;
   end record;

end Timers;
