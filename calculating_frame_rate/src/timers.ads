with SDL.Timers;

package Timers is

   type Timer is tagged private;

   procedure Start (Self : in out Timer);
   procedure Stop (Self : in out Timer);
   procedure Pause (Self : in out Timer);
   procedure Unpause (Self : in out Timer);

   function Get_Ticks (Self : Timer) return SDL.Timers.Milliseconds_Long;

   function Is_Started (Self : Timer) return Boolean;
   function Is_Paused (Self : Timer) return Boolean;

private

   type Timer is tagged record
      Paused        : Boolean := False;
      Started       : Boolean := False;
      Started_Ticks : SDL.Timers.Milliseconds_Long := 0;
      Paused_Ticks  : SDL.Timers.Milliseconds_Long := 0;
   end record;

end Timers;
