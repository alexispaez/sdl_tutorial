with SDL.Timers; use SDL.Timers;

package body Timers is

   procedure Start (Self : in out Timer) is
   begin
      Self.Started := True;
      Self.Paused := False;

      Self.Started_Ticks := SDL.Timers.Ticks;
      Self.Paused_Ticks := 0;
   end Start;

   procedure Stop (Self : in out Timer) is
   begin
      Self.Started := False;
      Self.Paused := False;

      Self.Started_Ticks := 0;
      Self.Paused_Ticks := 0;
   end Stop;

   procedure Pause (Self : in out Timer) is
   begin
      if Self.Started and then not Self.Paused then
         Self.Paused := True;

         Self.Paused_Ticks := SDL.Timers.Ticks - Self.Started_Ticks;
         Self.Started_Ticks := 0;
      end if;
   end Pause;

   procedure Unpause (Self : in out Timer) is
   begin
      if Self.Started and then Self.Paused then
         Self.Paused := False;

         Self.Started_Ticks := SDL.Timers.Ticks - Self.Paused_Ticks;
         Self.Paused_Ticks := 0;
      end if;
   end Unpause;

   function Get_Ticks (Self : Timer) return SDL.Timers.Milliseconds_Long is
      Time : SDL.Timers.Milliseconds_Long := 0;
   begin
      if Self.Started then
         if Self.Paused then
            Time := Self.Paused_Ticks;
         else
            Time := SDL.Timers.Ticks - Self.Started_Ticks;
         end if;
      end if;

      return Time;
   end Get_Ticks;

   function Is_Started (Self : Timer) return Boolean is
   begin
      return Self.Started;
   end Is_Started;

   function Is_Paused (Self : Timer) return Boolean is
   begin
      return Self.Paused;
   end Is_Paused;

end Timers;
