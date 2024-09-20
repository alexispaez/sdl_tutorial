with Ada.Text_IO;

package body Frame_Average_Counters is

   procedure Add_Frame (Self : in out Frame_Average_Counter) is
   begin
      --  Updates the current frame counter
      Self.Current_Frame_Counter := @ + 1;
   end Add_Frame;

   procedure Add_Sample (Self  : in out Frame_Average_Counter;
                         Ticks : SDL.Timers.Milliseconds_Long) is
      use type SDL.Timers.Milliseconds_Long;
   begin
      if Ticks /= 0 then
         --  Moves all the existing samples up by one position
         Self.Samples_Array (Sample_Range'First + 1 .. Self.Samples) :=
           Self.Samples_Array (Sample_Range'First .. Self.Samples - 1);

         --  Inserts a new sample with the current frame count and ticks
         Self.Samples_Array (Sample_Range'First) :=
           (Frames => Self.Current_Frame_Counter,
            Ticks  => Ticks);

         Self.Current_Frame_Counter := Natural'First;
      end if;
   end Add_Sample;

   function Get_Frame_Average (Self : in out Frame_Average_Counter)
                               return Float is
      Average      : Float := 0.0;
      Total_Ticks  : SDL.Timers.Milliseconds_Long := 0;
      Total_Frames : Natural := Natural'First;

      use type SDL.Timers.Milliseconds_Long;
   begin
      --  Computes the average over all the samples
      for I in Self.Samples_Array'Range loop
         Total_Ticks := @ + Self.Samples_Array (I).Ticks;
         Total_Frames := @ + Self.Samples_Array (I).Frames;
      end loop;

      if Total_Ticks /= 0 then
         Average := Float (Total_Frames) / Float (Total_Ticks);
      end if;

      return Average;
   end Get_Frame_Average;

end Frame_Average_Counters;
