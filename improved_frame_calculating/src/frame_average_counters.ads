with SDL.Timers;

package Frame_Average_Counters is

   subtype Sample_Range is Positive range 1 .. 100;
   type Frame_Average_Counter (Samples : Sample_Range) is tagged private;

   procedure Add_Frame (Self : in out Frame_Average_Counter);
   procedure Add_Sample (Self  : in out Frame_Average_Counter;
                         Ticks : SDL.Timers.Milliseconds_Long);
   function Get_Frame_Average (Self : in out Frame_Average_Counter)
                               return Float;

private

   type Sample is record
      Frames : Natural := Natural'First;
      Ticks  : SDL.Timers.Milliseconds_Long := 0;
   end record;

   type Samples_Array_Type is array (Positive range <>) of Sample;

   type Frame_Average_Counter (Samples : Sample_Range) is tagged record
      Current_Frame_Counter : Natural := Natural'First;
      Samples_Array         : Samples_Array_Type (1 .. Samples);
   end record;

end Frame_Average_Counters;
