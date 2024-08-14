with Ada.Text_IO;
with SDL.Video.Surfaces;
with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Video.Rectangles;

procedure First_Graphic_Window is
   Width : constant := 640;
   Height : constant := 480;

   Window   : SDL.Video.Windows.Window;
   Event    : SDL.Events.Events.Events;
   Surface : SDL.Video.Surfaces.Surface;

   procedure Wait is
      use type SDL.Events.Event_Types;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            if Event.Common.Event_Type = SDL.Events.Quit then
               return;
            end if;
         end loop;
      end loop;
   end Wait;
begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);

   Surface := Window.Get_Surface;

   Surface.Fill (Area => SDL.Video.Rectangles.Rectangle'(0, 0, Width, Height),
                 Colour => 16#00FFFFFF#);

   Window.Update_Surface;

   Wait;

   Surface.Finalize;
   Window.Finalize;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end First_Graphic_Window;
