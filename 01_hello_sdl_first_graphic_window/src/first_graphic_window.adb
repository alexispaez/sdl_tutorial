with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Video.Surfaces;
with SDL.Video.Rectangles;
with SDL.Video.Windows.Makers;

procedure First_Graphic_Window is
   Width : constant := 640;
   Height : constant := 480;

   Window  : SDL.Video.Windows.Window;
   Event   : SDL.Events.Events.Events;
   Surface : SDL.Video.Surfaces.Surface;

   procedure Wait is
      Finished : Boolean := False;
   begin
      --  The hack to get the window to stay up
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;
               when others => null;
            end case;
         end loop;

         exit when Finished;
      end loop;
   end Wait;
begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial - Hello SDL",
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
