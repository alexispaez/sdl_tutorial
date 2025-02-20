with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Video.Surfaces;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Windows.Makers;

procedure Getting_An_Image_On_The_Screen is
   Width  : constant := 640;
   Height : constant := 480;

   Window            : SDL.Video.Windows.Window;
   Event             : SDL.Events.Events.Events;
   Surface           : SDL.Video.Surfaces.Surface;
   Hello_World_Image : SDL.Video.Surfaces.Surface;

   procedure Wait is
      Finished : Boolean := False;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;
               when SDL.Events.Keyboards.Key_Up =>
                  case Event.Keyboard.Key_Sym.Key_Code is
                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;
                     when others => null;
                  end case;
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
      Title    => "SDL Tutorial - Getting an Image on the Screen",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);

   Surface := Window.Get_Surface;

   SDL.Video.Surfaces.Makers.Create
     (Hello_World_Image,
     "../resources/hello_world.bmp");

   Surface.Blit (Hello_World_Image);

   Window.Update_Surface;

   Wait;

   Hello_World_Image.Finalize;
   Surface.Finalize;
   Window.Finalize;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end Getting_An_Image_On_The_Screen;
