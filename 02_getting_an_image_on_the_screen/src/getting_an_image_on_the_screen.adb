with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
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

   function Initialise return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Getting an Image on the Screen",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      Surface := Window.Get_Surface;

      return True;
   end Initialise;

   procedure Close is
   begin
      Hello_World_Image.Finalize;
      Surface.Finalize;
      Window.Finalize;
      SDL.Finalise;
   end Close;

   procedure Load_Media is
   begin
      SDL.Video.Surfaces.Makers.Create
        (Hello_World_Image,
         "../resources/hello_world.bmp");
   end Load_Media;

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
   if not Initialise then
      return;
   end if;

   Load_Media;

   Surface.Blit (Hello_World_Image);

   Window.Update_Surface;

   Wait;

   Close;

   Put_Line ("Process completed.");
exception
   when Event : others =>
      Put_Line ("Process not completed.");
      Put_Line ("Exception raised: " &
                  Ada.Exceptions.Exception_Name (Event));
      Put_Line ("Exception mesage: " &
                  Ada.Exceptions.Exception_Message (Event));
end Getting_An_Image_On_The_Screen;
