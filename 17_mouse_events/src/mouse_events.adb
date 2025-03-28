pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Button;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Images;
with SDL.Images.IO;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with SDL.TTFs;

use SDL.Video.Renderers;

procedure Mouse_Events is

   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Texture  : SDL.Video.Textures.Texture;

   Total_Buttons : constant := 4;
   Buttons       : array (1 .. Total_Buttons) of Button.Button;
   Sprite_Clips  : Button.Sprite_Clips_Array;

   function Initialise return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Mouse Events",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
         Flags    => 0);

      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated or
           SDL.Video.Renderers.Present_V_Sync);

      return True;

   end Initialise;

   procedure Load_Texture
     (Texture   : in out SDL.Video.Textures.Texture;
      Renderer  : SDL.Video.Renderers.Renderer;
      File_Name : SDL.TTFs.UTF_Strings.UTF_String) is
      Surface    : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Surface, File_Name);

      SDL.Video.Textures.Makers.Create (Texture, Renderer, Surface);

      Surface.Finalize;
   end Load_Texture;

   procedure Load_Media is
      Button_Width  : constant := 300;
      Button_Height : constant := 200;
   begin
      Load_Texture (Texture, Renderer, "../resources/button.png");

      --  Set sprite positions
      declare
         I : Natural := 0;
      begin
         for S in Sprite_Clips'Range loop
            Sprite_Clips (S) := (0, SDL.Coordinate (I * 200), Button_Width, Button_Height);
            I := @ + 1;
         end loop;
      end;

      --  Set button dimensions and positions
      for I in Buttons'Range loop
         Buttons (I).Set_Dimensions (Button_Width, Button_Height);
      end loop;

      Buttons (1).Set_Position (0, 0);
      Buttons (2).Set_Position (Screen_Width - Button_Width, 0);
      Buttons (3).Set_Position (0, Screen_Height - Button_Height);
      Buttons (4).Set_Position (Screen_Width - Button_Width, Screen_Height - Button_Height);
   end Load_Media;

   procedure Free_Media is
   begin
      Texture.Finalize;
   end Free_Media;

   procedure Close is
   begin
      Free_Media;

      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

   procedure Handle_Events is
      Finished : Boolean := False;
      Event    : SDL.Events.Events.Events;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;
               when SDL.Events.Keyboards.Key_Down =>
                  case Event.Keyboard.Key_Sym.Key_Code is
                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;
                     when others => null;
                  end case;
               when SDL.Events.Mice.Motion |
                    SDL.Events.Mice.Button_Down |
                    SDL.Events.Mice.Button_Up =>
                  for I in Buttons'First .. Buttons'Last loop
                     Buttons (I).Handle_Event (Event);
                  end loop;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         for I in Buttons'First .. Buttons'Last loop
            Buttons (I).Render (Renderer, Texture, Sprite_Clips);
         end loop;

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

   Load_Media;

   Handle_Events;

   Close;

   Put_Line ("Process completed.");
exception
   when Event : others =>
      Put_Line ("Process not completed.");
      Put_Line ("Exception raised: " &
                  Ada.Exceptions.Exception_Name (Event));
      Put_Line ("Exception mesage: " &
                  Ada.Exceptions.Exception_Message (Event));
end Mouse_Events;
