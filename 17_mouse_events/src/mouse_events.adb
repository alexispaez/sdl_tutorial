pragma Ada_2022;

with Ada.Text_IO;
with Button;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Images;
with SDL.Images.IO;
with SDL.TTFs.Makers;
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
   Font     : SDL.TTFs.Fonts;

   Total_Buttons : constant := 4;
   Buttons       : array (1 .. Total_Buttons) of Button.Button;
   Sprite_Clips  : Button.Sprite_Clips_Array;

   --  procedure Load_From_Rendered_Text
   --    (Texture : in out SDL.Video.Textures.Texture;
   --     Text    : String;
   --     Colour  : SDL.Video.Palettes.Colour) is
   --     Text_Surface : SDL.Video.Surfaces.Surface;
   --  begin
   --     Text_Surface := Font.Render_Solid (Text, Colour);
   --
   --     SDL.Video.Textures.Makers.Create (Texture, Renderer, Text_Surface);
   --
   --     Text_Surface.Finalize;
   --  end Load_From_Rendered_Text;

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

   --  procedure Render (Renderer : in out SDL.Video.Renderers.Renderer;
   --                    Texture  : in out SDL.Video.Textures.Texture;
   --                    X        : SDL.Dimension;
   --                    Y        : SDL.Dimension) is
   --     Render_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
   --                          (X,
   --                           Y,
   --                           Texture.Get_Size.Width,
   --                           Texture.Get_Size.Height);
   --  begin
   --     Renderer.Copy (Texture, Render_Rectangle);
   --  end Render;

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
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   if not SDL.TTFs.Initialise then
      return;
   end if;

   if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
      Flags    => 0);

   SDL.Video.Renderers.Makers.Create
     (Window => Window,
      Rend   => Renderer,
      Flags  => SDL.Video.Renderers.Accelerated or
        SDL.Video.Renderers.Present_V_Sync);

   SDL.TTFs.Makers.Create (Font, "../resources//lazy.ttf", 28);

   Load_Media;

   Handle_Events;

   SDL.TTFs.Quit;
   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");

end Mouse_Events;
