pragma Ada_2022;

with Ada.Exceptions;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO; use Ada.Text_IO;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images;
with SDL.Images.IO;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
use SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with Interfaces.C; use Interfaces.C;

procedure Animated_Sprites_And_Vsync is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;
   type Sprite_Clip_Array is array (Positive range <>) of SDL.Video.Rectangles.Rectangle;

   Width  : constant := 640;
   Height : constant := 480;

   Window               : SDL.Video.Windows.Window;
   Event                : SDL.Events.Events.Events;
   Sprite_Sheet_Texture : SDL.Video.Textures.Texture;
   Sprite_Frames        : constant Positive := 4;
   Sprite_Clips         : Sprite_Clip_Array (1 .. Sprite_Frames);
   Renderer             : SDL.Video.Renderers.Renderer;

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
         Title    => "SDL Tutorial - Animated Sprites and VSync",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated or
           SDL.Video.Renderers.Present_V_Sync);

      Renderer.Set_Draw_Colour ((others => 255));

      return True;

   end Initialise;

   procedure Load_Texture
     (Texture   : in out SDL.Video.Textures.Texture;
      Renderer  : SDL.Video.Renderers.Renderer;
      File_Name : UTF_Strings.UTF_String) is
      Loaded_Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Loaded_Surface, File_Name);

      Loaded_Surface.Set_Colour_Key ((Red => 0,
                                      Green => 255,
                                      Blue  => 255,
                                      Alpha => 0), True);

      SDL.Video.Textures.Makers.Create
        (Texture,
         Renderer,
         Loaded_Surface);

      Loaded_Surface.Finalize;
   end Load_Texture;

   procedure Load_Media (Sprite_Clips : in out Sprite_Clip_Array) is
   begin
      Load_Texture (Sprite_Sheet_Texture,
                    Renderer,
                    "../resources/animated_foo.png");

      Sprite_Clips (2) := (0, 0, 64, 205);
      Sprite_Clips (2) := (64, 0, 64, 205);
      Sprite_Clips (3) := (128, 0, 64, 205);
      Sprite_Clips (4) := (192, 0, 64, 205);
   end Load_Media;

   procedure Free_Media is
   begin
      Sprite_Sheet_Texture.Finalize;
   end Free_Media;

   procedure Close is
   begin
      Free_Media;

      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

   procedure Render_Texture
     (Renderer         : in out SDL.Video.Renderers.Renderer;
      Texture          : in out SDL.Video.Textures.Texture;
      X                : SDL.Dimension;
      Y                : SDL.Dimension;
      Clip             : SDL.Video.Rectangles.Rectangle) is
      Render_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        (X,
         Y,
         Clip.Width,
         Clip.Height);
   begin
      Renderer.Copy (Texture, Clip, Render_Rectangle);
   end Render_Texture;

   procedure Handle_Events is
      Finished : Boolean := False;
      Frame    : Integer := 1;
      Animation_Frames : constant := Sprite_Frames;
      Slow_Down_Factor : constant := Sprite_Frames;
      Current_Clip : SDL.Video.Rectangles.Rectangle;
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

         --  Clear screen
         Renderer.Set_Draw_Colour ((255, 255, 255, 255));
         Renderer.Clear;

         --  Render frame
         Current_Clip := Sprite_Clips ((Frame / Slow_Down_Factor) + 1);
         Render_Texture (Renderer,
                         Sprite_Sheet_Texture,
                         (Width - Current_Clip.Width) / 2,
                         (Height - Current_Clip.Height) / 2,
                         Current_Clip);

         Renderer.Present;

         Frame := @ + 1;
         if (Frame / Slow_Down_Factor) + 1 > Animation_Frames then
            Frame := 0;
         end if;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

   Load_Media (Sprite_Clips);

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
end Animated_Sprites_And_Vsync;
