pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images;
with SDL.Images.IO;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with Interfaces.C;

procedure Clip_Rendering_And_Sprite_Sheets is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;
   type Sprite_Clip_Array is array (1 .. 49) of SDL.Video.Rectangles.Rectangle;

   Width  : constant := 640;
   Height : constant := 480;

   Window               : SDL.Video.Windows.Window;
   Event                : SDL.Events.Events.Events;
   Sprite_Sheet_Texture : SDL.Video.Textures.Texture;
   Sprite_Clips         : Sprite_Clip_Array;
   Renderer             : SDL.Video.Renderers.Renderer;

   procedure Load_Media
     (Texture   : in out SDL.Video.Textures.Texture;
      Renderer  : SDL.Video.Renderers.Renderer;
      Sprite_Clips : in out Sprite_Clip_Array;
      File_Name : UTF_Strings.UTF_String) is
      Loaded_Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Loaded_Surface, File_Name);

      Loaded_Surface.Set_Colour_Key ((0, 255, 255, 255), True);

      SDL.Video.Textures.Makers.Create
        (Texture,
         Renderer,
         Loaded_Surface);

      Loaded_Surface.Finalize;

      --  Set sprite positions
      Sprite_Clips (1) := (0, 0, 100, 100);
      Sprite_Clips (2) := (100, 0, 100, 100);
      Sprite_Clips (3) := (0, 100, 100, 100);
      Sprite_Clips (4) := (100, 100, 100, 100);
   end Load_Media;

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

      use type Interfaces.C.int;
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

         --  Render top left sprite
         Render_Texture (Renderer,
                         Sprite_Sheet_Texture,
                         0,
                         0,
                         Sprite_Clips (1));
         --  Render top right sprite
         Render_Texture (Renderer,
                         Sprite_Sheet_Texture,
                         Width - Sprite_Clips (2).Width,
                         0,
                         Sprite_Clips (2));
         --  Render top left sprite
         Render_Texture (Renderer,
                         Sprite_Sheet_Texture,
                         0,
                         Height - Sprite_Clips (3).Height,
                         Sprite_Clips (3));
         --  Render top left sprite
         Render_Texture (Renderer,
                         Sprite_Sheet_Texture,
                         Width - Sprite_Clips (4).Width,
                         Height - Sprite_Clips (4).Height,
                         Sprite_Clips (4));

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);
   SDL.Video.Renderers.Makers.Create
     (Window => Window,
      Rend   => Renderer,
      Flags  => SDL.Video.Renderers.Accelerated);

   Renderer.Set_Draw_Colour ((others => 255));

   Load_Media (Sprite_Sheet_Texture,
               Renderer,
               Sprite_Clips,
               "../resources/dots.png");

   Handle_Events;

   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end Clip_Rendering_And_Sprite_Sheets;
