pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images;
with SDL.Images.IO;
with SDL.Video; use SDL.Video;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with SDL.Video.Palettes; use SDL.Video.Palettes;

procedure Alpha_Blending is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Width  : constant := 640;
   Height : constant := 480;

   Window            : SDL.Video.Windows.Window;
   Renderer          : SDL.Video.Renderers.Renderer;
   Event             : SDL.Events.Events.Events;
   Modulated_Texture : SDL.Video.Textures.Texture;
   Background_Texture : SDL.Video.Textures.Texture;

   procedure Load_Texture
     (Texture        : in out SDL.Video.Textures.Texture;
      Renderer       : SDL.Video.Renderers.Renderer;
      File_Name      : UTF_Strings.UTF_String) is
      Loaded_Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Loaded_Surface, File_Name);

      Loaded_Surface.Set_Colour_Key ((0, 255, 255, 255), True);

      SDL.Video.Textures.Makers.Create
        (Texture,
         Renderer,
         Loaded_Surface);

      Loaded_Surface.Finalize;
   end Load_Texture;

   procedure Load_Media is
   begin
      Load_Texture (Modulated_Texture,
                    Renderer,
                    "../resources/fadeout.png");
      Modulated_Texture.Set_Blend_Mode (Alpha_Blend);

      Load_Texture (Background_Texture,
                    Renderer,
                    "../resources/fadein.png");
   end Load_Media;

   procedure Render_Texture
     (Renderer : in out SDL.Video.Renderers.Renderer;
      Texture  : in out SDL.Video.Textures.Texture;
      X        : SDL.Dimension;
      Y        : SDL.Dimension) is
      Render_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        (X,
         Y,
         Texture.Get_Size.Width,
         Texture.Get_Size.Height);
   begin
      Renderer.Copy (Texture, Render_Rectangle);
   end Render_Texture;

   procedure Handle_Events is
      Finished : Boolean := False;
      A        : Integer := 255;

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
                     when SDL.Events.Keyboards.Code_W =>
                        --  Increase alpha
                        if A + 32 >= 255 then
                           A := 255;
                        else
                           A := @ + 32;
                        end if;
                     when SDL.Events.Keyboards.Code_S =>
                        --  Decrease alpha
                        if A - 32 <= 0 then
                           A := 0;
                        else
                           A := @ - 32;
                        end if;
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((255, 255, 255, 255));
         Renderer.Clear;

         --  Render background
         Render_Texture (Renderer, Background_Texture, 0, 0);
         --  Modulate and render texture
         Modulated_Texture.Set_Alpha (Colour_Component (A));
         Render_Texture (Renderer, Modulated_Texture, 0, 0);

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

   Load_Media;

   Handle_Events;

   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end Alpha_Blending;
