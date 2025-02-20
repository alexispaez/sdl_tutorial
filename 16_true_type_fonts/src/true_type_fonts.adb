with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
use SDL.Video.Renderers;
with Interfaces.C; use Interfaces.C;
with SDL.TTFs;

procedure True_Type_Fonts is

   Width  : constant := 640;
   Height : constant := 480;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;
   Texture  : SDL.Video.Textures.Texture;
   Font     : SDL.TTFs.Fonts;

   procedure Load_From_Rendered_Text
     (Texture : in out SDL.Video.Textures.Texture;
      Text    : String;
      Colour  : SDL.Video.Palettes.Colour) is
      Text_Surface : SDL.Video.Surfaces.Surface;
   begin
      Text_Surface := Font.Render_Solid (Text, Colour);

      SDL.Video.Textures.Makers.Create (Texture, Renderer, Text_Surface);

      Text_Surface.Finalize;
   end Load_From_Rendered_Text;

   procedure Load_Media is
   begin
      Load_From_Rendered_Text
        (Texture,
         "The quick brown fox jumps over the lazy dog.",
         (others => 0));
   end Load_Media;

   procedure Render (Renderer : in out SDL.Video.Renderers.Renderer;
                     Texture  : in out SDL.Video.Textures.Texture;
                     X        : SDL.Dimension;
                     Y        : SDL.Dimension) is
      Render_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                           (X,
                            Y,
                            Texture.Get_Size.Width,
                            Texture.Get_Size.Height);
   begin
      Renderer.Copy_To (Texture, Render_Rectangle);
   end Render;

   procedure Handle_Events is
      Finished : Boolean := False;
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
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         Render (Renderer,
                 Texture,
                 (Width - Texture.Get_Size.Width) / 2,
                 (Height - Texture.Get_Size.Height) / 2);

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

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial - True Type Fonts",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
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
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");

end True_Type_Fonts;
