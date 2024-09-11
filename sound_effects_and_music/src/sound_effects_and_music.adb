pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Hints;
with SDL.Images;
with SDL.Images.IO;
with SDL.Mixer;
with SDL.Mixer.Music;
use SDL.Mixer.Music;
with SDL.Video.Rectangles;
with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
use SDL;
with Interfaces.C; use Interfaces.C;

procedure Sound_Effects_And_Music is

   Width  : constant := 640;
   Height : constant := 480;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;

   Texture  : SDL.Video.Textures.Texture;
   Music    : SDL.Mixer.Music.Music_Type;
   --  Music : SDL.Mixer.Music.;

   procedure Load_Media is

      package UTF_Strings renames Ada.Strings.UTF_Encoding;

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
   begin
      Load_Texture (Texture,
                    Renderer,
                    "../resources/prompt.png");
      SDL.Mixer.Music.Load ("../resources/beat.wav", Music);
   end Load_Media;

   procedure Render
     (Renderer         : in out SDL.Video.Renderers.Renderer;
      Texture          : in out SDL.Video.Textures.Texture;
      X                : SDL.Dimension;
      Y                : SDL.Dimension;
      Angle            : Long_Float;
      Flip_Type        : SDL.Video.Renderers.Renderer_Flip) is

      To_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        (X,
         Y,
         Texture.Get_Size.Width,
         Texture.Get_Size.Height);
      Center_Point     : constant SDL.Video.Rectangles.Point :=
        (Texture.Get_Size.Width / 2,
         Texture.Get_Size.Height / 2);
   begin
      Renderer.Copy_To (Texture,
                     To_Rectangle,
                     Angle,
                     Center_Point,
                     Flip_Type);

   end Render;

   procedure Free_Media is
   begin
      null;
   end Free_Media;

   procedure Handle_Events is
      Finished : Boolean := False;

      Angle     : Long_Float := 0.0;
      Flip_Type : constant SDL.Video.Renderers.Renderer_Flip := SDL.Video.Renderers.None;
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
                     when SDL.Events.Keyboards.Code_9 =>
                        SDL.Mixer.Music.Play (Music, -1);
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((255, 255, 255, 255));
         Renderer.Clear;

         --  Render arrow
         Angle := 0.0;

         Render (Renderer,
                 Texture,
                 (Width - Texture.Get_Size.Width) / 2,
                 (Height - Texture.Get_Size.Height) / 2,
                 Angle,
                 Flip_Type);

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not SDL.Initialise (Flags =>
                            SDL.Enable_Screen or
                              SDL.Enable_Audio)
   then
      return;
   end if;

   SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial - Sound effects and Music",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => SDL.Video.Windows.Shown);

   SDL.Video.Renderers.Makers.Create
     (Window => Window,
      Rend   => Renderer,
      Flags  => SDL.Video.Renderers.Accelerated or
        SDL.Video.Renderers.Present_V_Sync);

   Renderer.Set_Draw_Colour ((others => 255));

   if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
      return;
   end if;

   SDL.Mixer.Open (SDL.Mixer.Default_Frequency,
                   SDL.Mixer.Default_Format,
                   2,
                   2048);

   Load_Media;

   Handle_Events;

   Free_Media;
   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process complete.");
end Sound_Effects_And_Music;
