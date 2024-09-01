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

procedure Rotation_And_Flipping is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Width  : constant := 640;
   Height : constant := 480;

   Window        : SDL.Video.Windows.Window;
   Renderer      : SDL.Video.Renderers.Renderer;
   Event         : SDL.Events.Events.Events;
   Arrow_Texture : SDL.Video.Textures.Texture;

   procedure Load_Media
     (Texture   : in out SDL.Video.Textures.Texture;
      Renderer  : SDL.Video.Renderers.Renderer;
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

   end Load_Media;

   procedure Render
     (Renderer         : in out SDL.Video.Renderers.Renderer;
      Texture          : in out SDL.Video.Textures.Texture;
      X                : SDL.Dimension;
      Y                : SDL.Dimension;
      Angle            : Long_Float;
      Flip_Type        : SDL.Video.Renderers.Renderer_Flip) is

      use type Interfaces.C.int;

      From_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        (0,
         0,
         Texture.Get_Size.Width,
         Texture.Get_Size.Height);
      To_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        (X,
         Y,
         Texture.Get_Size.Width,
         Texture.Get_Size.Height);
      Center_Point     : constant SDL.Video.Rectangles.Point :=
        (Texture.Get_Size.Width / 2,
         Texture.Get_Size.Height / 2);
   begin
      Renderer.Copy_to (Texture,
                     --  From_Rectangle,
                     To_Rectangle,
                     Angle,
                     Center_Point,
                     Flip_Type);

   end Render;

   procedure Handle_Events is
      Finished : Boolean := False;
      Angle    : Long_Float := 0.0;
      Flip_Type : SDL.Video.Renderers.Renderer_Flip := SDL.Video.Renderers.None;

      use type Interfaces.C.int;
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
                     when SDL.Events.Keyboards.Code_A =>
                        Angle := @ - 60.0;
                     when SDL.Events.Keyboards.Code_D =>
                        Angle := @ + 60.0;
                     when SDL.Events.Keyboards.Code_Q =>
                        Flip_Type := SDL.Video.Renderers.Horizontal;
                     when SDL.Events.Keyboards.Code_W =>
                        Flip_Type := SDL.Video.Renderers.None;
                     when SDL.Events.Keyboards.Code_E =>
                        Flip_Type := SDL.Video.Renderers.Vertical;
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((255, 255, 255, 255));
         Renderer.Clear;

         --  Render arrow
         Render (Renderer,
                         Arrow_Texture,
                         (Width - Arrow_Texture.Get_Size.Width) / 2,
                         (Height - Arrow_Texture.Get_Size.Height) / 2,
                         Angle,
                         Flip_Type);

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

   Load_Media (Arrow_Texture,
               Renderer,
               "../resources/arrow.png");

   Handle_Events;

   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end Rotation_And_Flipping;
