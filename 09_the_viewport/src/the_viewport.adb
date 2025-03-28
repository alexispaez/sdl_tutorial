pragma Ada_2022;

with Ada.Exceptions;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO; use Ada.Text_IO;
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
with Interfaces.C; use Interfaces.C;

procedure The_Viewport is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Width  : constant := 640;
   Height : constant := 480;

   Window         : SDL.Video.Windows.Window;
   Event          : SDL.Events.Events.Events;
   Texture        : SDL.Video.Textures.Texture;
   Renderer       : SDL.Video.Renderers.Renderer;

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
         Title    => "SDL Tutorial - The Viewport",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);
      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated);

      Renderer.Set_Draw_Colour ((others => 255));

      return True;

   end Initialise;

   procedure Load_Texture
     (Texture   : in out SDL.Video.Textures.Texture;
      Renderer  : SDL.Video.Renderers.Renderer;
      File_Name : UTF_Strings.UTF_String) is
      Loaded_Surface    : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Loaded_Surface, File_Name);
      SDL.Video.Textures.Makers.Create
        (Texture,
         Renderer,
         Loaded_Surface);

      Loaded_Surface.Finalize;
   end Load_Texture;

   procedure Load_Media is
   begin
      Load_Texture (Texture, Renderer, "../resources/viewport.png");
   end Load_Media;

   procedure Free_Media is
   begin
      Texture.Finalize;
   end Free_Media;

   procedure Close is
   begin
      Free_Media;

      Renderer.Finalize;
      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

   procedure Handle_Events is
      Finished : Boolean := False;

      Top_Left_Viewport : constant SDL.Video.Rectangles.Rectangle :=
        (0, 0, Width / 2, Height / 2);
      Top_Right_Viewport : constant SDL.Video.Rectangles.Rectangle :=
        (Width / 2, 0, Width / 2,  Height / 2);
      Bottom_Viewport : constant SDL.Video.Rectangles.Rectangle :=
        (0, Height / 2, Width,  Height / 2);
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

         --  Top left corner viewport
         Renderer.Set_Viewport (Top_Left_Viewport);
         Renderer.Copy (Texture);

         --  Top right corner viewport
         Renderer.Set_Viewport (Top_Right_Viewport);
         Renderer.Copy (Texture);

         --  Bottom viewport
         Renderer.Set_Viewport (Bottom_Viewport);
         Renderer.Copy (Texture);

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
end The_Viewport;
