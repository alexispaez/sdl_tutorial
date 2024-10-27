pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Hints;
with SDL.Images;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures;
with SDL.Video.Windows.Makers;
with SDL.Video.Textures.Extensions;
with Dots;
with Interfaces.C;

procedure Scrolling is

   package Events renames SDL.Events;
   package Renderers renames SDL.Video.Renderers;
   package Textures renames SDL.Video.Textures;
   package Windows renames SDL.Video.Windows;

   use type Interfaces.C.int;

   Screen_Size : constant SDL.Positive_Sizes := (640, 480);
   Level_Size  : constant SDL.Positive_Sizes := (1280, 960);

   Window      : Windows.Window;
   Renderer    : Renderers.Renderer;
   Event       : Events.Events.Events;
   Dot_Texture : Textures.Texture;
   BG_Texture  : Textures.Texture;
   Dot         : Dots.Dot := Dots.Create (Dots.Dot_Width / 2,
                                          Dots.Dot_Height / 2);
   Camera      : SDL.Video.Rectangles.Rectangle;

   function Initialise return Boolean is
      use Renderers;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

      Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Scrolling",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => Screen_Size,
         Flags    => 0);

      Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => Renderers.Accelerated or Renderers.Present_V_Sync);

      Renderer.Set_Draw_Colour ((others => 255));

      if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
         return False;
      end if;

      return True;

   end Initialise;

   procedure Close is
   begin
      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

   procedure Load_Media is
   begin
      Textures.Extensions.Load_From_File (Dot_Texture,
                                          Renderer,
                                          "../resources/dot.bmp");
      Textures.Extensions.Load_From_File (BG_Texture,
                                          Renderer,
                                          "../resources/bg.png");
   end Load_Media;

   procedure Render_All (Renderer : in out Renderers.Renderer) is
   begin
      --  Draw the background
      Renderer.Copy_From (BG_Texture, Camera);

      --  Draw the dot
      Dot.Render (Renderer, Dot_Texture, Screen_Size);
   end Render_All;

   procedure Handle_Events is
      Finished : Boolean := False;
   begin

      Camera := (0, 0, Screen_Size.Width, Screen_Size.Height);

      loop
         while Events.Events.Poll (Event) loop

            case Event.Common.Event_Type is

               when Events.Quit =>
                  Finished := True;

               when Events.Keyboards.Key_Down =>

                  case Event.Keyboard.Key_Sym.Key_Code is
                     when Events.Keyboards.Code_Escape =>
                        Finished := True;

                     when others => null;
                  end case;
               when others => null;
            end case;

            --  Handle the event
            Dot.Handle_Events (Event);
         end loop;

         --  Move the dot
         Dot.Move (Level_Size);

         -- Center the camera over the dot

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render graphics
         Render_All (Renderer);

         --  Update the screen
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
      Put_Line ("Exception raised: " &
                  Ada.Exceptions.Exception_Name (Event));
      Put_Line ("Exception mesage: " &
                  Ada.Exceptions.Exception_Message (Event));
      Put_Line ("Full exception information:");
      Put_Line (Ada.Exceptions.Exception_Information (Event));
      Put_Line ("Process not completed.");
end Scrolling;
