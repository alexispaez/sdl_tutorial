pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Hints;
with SDL.Images;
with SDL.Timers; use SDL.Timers;
with SDL.TTFs;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

procedure Timing is
   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   Window          : SDL.Video.Windows.Window;
   Renderer        : SDL.Video.Renderers.Renderer;
   Event           : SDL.Events.Events.Events;
   Prompt_Texture  : SDL.Video.Textures.Texture;
   Time_Texture    : SDL.Video.Textures.Texture;
   Font            : SDL.TTFs.Fonts;

   Start_Time : SDL.Timers.Milliseconds_Long := 0;

   function Initialise return Boolean is
      use SDL.Video.Renderers;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Timing",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
         Flags    => 0);

      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated or
           SDL.Video.Renderers.Present_V_Sync);

      Renderer.Set_Draw_Colour ((others => 255));

      if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
         return False;
      end if;

      if not SDL.TTFs.Initialise then
         return False;
      end if;

      return True;

   end Initialise;

   procedure Close is
   begin
      SDL.TTFs.Quit;
      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

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
      SDL.TTFs.Makers.Create (Font, "../resources//lazy.ttf", 28);

      Load_From_Rendered_Text
        (Prompt_Texture,
         "Press Enter to reset start time.",
         (Alpha => 255, others => 0));
   end Load_Media;

   procedure Render (Renderer : in out SDL.Video.Renderers.Renderer) is
      Milliseconds : constant SDL.Timers.Milliseconds_Long :=
        SDL.Timers.Ticks - Start_Time;
      Milliseconds_Text : constant String :=
        "Milliseconds since start time" & Milliseconds'Image;

      Prompt_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
        ((Screen_Width - Prompt_Texture.Get_Size.Width) / 2,
         0,
         Prompt_Texture.Get_Size.Width,
         Prompt_Texture.Get_Size.Height);
   begin
      Renderer.Copy_To (Prompt_Texture, Prompt_Rectangle);

      Load_From_Rendered_Text
        (Time_Texture,
         Milliseconds_Text,
         (Alpha => 255, others => 0));

      declare
         Time_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
           ((Screen_Width - Time_Texture.Get_Size.Width) / 2,
            (Screen_Height - Time_Texture.Get_Size.Height) / 2,
            Time_Texture.Get_Size.Width,
            Time_Texture.Get_Size.Height);
      begin
         Renderer.Copy_To (Time_Texture, Time_Rectangle);
      end;
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

                     when SDL.Events.Keyboards.Code_Return =>
                        Start_Time := SDL.Timers.Ticks;

                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render graphics
         Render (Renderer);

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

   Ada.Text_IO.Put_Line ("Process completed.");
exception
   when Event : others =>
      Ada.Text_IO.Put_Line ("Exception raised: " &
                              Ada.Exceptions.Exception_Name (Event));
      Ada.Text_IO.Put_Line ("Exception mesage: " &
                              Ada.Exceptions.Exception_Message (Event));
      Ada.Text_IO.Put_Line ("Full exception information:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Event));
      Ada.Text_IO.Put_Line ("Process not completed.");
end Timing;
