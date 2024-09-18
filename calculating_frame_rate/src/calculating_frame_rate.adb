pragma Ada_2022;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Hints;
with SDL.Images;
with SDL.Timers;
with SDL.TTFs;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with Timers;

procedure Calculating_Frame_Rate is
   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   Window                : SDL.Video.Windows.Window;
   Renderer              : SDL.Video.Renderers.Renderer;
   Event                 : SDL.Events.Events.Events;
   Start_Prompt_Texture  : SDL.Video.Textures.Texture;
   Pause_Prompt_Texture  : SDL.Video.Textures.Texture;
   Time_Texture          : SDL.Video.Textures.Texture;
   Font                  : SDL.TTFs.Fonts;

   Timer : Timers.Timer;

   function Initialise return Boolean is
      use SDL.Video.Renderers;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Advanced Timers",
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
        (Start_Prompt_Texture,
         "Press S to start or stop the timer",
         (Alpha => 255, others => 0));
      Load_From_Rendered_Text
        (Pause_Prompt_Texture,
         "Press P to pause or unpause the timer",
         (Alpha => 255, others => 0));
   end Load_Media;

   procedure Render_All (Renderer : in out SDL.Video.Renderers.Renderer;
                         Time     : SDL.Timers.Milliseconds_Long) is

      procedure Render (Renderer : in out SDL.Video.Renderers.Renderer;
                        X        : SDL.Coordinate;
                        Y        : SDL.Coordinate;
                        Texture  : SDL.Video.Textures.Texture) is
         Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                       (X,
                        Y,
                        Texture.Get_Size.Width,
                        Texture.Get_Size.Height);
      begin
         Renderer.Copy_To (Texture, Rectangle);
      end Render;

      --  Format a float value as number with decimals
      function Format (Value : Float) return String is
         package FIO is new Ada.Text_IO.Float_IO (Float);
         package AS renames Ada.Strings;

         Result : String (1 .. 20);
      begin
         FIO.Put (To => Result, Item => Value, Aft => 2, Exp => 0);

         return AS.Fixed.Trim (Result, Side => AS.Both);
      end Format;

      Milliseconds_Text : constant String :=
        "Seconds since start time " & Format (Float (Time) / 1000.0);
   begin
      --  Render the prompts
      Render (Renderer,
              (Screen_Width - Start_Prompt_Texture.Get_Size.Width) / 2,
              0,
              Start_Prompt_Texture);

      Render (Renderer,
              (Screen_Width - Pause_Prompt_Texture.Get_Size.Width) / 2,
              Start_Prompt_Texture.Get_Size.Height,
              Pause_Prompt_Texture);

      --  Render the time
      Load_From_Rendered_Text
        (Time_Texture,
         Milliseconds_Text,
         (Alpha => 255, others => 0));

      Render (Renderer,
              (Screen_Width - Time_Texture.Get_Size.Width) / 2,
              (Screen_Height - Time_Texture.Get_Size.Height) / 2,
              Time_Texture);
   end Render_All;

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

                     when SDL.Events.Keyboards.Code_S =>
                        if Timer.Is_Started then
                           Timer.Stop;
                        else
                           Timer.Start;
                        end if;

                     when SDL.Events.Keyboards.Code_P =>
                        if Timer.Is_Paused then
                           Timer.Unpause;
                        else
                           Timer.Pause;
                        end if;

                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render graphics
         Render_All (Renderer, Timer.Get_Ticks);

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
end Calculating_Frame_Rate;
