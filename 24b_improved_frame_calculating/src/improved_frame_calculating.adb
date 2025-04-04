pragma Ada_2022;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
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
with Interval_Timers;
with Frame_Average_Counters;

procedure Improved_Frame_Calculating is

   package Renderers renames SDL.Video.Renderers;
   package Textures renames SDL.Video.Textures;

   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   Window       : SDL.Video.Windows.Window;
   Renderer     : Renderers.Renderer;
   Event        : SDL.Events.Events.Events;
   FPS_Texture  : Textures.Texture;
   Font         : SDL.TTFs.Fonts;

   function Initialise return Boolean is
      use Renderers;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Improved Frame Rate Calculation",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
         Flags    => 0);

      Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => Renderers.Accelerated or
           Renderers.Present_V_Sync);

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

   procedure Load_From_Rendered_Text (Texture : in out Textures.Texture;
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
   end Load_Media;

   procedure Render_All (Renderer : in out Renderers.Renderer;
                         FPS      : Float) is

      procedure Render (Renderer : in out Renderers.Renderer;
                        X        : SDL.Coordinate;
                        Y        : SDL.Coordinate;
                        Texture  : Textures.Texture) is
         Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                       (X,
                        Y,
                        Texture.Get_Size.Width,
                        Texture.Get_Size.Height);
      begin
         Renderer.Copy_To (Texture, Rectangle);
      end Render;

      --  Format a float value as a number with decimals
      function Format (Value : Float) return String is
         package FIO is new Ada.Text_IO.Float_IO (Float);
         package AS renames Ada.Strings;

         Result : String (1 .. 20);
      begin
         FIO.Put (To => Result, Item => Value, Aft => 2, Exp => 0);

         return AS.Fixed.Trim (Result, Side => AS.Both);
      end Format;

      FPS_Text : constant String :=
        "Average Frames Per Second " & Format (FPS);
   begin
      --  Render the time
      Load_From_Rendered_Text
        (FPS_Texture,
         FPS_Text,
         (Alpha => 255, others => 0));

      Render (Renderer,
              (Screen_Width - FPS_Texture.Get_Size.Width) / 2,
              (Screen_Height - FPS_Texture.Get_Size.Height) / 2,
              FPS_Texture);
   end Render_All;

   procedure Handle_Events is
      Finished        : Boolean := False;
      Interval_Timer  : Interval_Timers.Interval_Timer (Interval => 100);
      --  Taking 10 samples of 100ms each averages over one second
      --  FPS_Counter     : Frame_Average_Counters.Frame_Average_Counter (Samples => 10);
      --  Taking 30 samples of 100ms each averages over three seconds
      FPS_Counter     : Frame_Average_Counters.Frame_Average_Counter (Samples => 30);
   begin

      Interval_Timer.Start;

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

         --  Check if an interval has passed
         declare
            Elapsed_Ticks : SDL.Timers.Milliseconds_Long := 0;
            FPS_Average   : Float := 0.0;
         begin
            if Interval_Timer.Has_Elapsed (Elapsed_Ticks) then
               FPS_Counter.Add_Sample (Elapsed_Ticks);
            end if;

            --  The average will be in frames per millisecond
            --  so multiply by 1000 to get it in seconds
            FPS_Average := FPS_Counter.Get_Frame_Average * 1000.0;

            --  Render graphics
            Render_All (Renderer, FPS_Average);
         end;

         --  Update the screen
         Renderer.Present;

         --  Increment frame count by one
         FPS_Counter.Add_Frame;

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
end Improved_Frame_Calculating;
