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

procedure Capping_Frame_Rate is

   package Events renames SDL.Events;
   package Palettes renames SDL.Video.Palettes;
   package Rectangles renames SDL.Video.Rectangles;
   package Renderers renames SDL.Video.Renderers;
   package Surfaces renames SDL.Video.Surfaces;
   package Textures renames SDL.Video.Textures;
   package Windows renames SDL.Video.Windows;

   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   Window       : Windows.Window;
   Renderer     : Renderers.Renderer;
   Event        : Events.Events.Events;
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
         Title    => "SDL Tutorial - Capping Frame Rate",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Screen_Width, Screen_Height),
         Flags    => 0);

      Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => Renderers.Accelerated); --  No vsync in this case

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
                                      Colour  : Palettes.Colour) is
      Text_Surface : Surfaces.Surface;
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
         Rectangle : constant Rectangles.Rectangle :=
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
        "Average Frames Per Second (with cap) " & Format (FPS);
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
      Screen_FPS_Target      : constant := 60;
      Ticks_Per_Frame_Target : constant := 1000 / Screen_FPS_Target;

      Finished       : Boolean := False;
      Frames_Counted : Natural := Natural'First;

      FPS_Timer      : Timers.Timer;
      Cap_Timer      : Timers.Timer;
   begin

      FPS_Timer.Start;

      loop

         Cap_Timer.Start;

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
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Calculate frames per second
         declare
            --  Calculate frame average
            Ticks : constant SDL.Timers.Milliseconds_Long :=
                      FPS_Timer.Get_Ticks;
            FPS_Average : Float;

            use SDL.Timers;
         begin
            if Ticks /= 0 then
               FPS_Average := Float (Frames_Counted) /
                 (Float (Ticks) / 1000.0);
            end if;
            --  Render graphics
            Render_All (Renderer, FPS_Average);
         end;

         --  Update the screen
         Renderer.Present;

         --  Count one frame
         Frames_Counted := @ + 1;

         --  Check if the frame finished early
         declare
            Frame_Ticks : constant SDL.Timers.Milliseconds_Long :=
                            Cap_Timer.Get_Ticks;
            use SDL.Timers;
         begin
            if Frame_Ticks < Ticks_Per_Frame_Target then
               SDL.Timers.Wait_Delay (Ticks_Per_Frame_Target - Frame_Ticks);
            end if;
         end;

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
end Capping_Frame_Rate;
