pragma Ada_2022;

with Ada.Exceptions;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with SDL; use SDL;
with SDL.Events.Events;
with SDL.Events.Joysticks; use SDL.Events.Joysticks;
with SDL.Events.Keyboards;
with SDL.Hints;
with SDL.Images;
with SDL.Images.IO;
with SDL.Inputs.Joysticks; use SDL.Inputs.Joysticks;
with SDL.Inputs.Joysticks.Game_Controllers;
with SDL.Inputs.Joysticks.Game_Controllers.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with Interfaces.C;

procedure Force_Feedback is

   package TIO renames Ada.Text_IO;
   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Width  : constant := 640;
   Height : constant := 480;

   Window            : SDL.Video.Windows.Window;
   Renderer          : SDL.Video.Renderers.Renderer;
   Event             : SDL.Events.Events.Events;
   Splash_Texture    : SDL.Video.Textures.Texture;
   Joystick_Instance : SDL.Inputs.Joysticks.Instances;
   Game_Controller   : SDL.Inputs.Joysticks.Game_Controllers.Game_Controller;

   function Initialise return Boolean is
   begin

      if not SDL.Initialise (Flags => SDL.Enable_Screen
                             or SDL.Enable_Joystick
                             or SDL.Enable_Haptic
                             or SDL.Enable_Game_Controller)
      then
         return False;
      end if;

      SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

      --  Check for the presence of joysticks
      if SDL.Inputs.Joysticks.Total < 1 then
         TIO.Put_Line ("No joysticks connected! Exiting.");

         return False;
      else
         TIO.Put_Line ("Found " &
                     SDL.Inputs.Joysticks.Total'Image &
                     " joysticks.");
      end if;

      --  Check if the first joystick is game controller compatible
      if SDL.Inputs.Joysticks.Game_Controllers.Is_Game_Controller (1) = True then

         TIO.Put_Line ("Joystick is game controller interface compatible.");

         --  Open the game controller
         SDL.Inputs.Joysticks.Game_Controllers.Makers.Create (1, Game_Controller);
         declare
            --  Get the joystick to then get the joystick instance
            --  which is needed for the axis events
            Joystick : constant SDL.Inputs.Joysticks.Joystick :=
              SDL.Inputs.Joysticks.Game_Controllers.Get_Joystick (Game_Controller);
         begin
            Joystick_Instance := SDL.Inputs.Joysticks.Instance (Joystick);

            TIO.Put_Line ("Joystick has" &
                            SDL.Inputs.Joysticks.Axes (Joystick)'Image &
                            " axes.");
         end;

         --  Check if it supports rumble
         if SDL.Inputs.Joysticks.Game_Controllers.Has_Rumble (Game_Controller) then
            TIO.Put_Line ("Joystick supports rumble.");
         else
            TIO.Put_Line ("Joystick does not support rumble.");
         end if;
      end if;

      if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Force Feedback",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => SDL.Video.Windows.Shown);

      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated or
           SDL.Video.Renderers.Present_V_Sync);

      Renderer.Set_Draw_Colour ((others => 255));

      return True;

   end Initialise;

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

   procedure Free_Media is
   begin
      Splash_Texture.Finalize;
   end Free_Media;

   procedure Close is
   begin
      Free_Media;

      SDL.Inputs.Joysticks.Game_Controllers.Close (Game_Controller);
      Window.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end Close;

   procedure Render
     (Renderer         : in out SDL.Video.Renderers.Renderer;
      Texture          : in out SDL.Video.Textures.Texture;
      X                : SDL.Dimension;
      Y                : SDL.Dimension;
      Angle            : Long_Float;
      Flip_Type        : SDL.Video.Renderers.Renderer_Flip) is

      use type Interfaces.C.int;

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

   procedure Handle_Events is

      Joystick_Dead_Zone : constant := 8000;

      Finished : Boolean := False;

      X_Dir : Integer := 0;
      Y_Dir : Integer := 0;

      Angle    : Long_Float := 0.0;
      Flip_Type : constant SDL.Video.Renderers.Renderer_Flip :=
                    SDL.Video.Renderers.None;

      use type Interfaces.C.int;
      use type Interfaces.Unsigned_16;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;

               when SDL.Events.Joysticks.Button_Down =>

                  if SDL.Inputs.Joysticks.Game_Controllers.Rumble
                    (Game_Controller,
                     16#FFFF# * 3 / 4,
                     16#FFFF# * 3 / 4,
                     500) = -1
                  then
                     TIO.Put_Line ("Could not rumble");
                  end if;

               when SDL.Events.Joysticks.Axis_Motion =>

                  if Event.Joystick_Axis.Which = IDs (Joystick_Instance) then
                     --  Motion on the joystick, check axes 0 and 1 only
                     if Event.Joystick_Axis.Axis = 0 then
                        --  X axis motion
                        if Event.Joystick_Axis.Value < -Joystick_Dead_Zone then
                           --  Left of the dead zone
                           X_Dir := -1;
                        elsif Event.Joystick_Axis.Value > Joystick_Dead_Zone then
                           --  Right of the dead zone
                           X_Dir := 1;
                        else
                           X_Dir := 0;
                        end if;
                     elsif Event.Joystick_Axis.Axis = 1 then
                        --  Y axis motion
                        if Event.Joystick_Axis.Value < -Joystick_Dead_Zone then
                           --  Below the dead zone
                           Y_Dir := -1;
                        elsif Event.Joystick_Axis.Value > Joystick_Dead_Zone then
                           --  Above the dead zone
                           Y_Dir := 1;
                        else
                           Y_Dir := 0;
                        end if;
                     end if;
                  end if;

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
         Renderer.Set_Draw_Colour ((255, 255, 255, 255));
         Renderer.Clear;

         --  Render arrow
         if Y_Dir /= 0 or else X_Dir /= 0 then
            Angle := Arctan (Long_Float (Y_Dir), Long_Float (X_Dir)) *
              (180.0 / Pi);
         else
            Angle := 0.0;
         end if;

         Render (Renderer,
                         Splash_Texture,
                         (Width - Splash_Texture.Get_Size.Width) / 2,
                         (Height - Splash_Texture.Get_Size.Height) / 2,
                         Angle,
                         Flip_Type);

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Force_Feedback.Initialise then
      return;
   end if;

   Load_Media (Splash_Texture, Renderer, "../resources/splash.png");

   Handle_Events;

   Close;

   TIO.Put_Line ("Process completed.");
exception
   when Event : others =>
      TIO.Put_Line ("Process not completed.");
      TIO.Put_Line ("Exception raised: " &
                      Ada.Exceptions.Exception_Name (Event));
      TIO.Put_Line ("Exception mesage: " &
                      Ada.Exceptions.Exception_Message (Event));
end Force_Feedback;
