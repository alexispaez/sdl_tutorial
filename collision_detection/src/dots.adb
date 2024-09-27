with Interfaces;
with SDL.Events.Keyboards;
with SDL.Video.Textures.Extensions;

package body Dots is

   procedure Handle_Events (Self  : in out Dot;
                            Event : Events.Events.Events) is
      use type SDL.Events.Event_Types;
      use type Interfaces.Unsigned_8;
   begin
      if Event.Common.Event_Type = SDL.Events.Keyboards.Key_Down and then
        Event.Keyboard.Repeat = 0
      then
         --  A key was pressed, increase the velocity
         case Event.Keyboard.Key_Sym.Key_Code is
            when SDL.Events.Keyboards.Code_Up
               => Self.Vel_Y := @ - Velocity_Increment;
               when SDL.Events.Keyboards.Code_Down
               => Self.Vel_Y := @ + Velocity_Increment;
               when SDL.Events.Keyboards.Code_Left
               => Self.Vel_X := @ - Velocity_Increment;
               when SDL.Events.Keyboards.Code_Right
               => Self.Vel_X := @ + Velocity_Increment;
            when others => null;
         end case;

      elsif Event.Common.Event_Type = SDL.Events.Keyboards.Key_Up and then
        Event.Keyboard.Repeat = 0
      then
         --  A key was released, undo the velocity change
         case Event.Keyboard.Key_Sym.Key_Code is
            when SDL.Events.Keyboards.Code_Up
               => Self.Vel_Y := @ + Velocity_Increment;
               when SDL.Events.Keyboards.Code_Down
               => Self.Vel_Y := @ - Velocity_Increment;
               when SDL.Events.Keyboards.Code_Left
               => Self.Vel_X := @ + Velocity_Increment;
               when SDL.Events.Keyboards.Code_Right
               => Self.Vel_X := @ - Velocity_Increment;
            when others => null;
         end case;

      end if;
   end Handle_Events;

   procedure Move (Self        : in out Dot;
                   Screen_Size : SDL.Positive_Sizes;
                   Wall        : Rectangles.Rectangle) is
      procedure Collision_Detection (Rect_A : Rectangles.Rectangle;
                                     Rect_B : Rectangles.Rectangle) is
      begin
         null;
      end Collision_Detection;

   begin
      --  Move the dot left or right
      Self.Pos_X := @ + Self.Vel_X;

      if Self.Pos_X < 0 or else
        (Self.Pos_X + Dot_Width) > Integer (Screen_Size.Width)
      then
         --  Reached the right edge of the screen, move back one step
         Self.Pos_X := @ - Self.Vel_X;
      end if;

      --  Move the dot up or down
      Self.Pos_Y := @ + Self.Vel_Y;

      if Self.Pos_Y < 0 or else
        (Self.Pos_Y + Dot_Height) > Integer (Screen_Size.Height)
      then
         --  Reached the bottom edge of the screen, move back one step
         Self.Pos_Y := @ - Self.Vel_Y;
      end if;
   end Move;

   procedure Render (Self        : in out Dot;
                     Renderer    : in out Renderers.Renderer;
                     Texture     : Textures.Texture;
                     Screen_Size : SDL.Positive_Sizes) is
   begin
      SDL.Video.Textures.Extensions.Render (Texture,
                                            Renderer,
                                            SDL.Coordinate (Self.Pos_X),
                                            SDL.Coordinate (Self.Pos_Y));
   end Render;

end Dots;
