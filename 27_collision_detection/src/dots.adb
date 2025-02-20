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
      function Check_Collision
        (Rect_A : Rectangles.Rectangle;
         Rect_B : Rectangles.Rectangle) return Boolean is

         Left_A   : constant Integer := Integer (Rect_A.X);
         Right_A  : constant Integer := Integer (Rect_A.X) + Integer (Rect_A.Width);
         Top_A    : constant Integer := Integer (Rect_A.Y);
         Bottom_A : constant Integer := Integer (Rect_A.Y) + Integer (Rect_A.Height);

         Left_B   : constant Integer := Integer (Rect_B.X);
         Right_B  : constant Integer := Integer (Rect_B.X) + Integer (Rect_B.Width);
         Top_B    : constant Integer := Integer (Rect_B.Y);
         Bottom_B : constant Integer := Integer (Rect_B.Y) + Integer (Rect_B.Height);
      begin
         if Bottom_A <= Top_B then
            return False;
         end if;

         if Top_A >= Bottom_B then
            return False;
         end if;

         if Right_A <= Left_B then
            return False;
         end if;

         if Left_A >= Right_B then
            return False;
         end if;

         return True;
      end Check_Collision;

   begin
      --  Move the dot left or right
      Self.Pos_X := @ + Self.Vel_X;
      Self.Collider.X := SDL.Coordinate (Self.Pos_X);

      --  Check if the dot went too far left or right or collided
      if Self.Pos_X < 0 or else
        (Self.Pos_X + Dot_Width) > Integer (Screen_Size.Width) or else
        Check_Collision (Self.Collider, Wall)
      then
         --  Move back one step
         Self.Pos_X := @ - Self.Vel_X;
         Self.Collider.X := SDL.Coordinate (Self.Pos_X);
      end if;

      --  Move the dot up or down
      Self.Pos_Y := @ + Self.Vel_Y;
      Self.Collider.Y := SDL.Coordinate (Self.Pos_Y);

      --  Check if the dot went too far up or down or collided
      if Self.Pos_Y < 0 or else
        (Self.Pos_Y + Dot_Height) > Integer (Screen_Size.Height) or else
        Check_Collision (Self.Collider, Wall)
      then
         --  Move back one step
         Self.Pos_Y := @ - Self.Vel_Y;
         Self.Collider.Y := SDL.Coordinate (Self.Pos_Y);
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
