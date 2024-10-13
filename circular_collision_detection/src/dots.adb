with Interfaces;
with SDL.Events.Keyboards;
with SDL.Video.Textures.Extensions;

package body Dots is

   function Create (X, Y : Integer) return Dot is
   begin
      return D : Dot do
         --  Set position
         D.Pos_X := X;
         D.Pos_Y := Y;
         --  Set velocity
         D.Vel_X := 0;
         D.Vel_Y := 0;
         --  Set circle collider
         D.Collider := (R => Dot_Width / 2, others => 0);
         --  Set collider positions relative to position
         Shift_Colliders (D);
      end return;
   end Create;

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

   procedure Move (Self          : in out Dot;
                   Screen_Size   : SDL.Positive_Sizes;
                   Rect_Collider : Rectangles.Rectangle;
                   Circ_Collider      : Circle_Collider) is
      --  Check for collisions between rectangles
      function Check_Collision
        (A_Collider : Rectangles.Rectangle;
         B_Collider : Rectangles.Rectangle) return Boolean is

         Left_A   : constant Integer := Integer (A_Collider.X);
         Right_A  : constant Integer := Integer (A_Collider.X) + Integer (A_Collider.Width);
         Top_A    : constant Integer := Integer (A_Collider.Y);
         Bottom_A : constant Integer := Integer (A_Collider.Y) + Integer (A_Collider.Height);

         Left_B   : constant Integer := Integer (B_Collider.X);
         Right_B  : constant Integer := Integer (B_Collider.X) + Integer (B_Collider.Width);
         Top_B    : constant Integer := Integer (B_Collider.Y);
         Bottom_B : constant Integer := Integer (B_Collider.Y) + Integer (B_Collider.Height);
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
      --  Check for collisions between circles
      function Check_Collision (Circle_A : Circle_Collider;
                                Circle_B : Circle_Collider) return Boolean is
         function Distance_Squared (X1 : Integer;
                                    Y1 : Integer;
                                    X2 : Integer;
                                    Y2 : Integer) return Integer is
            Delta_X : constant Integer := X2 - X1;
            Delta_Y : constant Integer := Y2 - Y1;
         begin
            return Delta_X * Delta_X + Delta_Y * Delta_Y;
         end Distance_Squared;

         Total_Radius         : constant Integer := Circle_A.R + Circle_B.R;
         Total_Radius_Squared : constant Integer := Total_Radius * Total_Radius;
         Distance             : constant Integer := Distance_Squared
           (Circle_A.X,
            Circle_A.Y,
            Circle_B.X,
            Circle_B.Y);
      begin
         if Distance < Total_Radius_Squared then
            return True;
         else
            return False;
         end if;
      end Check_Collision;
      --  Check for collisions between circle and rectangle
      function Check_Collision (Circle    : Circle_Collider;
                                Rectangle : Rectangles.Rectangle) return Boolean is
      begin
         return False;
      end Check_Collision;
   begin
      --  Move the dot left or right
      Self.Pos_X := @ + Self.Vel_X;
      Shift_Colliders (Self);

      --  Check if the dot went too far left or right or collided
      if Self.Pos_X < 0 or else
        (Self.Pos_X + Dot_Width) > Integer (Screen_Size.Width) or else
        Check_Collision (Self.Collider, Rect_Collider) or else
        Check_Collision (Self.Collider, Circ_Collider)
      then
         --  Move back one step
         Self.Pos_X := @ - Self.Vel_X;
         Shift_Colliders (Self);
      end if;

      --  Move the dot up or down
      Self.Pos_Y := @ + Self.Vel_Y;
      Shift_Colliders (Self);

      --  Check if the dot went too far up or down or collided
      if Self.Pos_Y < 0 or else
        (Self.Pos_Y + Dot_Height) > Integer (Screen_Size.Height) or else
        Check_Collision (Self.Collider, Rect_Collider) or else
        Check_Collision (Self.Collider, Circ_Collider)
      then
         --  Move back one step
         Self.Pos_Y := @ - Self.Vel_Y;
         Shift_Colliders (Self);
      end if;
   end Move;

   function Get_Collider (Self : in out Dot) return Circle_Collider is
   begin
      return Self.Collider;
   end Get_Collider;

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

   procedure Shift_Colliders (Self : in out Dot) is
   begin
      Self.Collider.X := Self.Pos_X;
      Self.Collider.Y := Self.Pos_Y;
   end Shift_Colliders;

end Dots;
