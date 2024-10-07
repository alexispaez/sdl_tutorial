with Interfaces.C;
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
         --  Add a collider for each rectangle the dot is divided into
         D.Colliders.Append (Collider'(Width => 6, Height => 1, others => 0));
         D.Colliders.Append (Collider'(Width => 10, Height => 1, others => 0));
         D.Colliders.Append (Collider'(Width => 14, Height => 1, others => 0));
         D.Colliders.Append (Collider'(Width => 16, Height => 2, others => 0));
         D.Colliders.Append (Collider'(Width => 18, Height => 2, others => 0));
         D.Colliders.Append (Collider'(Width => 20, Height => 6, others => 0));
         D.Colliders.Append (Collider'(Width => 18, Height => 2, others => 0));
         D.Colliders.Append (Collider'(Width => 16, Height => 2, others => 0));
         D.Colliders.Append (Collider'(Width => 14, Height => 1, others => 0));
         D.Colliders.Append (Collider'(Width => 10, Height => 1, others => 0));
         D.Colliders.Append (Collider'(Width => 6, Height => 1, others => 0));
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

   procedure Move (Self            : in out Dot;
                   Screen_Size     : SDL.Positive_Sizes;
                   Other_Colliders : Collider_Vectors.Vector) is
      function Check_Collision
        (A_Colliders : Collider_Vectors.Vector;
         B_Colliders : Collider_Vectors.Vector) return Boolean is
         function Check_Collision
           (A_Collider : Collider;
            B_Collider : Collider) return Boolean is

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
      begin
         for A_Collider of A_Colliders loop
            for B_Collider of B_Colliders loop
               if Check_Collision (A_Collider, B_Collider) then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Check_Collision;

   begin
      --  Move the dot left or right
      Self.Pos_X := @ + Self.Vel_X;
      Shift_Colliders (Self);

      --  Check if the dot went too far left or right or collided
      if Self.Pos_X < 0 or else
        (Self.Pos_X + Dot_Width) > Integer (Screen_Size.Width) or else
          Check_Collision (Self.Colliders, Other_Colliders)
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
        Check_Collision (Self.Colliders, Other_Colliders)
      then
         --  Move back one step
         Self.Pos_Y := @ - Self.Vel_Y;
         Shift_Colliders (Self);
      end if;
   end Move;

   function Get_Colliders (Self : in out Dot) return Collider_Vectors.Vector is
   begin
      return Self.Colliders;
   end Get_Colliders;

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
      Row_Offset : Integer := 0;

      use type Interfaces.C.int;
   begin
      for C of Self.Colliders loop
         --  Center collision box
         C.X := SDL.Coordinate (Self.Pos_X + Integer (Dot_Width - C.Width) / 2);
         --  Set collision box row offset
         C.Y := SDL.Coordinate (Self.Pos_Y + Row_Offset);
         --  Move the row offset down the height of the collision box
         Row_Offset := @ + Integer (C.Height);
      end loop;
   end Shift_Colliders;

end Dots;
