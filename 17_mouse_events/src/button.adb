with SDL.Events.Mice; use SDL.Events.Mice;
with SDL.Inputs.Mice;
with SDL.Video;
with Interfaces.C;

package body Button is

   procedure Set_Position (Self : in out Button;
                           X    : Integer;
                           Y    : Integer) is
   begin
      Self.X := X;
      Self.Y := Y;
   end Set_Position;

   procedure Set_Dimensions (Self   : in out Button;
                             Width  : Integer;
                             Height : Integer) is
   begin
      Self.Width := Width;
      Self.Height := Height;
   end Set_Dimensions;

   procedure Handle_Event (Self  : in out Button;
                           Event : SDL.Events.Events.Events) is
      Is_Inside : Boolean := True;
      X, Y      : SDL.Events.Mice.Movement_Values;
      Button_State : SDL.Events.Mice.Button_Masks;
   begin
      Button_State := SDL.Inputs.Mice.Get_State (X, Y);

      if Integer (X) < Self.X then
         --  Mouse is left of this button
         Is_Inside := False;
      elsif Integer (X) > Self.X + Self.Width then
         --  Mouse is to the right of this button
         Is_Inside := False;
      elsif Integer (Y) < Self.Y then
         --  Mouse if above this button
         Is_Inside := False;
      elsif Integer (Y) > Self.Y + Self.Height then
         --  Mouse is below this button
         Is_Inside := False;
      end if;
      --  Set sprite
      if Is_Inside = False then
         Self.Current_Sprite := Mouse_Out;
      else
         case Event.Common.Event_Type is
            when SDL.Events.Mice.Motion =>
               Self.Current_Sprite := Mouse_Over_Motion;
            when SDL.Events.Mice.Button_Down =>
               Self.Current_Sprite := Mouse_Down;
            when SDL.Events.Mice.Button_Up =>
               Self.Current_Sprite := Mouse_Up;
            when others => null;
         end case;
      end if;
   end Handle_Event;

   procedure Render (Self     : in out Button;
                     Renderer : in out SDL.Video.Renderers.Renderer;
                     Texture  : SDL.Video.Textures.Texture;
                     Clips    : Sprite_Clips_Array) is
      To_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                           (SDL.Coordinate (Self.X),
                            SDL.Coordinate (Self.Y),
                            SDL.Natural_Dimension (Self.Width),
                            SDL.Natural_Dimension (Self.Height));

      use type Interfaces.C.int;
      Center_Point : constant SDL.Video.Rectangles.Point :=
                       (Texture.Get_Size.Width / 2,
                        Texture.Get_Size.Height / 2);
   begin
      Renderer.Copy (Texture,
                     Clips (Self.Current_Sprite),
                     To_Rectangle,
                     0.0,
                     Center_Point,
                     SDL.Video.Renderers.None);
   end Render;

end Button;
