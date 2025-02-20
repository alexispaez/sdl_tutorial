with SDL.Events.Events;
with SDL.Video.Renderers;
with SDL.Video.Textures;

package Dots is

   Dot_Width  : constant := 20;
   Dot_Height : constant := 20;

   Velocity_Increment : constant := 10;

   type Dot is tagged private;

   procedure Handle_Events (Self  : in out Dot;
                            Event : SDL.Events.Events.Events);

   procedure Move (Self        : in out Dot;
                   Screen_Size : SDL.Positive_Sizes);

   procedure Render (Self        : in out Dot;
                     Renderer    : in out SDL.Video.Renderers.Renderer;
                     Texture     : SDL.Video.Textures.Texture;
                     Screen_Size : SDL.Positive_Sizes);
private

   type Dot is tagged record
      Pos_X, Pos_Y : Integer := 0;
      Vel_X, Vel_Y : Integer := 0;
   end record;
end Dots;
