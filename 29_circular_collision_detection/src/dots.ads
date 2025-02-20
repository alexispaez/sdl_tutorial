with SDL.Events.Events;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Textures;

package Dots is

   package Events renames SDL.Events;
   package Rectangles renames SDL.Video.Rectangles;
   package Renderers renames SDL.Video.Renderers;
   package Textures renames SDL.Video.Textures;

   Dot_Width          : constant := 20;
   Dot_Height         : constant := 20;
   Velocity_Increment : constant := 1;

   type Dot is tagged limited private;
   type Circle_Collider is private;

   function Create (X, Y : SDL.Coordinate) return Dot;

   procedure Handle_Events (Self  : in out Dot;
                            Event : Events.Events.Events);

   procedure Move (Self          : in out Dot;
                   Screen_Size   : SDL.Positive_Sizes;
                   Rect_Collider : Rectangles.Rectangle;
                   Circ_Collider : Circle_Collider);

   function Get_Collider (Self : in out Dot) return Circle_Collider;

   procedure Render (Self        : in out Dot;
                     Renderer    : in out Renderers.Renderer;
                     Texture     : Textures.Texture;
                     Screen_Size : SDL.Positive_Sizes);
private

   type Circle_Collider is record
      X, Y : SDL.Coordinate := 0;
      R    : SDL.Dimension := 0;
   end record;

   type Dot is tagged limited record
      Pos_X, Pos_Y : SDL.Coordinate := 0;
      Vel_X, Vel_Y : SDL.Dimension := 0;
      Collider     : Circle_Collider;
   end record;

   procedure Shift_Colliders (Self : in out Dot);
end Dots;
