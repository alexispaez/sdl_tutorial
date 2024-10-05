with Ada.Containers.Vectors;
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

   type Dot is tagged private;
   type Collider is new Rectangles.Rectangle;

   package Collider_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Collider);

   function Create (X, Y : Integer) return Dot;

   procedure Handle_Events (Self  : in out Dot;
                            Event : Events.Events.Events);

   procedure Move (Self        : in out Dot;
                   Screen_Size : SDL.Positive_Sizes;
                   Wall        : Rectangles.Rectangle);

   procedure Render (Self        : in out Dot;
                     Renderer    : in out Renderers.Renderer;
                     Texture     : Textures.Texture;
                     Screen_Size : SDL.Positive_Sizes);
private

   type Dot is tagged record
      Pos_X, Pos_Y : Integer := 0;
      Vel_X, Vel_Y : Integer := 0;
      Colliders    : Collider_Vectors.Vector;
   end record;

end Dots;
