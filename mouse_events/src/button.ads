with SDL.Events.Events;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Textures;

package Button is

   type Button_Sprite is
     (Mouse_Out,
      Mouse_Over_Motion,
      Mouse_Down,
      Mouse_Up);

   type Sprite_Clips_Array is array (Button_Sprite'Range) of
     SDL.Video.Rectangles.Rectangle;

   type Button is tagged private;

   procedure Set_Position (Self : in out Button;
                           X    : Integer;
                           Y    : Integer);

   procedure Set_Dimensions (Self  : in out Button;
                             Width  : Integer;
                             Height : Integer);

   procedure Handle_Event (Self  : in out Button;
                           Event : SDL.Events.Events.Events);

   procedure Render (Self     : in out Button;
                     Renderer : in out SDL.Video.Renderers.Renderer;
                     Texture  : SDL.Video.Textures.Texture;
                     Clips     : Sprite_Clips_Array);
private

   type Button is tagged record
      X, Y           : Integer := 0;
      Width, Height  : Integer := 0;
      Current_Sprite : Button_Sprite := Mouse_Out;
   end record;

end Button;
