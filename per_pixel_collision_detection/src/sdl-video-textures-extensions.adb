with SDL.Images.IO;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;

package body SDL.Video.Textures.Extensions is

   procedure Load_From_File (Self      : in out SDL.Video.Textures.Texture;
                             Renderer  : SDL.Video.Renderers.Renderer;
                             File_Name : UTF_Strings.UTF_String) is
      Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Surface, File_Name);

      SDL.Video.Textures.Makers.Create (Self, Renderer, Surface);

      Surface.Finalize;
   end Load_From_File;

   procedure Load_From_Rendered_Text (Self     : in out SDL.Video.Textures.Texture;
                                      Renderer : SDL.Video.Renderers.Renderer;
                                      Font     : SDL.TTFs.Fonts;
                                      Text     : String;
                                      Colour   : SDL.Video.Palettes.Colour) is
      Surface : SDL.Video.Surfaces.Surface;
   begin
      Surface := Font.Render_Solid (Text, Colour);

      SDL.Video.Textures.Makers.Create (Self, Renderer, Surface);

      Surface.Finalize;
   end Load_From_Rendered_Text;

   procedure Render (Self     : SDL.Video.Textures.Texture;
                     Renderer : in out Renderers.Renderer;
                     X        : SDL.Coordinate;
                     Y        : SDL.Coordinate) is
      Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                    (X,
                     Y,
                     Self.Get_Size.Width,
                     Self.Get_Size.Height);
   begin
      Renderer.Copy_To (Self, Rectangle);
   end Render;

end SDL.Video.Textures.Extensions;
