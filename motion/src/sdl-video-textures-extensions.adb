with SDL.Images.IO;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;

package body SDL.Video.Textures.Extensions is

   procedure Load_From_File (Self : in out SDL.Video.Textures.Texture;
                             Renderer : SDL.Video.Renderers.Renderer;
                             File_Name : UTF_Strings.UTF_String) is
      Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Surface, File_Name);

      SDL.Video.Textures.Makers.Create (Self, Renderer, Surface);

      Surface.Finalize;
   end Load_From_File;

   procedure Load_From_Rendered_Text (Texture : in out SDL.Video.Textures.Texture;
                                      Text    : String;
                                      Colour  : SDL.Video.Palettes.Colour) is
      Text_Surface : SDL.Video.Surfaces.Surface;
   begin
      Text_Surface := Font.Render_Solid (Text, Colour);

      SDL.Video.Textures.Makers.Create (Texture, Renderer, Text_Surface);

      Text_Surface.Finalize;
   end Load_From_Rendered_Text;

end SDL.Video.Textures.Extensions;
