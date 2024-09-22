with Ada.Strings.UTF_Encoding;
with SDL.Video.Renderers;

package SDL.Video.Textures.Extensions is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   procedure Load_From_File (Self      : in out SDL.Video.Textures.Texture;
                             Renderer  : SDL.Video.Renderers.Renderer;
                             File_Name : UTF_Strings.UTF_String);
   
   procedure Load_From_Rendered_Text (Texture : in out SDL.Video.Textures.Texture;
                                      Text    : String;
                                      Colour  : SDL.Video.Palettes.Colour);
end SDL.Video.Textures.Extensions;
