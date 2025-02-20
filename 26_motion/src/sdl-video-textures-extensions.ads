with Ada.Strings.UTF_Encoding;
with SDL.TTFs;
with SDL.Video.Renderers;

--  Helper procedures for textures
package SDL.Video.Textures.Extensions is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   procedure Load_From_File (Self      : in out SDL.Video.Textures.Texture;
                             Renderer  : SDL.Video.Renderers.Renderer;
                             File_Name : UTF_Strings.UTF_String);

   procedure Load_From_Rendered_Text (Self     : in out SDL.Video.Textures.Texture;
                                      Renderer : SDL.Video.Renderers.Renderer;
                                      Font     : SDL.TTFs.Fonts;
                                      Text     : String;
                                      Colour   : SDL.Video.Palettes.Colour);

   procedure Render (Self     : SDL.Video.Textures.Texture;
                     Renderer : in out Renderers.Renderer;
                     X        : SDL.Coordinate;
                     Y        : SDL.Coordinate);

end SDL.Video.Textures.Extensions;
