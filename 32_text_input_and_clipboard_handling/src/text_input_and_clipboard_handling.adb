with Ada.Text_IO;
with Ada.Strings.Unbounded;
with SDL.Clipboard;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with SDL.Inputs.Keyboards;
with SDL.TTFs;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

with Interfaces.C; use Interfaces.C;

use SDL.Video.Renderers;

procedure Text_Input_And_Clipboard_Handling is

   package ASU renames Ada.Strings.Unbounded;

   Width  : constant := 640;
   Height : constant := 480;

   Window              : SDL.Video.Windows.Window;
   Renderer            : SDL.Video.Renderers.Renderer;
   Event               : SDL.Events.Events.Events;
   Prompt_Texture      : SDL.Video.Textures.Texture;
   Input_Text_Texture  : SDL.Video.Textures.Texture;
   Font                : SDL.TTFs.Fonts;

   Input_Text          : ASU.Unbounded_String :=
                           ASU.To_Unbounded_String ("Some Text");
   Render_Text : Boolean := False;

   procedure Load_From_Rendered_Text
     (Texture : in out SDL.Video.Textures.Texture;
      Text    : String;
      Colour  : SDL.Video.Palettes.Colour) is
      Text_Surface : SDL.Video.Surfaces.Surface;
   begin
      Text_Surface := Font.Render_Solid (Text, Colour);

      SDL.Video.Textures.Makers.Create (Texture, Renderer, Text_Surface);

      Text_Surface.Finalize;
   end Load_From_Rendered_Text;

   procedure Load_Media is
   begin
      Load_From_Rendered_Text (Prompt_Texture, "Enter text:", (others => 0));
      Load_From_Rendered_Text (Input_Text_Texture,
                               ASU.To_String (Input_Text), (others => 0));
   end Load_Media;

   procedure Render (Renderer : in out SDL.Video.Renderers.Renderer;
                     Texture  : in out SDL.Video.Textures.Texture;
                     X        : SDL.Dimension;
                     Y        : SDL.Dimension) is
      Render_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
                           (X,
                            Y,
                            Texture.Get_Size.Width,
                            Texture.Get_Size.Height);
   begin
      Renderer.Copy_To (Texture, Render_Rectangle);
   end Render;

   procedure Handle_Events is
      Finished : Boolean := False;

      use type ASU.Unbounded_String;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;

               when SDL.Events.Keyboards.Text_Input =>
                  ASU.Append (Input_Text, To_Ada (Event.Text_Input.Text));
                  Render_Text := True;

               when SDL.Events.Keyboards.Key_Down =>
                  case Event.Keyboard.Key_Sym.Key_Code is

                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;

                     when SDL.Events.Keyboards.Code_Backspace =>
                        declare
                           Text_Length : constant Natural :=
                                           ASU.Length (Input_Text);
                        begin
                           if Text_Length > 0 then
                              Input_Text := ASU.Head (Input_Text, Text_Length - 1);
                              Render_Text := True;
                           end if;
                        end;

                     when SDL.Events.Keyboards.Code_C =>
                        if (SDL.Inputs.Keyboards.Get_Modifiers
                          or SDL.Events.Keyboards.Modifier_Control) = SDL.Events.Keyboards.Modifier_Control
                        then
                           --  Copy text to clipboard
                           SDL.Clipboard.Set (ASU.To_String (Input_Text));
                        end if;

                     when SDL.Events.Keyboards.Code_V =>
                        if (SDL.Inputs.Keyboards.Get_Modifiers
                          or SDL.Events.Keyboards.Modifier_Control) = SDL.Events.Keyboards.Modifier_Control
                        then
                           --  Copy text from clipboard into temporary buffer
                           Input_Text := ASU.To_Unbounded_String (SDL.Clipboard.Get);
                           Render_Text := True;
                        end if;

                     when SDL.Events.Keyboards.Code_Delete =>
                        Input_Text := ASU.To_Unbounded_String ("");
                        Render_Text := True;
                     when others => null;
                  end case;

               when others => null;
            end case;
         end loop;

         if Render_Text then
            if Input_Text /= "" then
               Load_From_Rendered_Text (Input_Text_Texture,
                                        ASU.To_String (Input_Text),
                                        (others => 0));
            else
               Load_From_Rendered_Text (Input_Text_Texture, " ", (others => 0));
               Render_Text := False;
            end if;
         end if;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         Render (Renderer,
                 Prompt_Texture,
                 (Width - Prompt_Texture.Get_Size.Width) / 2,
                 0);
         Render (Renderer,
                 Input_Text_Texture,
                 (Width - Input_Text_Texture.Get_Size.Width) / 2,
                 Prompt_Texture.Get_Size.Height);

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not SDL.Initialise (Flags => SDL.Enable_Screen) then
      return;
   end if;

   if not SDL.TTFs.Initialise then
      return;
   end if;

   SDL.Video.Windows.Makers.Create
     (Win      => Window,
      Title    => "SDL Tutorial - Text Input And Clipboard Handling",
      Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
      Size     => SDL.Positive_Sizes'(Width, Height),
      Flags    => 0);

   SDL.Video.Renderers.Makers.Create
     (Window => Window,
      Rend   => Renderer,
      Flags  => SDL.Video.Renderers.Accelerated or
        SDL.Video.Renderers.Present_V_Sync);

   SDL.TTFs.Makers.Create (Font, "../resources//lazy.ttf", 28);

   Load_Media;

   SDL.Inputs.Keyboards.Start_Text_Input;

   Handle_Events;

      SDL.Inputs.Keyboards.Stop_Text_Input;

   SDL.TTFs.Quit;
   Window.Finalize;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");

end Text_Input_And_Clipboard_Handling;
