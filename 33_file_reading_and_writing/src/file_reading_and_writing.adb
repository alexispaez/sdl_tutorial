pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SDL.Error;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with SDL.RWops;
with SDL.RWops.Streams;
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

procedure File_Reading_And_Writing is

   package Textures renames SDL.Video.Textures;

   Total_Data : constant := 10;

   type Numeric_Data_Array is array (Positive range <>) of Interfaces.Integer_32;
   type Texture_Array is array (Positive range <>) of Textures.Texture;
   subtype Data_Index is Integer range 1 .. Total_Data;

   Width  : constant := 640;
   Height : constant := 480;

   Numeric_Data : Numeric_Data_Array (1 .. Total_Data);
   Texture_Data : Texture_Array (1 .. Total_Data);

   Window         : SDL.Video.Windows.Window;
   Renderer       : SDL.Video.Renderers.Renderer;
   Event          : SDL.Events.Events.Events;
   Prompt_Texture : Textures.Texture;
   Font           : SDL.TTFs.Fonts;

   Text_Colour      : constant SDL.Video.Palettes.Colour := (others => 0);
   Highlight_Colour : constant SDL.Video.Palettes.Colour := (255, 0, 0, 255);

   function Initialise return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      if not SDL.TTFs.Initialise then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - File Reading and Writing",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated or
           SDL.Video.Renderers.Present_V_Sync);

      return True;

   end Initialise;

   procedure Load_From_Rendered_Text
     (Texture : in out Textures.Texture;
      Text    : String;
      Colour  : SDL.Video.Palettes.Colour) is
      Text_Surface : SDL.Video.Surfaces.Surface;
   begin
      Text_Surface := Font.Render_Solid (Text, Colour);

      Textures.Makers.Create (Texture, Renderer, Text_Surface);

      Text_Surface.Finalize;
   end Load_From_Rendered_Text;

   procedure Load_Media is
      RWops        : SDL.RWops.RWops;
      RWops_Stream : aliased SDL.RWops.Streams.RWops_Stream;
   begin
      --  Open the font
      SDL.TTFs.Makers.Create (Font, "../resources/lazy.ttf", 28);
      --  Render the prompt into the texture
      Load_From_Rendered_Text (Prompt_Texture, "Enter Data:", Text_Colour);

      --  Read the binary file
      begin
         --  Open the file
         SDL.RWops.From_File ("../resources/nums.bin", SDL.RWops.Read_Binary, RWops);
         --  Create an Ada stream
         SDL.RWops.Streams.Open (RWops, RWops_Stream);
         --  Read the data into the array, needs the access attribute
         Numeric_Data_Array'Read (RWops_Stream'Access, Numeric_Data);
      exception
            --  If the file could not be opened, create it
         when others =>
            Put_Line ("Warning: Unable to open file! SDL Error: " & SDL.Error.Get);
            --  Open the file for creating and writing
            SDL.RWops.From_File ("../resources/nums.bin", SDL.RWops.Create_To_Write_Binary, RWops);
            --  Create an Ada stream
            SDL.RWops.Streams.Open (RWops, RWops_Stream);
            --  Initialise the numeric data
            for Index in 1 .. Total_Data loop
               Numeric_Data (Index) := 0;
            end loop;
            --  Write the data, needs the access attribute
            Numeric_Data_Array'Write (RWops_Stream'Access, Numeric_Data);
      end;
      --  Close the stream
      SDL.RWops.Streams.Close (RWops_Stream);

      for Index in 1 .. Total_Data loop
         Load_From_Rendered_Text (Texture_Data (Index),
                                  Numeric_Data (Index)'Image,
                                  Text_Colour);
      end loop;
   end Load_Media;

   procedure Free_Media is
   begin
      Font.Finalize;

      Prompt_Texture.Finalize;

      for Index in 1 .. Total_Data loop
         Texture_Data (Index).Finalize;
      end loop;
   end Free_Media;

   procedure Close is
      RWops        : SDL.RWops.RWops;
      RWops_Stream : aliased SDL.RWops.Streams.RWops_Stream;
   begin
      --  Save the numeric data
      SDL.RWops.From_File ("../resources/nums.bin", SDL.RWops.Read_Write_Binary, RWops);
      SDL.RWops.Streams.Open (RWops, RWops_Stream);
      Numeric_Data_Array'Write (RWops_Stream'Access, Numeric_Data);
      SDL.RWops.Streams.Close (RWops_Stream);

      Free_Media;

      SDL.TTFs.Quit;
      Window.Finalize;
      SDL.Finalise;
   end Close;

   procedure Render (Renderer : in out SDL.Video.Renderers.Renderer;
                     Texture  : in out Textures.Texture;
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
      Finished     : Boolean := False;
      Current_Data : Data_Index := Data_Index'First;

      use type Interfaces.Integer_32;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;

               when SDL.Events.Keyboards.Key_Down =>
                  case Event.Keyboard.Key_Sym.Key_Code is

                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;

                     when SDL.Events.Keyboards.Code_Up =>

                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                  Numeric_Data (Current_Data)'Image,
                                                Text_Colour);

                        if Current_Data = Data_Index'First then
                           Current_Data := Data_Index'Last;
                        else
                           Current_Data := @ - 1;
                        end if;

                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                  Numeric_Data (Current_Data)'Image,
                                                Highlight_Colour);

                     when SDL.Events.Keyboards.Code_Down =>

                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                  Numeric_Data (Current_Data)'Image,
                                                Text_Colour);

                        if Current_Data = Data_Index'Last then
                           Current_Data := Data_Index'First;
                        else
                           Current_Data := @ + 1;
                        end if;

                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                  Numeric_Data (Current_Data)'Image,
                                                Highlight_Colour);

                     when SDL.Events.Keyboards.Code_Left =>
                        Numeric_Data (Current_Data) := @ - 1;
                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                                 Numeric_Data (Current_Data)'Image,
                                                 Highlight_Colour);

                     when SDL.Events.Keyboards.Code_Right =>
                        Numeric_Data (Current_Data) := @ + 1;
                        Load_From_Rendered_Text (Texture_Data (Current_Data),
                                                 Numeric_Data (Current_Data)'Image,
                                                 Highlight_Colour);

                     when others => null;
                  end case;

               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render prompt
         Render (Renderer,
                 Prompt_Texture,
                 (Width - Prompt_Texture.Get_Size.Width) / 2,
                 0);

         --  Render numeric data
         for Index in 1 .. Total_Data loop
            Render (Renderer,
                    Texture_Data (Index),
                    (Width - Texture_Data (Index).Get_Size.Width) / 2,
                    Prompt_Texture.Get_Size.Height +
                      Texture_Data (Index).Get_Size.Height * SDL.Dimension (Index - 1));
         end loop;

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

   Load_Media;

   Handle_Events;

   Close;

   Put_Line ("Process completed.");
exception
   when Event : others =>
      Put_Line ("Process not completed.");
      Put_Line ("Exception raised: " &
                  Ada.Exceptions.Exception_Name (Event));
      Put_Line ("Exception mesage: " &
                  Ada.Exceptions.Exception_Message (Event));
end File_Reading_And_Writing;
