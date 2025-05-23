pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with SDL.Audio;
with SDL.Audio.Devices;
with SDL.Error;
with SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with SDL.TTFs;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

use SDL.Video.Renderers;

procedure Audio_Recording is

   type Texture_Array is array (Positive range <>) of SDL.Video.Textures.Texture;
   type Buffer_T is array (Positive range <>) of Interfaces.Unsigned_8;
   package Audio_Devices is new SDL.Audio.Devices (Frame_Type => Interfaces.Unsigned_8,
                                                   Buffer_Index => Positive,
                                                   Buffer_Type  => Buffer_T);
   type Recording_State is (Selecting_Device,
                            Stopped,
                            Recording,
                            Recorded,
                            PlayBack,
                            Error);

   Width  : constant := 640;
   Height : constant := 480;

   Window   : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Event    : SDL.Events.Events.Events;
   Font     : SDL.TTFs.Fonts;

   Text_Colour : constant SDL.Video.Palettes.Colour := (others => 0);

   Max_Recording_Devices    : constant := 10;
   Max_Recording_Seconds    : constant := 5;
   Recording_Buffer_Seconds : constant := Max_Recording_Seconds + 1;
   Recording_Device_Count : Natural;

   Prompt_Texture : SDL.Video.Textures.Texture;
   Device_Textures : Texture_Array (1 .. Max_Recording_Devices);

   function Initialise return Boolean is
      use type SDL.Init_Flags;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen or SDL.Enable_Audio)
      then
         return False;
      end if;

      if not SDL.TTFs.Initialise then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Audio Recording",
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
     (Texture : in out SDL.Video.Textures.Texture;
      Text    : String;
      Colour  : SDL.Video.Palettes.Colour) is
      Text_Surface : SDL.Video.Surfaces.Surface;
   begin
      Text_Surface := Font.Render_Solid (Text, Colour);

      SDL.Video.Textures.Makers.Create (Texture, Renderer, Text_Surface);

      Text_Surface.Finalize;
   end Load_From_Rendered_Text;

   function Load_Media return Boolean is
   begin
      --  Open the font
      SDL.TTFs.Makers.Create (Font, "../resources/lazy.ttf", 28);
      --  Set starting prompt
      Load_From_Rendered_Text (Prompt_Texture, "Select your recording device:", Text_Colour);
      --  Get capture device count
      Recording_Device_Count := Audio_Devices.Total_Devices (True);

      if Recording_Device_Count < 1 then
         Put_Line ("Unable to get audio capture device! SDL Error: " & SDL.Error.Get);

         return False;
      else
         if Recording_Device_Count > Max_Recording_Devices then
            Recording_Device_Count := Max_Recording_Devices;
         end if;

         --  Start loop from 0 to start indexing the devices from 0 for the selection
         for Index in 0 .. Recording_Device_Count - 1 loop
            Load_From_Rendered_Text (Device_Textures (Index + 1),
                                     Index'Image & ": " & Audio_Devices.Get_Name (Index + 1),
                                     Text_Colour);
         end loop;

      end if;

      return True;
   end Load_Media;

   procedure Free_Media is
   begin
      Prompt_Texture.Finalize;

      for Index in 1 .. Max_Recording_Devices loop
         Device_Textures (Index).Finalize;
      end loop;

      Font.Finalize;
   end Free_Media;

   procedure Close is
   begin
      Free_Media;

      Renderer.Finalize;
      Window.Finalize;

      SDL.TTFs.Quit;
      Window.Finalize;
      SDL.Finalise;
   end Close;

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
      Finished      : Boolean := False;
      Current_State : Recording_State := Selecting_Device;
      subtype Number_Keys_Range is SDL.Events.Keyboards.Key_Codes range
        SDL.Events.Keyboards.Code_0 .. SDL.Events.Keyboards.Code_9;
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

                     when others =>
                        case Current_State is
                           when Selecting_Device =>
                              if Event.Keyboard.Key_Sym.Key_Code
                              in Number_Keys_Range
                              then
                                 declare
                                    Index : Natural :=
                                              Natural (Event.Keyboard.Key_Sym.Key_Code) -
                                              Natural (SDL.Events.Keyboards.Code_0);
                                 begin
                                    if Index < Recording_Device_Count then
                                       Put_Line ("Do it for index: " &  Index'Image);
                                    end if;
                                 end;
                              end if;
                           when Stopped => null;
                           when Recorded => null;
                           when others => null;
                        end case;
                  end case;

               when others => null;
            end case;

            if Current_State = Recording then
               null;
            elsif Current_State = PlayBack then
               null;
            end if;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render
         Render (Renderer,
                 Prompt_Texture,
                 (Width - Prompt_Texture.Get_Size.Width) / 2,
                 0);
         for Index in 1 .. Recording_Device_Count loop
            Render (Renderer,
                    Device_Textures (Index),
                    (Width - Prompt_Texture.Get_Size.Width) / 2,
                    Prompt_Texture.Get_Size.Height);
         end loop;

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

   if Load_Media then
      Handle_Events;
   end if;

   Close;

   Put_Line ("Process completed.");
--  exception
--     when Event : others =>
--        Put_Line ("Process not completed.");
--        Put_Line ("Exception raised: " &
--                    Ada.Exceptions.Exception_Name (Event));
--        Put_Line ("Exception mesage: " &
--                    Ada.Exceptions.Exception_Message (Event));
end Audio_Recording;
