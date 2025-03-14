pragma Ada_2022;

with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images;
with SDL.Images.IO;
with SDL.Video.Surfaces;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Windows.Makers;

procedure Extension_Libraries is

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Width  : constant := 640;
   Height : constant := 480;

   Window         : SDL.Video.Windows.Window;
   Event          : SDL.Events.Events.Events;
   Window_Surface : SDL.Video.Surfaces.Surface;
   Image_Surface  : SDL.Video.Surfaces.Surface;

   function Initialise return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      if not SDL.Images.Initialise (Flags => SDL.Images.Enable_PNG) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Extension Libraries and Loading Other Image Formats",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      Window_Surface := Window.Get_Surface;

      return True;

   end Initialise;

   procedure Handle_Events is
      Finished : Boolean := False;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Finished := True;
               when SDL.Events.Keyboards.Key_Up =>
                  case Event.Keyboard.Key_Sym.Key_Code is
                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         Window_Surface.Blit_Scaled (Image_Surface);
         Window.Update_Surface;

         exit when Finished;
      end loop;
   end Handle_Events;

   procedure Load_Surface
     (Surface        : in out SDL.Video.Surfaces.Surface;
      Screen_Surface : in out SDL.Video.Surfaces.Surface;
      File_Name : UTF_Strings.UTF_String) is
      Optimized_Surface : SDL.Video.Surfaces.Surface;
      Loaded_Surface    : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Loaded_Surface, File_Name);
      SDL.Video.Surfaces.Makers.Convert
        (Optimized_Surface,
         Loaded_Surface,
         Screen_Surface.Pixel_Format);

      Surface := Optimized_Surface;

      Loaded_Surface.Finalize;
   end Load_Surface;

begin
   if not Initialise then
      return;
   end if;

   Load_Surface (Image_Surface, Window_Surface, "../resources/loaded.png");

   Handle_Events;

   Image_Surface.Finalize;
   Window_Surface.Finalize;
   Window.Finalize;
   SDL.Images.Finalise;
   SDL.Finalise;

   Ada.Text_IO.Put_Line ("Process completed.");
end Extension_Libraries;
