with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Video.Surfaces;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Windows.Makers;

procedure Key_Presses is

   type Key_Press_Surface_Index is
     (Default,
      Up,
      Down,
      Left,
      Right);
   type Key_Press_Surfaces is array (Key_Press_Surface_Index) of
     SDL.Video.Surfaces.Surface;

   Width           : constant := 640;
   Height          : constant := 480;
   Surfaces        : Key_Press_Surfaces;
   Window_Surface  : SDL.Video.Surfaces.Surface;
   Current_Surface : SDL.Video.Surfaces.Surface;
   Window          : SDL.Video.Windows.Window;
   Event           : SDL.Events.Events.Events;

   function Initialise return Boolean is
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create
        (Win      => Window,
         Title    => "SDL Tutorial - Key Presses",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      Window_Surface := Window.Get_Surface;

      return True;
   end Initialise;

   procedure Free_Media (Surfaces : in out Key_Press_Surfaces) is
   begin
      for S of Surfaces loop
         S.Finalize;
      end loop;
   end Free_Media;

   procedure Close is
   begin
      Free_Media (Surfaces);

      Window_Surface.Finalize;
      Window.Finalize;
      SDL.Finalise;
   end Close;

   procedure Load_Media (Surfaces : in out Key_Press_Surfaces) is
   begin
      SDL.Video.Surfaces.Makers.Create (Surfaces (Default),
                                        "../resources/press.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Up),
                                        "../resources/up.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Down),
                                        "../resources/down.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Left),
                                        "../resources/left.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Right),
                                        "../resources/right.bmp");
   end Load_Media;

   procedure Handle_Events is
      Finished : Boolean := False;
   begin
      loop
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               --  User requested quit
               when SDL.Events.Quit =>
                  Finished := True;
               when SDL.Events.Keyboards.Key_Down =>
                  case Event.Keyboard.Key_Sym.Key_Code is
                     --  Handle Escape key
                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;
                        --  Handle direction keys
                     when SDL.Events.Keyboards.Code_Up =>
                        Current_Surface := Surfaces (Up);
                     when SDL.Events.Keyboards.Code_Down =>
                        Current_Surface := Surfaces (Down);
                     when SDL.Events.Keyboards.Code_Left =>
                        Current_Surface := Surfaces (Left);
                     when SDL.Events.Keyboards.Code_Right =>
                        Current_Surface := Surfaces (Right);
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         Window_Surface.Blit (Current_Surface);
         Window.Update_Surface;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

   Load_Media (Surfaces);

   Current_Surface := Surfaces (Default);

   Handle_Events;

   Close;

   Put_Line ("Process complete.");
exception
   when Event : others =>
      Put_Line ("Process not completed.");
      Put_Line ("Exception raised: " &
                  Ada.Exceptions.Exception_Name (Event));
      Put_Line ("Exception mesage: " &
                  Ada.Exceptions.Exception_Message (Event));
end Key_Presses;
