pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Inputs.Keyboards;
with SDL.Video.Surfaces;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Windows.Makers;

procedure Key_States is

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
         Title    => "SDL Tutorial - Key States",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);

      Window_Surface := Window.Get_Surface;

      return True;

   end Initialise;

   procedure Load_Media (Surfaces : in out Key_Press_Surfaces) is
      Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Video.Surfaces.Makers.Create (Surface,
                                        "../resources/press.bmp");
      Surfaces (Default) := Surface;
      SDL.Video.Surfaces.Makers.Create (Surface,
                                        "../resources/up.bmp");
      Surfaces (Up) :=  Surface;
      SDL.Video.Surfaces.Makers.Create (Surfaces (Down),
                                        "../resources/down.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Left),
                                        "../resources/left.bmp");
      SDL.Video.Surfaces.Makers.Create (Surfaces (Right),
                                        "../resources/right.bmp");
   end Load_Media;

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

   procedure Handle_Events is
      Finished : Boolean := False;
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
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         declare
            Key_State : constant SDL.Inputs.Keyboards.Key_State_Access :=
                          SDL.Inputs.Keyboards.Get_State;
         begin
            if Key_State (SDL.Events.Keyboards.Scan_Code_Up) then
               Current_Surface := Surfaces (Up);
            elsif Key_State (SDL.Events.Keyboards.Scan_Code_Down) then
               Current_Surface := Surfaces (Down);
            elsif Key_State (SDL.Events.Keyboards.Scan_Code_Left) then
               Current_Surface := Surfaces (Left);
            elsif Key_State (SDL.Events.Keyboards.Scan_Code_Right) then
               Current_Surface := Surfaces (Right);
            else
               Current_Surface := Surfaces (Default);
            end if;
         end;

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

   --  Set default image to display
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
end Key_States;
