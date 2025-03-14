pragma Ada_2022;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Windows.Makers;
with Interfaces.C;

procedure Geometry_Rendering is

   use type Interfaces.C.int;

   Width  : constant := 640;
   Height : constant := 480;

   Window         : SDL.Video.Windows.Window;
   Event          : SDL.Events.Events.Events;
   Renderer       : SDL.Video.Renderers.Renderer;

   Fill_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
     (X      => Width / 4,
      Y      => Height / 4,
      Width  => Width / 2,
      Height => Height / 2);
   Outline_Rectangle : constant SDL.Video.Rectangles.Rectangle :=
     (X      => Width / 6,
      Y      => Height / 6,
      Width  => Width * 2 / 3,
      Height => Height * 2 / 3);

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
         Title    => "SDL Tutorial - Geometry Rendering",
         Position => SDL.Natural_Coordinates'(X => 20, Y => 20),
         Size     => SDL.Positive_Sizes'(Width, Height),
         Flags    => 0);
      SDL.Video.Renderers.Makers.Create
        (Window => Window,
         Rend   => Renderer,
         Flags  => SDL.Video.Renderers.Accelerated);

      return True;

   end Initialise;

   procedure Close is
   begin
      Renderer.Finalize;
      Window.Finalize;
      SDL.Images.Finalise;
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
               when SDL.Events.Keyboards.Key_Up =>
                  case Event.Keyboard.Key_Sym.Key_Code is
                     when SDL.Events.Keyboards.Code_Escape =>
                        Finished := True;
                     when others => null;
                  end case;
               when others => null;
            end case;
         end loop;

         --  Clear screen
         Renderer.Set_Draw_Colour ((others => 255));
         Renderer.Clear;

         --  Render red filled quad
         Renderer.Set_Draw_Colour ((255, 0, 0, 255));
         Renderer.Fill (Fill_Rectangle);

         --  Render green outlined quad
         Renderer.Set_Draw_Colour ((0, 255, 0, 255));
         Renderer.Draw (Outline_Rectangle);

         --  Draw blue horizontal line
         Renderer.Set_Draw_Colour ((0, 0, 255, 255));
         Renderer.Draw (SDL.Video.Rectangles.Line_Segment'(
                        (0, Height / 2),
                        (Width, Height / 2)));

         --  Draw vertical line of yellow dots
         Renderer.Set_Draw_Colour ((255, 255, 0, 255));
         declare
            I : Interfaces.C.int := 0;
         begin
            loop
               Renderer.Draw (SDL.Video.Rectangles.Point'(
                              (Width / 2, I)));
               I := @ + 4;
               exit when I >= Height;
            end loop;
         end;

         Renderer.Present;

         exit when Finished;
      end loop;
   end Handle_Events;

begin
   if not Initialise then
      return;
   end if;

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
end Geometry_Rendering;
