with Ada.Directories;
with Ada.Text_IO;

with Shell;

with Analytical_Engine.Environment;

package body Analytical_Engine.Schematic_Handler is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   function "&" (Left  : in String;
                 Right : in Ada.Strings.Unbounded.Unbounded_String) return String;

   procedure Run_With_Prefixed_Output
     (Prefix            : in     String;
      Program           : in     String;
      Arguments         : in     Shell.String_Array := Shell.Nil_Strings;
      Working_Directory : in     String := ".");

   task type Log_From_Pipe is
      entry Configure (Name : in String;
                       Pipe : in Shell.Pipe);
   end Log_From_Pipe;

   task body Log_From_Pipe is
      Step   : Ada.Strings.Unbounded.Unbounded_String;
      Source : Shell.Pipe;
   begin
      accept Configure (Name : in String;
                        Pipe : in Shell.Pipe) do
         Step   := +Name;
         Source := Pipe;
      end Configure;

      loop
         Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                          Item => "[" & Step & "] " & Shell.To_String (Source));
      end loop;
   exception
      when others =>
         Shell.Close (Source);
   end Log_From_Pipe;

   function "&" (Left  : in String;
                 Right : in Ada.Strings.Unbounded.Unbounded_String) return String is
   begin
      return Left & Ada.Strings.Unbounded.To_String (Right);
   end "&";

   procedure Bootstrap (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Directories.Create_Directory (Environment.Cache_Directory);
   exception
      when others =>
         null; --  No problem.  We assume it already was there.
   end Bootstrap;

   procedure Build (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> build");
   end Build;

   procedure Checkout (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> checkout");

      declare
         use Ada.Directories;

         URL             : constant String := Item.Schematic.Get ("source").Get ("url");
         Branch          : constant String := Item.Schematic.Get ("source").Get ("ref", Default => "master");
         Repos_Directory : constant String := Compose (Environment.Cache_Directory,
                                                       "repos");
      begin
         Item.Checkout_Directory := +Compose (Repos_Directory, Item.Name);

         if not Exists (Repos_Directory) then
            Create_Directory (Repos_Directory);
         end if;

         if Exists (+Item.Checkout_Directory) then
            Run_With_Prefixed_Output
              (Prefix            => Item.Name,
               Program           => "git",
               Arguments         => (1 => +"fetch"),
               Working_Directory => +Item.Checkout_Directory);
         else
            Run_With_Prefixed_Output
              (Prefix            => Item.Name,
               Program           => "git",
               Arguments         => (+"clone",
                                     +"--depth",
                                     +"1",
                                     +URL,
                                     Item.Checkout_Directory));
         end if;

         Run_With_Prefixed_Output
           (Prefix            => Item.Name,
            Program           => "git",
            Arguments         => (+"checkout",
                                  +("origin/" & Branch)),
            Working_Directory => +Item.Checkout_Directory);
      end;
   end Checkout;

   function Create (Schematic : in YAML.Object.Instance)
                   return Class is
   begin
      return Instance'(Schematic          => Schematic,
                       Checkout_Directory => <>);
   end Create;

   procedure Install (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> install");
   end Install;

   function Name (Item : in Class) return String is
   begin
      return Item.Schematic.Get ("name");
   end Name;

   procedure Prepare (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> prepare");
   end Prepare;

   procedure Run_With_Prefixed_Output
     (Prefix            : in     String;
      Program           : in     String;
      Arguments         : in     Shell.String_Array := Shell.Nil_Strings;
      Working_Directory : in     String := ".")
   is
      use Shell;
      Prefixed_Output : Log_From_Pipe;
      Output          : Shell.Pipe;
      Process         : Shell.Process :=
                          Start (Program           => Program,
                                 Arguments         => Arguments,
                                 Working_Directory => Working_Directory,
                                 Output            => Output) with Unreferenced;
   begin
      Prefixed_Output.Configure (Name => Prefix,
                                Pipe => Output);
   end Run_With_Prefixed_Output;

end Analytical_Engine.Schematic_Handler;
