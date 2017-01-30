with Ada.Directories;
with Ada.Text_IO;

with Analytical_Engine.Environment;

package body Analytical_Engine.Schematic_Handler is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

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

end Analytical_Engine.Schematic_Handler;
