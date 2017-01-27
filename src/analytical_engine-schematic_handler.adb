with Ada.Directories;
with Ada.Text_IO;

with Analytical_Engine.Environment;

package body Analytical_Engine.Schematic_Handler is

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
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> checkout");
   end Checkout;

   function Create (Schematic : in Analytical_Engine.Schematic.Instance)
                   return Class is
   begin
      return Instance'(Schematic => Schematic);
   end Create;

   procedure Install (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> install");
   end Install;

   function Name (Item : in Class) return String is
   begin
      return Item.Schematic.Name;
   end Name;

   procedure Prepare (Item : in out Instance) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (">> prepare");
   end Prepare;

end Analytical_Engine.Schematic_Handler;
