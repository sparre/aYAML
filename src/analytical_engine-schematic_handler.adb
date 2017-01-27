package body Analytical_Engine.Schematic_Handler is

   function Create (Schematic : in Schematic.Instance) return Class is
   begin
      return Instance'(Schematic => Schematic);
   end Create;

   function Name (Item : in Class) return String is
   begin
      return Item.Schematic.Name;
   end Name;

   procedure Bootstrap (Item : in out Instance) is
   begin
      Ada.Directories.Create_Directory (Environment.Cache_Directory);
   exception
      when others =>
         null; --  No problem.  We assume it already was there.
   end Bootstrap;

   procedure Checkout (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> checkout");
   end Checkout;

   procedure Prepare (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> prepare");
   end Prepare;

   procedure Build (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> build");
   end Build;

   procedure Install (Item : in out Instance) is
   begin
      Ada.Text_IO.Put_Line (">> install");
   end Install;

end Analytical_Engine.Schematic_Handler;
