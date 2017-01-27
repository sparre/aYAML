with Analytical_Engine.Schematic;

package Analytical_Engine.Schematic_Handler is

   type Instance (<>) is tagged private;
   subtype Class is Instance'Class;

   function Create (Schematic : in Analytical_Engine.Schematic.Instance)
                   return Class;

   function Name (Item : in Class) return String;

   procedure Bootstrap (Item : in out Instance);
   procedure Checkout  (Item : in out Instance);
   procedure Prepare   (Item : in out Instance);
   procedure Build     (Item : in out Instance);
   procedure Install   (Item : in out Instance);

private

   type Instance is tagged
      record
         Schematic : Analytical_Engine.Schematic.Instance;
      end record;

end Analytical_Engine.Schematic_Handler;
