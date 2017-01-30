with YAML.Object;

private with Ada.Strings.Unbounded;

package Analytical_Engine.Schematic_Handler is

   type Instance (<>) is tagged private;
   subtype Class is Instance'Class;

   function Create (Schematic : in YAML.Object.Instance)
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
         Schematic          : YAML.Object.Instance;
         Checkout_Directory : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Analytical_Engine.Schematic_Handler;
