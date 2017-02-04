with YAML.Abstract_Object;

package YAML.Object is

   subtype Parent is Abstract_Object.Instance;
   subtype Parent_Class is Abstract_Object.Class;

   type Instance is new Parent with private;
   subtype Class is Instance'Class;

   overriding
   function Get (Item : in Instance;
                 Name : in String) return Parent_Class;

   overriding
   function Get (Item : in Instance;
                 Name : in String) return String;

   overriding
   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String;

   overriding
   function Has (Item : in Instance;
                 Name : in String) return Boolean;

   overriding
   function Node_Type (Item : in Instance;
                       Name : in String) return Node_Types;

   generic
      type Element_Type is private;
      with function Value (Item : in String) return Element_Type;
      with function Image (Item : in Element_Type) return String;
   package Parse is
      function Get (Item : in Instance;
                    Name : in String) return Element_Type;

      function Get (Item    : in Instance;
                    Name    : in String;
                    Default : in Element_Type) return Element_Type;
   end Parse;

private

   type Instance is new Parent with
      record
         null;
      end record;

end YAML.Object;
