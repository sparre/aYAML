package YAML.Object is

   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Get (Item : in Instance;
                 Name : in String) return Class;

   function Get (Item : in Instance;
                 Name : in String) return String;

   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String;

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

   type Instance is tagged
      record
         null;
      end record;

end YAML.Object;
