package YAML.Abstract_Object is

   type Instance is abstract tagged null record;
   subtype Class is Instance'Class;

   function Get (Item : in Instance;
                 Name : in String) return Class is abstract;

   function Get (Item : in Instance;
                 Name : in String) return String is abstract;

   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String is abstract;

   function Has (Item : in Instance;
                 Name : in String) return Boolean is (False);

   function Node_Type (Item : in Instance;
                       Name : in String) return Node_Types is abstract;

end YAML.Abstract_Object;
