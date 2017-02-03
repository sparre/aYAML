package body YAML.Object is

   function Get (Item : in Instance;
                 Name : in String) return Class is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   function Get (Item : in Instance;
                 Name : in String) return String is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String is
      pragma Unreferenced (Item, Name, Default);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   package body Parse is
      function Get (Item : in Instance;
                    Name : in String) return Element_Type is
      begin
         return Value (Item.Get (Name => Name));
      end Get;

      function Get (Item    : in Instance;
                    Name    : in String;
                    Default : in Element_Type) return Element_Type is
      begin
         return Value (Item.Get (Name    => Name,
                                 Default => Image (Default)));
      end Get;
   end Parse;

end YAML.Object;
