package body YAML.Object is

   overriding
   function Get (Item : in Instance;
                 Name : in String) return Parent_Class is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Get (Item : in Instance;
                 Name : in String) return String is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String is
      pragma Unreferenced (Item, Name, Default);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Has (Item : in Instance;
                 Name : in String) return Boolean is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Has;

   overriding
   function Node_Type (Item : in Instance;
                       Name : in String) return Node_Types is
      pragma Unreferenced (Item, Name);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Node_Type;

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
