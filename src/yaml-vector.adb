package body YAML.Vector is

   overriding
   function Get (Item : in Instance;
                 Name : in String) return Parent_Class is
      pragma Unreferenced (Item, Name);
   begin
      return raise Constraint_Error with "YAML.Object uses index-based look-up.";
   end Get;

   overriding
   function Get (Item  : in Instance;
                 Index : in Positive) return Parent_Class is
      pragma Unreferenced (Item, Index);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Get (Item : in Instance;
                 Name : in String) return String is
      pragma Unreferenced (Item, Name);
   begin
      return raise Constraint_Error with "YAML.Object uses index-based look-up.";
   end Get;

   overriding
   function Get (Item  : in Instance;
                 Index : in Positive) return String is
      pragma Unreferenced (Item, Index);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Get (Item    : in Instance;
                 Name    : in String;
                 Default : in String) return String is
      pragma Unreferenced (Item, Name, Default);
   begin
      return raise Constraint_Error with "YAML.Object uses index-based look-up.";
   end Get;

   overriding
   function Get (Item    : in Instance;
                 Index   : in Positive;
                 Default : in String) return String is
      pragma Unreferenced (Item, Index, Default);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Get;

   overriding
   function Has (Item : in Instance;
                 Name : in String) return Boolean is
      pragma Unreferenced (Item, Name);
   begin
      return False;
   end Has;

   overriding
   function Has (Item  : in Instance;
                 Index : in Positive) return Boolean is
      pragma Unreferenced (Item, Index);
   begin
      return raise Program_Error with "Not implemented yet.";
   end Has;

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

end YAML.Vector;
