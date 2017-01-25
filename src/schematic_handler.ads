package Schematic_Handler is

   type Instance (<>) is tagged with private;
   subtype Class is Instance'Class;

   function Create (Schematic : in Schematic.Instance) return Class;

   function Name (Item : in Class) return String;

   function Bootstrap (Item : in Class) return Class;

private

   type Instance (<>) is tagged
      record
         Schematic : Schematic.Instance;
      end record;

end Schematic_Handler;

package body Schematic_Handler is

   function Create (Schematic : in Schematic.Instance) return Class;

   function Name (Item : in Class) return String is
   begin
      return Item.Schematic.Name;
   end Name;

   function Bootstrap (Item : in Class) return Class is
   begin
   end Bootstrap;

end Schematic_Handler;


class SchematicHandler:
    def bootstrap(self):
        if not os.path.isdir(CACHE_DIR):
            os.mkdir(CACHE_DIR)
        return self

    def checkout(self):
        print '>> checkout'
        url = self.schematic['source']['url']
        branch = self.schematic['source']['ref'] or 'master'
        repos_dir = os.path.join(CACHE_DIR, 'repos')
        self.checkout_dir = os.path.join(repos_dir, self.name)

        if not os.path.isdir(repos_dir):
            os.mkdir(repos_dir)

        if not os.path.isdir(self.checkout_dir):
            command = "git clone --depth 1 %s %s" % (url, self.checkout_dir)
        else:
            command = "cd %s && git fetch" % (self.checkout_dir)

        self.__readpipe__(os.popen(command))
        self.__readpipe__(os.popen("cd %s && git checkout origin/%s" % (self.checkout_dir, branch)))
        return self

    def prepare(self):
        print '>> prepare'
        steps = self.schematic['prepare'] or []
        self.__run_steps__(steps)
        return self

    def build(self):
        print '>> build'
        steps = self.schematic['build'] or []
        self.__run_steps__(steps)
        return self

    def install(self):
        print '>> install'
        steps = self.schematic['install'] or []
        self.__run_steps__(steps)
        return self

    def __run_steps__(self, steps):
        for s in steps:
            self.__readpipe__(os.popen("cd %s && %s" % (self.checkout_dir, s)))

    def __readpipe__(self, pipe):
        while True:
            line = pipe.readline()
            if not line:
                break
            sys.stdout.write("[%s] %s" % (self.name, line))
            sys.stdout.flush()


def traverse_schematic_graph(graph, key=None):
    if not key:
        install = []
        for key in graph.iterkeys():
            result = traverse_schematic_graph(graph, key)
            if result:
                install.append(result)
            install.append(key)
        return install
    if not graph.has_key(key):
        return key
    for key in graph[key]:
        return traverse_schematic_graph(graph, key)



class FileCatalog:
    def __init__(self, uri):
        self.uri = uri
        self.schematics = {}

    def load(self):
        path = re.split('file:\/\/', self.uri)[1]
        catalog_dir = os.path.join(path, 'catalog')
        if not os.path.isdir(catalog_dir):
            return

        for root, dirs, files in os.walk(catalog_dir):
            for f in files:
                if not f.endswith('.yaml'):
                    continue
                publisher = os.path.basename(root)
                name = re.sub('\.yaml', '', f)
                qualified_name = '/'.join([publisher, name])

                self.schematics[qualified_name] = yaml.load(file(os.path.join(root, f)))


    def has_schematic(self, schematic):
        if self.schematics.has_key(schematic):
            return self.schematics[schematic]
        return None

    def __str__(self):
        return "FileCatalog(%s)" % self.uri

def install():
    if not os.path.isfile('schematics.yaml'):
        return 1
    schematics = yaml.load(file('schematics.yaml'))

    catalogs = []
    for catalog in schematics['catalogs']:
        c = FileCatalog(catalog)
        c.load()
        catalogs.append(c)

    graph = {}
    todo = {}
    dependencies = []
    for schematic in schematics['schematics']:
        for catalog in catalogs:
            graph[schematic] = []
            s = catalog.has_schematic(schematic)
            if not s:
                print "!!! Could not find schematic for %s !!!" % schematic
                return 1
            todo[schematic] = SchematicHandler(s)
            depends = s['schematics'] or []
            graph[schematic].extend(depends)
            dependencies.extend(depends)

    for schematic in dependencies:
        for catalog in catalogs:
            s = catalog.has_schematic(schematic)
            if not s:
                print "!!! Could not find schematic for %s !!!" % schematic
                return 1
            todo[schematic] = SchematicHandler(s)
            depends = s['schematics'] or []
            for d in depends:
                if not graph.has_key(d):
                    graph[d] = []
                graph[d].append(schematic)

    for schematic in traverse_schematic_graph(graph):
        print "Installing schematic %s" % schematic
        todo[schematic].bootstrap().checkout().prepare().build().install()
    return 0

def run():
    if len(sys.argv) < 3:
        print "Must specify a command to run"
        return -1
    env = {}
    for e in os.environ:
        env[e] = os.environ[e]

    project_path = os.path.join(ROOT_DIR, 'lib', 'gnat')
    key = 'ADA_PROJECT_PATH'

    if os.environ.has_key(key):
        env[key] = ':'.join([project_path, os.environ[key]])
    else:
        env[key] = project_path

    key = 'GPR_PROJECT_PATH'
    if os.environ.has_key(key):
        env[key] = ':'.join([project_path, os.environ[key]])
    else:
        env[key] = project_path

    key = 'ADA_INCLUDE_PATH'

    include_path = os.path.join(ROOT_DIR, 'include')
    if os.environ.has_key(key):
        env[key] = ':'.join([include_path, os.environ[key]])
    else:
        env[key] = include_path
    return os.execvpe(sys.argv[2], sys.argv[2:], env)


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == 'install':
            sys.exit(install())
        if sys.argv[1] == 'run':
            sys.exit(run())
    else:
        print "Welcome to the Analytical Engine"


if __name__ == '__main__':
    main()
