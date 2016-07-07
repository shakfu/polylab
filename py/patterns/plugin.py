import sys, os, glob
import importlib
import yaml

PLUGIN_INFO_EXT = 'plugin'

class IPlugin(object):
    '''interface to be inherited when creating a plugin
    '''
    def __init__(self):
        self.is_activated = False
    
    def activate(self):
        "called at plugin activation"
        self.is_activated = True

    def deactivate(self):
        "called when the piplugin is disabled"
        self.is_activated = False

class PluginInfo(object):
    '''contains plugin metadata
    '''
    def __init__(self, **kwds):
        self.name = kwds.get('name')
        self.path = kwds.get('path')
        self.author = kwds.get('author', 'Unknown')
        self.version = kwds.get('version', '0.0')
        self.website = kwds.get('website', 'Unknown')
        self.description = kwds.get('description', '')
        self.category = kwds.get('category')
        self.active = kwds.get('active', False)

class PluginManager(object):
    '''loads plugins from a specified directory
    '''
    plugins = {}
    plugin_ext = "*.yaml"

    def __init__(self, directory='plugins'):
        self.directory = os.path.abspath(directory)
        self.registry = {}

    def info(self):
        for name in self.plugins:
            plugin = self.plugins[name]
            print plugin.info

    def scan(self):
        plugins = os.path.join(self.directory, self.plugin_ext)
        for path in glob.glob(plugins):
            config = yaml.load(file(path).read())
            self.registry[config['name']] = PluginInfo(**config)

    def get(self, name):
        if name in self.plugins:
            plugin = self.plugins[name]
        elif name in self.registry:
            if self.registry[name].active:
                try:
                    qualified_name = os.path.basename(self.directory + '.' + name)
                    plugin = importlib.import_module(qualified_name)
                    self.plugins[name] = plugin
                except ImportError, e:
                    print "ImportError: plugin '{0}' not found".format(name)
                    return
            else:
                print "plugin '{0}' not active".format(name)
                return
        else:
            print "plugin '{0}' not found".format(name)
            return
        return plugin



if __name__ == '__main__':
    manager = PluginManager()
    manager.scan()
    for plugin in manager.registry:
        print plugin

