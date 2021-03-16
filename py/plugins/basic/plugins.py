# plugins.py
"""
Assume you have the following greeter package:

greeter/
│
├── __init__.py
├── hello.py
├── howdy.py
└── yo.py

Each greeter module defines a function that takes one name argument.

Note how they’re all registered as plugins using the @register decorator:

# greeter/hello.py
import plugins

@plugins.register
def greet(name):
    print(f"Hello {name}, how are you today?")

# greeter/howdy.py
import plugins

@plugins.register
def greet(name):
    print(f"Howdy good {name}, honored to meet you!")

# greeter/yo.py
import plugins

@plugins.register
def greet(name):
    print(f"Yo {name}, good times!")


Note: To simplify the discovery and import of plugins, each plugin’s name is based on the name of the module that contains it instead of the function name. This restricts you to having only one plugin per file.
To finish setting up greeter as a plugin package, you can use the factory functions in plugins to add functionality to the greeter package itself:

# greeter/__init__.py

import plugins

greetings = plugins.names_factory(__package__)
greet = plugins.call_factory(__package__)

You can now use greetings() and greet() as follows:

>>>
>>> import greeter
>>> greeter.greetings()
['hello', 'howdy', 'yo']

>>> greeter.greet(plugin="howdy", name="Guido")
Howdy good Guido, honored to meet you!




"""

import functools
import importlib
from collections import namedtuple
from importlib import resources

# Basic structure for storing information about one plugin
Plugin = namedtuple("Plugin", ("name", "func"))

# Dictionary with information about all registered plugins
_PLUGINS = {}

def register(func):
    """Decorator for registering a new plugin"""
    package, _, plugin = func.__module__.rpartition(".")
    pkg_info = _PLUGINS.setdefault(package, {})
    pkg_info[plugin] = Plugin(name=plugin, func=func)
    return func

def names(package):
    """List all plugins in one package"""
    _import_all(package)
    return sorted(_PLUGINS[package])

def get(package, plugin):
    """Get a given plugin"""
    _import(package, plugin)
    return _PLUGINS[package][plugin].func

def call(package, plugin, *args, **kwargs):
    """Call the given plugin"""
    plugin_func = get(package, plugin)
    return plugin_func(*args, **kwargs)

def _import(package, plugin):
    """Import the given plugin file from a package"""
    importlib.import_module(f"{package}.{plugin}")

def _import_all(package):
    """Import all plugins in a package"""
    files = resources.contents(package)
    plugins = [f[:-3] for f in files if f.endswith(".py") and f[0] != "_"]
    for plugin in plugins:
        _import(package, plugin)

def names_factory(package):
    """Create a names() function for one package"""
    return functools.partial(names, package)

def get_factory(package):
    """Create a get() function for one package"""
    return functools.partial(get, package)

def call_factory(package):
    """Create a call() function for one package"""
    return functools.partial(call, package)
