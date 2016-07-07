from plugin import IPlugin

info = dict(
    name = 'myplugin',
    author = 'sa',
    description = 'a dummy',
    version = '1.0',
    category = 'general',
)


class MyPlugin(IPlugin):
    def say(self):
        print self.__class__.__name__

