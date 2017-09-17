from pluginbase import PluginBase

plugin_base = PluginBase(package='example.plugins')

plugin_source = plugin_base.make_plugin_source(searchpath=['./plugins'])

def test_plugins():
    results = []
    for name in plugin_source.list_plugins():
        plugin = plugin_source.load_plugin(name)
        results.append((name, plugin.info()))
    assert results == [('hello', 'world')]

def test_plugin_import():
    with plugin_source:
        from example.plugins import hello
    assert hello.info() == 'world'

def test_plugin_import_register():
    class Application:
        registry = {}
    app = Application()
    with plugin_source:
        from example.plugins import hello
    hello.register(app)
    assert app.registry['Hello']() == 'world'
