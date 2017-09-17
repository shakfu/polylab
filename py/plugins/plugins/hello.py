'''
A test plugin


'''

def info():
    """provides meta data about plugin
    """
    return 'world'


def register(app):
    app.registry['Hello'] = info
