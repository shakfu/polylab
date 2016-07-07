import yaml
import easydict
from sap.ext.enhpath import path
from os.path import join

ROOT = path(__file__).abspath().dirname()
DEFAULT = join(ROOT, 'config.yml')

class Config(object):
    def __init__(self, filename=DEFAULT):
        self.filename = filename
        with open(filename) as f:
            self.db = easydict.EasyDict(yaml.load(f.read()))
            # self.ROOT = path(filename).abspath().dirname().dirname()
            self.ROOT = path('/home/user/app')
            self.OUTPUT = self.ROOT / 'output'

    def __getattr__(self, attr):
        return getattr(self.db, attr)

    def path(self, path, *args, **kwds):
        return self.ROOT / path.format(*args, **kwds)
