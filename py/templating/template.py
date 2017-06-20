import re


class Person(object): pass
p = Person()
p.name = 'sa'


class Template(object):
    def __init__(self, txt):
        self.txt = txt


def render(template):
      """
      Replace each $-substitutions in the given template string with
      the corresponding terminal control string (if it's defined) or
      '' (if it's not).
      """
      return re.sub(r'\$\$|\${\w+}', _render_sub, template)

def _render_sub(match):
    s = match.group()
    if s == '$$': 
        return s
    else:
        return getattr(p, s[2:-1])

print render("${name} is cool")
