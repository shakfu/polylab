import re

class _multimap:
    """Helper class for combining multiple mappings.

    Used by .substitute() to combine the mapping and keyword
    arguments.
    """
    def __init__(self, primary, secondary):
        self._primary = primary
        self._secondary = secondary

    def __getitem__(self, key):
        try:
            return self._primary[key]
        except KeyError:
            return self._secondary[key]


class _TemplateMetaclass(type):
    pattern = r"""
    %(delim)s(?:
      (?P<escaped>%(delim)s) |   # Escape sequence of two delimiters
      (?P<named>%(id)s)      |   # delimiter and a Python identifier
      {(?P<braced>%(id)s)}   |   # delimiter and a braced identifier
      (?P<invalid>)              # Other ill-formed delimiter exprs
    )
    """

    def __init__(cls, name, bases, dct):
        super(_TemplateMetaclass, cls).__init__(name, bases, dct)
        if 'pattern' in dct:
            pattern = cls.pattern
        else:
            pattern = _TemplateMetaclass.pattern % {
                'delim' : re.escape(cls.delimiter),
                'id'    : cls.idpattern,
                }
        cls.pattern = re.compile(pattern, re.IGNORECASE | re.VERBOSE)


class Template:
    """A string class for supporting $-substitutions."""
    __metaclass__ = _TemplateMetaclass

    delimiter = '$'
    idpattern = r'[_a-z][_a-z0-9\.]*'

    def __init__(self, template):
        self.template = template

    # Search for $$, $identifier, ${identifier}, and any bare $'s

    def _invalid(self, mo):
        i = mo.start('invalid')
        lines = self.template[:i].splitlines(True)
        if not lines:
            colno = 1
            lineno = 1
        else:
            colno = i - len(''.join(lines[:-1]))
            lineno = len(lines)
        raise ValueError('Invalid placeholder in string: line %d, col %d' %
                         (lineno, colno))

    def substitute(self, *args, **kws):
        if len(args) > 1:
            raise TypeError('Too many positional arguments')
        if not args:
            mapping = kws
        elif kws:
            mapping = _multimap(kws, args[0])
        else:
            mapping = args[0]
        # Helper function for .sub()
        def convert(mo):
            # Check the most common path first.
            named = mo.group('named') or mo.group('braced')
            if named is not None:
                # We use this idiom instead of str() because the latter will
                # fail if val is a Unicode containing non-ASCII characters.
                if '.' in named:
                    return eval(named, kws)
                else:
                    val = mapping[named]
                    return '%s' % (val,)
                    
            if mo.group('escaped') is not None:
                return self.delimiter
            if mo.group('invalid') is not None:
                self._invalid(mo)
            raise ValueError('Unrecognized named group in pattern',
                             self.pattern)
        return self.pattern.sub(convert, self.template)
        
class Template(object):
    _pattern = re.compile(r"\[\[|]]|\{|\}")    
    _lookup = {
        "[[": "{",
        "]]": "}",
        "{": "{{",
        "}": "}}",
    }
    
    def __init__(self, txt):
        self.txt = txt
    
    def _substitute(self, m):
        return self._lookup[m.group()]
    
    def render(self, *args, **kwds):
        return self._pattern.sub(
            self._substitute, self.txt).format(*args, **kwds)


def test_templates_2():

    _lookup = {
        "[[": "{",
        "]]": "}",
        "{": "{{",
        "}": "}}",
    }

    PATTERN = re.compile(r"\[\[|]]|\{|\}")

    def _substitute(m):
        return _lookup[m.group()]

    sub = lambda m: _lookup[m.group()]

    def custom_format(template, *args, **kw):
        return (PATTERN.sub(sub, template).format(*args, **kw))

    code = "class [[0]]Model { public bool IsModel(){ return a[42] || true; } }"
    print custom_format(code, "My") 

def test_Template():
    class P: pass
    p = P()
    p.name = 'ak'
    code = "{ hello there [[o.name]] }"
    print Template(code).render(o=p) 


def test_templates():
    class P: pass
    p = P()
    p.name = 'ak'
    txt = 'hello there ${o.name}'
    t = Template(txt)
    assert t.substitute(o=p) == 'hello there ak'

if __name__ == '__main__': 
    test_Template()
