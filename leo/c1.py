import sys, string
from datetime import datetime

def level(current, prior=''):
    return '.'.join([str(current), str(prior)])

def update(dikt, **kwds):
    dikt.update(kwds)
    return dikt

to = lambda node: node
to.stdout = lambda s: sys.stdout.write(s+'\n')

class Decorator(object):
    def __init__(self, node):
        self.node = node

    def __call__(self):
        return self.node

class ToBody(Decorator):
    def data(self, string):
        self.node.body.data = string
    def code(self, string):
        self.node.body.code = string
    def view(self, string):
        self.node.body.view = string

class ToHead(Decorator):
    def data(self, string):
        self.node.head.data = string
    def code(self, string):
        self.node.head.code = string
    def view(self, string):
        self.node.head.view = string

class ToThis(Decorator):
    def __init__(self, node):
        super(ToThis, self).__init__(node)
        self.head = ToHead(node)
        self.body = ToBody(node)

class ToParent(Decorator):
    def __init__(self, node):
        super(ToParent, self).__init__(node)
        self.head = ToHead(node.parent)
        self.body = ToBody(node.parent)

class To(Decorator):
    def __init__(self, node):
        super(To, self).__init__(node)
        self.this = ToThis(node)
        # self.parent = ToParent(node)

clean = lambda directive, txt: txt.replace(directive,'').lstrip().strip()

def block(directive, txt):
    lines = txt.split('\n')
    _block = []
    in_block = False
    for i in lines:
        if i.startswith('@'): in_block = False
        if in_block: _block.append(i)
        if i.startswith(directive): in_block = True
    code = '\n'.join(_block)
    return code



DIRECTIVES = {
    #symbol     type/name   clean_func
    '@py':     ('py-expr',  clean), 
    '@py:':    ('py-suite', block),
    '@pipe':   ('pipe',     clean),
    '@imports':('imports',  block),
}


class LeoObject(object):    
    def __init__(self, txt, namespace={}):
        self.txt = txt
        self.code = ''
        self.data = ''
        self.type = 'user-data'
        self._cache = {}
        self.namespace = namespace
        self.parse()

    @property
    def view(self):
        if not 'view' in self._cache:
            self.render()
        return self._cache['view']
    
    def render(self, **kwds):
        self._cache['view'] = self.eval(**kwds)

    def parse(self):
        '''returns (code, type, data) tuple of object
        '''
        is_code = False
        for directive in DIRECTIVES:
            if self.txt.startswith(directive):
                self.__parse(directive)
                is_code = True

        if not is_code:
            self.code, self.type, self.data = ('', 'user-data', self.txt)  

    def __parse(self, directive):
        self.type, clean_func = DIRECTIVES[directive]
        self.code = clean_func(directive, self.txt)

    def eval(self, **kwds):
        dispatch = {
            'imports': self.eval_py_imports,
            'py-expr': self.eval_py_expression,
            'py-suite': self.eval_py_suite,
            'pipe': self.eval_leo_pipe,
        }
        return dispatch[self.type](self.code, **kwds)

    def eval_py_imports(self, code, **kwds):
        ''' @imports
        '''
        self.eval_py_suite(code, **kwds)

    def eval_py_expression(self, code, **kwds):
        ''' @py <expression>
        '''
        self.namespace.update(kwds)
        return eval(code, globals(), self.namespace)

    def eval_py_suite(self, code, **kwds):
        '''
            @py
            <suite>
            @
        '''
        self.namespace.update(kwds)
        exec code in self.namespace

    def eval_leo_pipe(self, code, **kwds):
        '''
            @pipe node.body | upper | to.body
            @pipe node.body | upper(s=1) | to.body
        '''
        self.namespace.update(kwds)
        lst = [i.strip() for i in code.split('|')]
        arg, funcs = lst[0], lst[1:]
        # func composer over arg
        def pipe(funcs, arg):
            res = arg 
            for f in funcs:
                res = f(res)
            return res    
        arg = eval(arg, globals(), self.namespace)
        funcs = (eval(f, globals(), self.namespace) for f in funcs)
        return pipe(funcs, arg)
    
    def template(self, **kwds):
        template = string.Template(self.data)
        return template.substitute(**self.namespace)

class Head(LeoObject): pass
class Body(LeoObject): pass

class Node(object):
    def __init__(self, head=None, body=None, children=[], parent=None, 
                type=None, attributes={}, namespace={}):
        self.namespace = update(namespace, node=self, to=To(self))
        self.head = Head(head, self.namespace)
        self.body = Body(body, self.namespace)
        self.children = children
        self.parent = parent
        self.attributes = attributes
        self.level = level(1)
        self.write = lambda x: sys.stdout.write(x+'\n')
        self.type = None

    @classmethod
    def clone(cls, name=None, **attributes):
        if not name:
            name=cls.__name__+'Node'
        return type(name, (cls,), attributes)

    def __iter__(self):
        yield self
        for child in self.children:
            for subchild in iter(child):
                yield subchild

    def __repr__(self):
        name = self.__class__.__name__
        return "<%s '%s' [%s]>" % (name, 
            self.level, self.head)

    def show(self, level=1):
        for i in range(level):
            self.write('    ')
        self.write('%s.  %s' % (self.head, level))
        for child in self.children:
            child.show(level + 1)

    def render(self, **kwds):
        self.head.render(**kwds)
        self.body.render(**kwds)
        
    def update_from(self, *nodes):
        for node in nodes:
            self.namespace.update(node.namespace)

    def update_to(self, *nodes):
        for node in nodes:
            node.namespace.update(self.namespace)            

# TESTING
# ---------------------------------------------------
a0 = "@py 1+1"
b0 = """
@py:
a = 10
node.body.data = a * 2132
@
bullcrap
""".lstrip()

def mkTree(body):
    # a tree
    return Node(a0, body, [
        Node('branch1', 'b1', []),
        Node('branch2', 'b2', [
            Node('leaf2.1', 'l1', []),
            Node('leaf2.2', 'l2', []),         
        ])
    ])

def out(node, kind):
    obj = getattr(node, kind)
    attrs = ['type', 'code', 'data', 'view', 'data']
    for attr in attrs:
        print '%s.%s:' % (kind, attr), getattr(obj, attr)

# bodies=[b0, b1, b2, b3, b4]
bodies=[b0]

for i, body in enumerate(bodies):
    print '-'* 40
    print 'test', i+1
    print '-'* 40
    root = mkTree(body)
    print root.show()
    # print 'root:', root
    # print 'root.children', root.children
    # out(root, 'body')
    # out(root, 'head')

