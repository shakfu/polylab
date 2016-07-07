import sys
from pprint import pprint 
from pyparsing import Literal, Group, OneOrMore, restOfLine
from datetime import datetime


from os import listdir
from fnmatch import fnmatch
from os.path import join, abspath, basename, normpath, isfile, isdir, splitext

DEFAULT_IGNORE = ['*.pyc', '*.leo', '*.gif', '*.png', '*.jpg', '*.json']
DEFAULT_PARSE = { 'py':'shadow', 'txt':'auto', 'css':'auto', 'html':'auto'}


class LeoDirectory(object):
    def __init__(self, path, to_parse=DEFAULT_PARSE, to_ignore=DEFAULT_IGNORE,
            relative_paths=True, sort=True):
        self.path = path
        self.to_parse = to_parse
        self.to_ignore = to_ignore
        self.relative_paths = relative_paths
        self.sort = sort
        assert isdir(path), "%s is not a directory" % path

    def match_to_ignore(self, string):
        return any(fnmatch(string, p) for p in self.to_ignore)

    def is_ignorable(self, string):
        return any([string.startswith('.'), self.match_to_ignore(string)])

    def parse(self, path):
        name, ext = splitext(path)
        ext = ext[1:]
        return name, ext

    def is_parsable(self, path):
        if isfile(path):
            name, ext = self.parse(path)
            if ext in self.to_parse:
                return True
        if isdir(path):
            # is directory can pass
            g.es('---> '+ path)
            return True

        return False

    def headline_from_path(self, path):
        filename = basename(path)
        name, ext = self.parse(path)
        nodetype = self.to_parse[ext] if (ext in self.to_parse) else 'asis'
        return "@%s %s" % (nodetype, filename)

    def body_from_path(self, path):
        body = "@path %s" % normpath(path)
        return body

    def leo_from_directory(self, directory, parent=None, isroot=True, sort=True):
        if not self.relative_paths: directory = abspath(directory)
        if isroot:
            body = self.body_from_path(directory)
            c.setBodyString(p, body)

        dirlist = sorted(listdir(directory)) if sort else listdir(directory)
        for name in dirlist:
            if self.is_ignorable(name):
                g.es("ignore: "+name)
                continue
            path = join(directory, name)
            if isfile(path):
                g.es('file:', path)
                headline = self.headline_from_path(path)
                if parent:
                    node = parent
                else:
                    node = p
                child = node.insertAsLastChild()
                child.initHeadString(headline)
            else:
                g.es('dir:', path)
                headline = basename(path)
                body = self.body_from_path(path)
                if parent:
                    node = parent
                else:
                    node = p
                child = node.insertAsLastChild()
                child.initHeadString(headline)
                child.initBodyString(body)
                self.leo_from_directory(path, parent=child, isroot=False)

    def render(self):
        try:
            self.leo_from_directory(self.path, sort=self.sort)
        finally:
            c.redraw()

#LeoDirectory('.').render()


class ProtoType(object):
    @classmethod
    def clone(cls, name=None, **attributes):
        if not name:
            name=cls.__name__+'ProtoType'
        return type(name, (cls,), attributes)

# pyparsing objects
directive = Group(Literal('@') + restOfLine)
directives = OneOrMore(directive)

# utility functions
upper = lambda s: s.upper()
first = lambda lst: lst[0]
tail = lambda lst: lst[1:]
last = lambda lst: lst[-1]
rest = lambda lst: lst[:-1]
join = lambda lst, obj: '.'.join(lst + [str(obj)])
insert = lambda obj, lst: '.'.join([str(obj)] + lst)
split = lambda lst: lst.split('.')
inc = lambda lst, i: join(rest(lst), i)

def update(dikt, **kwds):
    dikt.update(kwds)
    return dikt

def include(dikt, exclude=[]):
    d = dikt.copy()
    for i in exclude:
        del d[i]
    return d

def display(i):
    print 'node:', i
    print 'parent:', i.parent
    print 'children:', i.children
    print

def level(current, prior=''):
    return '.'.join([str(current), str(prior)])

class up( Exception ): pass
class down( Exception ): pass

def outline( ):
    stack= [1]
    while True:
        try:
            yield '.'.join(str(s) for s in stack)
            stack[-1]+= 1
        except down:
            stack.append(1)
        except up:
            stack.pop(-1)
            stack[-1] += 1 


def pipe(funcs, arg):
    res = arg 
    for f in funcs:
        res = f(res)
    return res

def walk(node, of=None):
    '''tree walker applies decorator 'of' to root
    '''
    if of: yield of(node)
    else: yield node
    for child in node.children:
        child.parent = node
        for subsubchild in walk(child):
            yield subsubchild


# decorator functions
def hello(string):
    def _node_func(node):
        node.body_data = "Hello " + string
        return node
    return _node_func

def shadow(node):
    def _func(node):
        node.attributes['type'] = 'shadow'
        return node
    return _func(node)


def transform(head=[], body=[], children=[]):
    def f(node):
        if head:
            node.head = pipe(head, node)
        if body:
            node.body = pipe(body, node)
        if children:
            node.children = pipe(children, node)
        return node
    return f
    
# lambda decorators
addit = lambda a: lambda n: float(n.body_data.strip()) + a
foo = lambda b: lambda n: n + b 
exp = lambda e: lambda n: eval(e)
express = lambda e: lambda n: n.expression(e)
template = lambda type, namespaces: lambda n: n.template(type, namespaces)



class Node(ProtoType):
    def __init__(self, head=None, body=None, children=[], parent=None, 
                type=None, attributes={}, namespace={}):
        self.head = head
        self.body = body
        self.children = children
        self.parent = parent
        self.attributes = attributes
        self.namespace = update(namespace, node=self)
        self.head_code, self.head_data = self.parse_head(head)
        self.body_code, self.body_data = self.parse_body(body)
        self.views = {'head':None, 'body':None}
        self.level = level(1)
        self.write = lambda x: sys.stdout.write(x+'\n')

    def __repr__(self):
        name = self.__class__.__name__
        return "<%s '%s' [%s]>" % (name, self.level, self.head)

    def __iter__(self):
        yield self
        for child in self.children:
            for subchild in iter(child):
                yield subchild

    def __call__(self, other):
        other.data = self.data
        other.head = self.head
        other.body = self.body
        other.children = self.children
        other.type = self.type
        other.namespace.update(include(self.namespace, 
                               exclude=['self']))
        return other

    def show(self, level=1):
        self.showLevel(level)
        self.write('%s.  %s' % (self.head, level))
        for child in self.children:
            child.show(level + 1)
            
    def showLevel(self, level):
        for i in range(level):
            self.write('    ') 


    def parse_head(self, head):
        empty=''
        try:
            h = head.split()[0]
            if h.startswith('@'):
                return (empty, h[1:])
            else:
                return (h, empty)
        except IndexError:
            return (empty, empty)

    def parse_body(self, body):
        try:
            lst = body.split('\n\n')
            code, data = lst[0], lst[1:]
            data = "\n\n".join(data)
        except ValueError:
            code, data = '', body
        return code, data

    def expression(self, **kwds):
        self.namespace.update(kwds)
        return self.eval(self.body_code, self.namespace)
        
    def eval(self, src, namespace):
        # print directives.parseString(src)
        ds = [x for _,x in directives.parseString(src)]
        _locals = locals()#.copy()
        _locals.update(namespace)
        fs = (eval(x, globals(), _locals) for x in ds)
        return pipe(fs, self)
    
    def eval_head(self, **kwds):
        return self.eval(self.head_code, kwds)
        
    def eval_body(self, **kwds):
        return self.eval(self.body_code, kwds)


    def template(self, type, namespaces):
        if type == 'cheetah':
            from Cheetah.Template import Template
            template = Template(self.body_data, namespaces=namespaces)
            return str(template)
            


class Decorator(object):
    def __init__(self, node):
        self.node = node

    def __call__(self):
        return self.node

class FromThis(Decorator):
    def head(self, node):
        return node.head

    def body(self, node):
        return node.body

class FromParent(Decorator):
    def head(self, node):
        return node.parent.head

    def body(self, node):
        return node.parent.body


class ToThis(Decorator):
    def head(self, string):
        self.node.head = string
        return self.node

    def body(self, string):
        self.node.body = string
        return self.node

class ToParent(Decorator):
    def head(self, string):
        self.node.parent.head = string
        return self.node

    def body(self, string):
        self.node.parent.body = string
        return self.node

class To(Decorator):
    def __init__(self, node):
        super(To, self).__init__(node)
        self.this = ToThis(node)
        self.parent = ToParent(node)


class LeoNode(Node):
    def configure(self, p, c):
        self.p = p
        self.c = c

    def tree_from(self, node, parent=None, isroot=True):
        if isroot:
            self.p.insertAsLastChild()
            self.p.initHeadString(node.head)
            self.c.setBodyString(self.p, node.body)

        for childnode in node.children:
            if parent:
                parentnode = parent
            else:
                parentnode = self.p

            leonode = parentnode.insertAsLastChild()
            leonode.initHeadString(childnode.head)
            leonode.initBodyString(childnode.body)
            self.tree_from(child, parent=childnode)


# TESTING
# ---------------------------------------------------

b0="""
@py
print "hello"
@c

sweet
"""

b1="""
@hello('Leo!')

sweet
"""

b2="""
@addit(a=1)
@foo(b=2)

100
"""

b3="""
@template(type='cheetah', namespaces=[{'a':'big'}])

I really like the $a spaces. 
"""

b4="""
@template(type='cheetah', namespaces=[{'date':str(datetime.now())}])

Is it really a $date ?
"""

def test(verbose=False):

    def mkTree(body):
        # a tree
        return Node('root', body, [
            shadow(Node('branch1', 'b1', [])),
            Node('branch2', 'b2', [
                shadow(Node('leaf2.1', 'l1', [])),
                shadow(Node('leaf2.2', 'l2', [])),         
            ])
        ])

    # bodies=[b0, b1, b2, b3, b4]
    bodies=[b0]
    
    for i, body in enumerate(bodies):
        print '-'* 40
        print 'test', i+1
        print '-'* 40
        root = mkTree(body)
        root.show()
        # print 'root:', root
        # print 'root.children', root.children
        # print 'root.body_code:', root.body_code.lstrip()
        # print 'root.body_data before:', root.body_data.strip()
        # print 'root.eval_body():', root.eval_body()
        # print 'root.body_data after:', root.body_data

        print
        if verbose:
            print 'internal node iterator'.upper()
            for i in root:
                display(i)

        # print 'external node iterator'.upper()
        # print
        for i in walk(root):
            pass
            #display(i)

        node = root.children[0]

        print 'Redirection Test',
        to = To(node)
        this = FromThis(node)
        parent = FromParent(node)

        assert this.head(node) == 'branch1'
        assert upper(this.head(node)) == 'BRANCH1'

        node1 = to.parent.body(upper(this.head(node)))
        assert node1.parent.body == 'BRANCH1'
        # print node1

        node2 = transform(body=[this.head, upper, to.parent.body])(node)
        assert node2.parent.body == 'BRANCH1'
        print 'OK'


        

if __name__ == '__main__': pass
    #test()
    