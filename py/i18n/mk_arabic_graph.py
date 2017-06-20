#!/usr/bin/env python
# -*- coding: utf-8 -*-



import pygraphviz

g = pygraphviz.AGraph(directed=True)

a = 'شخص'.decode('utf8')
b = 'شركة'.decode('utf8')
c = 'العميل'.decode('utf8')


g.add_edge(a, b)
g.add_edge(b, c)
g.add_edge(c, a)

g.layout()
g.draw('simple.png')

