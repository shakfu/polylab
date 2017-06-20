#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pygraphviz
import sap.i18n as i18n
_ = i18n.translate_decode

g = pygraphviz.AGraph(directed=True, ranksep='1.0')
style = dict(fontname='Monospace', fontsize=9)
g.node_attr.update(style)
g.edge_attr.update(style)

class Node(object):
    shape = 'box'
    def __init__(self, id):
        self.id = id
    def __unicode__(self):
        return self.id

class Decision(Node):
    shape = 'diamond'

sa =  Node(_('Project Accountant'))
pm =  Node(_('Project Manager'))
fm =  Node(_('Finance Manager'))
dgm = Node(_('Deputy General Manager'))


nodes = [sa, pm, fm, dgm]
for node in nodes:
    g.add_node(node.id, shape=node.shape)


g.add_edge(sa, pm,  label=_('(1) cash requirements'))
g.add_edge(pm, sa,  label=_('(2) agree'))
g.add_edge(sa, fm,  label=_('(3) request cash'))
g.add_edge(fm, dgm, label=_('(4) ask'))
g.add_edge(dgm, fm, label=_('(5) approve'))
g.add_edge(fm, sa,  label=_('(6) cash'))

g.layout(prog='dot')
g.draw('simple.png')

