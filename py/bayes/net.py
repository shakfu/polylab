# basic bayes network
'''

P(A|B) = (P(B|A) * P(A)) / P(B)

'''
from textwrap import dedent
import pygraphviz as pgv

class Node(object):
    shape='circle'
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return self.name
    __str__ = __repr__

class Event(Node):
    color='cadetblue'
    def __init__(self, name, probability):
        self.name = name
        self.probability = probability

class ConditionalEvent(Event):
    color='cadetblue'


class Bayes(object):
    def __init__(self, p_a, pb_na, pb_a, **kwds):
        self.p_a   = p_a
        self.pb_na = pb_na
        self.pb_a  = pb_a
        self.p_na = 1 - self.p_a
        self.p_b  = (self.pb_a * self.p_a) + (self.pb_na * self.p_na)
        self.pa_b = (self.pb_a * self.p_a) / self.p_b        
        self.config = {
            'A'     : 'A',
            'B'     : 'B',
            'eventA': 'event A occurs',
            'eventB': 'event B occurs',
            'p_a'   : p_a,
            'pb_a'  : pb_a,
            'pb_na' : pb_na,
            'p_na'  : self.p_na,
            'p_b'   : self.p_b,
            'pa_b'  : self.pa_b,
        }
        self.config.update(kwds)
        
    def summary(self):
        print dedent("""
        P({A})\t= {p_a}\tprob {eventA}
        P(~{A})\t= {p_na}\tprob not {eventA}
        P(B|~A)\t= {pb_na}\tprob {eventB} given not {eventA}
        P(B|A)\t= {pb_a}\tprob {eventB} given {eventA}
        P({B})\t= {p_b}\tprob {eventB}
        """).format(**self.config)

    def visualize(self):
        g = pgv.AGraph(
            directed=True,
            rankdir='LR',
            ranksep='2',
        )
        g.node_attr['shape']     = 'circle'
        g.node_attr['fontsize']  = '9'
        g.edge_attr['arrowsize'] = '0.6'
        g.edge_attr['fontsize']  = '9'
        
        for node in self.nodes:
            g.add_node(node,
                shape=node.shape,
                color=node.color,
                style="filled")
        
        for p in self.plants:
            for m, cost in p.markets:
                g.add_edge(p, m, label=cost)
        
        g.layout(prog='dot')
        g.draw('graph.png')

if __name__ == '__main__':
    b = Bayes(p_a=0.5, pb_na=0.15, pb_a=0.75)
    b.summary()
    

    