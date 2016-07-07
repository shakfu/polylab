#!/usr/bin/env python
from operator import xor
from datetime import datetime
import time

from sqlalchemy import create_engine
from sqlalchemy import Table, Column, Integer, String, ForeignKey, Boolean
from sqlalchemy.orm import relationship, backref, sessionmaker
from sqlalchemy.ext.declarative import declarative_base, declared_attr

import pika

engine = create_engine('sqlite:///:memory:', echo=False)
#engine = create_engine('sqlite:///db.sqlite', echo=True)

# custom namespace for inserting builtins etc..
NAMESPACE = {
    'a': 2,
    'date': str(datetime.now().date()),
    'upper': lambda s: s.upper(),
}


# utility functions
def combine(*namespaces):
    """combines dictionaries
       last dict is prior.

        >>> combine({1:2}, {1:3})
        {1: 3}
    """
    combined = {}
    for namespace in namespaces:
        combined.update(namespace)
    return combined

# classes


class ORMBase(object):

    @declared_attr
    def __tablename__(cls):
        return cls.__name__.lower()

    ##__table_args__ = {'mysql_engine': 'InnoDB'}

    id = Column(Integer, primary_key=True)
    name = Column(String(50))

Base = declarative_base(cls=ORMBase)

Session = sessionmaker(bind=engine)
session = Session()


class Base(Base):
    __abstract__ = True

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "<%s:'%s'>" % (self.__class__.__name__, self.name)




class Rule(Base):
    parent_id = Column(Integer, ForeignKey('rule.id'))
    children = relationship("Rule",
        backref=backref('parent', remote_side='Rule.id'))
    condition = Column(String(100))
    children_relation = Column(String(3))

    def __init__(self, condition, children_relation=None, parent=None,
            **namespace):
        self.condition = condition
        self.children_relation = children_relation if children_relation else 'AND'
        self.parent = parent
        self.namespace = namespace
        self.namespace.update(dict(self=self))
        self.namespace.update(NAMESPACE)

    def evaluate(self, func=None):
        if func:
            yield func(self)
        else:
            yield self.value
        for i in self:
            if func:
                yield func(i)
            else:
                yield i.value

    @property
    def all(self):
        children_ts = list(self.evaluate(func=lambda x: x.eval_children()))
        return all([self.value] + children_ts)

    @property
    def any(self):
        children_ts = list(self.evaluate(func=lambda x: x.eval_children()))
        return any([self.value] + children_ts)

    def eval_children(self):
        values = (child.value  for child in self.children)
        relation = self.children_relation
        if relation == 'AND':
            return all(values)
        elif relation == 'OR':
            return any(values)
        elif relation == 'XOR':
            # must be only two values
            assert len(values) == 2
            x, y = values
            return xor(x, y)
        else:
            raise NotImplementError('rule relation must be: and|or|xor')

    def __iter__(self):
        yield self
        if self.children:
            for child in self.children:
                child.parent = self
                for grandchild in child:
                    yield grandchild

    @property
    def value(self):
            return eval(self.condition, globals(), self.namespace)

    def __call__(self):
        return self.value

    def __repr__(self):
        return "<%s:'%s'>" % (self.__class__.__name__,
                              self.condition)


class Event:
    def __init__(self, name):
        self.name = name
        self.listeners = {}

    def add(self, function, data=None):
        self.listeners[function] = data
    
    def delete(self, function):
        self.listeners.pop(function)

    def called(self, data=None):
        for function, d in self.listeners.items():
            if data is None:
                if d is None:
                    function()
                else:
                    if type(d) == type([]):
                        function(*d)
                    elif type(d) == type({}):
                        function(**d)
                    else:
                        function(d)
            else:
                if type(data) == type([]):
                    function(*data)
                elif type(data) == type({}):
                    function(**data)
                else:
                    function(data)


class EventManager:
    def __init__(self):
        self.events = {}

    def add_event(self, Event):
        self.events[Event.name] = Event

    def del_event(self, Event):
        self.events.pop(Event.name)

    def connect(self, event, function, data=None):
        self.events[event].add(function, data)

    def disconnect(self, event, function):
        self.events[event].delete(function)

    def signal(self, event, data=None):
        if data is None:
            self.events[event].called()
        else:
            self.events[event].called(data)

if __name__ == '__main__':
    Base.metadata.create_all(engine)

    # rules
    r1 = Rule('2 > 3')
    r2 = Rule('3 > 2', parent=r1, children_relation='OR')
    r3 = Rule('4+4 == 5', parent=r2)
    r4 = Rule('5 == 4+1', parent=r2)

    rules = [r1, r2, r3, r4]
    session.add_all(r for r in rules)

    session.commit()
    
    def func1(*args):
        print "Im the func1 with args: " + str(args)

    def func2(*args):
        print "Im the func2 with args: " + str(args)

    def func3(*args):
        print "Im the func3 with args: " + str(args)

    # Creating the eventManager
    em = EventManager()

    # Create some events
    event1 = Event('event1')
    event2 = Event('event2')

    em.add_event(event1)
    em.add_event(event2)

    # Connecting functions with events
    em.connect('event1', func3, [1,2])
    em.connect('event1', func2)
    em.connect('event2', func1)

    # Sending signals
    print "sending event1"
    em.signal('event1')
    print "sending event2"
    em.signal('event2')

    # sending signal with arguments
    print "sending event1"
    em.signal('event1', [1,2,3])

    connection = pika.BlockingConnection(pika.ConnectionParameters(
            host='localhost'))
    channel = connection.channel()

    channel.queue_declare(queue='task_queue', durable=True)
    print ' [*] Waiting for messages. To exit press CTRL+C'

    def callback(ch, method, properties, body):
        print " [x] Received %r" % (body,)
        time.sleep( body.count('.') )
        print " [x] Done"
        ch.basic_ack(delivery_tag = method.delivery_tag)

    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(callback,
                          queue='task_queue')

    channel.start_consuming()

