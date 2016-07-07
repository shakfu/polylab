import random
import simpy
from simpy.util import any_of, all_of

def hello(event, evt_type, value):
    print "hello world"
    print 'event:', event
    print 'evt_type:', evt_type
    print 'value:', value

class MyEvent(simpy.core.Event): pass

class Machine(object):
    def __init__(self, env, name, duration):
        self.env = env
        self.name = name
        self.duration = duration
        self.action = env.start(self.run())
        self.myevent = MyEvent(env)
        self.myevent.callbacks.append(hello)

    def run(self):
        while True:
            print 'started at %d' % env.now
            try:
                yield env.start(self.engage(self.duration))
            except simpy.Interrupt:
                print 'was interrupted...'
            print 'start finishing at %d' % env.now
            yield env.timeout(2)
            
    def engage(self, duration):
        yield self.env.timeout(duration)

def manager(env, machine):
    yield env.timeout(3)
    machine.myevent.succeed()
    machine.action.interrupt()

env = simpy.Environment()

for i in range(10):
    env.start(manager(env, Machine(env, 'p%s'%str(i), 10+i)))
    
simpy.simulate(env, until=100)