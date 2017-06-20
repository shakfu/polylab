from string import upper

class InitializationError(Exception): pass

class StateMachine(object):
    def __init__(self):
        self.handlers = {}
        self.startState = None
        self.endStates = []

    def add_state(self, name, handler, end_state=0):
        name = upper(name)
        self.handlers[name] = handler
        if end_state:
            self.endStates.append(name)

    def set_start(self, name):
        self.startState = upper(name)

    def run(self, cargo):
        try:
            handler = self.handlers[self.startState]
        except:
            raise InitializationError, "must call .set_start() before .run()"

        if not self.endStates:
            raise InitializationError, "at least one state must be an end_state"

        while True:
            (newState, cargo) = handler(cargo)
            if upper(newState) in self.endStates:
                break 
            else:
                handler = self.handlers[upper(newState)]

#from statemachine import StateMachine

def ones_counter(val):
    print "ONES State: ",
    while True:
        if val <= 0 or val >= 30:
            newState =  "Out_of_Range" ; break
        elif 20 <= val < 30:
            newState =  "TWENTIES"; break
        elif 10 <= val < 20:
            newState =  "TENS"; break
        else:
            print " @ %2.1f+" % val,
            val = math_func(val)
            print " >>"
    return (newState, val)

def tens_counter(val):
    print "TENS State: ",
    while True:
        if val <= 0 or val >= 30:
            newState =  "Out_of_Range"; break
        elif 1 <= val < 10:
            newState =  "ONES"; break
        elif 20 <= val < 30:
            newState =  "TWENTIES"; break
        else:
            print " #%2.1f+" % val,
            val = math_func(val)
            print " >>"
    return (newState, val)

def twenties_counter(val):
    print "TWENTIES State:",
    while 1:
        if val <= 0  or  val >= 30:
            newState =  "Out_of_Range"; break
        elif 1 <= val < 10:
            newState =  "ONES"; break
        elif 10 <= val < 20:
            newState =  "TENS"; break
        else:
            print " *%2.1f+" % val,
            val = math_func(val)
            print " >>"
    return (newState, val)

def math_func(n):
    from math import sin
    return abs(sin(n))*31

if __name__== "__main__":
    m = StateMachine()
    m.add_state("ONES", ones_counter)
    m.add_state("TENS", tens_counter)
    m.add_state("TWENTIES", twenties_counter)
    m.add_state("OUT_OF_RANGE", None, end_state=1)
    m.set_start("ONES")
    m.run(1)


