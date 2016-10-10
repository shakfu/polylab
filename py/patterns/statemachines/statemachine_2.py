from string import upper

class InitializationError(Exception): pass

class StateMachine(object):
    def __init__(self):
        self.handlers = {}
        self.start_state = None
        self.end_states = []

    def add_state(self, name, handler, end_state=0):
        name = upper(name)
        self.handlers[name] = handler
        if end_state:
            self.end_states.append(name)

    def set_start(self, name):
        self.start_state = upper(name)

    def run(self, cargo):
        try:
            handler = self.handlers[self.start_state]
        except:
            raise InitializationError, "must call .set_start() before .run()"

        if not self.end_states:
            raise InitializationError, "at least one state must be an end_state"

        while True:
            (new_state, cargo) = handler(cargo)
            if upper(new_state) in self.end_states:
                break
            else:
                handler = self.handlers[upper(new_state)]

GOOD = "GOOD"
BAD = "BAD"
UGLY = "UGLY"
END = "=END="

def handler(cargo):
    return ("OK", cargo)

def good_handler(cargo):
    print 'good:', cargo
    size = len(cargo)
    if size >= 2:
        return (UGLY, cargo)
    else:
        return (BAD, cargo)

def bad_handler(cargo):
    print 'bad:', cargo
    return (END, cargo + [100])

def ugly_handler(cargo):
    print 'ugly:', cargo
    return (END, cargo + [1000])


if __name__== "__main__":
    m = StateMachine()
    m.add_state(GOOD, good_handler)
    m.add_state(BAD, bad_handler)
    m.add_state(UGLY, ugly_handler)
    m.add_state(END, None, end_state=True)
    m.set_start(GOOD)
    m.run([1,2,3])
