class IState(object):
    def write_name(self, state_context, name):
        "single method of state interface"

class StateA(IState):
    def write_name(self, state_context, name):
        print "A", name
        state_context.state = StateB()

class StateB(IState):
    count = 0
    def write_name(self, state_context, name):
        print "B", name, self.count
        self.count += 1
        if self.count > 1:
            state_context.state = StateA()

class StateContext(object):
    def __init__(self, state=None):
        self.state = state if state else StateA()

    def write_name(self, name):
        self.state.write_name(self, name)


if __name__ == '__main__':
    context = StateContext()
    for day in ["mon", "tue", "wed", "thu", "fri", "sat", "sun"]:
        context.write_name(day)


