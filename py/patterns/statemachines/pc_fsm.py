class StateMachine:
    def __init__(self):
        self.handlers = {}
        self.endStates = []
        self.startState = None

    def add_state(self, name, handler, end_state=0):
        self.handlers[name] = handler
        if end_state:
            self.endStates.append(name)

    def set_start(self, name):
        self.startState = name

    def run(self, content):
        if self.startState in self.handlers:
            handler = self.handlers[self.startState]
        else:
            raise "InitializationError", ".set_start() has to be called before .run()"
        if not self.endStates:
            raise  "InitializationError", "at least one state must be an end_state"

        oldState = self.startState
        while 1:
            (newState, content) = handler(content, oldState)
            if newState in self.endStates:
                print "reached ", newState, "which is an end state"
                break 
            else:
                handler = self.handlers[newState]
            oldState = newState

positive_adjectives = ["great","super", "fun", "entertaining", "easy"]
negative_adjectives = ["boring", "difficult", "ugly", "bad"]

def transitions(txt, state):
    splitted_txt = txt.split(None,1)
    word, txt = splitted_txt if len(splitted_txt) > 1 else (txt,"")
    if state == "Start":
        if word == "Python":
            newState = "Python_state"
        else:
            newState = "error_state"
        return (newState, txt)
    elif state == "Python_state":
        if word == "is":
            newState = "is_state"
        else:
            newState = "error_state"
        return (newState, txt) 
    elif state == "is_state":
        if word == "not":
            newState = "not_state"
        elif word in positive_adjectives:
            newState = "pos_state"
        elif word in negative_adjectives:
            newState = "neg_state"
        else:
            newState = "error_state"
        return (newState, txt)
    elif state == "not_state":
        if word in positive_adjectives:
            newState = "neg_state"
        elif word in negative_adjectives:
            newState = "pos_state"
        else:
            newState = "error_state"
        return (newState, txt)
    

if __name__== "__main__":
    m = StateMachine()
    m.add_state("Start", transitions)
    m.add_state("Python_state", transitions)
    m.add_state("is_state", transitions)
    m.add_state("not_state", transitions)
    m.add_state("neg_state", None, end_state=1)
    m.add_state("pos_state", None, end_state=1)
    m.add_state("error_state", None, end_state=1)
    m.set_start("Start")
    m.run("Python is great")
    m.run("Python is difficult")
    m.run("Perl is ugly")
