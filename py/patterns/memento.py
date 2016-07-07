
class Originator(object):
    state = 'default'
    class Memento:
        def __init__(self, state):
            self.state = state

    def save(self):
        return self.Memento(self.state)

    def restore(self, memento):
        self.state = memento.state


if __name__ == '__main__':
    saved = []
    orig = Originator()
    orig.state = "state 1"
    saved.append(orig.save())
    orig.state = "state 2"
    saved.append(orig.save())
    orig.state = "state 3"
    orig.restore(saved[0])
    assert orig.state == "state 1"


