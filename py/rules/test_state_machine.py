from state_machine import *

@acts_as_state_machine
class Person():
    name = 'Billy'

    sleeping = State(initial=True)
    running = State()
    cleaning = State()

    run = Event(from_states=sleeping, to_state=running)
    cleanup = Event(from_states=running, to_state=cleaning)
    sleep = Event(from_states=(running, cleaning), to_state=sleeping)

    @before('sleep')
    def do_one_thing(self):
        print "{} is sleepy".format(self.name)

    @before('sleep')
    def do_another_thing(self):
        print "{} is REALLY sleepy".format(self.name)

    @after('sleep')
    def snore(self):
        print "Zzzzzzzzzzzz"

    @after('sleep')
    def big_snore(self):
        print "Zzzzzzzzzzzzzzzzzzzzzz"

person = Person()
print person.current_state == Person.sleeping       # True
print person.is_sleeping                            # True
print person.is_running                             # False
person.run()
print person.is_running                             # True
person.sleep()

# Billy is sleepy
# Billy is REALLY sleepy
# Zzzzzzzzzzzz
# Zzzzzzzzzzzzzzzzzzzzzz

print person.is_sleeping                            # True
