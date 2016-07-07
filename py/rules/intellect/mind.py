import sys, logging

from intellect.Intellect import Intellect
from intellect.Intellect import Callable

from classes import Person

class MyIntellect(Intellect):

        @Callable
        def bar(self):
                self.log(logging.DEBUG, ">>>>>>>>>>>>>>  called MyIntellect's bar method as it was decorated as callable.")

if __name__ == "__main__":

        # set up logging
        logging.basicConfig(level=logging.DEBUG,
                format='%(asctime)s %(name)-12s%(levelname)-8s%(message)s',
                #filename="rules.log")
                stream=sys.stdout)

        print "*"*80
        print """create an instance of MyIntellect extending Intellect, create some facts, and exercise the MyIntellect's ability to learn and forget"""
        print "*"*80

        myIntellect = MyIntellect()

        policy_a = myIntellect.learn(Intellect.local_file_uri("./test_a.policy"))

        p1 = Person(property0='shakeeb')
        p2 = Person(property0='riccardo')
        myIntellect.knowledge = [p1, p2]

        myIntellect.reason(["test_a" ])

        myIntellect.forget_all()