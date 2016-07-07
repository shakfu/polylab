class Person(object):
    '''
    An example fact
    '''

    globalInClassA_1 = "a global"
    globalInClassA_2 = "another global"

    def __init__(self, property0 = None, property1 = None):
        '''
        Person initializer
        '''
        self.name = "shakeeb"
        self.__hiddenAttribute1 = "super secret hidden attribute. nah!"

        self.property0 = property0
        self.property1 = property1

        print "created an instance of ClassA"
    
    def __repr__(self):
        return '<Person: %s>'% self.property0

    @property
    def property0(self):
        return self._property0

    @property0.setter
    def property0(self, value):
        self._property0 = value

    @property
    def property1(self):
        return self._property1

    @property1.setter
    def property1(self, value):
        self._property1 = value

    def someMethod(self):
        print("someMethod called")

    @staticmethod
    def classAStaticMethod(self):
        print("classAStaticMethod called")

    def classASomeOtherMethod(self):
        print("classASomeOtherMethd called")

    def __classAHiddenMethod(self):
        print("classAHiddenMethod called")

