from collections import OrderedDict

class Observer(object):
    def notify(self):
        print self.__class__.__name__

class ObserverA(Observer): pass
class ObserverB(Observer): pass


class Subject(object):
    observers = OrderedDict()
    
    def notify(self):
        for key in self.observers:
            self.observers[key].notify()


if __name__ == '__main__':
    sub = Subject()
    sub.observers['a'] = ObserverA()
    sub.observers['b'] = ObserverB()
    sub.notify()




