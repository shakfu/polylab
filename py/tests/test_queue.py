import collections
import operator
import datetime

class Queue(object):
    def __init__(self, maxsize):
        self.data = collections.deque(maxsize * [(None, None)])

    def add(self, obj):
        self.data.popleft()
        self.data.appendleft((obj, datetime.datetime.now()))

    def __iter__(self):
        it = iter(self.data)
        return it.dropwhile(lambda x: x[1] is None, self.data)

class PriorityQueue(object):
    def __init__(self, maxsize):
        self.data = collections.deque(maxsize * [(None, None, None)])

    def add(self, obj, priority):
        self.data.popleft()
        self.data.append((obj, datetime.datetime.now(), priority))

    def __repr__(self):
        return repr(self.data)

    def __iter__(self):
        it = iter(self.data)
        return sorted(it.dropwhile(lambda x: x[1] is None, self.data),
                      key=operator.itemgetter(2))


q = PriorityQueue(3)
