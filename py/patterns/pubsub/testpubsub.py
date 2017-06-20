import pubsub2 as ps

ps.logToStdOut()

# setup a static tree:
class A(ps.Message):
    class B1(ps.Message):
        class C1(ps.Message):
            pass
    class B2(ps.Message):
        class C2(ps.Message):
            pass
# parts (or all) can also be dynamic
class D2(ps.Message):
    pass
A.B2.C2.D2 = D2
# setup chaining so messages go from most to least specific
ps.setupChaining(A)
assert A.getMessageType() == 'A'
assert A.B1.C1.getMessageType() == 'A.B1.C1'

callbackCalled = []
def callback(MsgType, data=None):
    global callbackCalled
    callbackCalled.append(data)
    print 'Msg type received:', MsgType.getMessageType()

# subscribe:
assert not A.hasListeners()
assert not A.B2.C2.hasListeners()
ps.subscribe(callback, A.B2.C2, id='callbacker1')
ps.subscribe(callback, A,       id='callbacker2')
assert A.B2.C2.hasListeners()
assert A.hasListeners()
assert not A.B2.hasListeners()
# subscribe multiple times, change id last time:
ps.subscribe(callback, A,       id='callbacker2')
ps.subscribe(callback, A,       id='callbacker3')
listeners = A.getListeners()
assert len(listeners) == 1
assert listeners[0].id == 'callbacker3'

# send:
callbackCalled = []
exceps = A.B2.C2().send('sender1')
if exceps:
    for exc in exceps:
        print exc
assert callbackCalled == [None, None]

# subscribe on another branch:
ps.subscribe(callback, A.B1.C1)
assert not A.B1.hasListeners()

# send another:
callbackCalled = []
ps.send(A.B1.C1('sender'))
assert callbackCalled == ['sender', 'sender']

# callbacks that generate exception:
def callbackBad(MsgType, data=None):
    raise RuntimeError('test')

ps.subscribe(callbackBad, A.B2, id='callbacker4')
try:
    ps.send(A.B2.C2.D2(), id='bad')
except ps.ListenerError, exc:
    print exc.exceptions

class wrapper:
    def __init__(self, fn):
        self.fn = fn
    def __call__(self):
        return self.fn()

ps.subscribe(wrapper(callback), A, id='callbacker5')
try:
    ps.send(A(), id='doa')
except ps.ListenerError, exc:
    print exc.exceptions[0]
