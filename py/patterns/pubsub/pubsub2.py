'''
This provides a simple but versatile messaging system, ideal for decoupling 
parts of your application: senders and listeners don't need to know about 
each other. 

Usage by example: 

First, create a file yourmsgs.py that defines the 
message 'types' available in your application:

# yourmsgs.py:
import pubsub2 as ps
class Foo(ps.Message):
    'Basic type of message'
    class Bar(ps.Message):
        """More specific type of message, "a kind of Foo message". Yes, 
        we use membership rather than inheritence, to show message 
        subtyping."""
        pass

Register some callbacks. E.g. if you have GUI module in which a GUI panel 
is listening for some changes from a controller:
# gui.py
import yourmsgs as msgs
def handler(MsgType, arg1):
    print 'Got msg of type %s, data=%s' % (MsgType.getMessageType(), arg1)
msgs.subscribe(handler, msgs.Foo)

Somewhere else in your application, generate a message:
# controller.py
import yourmsgs as msgs
msgs.send( msgs.Foo.Bar('hello world') )

The send() will send the Foo.Bar instance to handler(), which will cause 

    Got msg of type Foo.Bar, data='hello world'

to be output. Yet controller.py and gui.py know nothing about each other. 

This is just an example of what is possible with pubsub2. In addition, 

- the class mechanism allows pychecker/pylint etc to check for spelling 
  mistakes etc
- it also facilitates documentation of the message types and data that will
  be carried from sender to listeners
- it supports debugging via logging (which is normally off -- turn it on
  by specifying an id to send() or subscribe())
- it supports multi-level messaging, whereby listeners of a more general 
  topic (type) get messages sent to listeners of more specific type; this 
  would mean that listeners of Foo would also have received the data, 
  assuming that setupChaining(Foo) had been called once in yourmsgs.py
- it handles exceptions being raised in listeners, by catching them and
  returning a list of exceptions raised during the send; this makes sense 
  as the order of delivery of message is not specified, hence the listeners 
  can be thought of as running in parallel, and one throwing an exception 
  should not prevent others from receiving the message. 

In general: 

- in a file, create your 'static' messaging classes
- import that file into your application, ie in files where messages are
  generated, and in files where listeners must register; e.g. import mymsgs
- to send a message of type A.B.C, just give an instance of the required 
  message class to send(), as in mymsgs.A.B.C().send().
- to receive messages of type A.B.C, just register a "callback" that will
  be called when a message is sent. The only requirement on the callback
  is that it has at least one argument, the class of the message being 
  received.
- can use setLog() to change the destination of output messages. Most output
  messages are only generated when an id is specified (ie there is 
  no output for "anonymous" messages

Note: Listeners (callbacks) are held only by weak reference, which in 
general is adequate (this prevents the messaging system from keeping alive
callables that are no longer used by anyone). However, if you want the 
callback to be a wrapper around one of your functions, that wrapper must 
be stored somewhere so that the weak reference isn't the only reference 
to it (which will cause it to die). 

Todo: 
- include the traceback in the exception info
- support subtopic access in supertopic messages

:Author:      Oliver Schoenborn
:Since:       Apr 2004
:Version:     $Id: pubsub.py,v 1.8 2006/06/11 00:12:59 RD Exp $
:Copyright:   \(c) 2007 Oliver Schoenborn
:License:     Python Software Foundation

'''

import weakmethod


class NullLog:
    '''A log output that gets rid of everything given to it (doesn't print
    anything, like /dev/null on *nix systems). '''
    def write(self, msg):
        pass

_nullLog = NullLog()

_log = _nullLog.write

def setLog(writer):
    '''Several functions/methods output (or can be told to output) some 
    information messages. By default, a NullLog() is used, so no output
    is visible. The 'writer' can be a callable, or not; if latter, it is 
    assumed to be a file object (has a write() method).'''
    global _log
    if callable(writer):
        _log = writer
    else:
        _log = writer.write
        

def logToStdOut():
    '''Call this to cause log messages, if any, to be sent to stdout. 
    The caller could also use setLog(sys.stdout). '''
    import sys
    setLog(sys.stdout.write)


class ListenerError(RuntimeError):
    '''Gets raised when one or more of the listeners raise an exception. 
    The list of all exceptions raised during the send() are put in the
    exceptions data member.'''
    def __init__(self, exceps):
        self.exceptions = exceps
        RuntimeError.__init__(self, '%s exceptions raised' % len(exceps))
    

def send(msg, id=None):
    '''Send an instance of subclass of Message to its registered 
    listeners. If id is given, 
    it is used to identify the sender in a more human-readable fashion
    in log messages. Note that log messages are only produced if id is
    given, and the only exception that is allowed to escape is 
    ListenerException, which will contain a list of all exceptions 
    raised during the send().'''
    msg.send(id)


def subscribe(listener, MsgClass, id=None):
    '''Subscribe listener to messages of type MsgClass. If id is given, 
    it is used to identify the listener in a more human-readable fashion
    in log messages. Note that log messages are only produced if id is
    given. '''
    MsgClass.subscribe(listener, id)


def setupChaining(RootClass):
    '''Call this function to setup RootClass, and all its children, 
    so that messages of a subtype also get sent to messages of 
    super type. '''
    RootClass.setupChaining()


class Listener:
    '''
    Represent a listener of messages of a given class. 
    Note that callback must give callable(callback) == True.
    '''
    
    def __init__(self, callback, id=None):
        assert callable(callback), '%s is not callable' % callback
        self.__callable = weakmethod.getWeakRef(callback)
        self.id = id
        self.weakID = str(self) # save this now in case callable weak ref dies
        
    def __call__(self, *args, **kwargs):
        cb = self.__callable()
        if cb:
            cb(*args, **kwargs)
        else:
            raise RuntimeError('Callback %s no longer exists' % self.weakID)
        
    def __eq__(self, rhs):
        return self.__callable == rhs
    
    def __str__(self):
        return self.id or str(self.__callable())


class Message:
    '''
    Represent a message to be sent from a sender to a listener. 
    This class should be derived, and the derived class should 
    be documented, to help explain the message and its delivery. 
    E.g. provide a documented __init__() to help explain the data
    carried by the message, the purpose of this type of message, etc.
    '''
    
    _listeners   = None # class-wide registry of listeners
    _parentClass = None # class-wide parent of messages of our type
    _type = 'Message'   # a string for type
    
    def __init__(self, *args, **kwargs):
        '''The args and kwargs will be given to listener callback when 
        message delivered. Subclasses of Message can define an __init__
        that has specific args/kwargs to better document the messaging
        system.'''
        self.__args   = args
        self.__kwargs = kwargs
    
    def send(self, senderID=None):
        '''Send this instance to registered listeners. If senderID is 
        given, some messages will be sent to log(), using it to identify 
        the caller. Returns a list of all exceptions that occurred when 
        delivering to listener(s). '''
        exceps = self.__deliver(senderID)
        
        # make parents up chain send with same data
        parent = self._parentClass
        while parent is not None:
            msg = parent(*self.__args, **self.__kwargs)
            parent, exceptInfo = msg.sendSpecific(senderID)
            exceps.extend(exceptInfo)
            
        if exceps:
            raise ListenerError(exceps)
        
    def sendSpecific(self, senderID=None):
        '''Send self to listeners. If senderID is given, some messages 
        will be sent to log(), using it to identify the caller. 
        Returns self's parent message class and a list of exceptions 
        raised by listeners.'''
        exceptInfo = self.__deliver(senderID)
        return self._parentClass, exceptInfo
        
    def __deliver(self, senderID):
        if not self._listeners:
            if senderID: 
                _log( 'No listeners of %s for sender "%s"\n' 
                    % (self.getMessageType(), senderID) )
            return []
        
        if senderID:
            _log( 'Message of type %s from sender "%s" should reach %s listeners\n'
                % (self.getMessageType(), senderID, len(self._listeners)) )
            
        received = 0
        exceptInfo = []
        for listener in self._listeners:
            if senderID or listener.id:
                _log( 'Sending message from sender "%s" to listener "%s"\n' 
                    % (senderID or 'anonymous', str(listener)))
                    
            try:
                listener(self.__class__, *self.__args, **self.__kwargs)
                received += 1
            except Exception, exc:
                exceptInfo.append(exc)
    
        if senderID:
            _log( 'Delivered message from sender "%s" to %s listeners\n'
                % (senderID, received))
        
        return exceptInfo
    
    @classmethod 
    def getMessageType(cls):
        return cls._type
    
    @classmethod
    def hasListeners(cls):
        '''Return True only if at least one listener is registered 
        for this class of messages.'''
        return cls._listeners is not None
    
    @classmethod
    def subscribe(cls, who, id=None):
        '''Subscribe who to message of our class.'''
        if id:
            _log( 'Subscribing %s to messages of type %s\n' 
                % (id or who, cls.getMessageType()) )
            
        listener = Listener(who, id)
        if cls._listeners is None: 
            cls._listeners = [listener]
            
        else:
            if listener in cls._listeners:
                idx = cls._listeners.index(listener)
                origListener = cls._listeners[idx]
                if listener.id != origListener.id:
                    _log('Changing id of Listener "%s" to "%s"\n' 
                        % (origListener.id or who, listener.id or 'anonymous'))
                    origListener.id = listener.id
                    
                elif listener.id:
                    _log( 'Listener %s already subscribed (as "%s")\n' % (who, id) )
                
            else:
                cls._listeners.append( listener )
                
    @classmethod
    def getListeners(cls):
        '''Get list of listeners for this message class. Each 
        item is an instance of Listener.'''
        #_log( 'Listeners of %s: %s' % (cls, cls._listeners) )
        return cls._listeners
    
    @classmethod
    def setupChaining(cls, parents=None):
        '''Chain all the message classes children of cls so that, when a 
        message of type 'cls.childA.subChildB' is sent, listeners of 
        type cls.childA and of type cls get it too. '''
        # parent:
        if parents:
            cls._parentClass = parents[-1]
            lineage = parents[:] + [cls]
            cls._type = '.'.join(item.__name__ for item in lineage)
            _log( '%s will chain up to %s\n' 
                % (cls._type, cls._parentClass.getMessageType()) )
        else:
            cls._parentClass = None
            lineage = [cls]
            cls._type = cls.__name__
            _log( '%s is at root (top) of messaging tree\n' % cls._type )
        
        # go down into children:
        for childName, child in vars(cls).iteritems():
            if (not childName.startswith('_')) and issubclass(child, Message):
                child.setupChaining(lineage)

