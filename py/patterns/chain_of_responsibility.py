class AbstractLogger(object):
    ERR = 3
    NOTICE = 5
    DEBUG = 7

    _mask = 10

    def message(self, msg, priority):
        if priority <= self._mask:
            #print 'priority <= self._mask'
            self.write(msg)
        
        if hasattr(self, 'next'):
            #print "delegating to", self.next
            self.next.message(msg, priority)

    def write(self, msg):
        print msg

    def __repr__(self):
        return "<{0}'{1}'{2}>".format(
            self.__class__.__name__,
            self._mask,
            "->"+str(id(self.next)) if hasattr(self, 'next') else ''
        )

class Logger(AbstractLogger):
    def __init__(self, mask):
        self._mask = mask    

class StdoutLogger(Logger):
    def write(self, msg):
        print "to stdout:", msg

class StderrLogger(Logger):
    def write(self, msg):
        print "to stderr:", msg

def test():
    logger = StdoutLogger(Logger.DEBUG)
    logger.next = StderrLogger(Logger.NOTICE)

    # chain it
    logger.message("hello there", Logger.DEBUG)
    return logger

log = test()