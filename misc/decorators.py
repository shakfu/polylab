from __future__ import with_statement
import time, itertools
import threading, multiprocessing
from decorator import decorator

def blocking(not_avail="Not Available"):
    def _blocking(f, *args, **kw):
        if not hasattr(f, "thread"): # no thread running
            def set_result(): f.result = f(*args, **kw)
            f.thread = threading.Thread(None, set_result)
            f.thread.start()
            return not_avail
        elif f.thread.isAlive():
            return not_avail
        else: # the thread is ended, return the stored result
            del f.thread
            return f.result
    return decorator(_blocking)

@blocking("Please wait ...")
def read_data():
    time.sleep(3) # simulate a blocking resource
    return "some data"


def test_blocking():
    print read_data() # data is not available yet

    time.sleep(1)
    print read_data() # data is not available yet

    time.sleep(1)
    print read_data() # data is not available yet

    time.sleep(1)
    print read_data() # data is not available yet

def on_success(result): # default implementation
    "Called on the result of the function"
    return result

def on_failure(exc_info): # default implementation
    "Called if the function fails"
    pass
    
def on_closing(): # default implementation
    "Called at the end, both in case of success and failure"
    pass

class Async(object):
    """
    A decorator converting blocking functions into asynchronous
    functions, by using threads or processes. Examples:

    async_with_threads =  Async(threading.Thread)
    async_with_processes =  Async(multiprocessing.Process)
    """

    def __init__(self, threadfactory):
        self.threadfactory = threadfactory

    def __call__(self, func, on_success=on_success,
                 on_failure=on_failure, on_closing=on_closing):
        # every decorated function has its own independent thread counter
        func.counter = itertools.count(1)
        func.on_success = on_success
        func.on_failure = on_failure
        func.on_closing = on_closing
        return decorator(self.call, func)

    def call(self, func, *args, **kw):
        def func_wrapper():
            try:
                result = func(*args, **kw)
            except:
                func.on_failure(sys.exc_info())
            else:
                return func.on_success(result)
            finally:
                func.on_closing()
        name = '%s-%s' % (func.__name__, func.counter.next())
        thread = self.threadfactory(None, func_wrapper, name)
        thread.start()
        return thread

async = Async(threading.Thread)
# async = Async(multiprocessing.Process)
datalist = [] # for simplicity the written data are stored into a list.

@async
def write(data):
    # append data to the datalist by locking
    with threading.Lock():
        time.sleep(1) # emulate some long running operation
        datalist.append(data)
    # other operations not requiring a lock here

write("data1")
time.sleep(.1) # wait a bit, so we are sure data2 is written after data1
write("data2")
time.sleep(2) # wait for the writers to complete
print datalist


