from libc.stdio cimport printf
from libc.stdlib cimport malloc, free

cdef extern from "chan.h":
    cdef struct chan_t:
        pass

    chan_t* chan_init(size_t capacity)
    void chan_dispose(chan_t* chan)
    int chan_close(chan_t* chan)
    int chan_is_closed(chan_t* chan)
    int chan_send(chan_t* chan, void* data)
    int chan_recv(chan_t* chan, void** data)
    int chan_size(chan_t* chan)



cdef extern from "pthread.h":
    cdef struct pthread_t:
        pass

    cdef struct pthread_attr_t:
        pass

    int pthread_create(
            pthread_t *thread,
            pthread_attr_t *attr,
            void *(*start_routine) (void *),
            void *arg
        )

ctypedef char *string


cdef enum State:
    OPEN = 0
    CLOSED = 1


#cdef chan_t* mychan


#cdef void* ping():
#    # Send blocks until receiver is ready.
#    chan_send(mychan, "ping")
#    return NULL


#def do_thread():
#    # Initialize unbuffered channel.
#    mychan = chan_init(0)

#    cdef pthread_t th
#    pthread_create(&th, NULL, ping, NULL)

#    # Receive blocks until sender is ready.
#    cdef void* msg
#    chan_recv(mychan, &msg)
#    printf("%s\n", msg)

#    # Clean up channel.
#    chan_dispose(mychan)


cdef class Channel:
    cdef chan_t* chan
    cdef void* msg

    def __init__(self):
        self.chan = chan_init(0)

    def __dealloc__(self):
        if self.chan is not NULL:
            chan_dispose(self.chan)

    def __repr__(self):
        return "<Channel: %s>" % id(self)

    def send(self, string msg):
        chan_send(self.chan, msg)
        printf("sent: %s\n", msg)

    def receive(self):
        chan_recv(self.chan, &self.msg)
        printf("received: %s\n", self.msg)

    def size(self):
        return chan_size(self.chan)

    def close(self):
        chan_close(self.chan)

    property is_closed:
        def __get__(self):
            return bool(chan_is_closed(self.chan))



cdef class BChannel:
    cdef chan_t* chan
    cdef int n_msgs
    cdef void* msg
    cdef char** msgs
    cdef readonly bint is_sent
    cdef readonly bint is_received
    cdef State state

    def __init__(self, list messages):
        self.n_msgs = len(messages)
        self.chan = chan_init(self.n_msgs)
        self.msgs = <char**>malloc(self.n_msgs * sizeof(char*))
        for i, msg in enumerate(messages):
            self.msgs[i] = <string>msg
        self.is_sent = False
        self.is_received = False
        self.state = OPEN

    def __dealloc__(self):
        if self.chan is not NULL:
            chan_dispose(self.chan)
        if self.msgs is not NULL:
            free(self.msgs)

    def __repr__(self):
        return "<Channel: %s>" % id(self)

    def send(self):
        if not self.is_sent:
            for i in range(self.n_msgs):
                chan_send(self.chan, self.msgs[i])
                printf("sent: %s\n", self.msgs[i])
            self.is_sent = True

    def receive(self):
        if not self.is_received:
            for i in range(self.n_msgs):
                chan_recv(self.chan, &self.msg)
                printf("received: %s\n", self.msg)
            self.is_received = True

    def size(self):
        return chan_size(self.chan)

    def close(self):
        chan_close(self.chan)
        self.state = CLOSED

    property is_closed:
        def __get__(self):
            return bool(chan_is_closed(self.chan))
