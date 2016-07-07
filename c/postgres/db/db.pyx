cdef extern from "libpq-fe.h":
    pass

cdef extern from "query.h":
    ctypedef struct PGconn:
        pass
    ctypedef struct PGresult:
        pass
    
    void db_exit(PGconn * conn)
    PGconn * db_connect()
    int db_command(PGconn * conn, char * qstring)
    PGresult * db_query(PGconn * conn, char * qstring)
    void db_display(PGresult * res)
    void db_analyze(PGresult * res)

def add(int x, int y):
    return x+y

ctypedef char* string

cdef enum State:
    READY = 0
    OK    = 1
    ERROR = 2

cdef class Query:
    cdef PGconn* conn
    cdef PGresult* res
    cdef char* qstring
    cdef public State status
    cdef str query

    def __cinit__(self):
        self.conn = db_connect()
        self.status = READY
        self.res = NULL

    def __init__(self, str query):
        self.query = query
        self.qstring = <string>query

    def __dealloc__(self):
        if self.conn is not NULL:
            db_exit(self.conn)

    def __repr__(self):
        return "<Query: '%s'>" % self.query

    def __call__(self, str query):
        self.qstring = <string>query
        self.res = db_query(self.conn, self.qstring)
        db_display(self.res)

    def run(self):
        self.res = db_query(self.conn, self.qstring)

    def test(self):
        self.res = db_query(self.conn, self.qstring)
        db_display(self.res)

    def close(self):
        db_exit(self.conn)

