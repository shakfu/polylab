cdef extern from "update.h":
    void update_srcdir(char *path)

def update(path):
    update_srcdir(path)


