cdef extern from "stdio.h":
    pass

cdef extern from "stdlib.h":
    pass

cdef extern from "string.h":
    pass

cdef extern from "set.h":
    Set *set_new
    void set_free
    void set_register_free_function
    int set_insert
    int set_remove
    int set_query
    int set_num_entries
    SetValue *set_to_array
    Set *set_union
    Set *set_intersection
    void set_iterate
    int set_iter_has_more
    SetValue set_iter_next

cdef extern from "hash-string.h":
    unsigned long string_hash

cdef extern from "compare-string.h":
    int string_equal


cdef int doit():
    cdef:
        Set *set
        char buf[10]
        int i
        char *value
    
    set = set_new(string_hash, string_equal)
    for i from 0 <= i < 10000:
        set_insert(set, i)
    set_register_free_function(set, free)

def test():
    doit()

