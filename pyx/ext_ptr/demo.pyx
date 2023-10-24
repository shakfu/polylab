from libc.stdlib cimport malloc, free
from libc.stdio cimport printf
from libc.string cimport strcpy, strlen


ctypedef struct t_person:
    char* name
    int age


cdef show_person(t_person *p):
    printf("person.name: %s\n", p.name)


cdef class Person:
    """A wrapper class for a t_person struct"""
    cdef t_person *_ptr
    cdef bint ptr_owner

    def __init__(self, str name, int age):
        self._ptr = <t_person *>malloc(sizeof(t_person))

        if self._ptr is NULL:
            raise MemoryError

        self._ptr.name = <char *>malloc((len(name)+1) * sizeof(char))

        if self._ptr.name is NULL:
            raise MemoryError

        strcpy(self._ptr.name, name.encode('utf8'))

        self._ptr.age = age

    def __dealloc__(self):
        if self._ptr is not NULL:
            free(self._ptr.name)
            self._ptr.name = NULL
            free(self._ptr)
            self._ptr = NULL

    @property
    def name(self):
        return (self._ptr.name).decode('utf8') if self._ptr is not NULL else None

    @property
    def age(self):
        return self._ptr.age if self._ptr is not NULL else None


cdef class Person1:
    """A wrapper class for a C/C++ data structure"""
    cdef t_person *_ptr
    cdef bint ptr_owner

    def __cinit__(self):
        self.ptr_owner = False

    def __dealloc__(self):
        # De-allocate if not null and flag is set
        if self._ptr is not NULL and self.ptr_owner is True:
            free(self._ptr.name)
            self._ptr.name = NULL
            free(self._ptr)
            self._ptr = NULL

    def __init__(self):
        # Prevent accidental instantiation from normal Python code
        # since we cannot pass a struct pointer into a Python constructor.
        raise TypeError("This class cannot be instantiated directly.")

    # Extension class properties
    @property
    def name(self):
        return (self._ptr.name).decode('utf8') if self._ptr is not NULL else None

    @property
    def age(self):
        return self._ptr.age if self._ptr is not NULL else None


    @staticmethod
    cdef Person1 from_ptr(t_person *_ptr, bint owner=False):
        """Factory function to create Person1 objects from
        given t_person pointer.

        Setting ``owner`` flag to ``True`` causes
        the extension type to ``free`` the structure pointed to by ``_ptr``
        when the wrapper object is deallocated."""
        # Fast call to __new__() that bypasses the __init__() constructor.
        cdef Person1 wrapper = Person1.__new__(Person1)
        wrapper._ptr = _ptr
        wrapper.ptr_owner = owner
        return wrapper

    @staticmethod
    def new(str name, int age) -> Person1:
        """Factory function to create Person1 objects with
        newly allocated t_person"""
        cdef t_person *_ptr = <t_person *>malloc(sizeof(t_person))

        if _ptr is NULL:
            raise MemoryError

        _ptr.name = <char *>malloc((len(name)+1) * sizeof(char))

        if _ptr.name is NULL:
            raise MemoryError

        strcpy(_ptr.name, name.encode('utf8'))

        _ptr.age = age
        return Person1.from_ptr(_ptr, owner=True)



def test():
    p = Person("sam", 50)
    show_person(p._ptr)

    cdef Person1 p1 = Person1.new("sue", 50)
    show_person(p1._ptr)



