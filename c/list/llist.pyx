
cdef extern from "stdlib.h":
    ctypedef unsigned long size_t
    void free(void *ptr)
    void *malloc(size_t size)
    void *realloc(void *ptr, size_t size)
    size_t strlen(char *s)
    char *strcpy(char *dest, char *src)


cdef struct node:
    int data
    node *next


cdef node* add(node *head, int data):
    cdef node *tmp
    if (head == NULL):
        head = <node *>malloc(sizeof(node))
        if (head == NULL):
            print("Error! memory is not available")
        head.data = data
        head.next = head
    else:
        tmp = head
        while (tmp.next != head):
            tmp = tmp.next
            tmp.next = <node *>malloc(sizeof(node))
            if (tmp.next == NULL):
                print("Error! memory is not available")
            tmp = tmp.next
            tmp.data = data
            tmp.next = head
        return head


cdef printlist(node *head):
    cdef node *current
    current = head
    if current != NULL:
        while (current != head):
            print("%d\t" % current.data)
            current = current.next
    else:
        print("The list is empty")

cdef destroy(node *head):
    cdef node *current, *tmp
    current = head.next
    head.next = NULL
    while (current != NULL):
        tmp = current.next
        free(current)
        current = tmp


def main():
    cdef node *head = NULL
    head = add(head, 1)
    printlist(head)
    #destroy(head)

