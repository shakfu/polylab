#include <stdio.h>
#include <stdlib.h>

typedef char *string;

struct llist {
    string line;        // a line of input
    struct llist *next  // pointer to the next line
};

// var pointing to the head of the linked list
struct llist *theHead = NULL;

string read_line(string, int *, struct llist *);
string allocmem(string, int);
struct llist *make_elem(string, struct llist *);
void print_list(struct llist *);

int main() {
    string s = NULL;
    struct llist *head = NULL;
    int max = 10; // initial size of input array
    extern struct llist *theHead;

    s = (string) malloc(max);
    ...
