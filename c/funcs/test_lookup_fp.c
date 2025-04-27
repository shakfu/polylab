// from: https://c-faq.com/misc/symtab.html

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


int one_func(void)
{
    printf("one\n");
    return EXIT_SUCCESS;

}

int two_func(void)
{
    printf("two\n");
    return EXIT_SUCCESS;

}

int red_func(void)
{
    printf("red\n");
    return EXIT_SUCCESS;
    
}

int blue_func(void)
{
    printf("blue\n");
    return EXIT_SUCCESS;
}


typedef int (*funcptr)(void);

typedef struct {
    char *name;
    int (*funcptr)(void);
} funcmap;

funcmap symtab[] = {
    "one_func", one_func,
    "two_func", two_func,
    "red_func", red_func,
    "blue_func", blue_func,
};

// Then, search the table for the name, and call via the 
// associated function pointer, with code like this:

funcptr findfunc(const char *name)
{
    for(int i = 0; i < sizeof(symtab) / sizeof(symtab[0]); i++) {
        if(strcmp(name, symtab[i].name) == 0)
            return symtab[i].funcptr;
        }

    return NULL;
}

int main(void)
{
    const char *funcname = "one_func";
    int (*funcp)(void) = findfunc(funcname);
    if(funcp != NULL)
        (*funcp)();
    return EXIT_SUCCESS;
}
