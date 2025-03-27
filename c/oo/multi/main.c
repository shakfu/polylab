#include "person.h"
#include <stdio.h>

int main()
{
    // redirect to output.txt
    // freopen("output.txt", "w", stdout);

    // create it
    Person person = Person_new("sam", 21);
    // do something
    Person_show(person);
    Person_change(person);
    Person_show(person);
    // delete it
    Person_delete(&person);
    printf("done.\n");
}
