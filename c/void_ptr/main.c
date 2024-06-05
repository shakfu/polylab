#include <stdio.h>
#include <stdlib.h>


typedef struct _person {
    int age;
    char* name;
} t_person;


void display(void* data)
{
    t_person *p = (t_person*)data;
    printf("p->age = %d\n", p->age);
}


int main(void)
{
    t_person p;
    p.age = 100;
    printf("p.age = %d\n", p.age);

    t_person* p1 =  malloc(sizeof (t_person*));
    if (p1 == NULL)
        return -1;
    
    p1->age = 200;
    display((t_person*)p1);
    free(p1);

    return 0;
}

