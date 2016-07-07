#include <stdio.h>
#include <ctype.h>

#define INCHES_PER_FOOT 12
#define FEET_PER_YARD

typedef char *str;

struct Person
{
    str name;
    int age;
    void (*ptr_print)(str, str);
};

void myprintf(str s1, str s2) {
    printf(s1, s2);
}

void show(struct Person *p) {
    printf("name: %s \n", p->name);
    printf("age: %i \n", p->age);
    p->ptr_print("hello %s \n", "world");
}

int main()
{
    struct Person person;
    person.name = "sam";
    person.age = 21;
    person.ptr_print = &myprintf;

    // show it 
    show(&person);
}

