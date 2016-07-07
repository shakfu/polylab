#include <stdio.h>
#include <ctype.h>


typedef char *str;

typedef struct Person
{
    str name;
    int age;
    void (*print)(str, str);
} Person;

void myprintf(str s1, str s2) {
    printf(s1, s2);
}

void show(Person *p) {
    printf("name: %s \n", p->name);
    printf("age: %i \n", p->age);
    p->print("hello %s \n", "world");
}

int main()
{
    Person person;
    person.name = "sam";
    person.age = 21;
    person.print = &myprintf;

    // show it 
    show(&person);
}

