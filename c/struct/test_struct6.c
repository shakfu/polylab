#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

// struct inheritance

typedef char* str;


void myprintf(str s1, str s2) { printf(s1, s2); }

void mytalk(str speach) { printf("%s \n", speach); }

typedef struct Person {
    // data
    str name;
    int age;

    // methods
    void (*print)(str txt, str sub);
    void (*talk)(str txt);
} Person;


typedef struct Employee {
    Person person;
    int salary;
} Employee;

void Person_init(Person* self, str name, int age)
{
    self->name = name;
    self->age = age;

    // methods
    self->print = &myprintf;
    self->talk = &mytalk;
}

void Person_print(Person* self, str txt, str sub) { self->print(txt, sub); }

void Person_talk(Person* self, str txt)
{
    printf("%s says: ", self->name);
    self->talk(txt);
}

void Employee_init(Employee* self, str name, int age, int salary)
{
    Person_init((Person*)self, name, age);
    self->salary = salary;
}


int main()
{
    // create it
    Person sam, tom, sue;
    Employee jon;

    Person_init(&sam, "sam", 21);
    Person_init(&tom, "tom", 22);
    Person_init(&sue, "sue", 23);

    // Person_init((Person *)&jon, "jon", 24);
    Employee_init(&jon, "jon", 24, 1000);
    Person_talk((Person*)&jon, "nice");

    // show it
    Person_print(&sam, "this is %s\n", "sam");
    Person_talk(&sue, "hello world");
}
