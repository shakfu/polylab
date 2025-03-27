#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

typedef char* str;

typedef struct Person {
    // data
    str name;
    int age;

    // methods
    void (*print)(str txt, str sub);
    void (*talk)(str txt);
} Person;


void myprintf(str s1, str s2) { printf(s1, s2); }

void mytalk(str speach) { printf("say: %s \n", speach); }

Person* makePerson(str name, int age)
{
    Person* p = malloc(sizeof(Person));
    if (p != NULL) {
        p->name = name;
        p->age = age;
        p->print = &myprintf;
        p->talk = &mytalk;
    }
    return p;
}

void show(Person* p)
{
    printf("name: %s \n", p->name);
    printf("age: %i  \n", p->age);
    p->print("hello %s \n", "world");
    p->talk("I love you");
}

int main()
{
    // create it
    Person* person = makePerson("sam", 21);

    // show it
    show(person);

    // finale
    person->print("nice one %s\n", "!");
}
