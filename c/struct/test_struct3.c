#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

typedef struct Person
{
    // data
    char *name;
    int age;
    
    // methods
    void (*print)(char *txt, char *sub);
    void (*talk) (char *txt);
} Person;



void myprintf(char *s1, char *s2) {
    printf(s1, s2);
}

void mytalk(char *speach) {
    printf("say: %s \n", speach);
}

Person *makePerson(char *name, int age) {
    Person *p = malloc(sizeof(Person));
    if (p != NULL) {
        p->name = name;
        p->age = age;
        p->print = &myprintf;
        p->talk = &mytalk;
    }
    return p;
}

void show(Person *p) {
    printf("name: %s \n", p->name);
    printf("age: %i \n", p->age);
    p->print("hello %s \n", "world");
    p->talk("I love you");
}

int main()
{
    // create it   
    Person *person = makePerson("sam", 21);
    
    // show it 
    show(person);
}

