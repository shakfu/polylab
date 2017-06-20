#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

typedef char *str;


void myprintf(str s1, str s2) {
    printf(s1, s2);
}

void mytalk(str speach) {
    printf("%s \n", speach);
}

typedef struct Person
{
    // data
    str name;
    int age;
    
    // methods
    void (*print)(str txt, str sub);
    void (*talk) (str txt);
} Person;

void Person_init(Person *self, str name, int age)
{
    self->name = name;
    self->age  = age;
    
    // methods
    self->print = &myprintf;
    self->talk  = &mytalk;
}

void Person_print(Person *self, str txt, str sub)
{
    self->print(txt, sub);
}

void Person_talk(Person *self, str txt)
{
    printf("%s says: ", self->name);
    self->talk(txt);
}



int main()
{
    // create it   
    Person sam, tom, sue;
    
    Person_init(&sam, "sam", 21);
    Person_init(&tom, "tom", 22);
    Person_init(&sue, "sue", 23);
    
    // show it 
    Person_print(&sam, "this is %s\n", "sam");
    Person_talk(&sue, "hello world");

}

