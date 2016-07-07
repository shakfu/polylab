#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "person.h"

static void myprintf(char *s1, char *s2) {
    printf(s1, s2);
}

static void mytalk(char *speech) {
    printf("say: %s \n", speech);
}


Person Person_new(char *name, int age) {
    Person this = malloc(sizeof(struct sPerson));
    if (this != NULL) {
        this->name = name;
        this->age = age;
        
        // default function assigned
        this->print = &myprintf;
        this->talk = &mytalk;
    }
    return this;
}

void Person_delete(Person *this) {
    if(this && *this)
    {
        free(*this);
        *this = NULL;
    }
}


void Person_show(Person this) {
    printf("name: %s \n", this->name);
    printf("age: %i \n", this->age);
    this->print("hello %s \n", "world");
    this->talk("I love you");
}

void Person_change(Person this) {
    this->name = "jon";
    this->age = 30;
}


