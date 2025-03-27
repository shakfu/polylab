#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STR_LEN 256


typedef struct _person {
    char name[MAX_STR_LEN];
    int age;
    void (*speak)(struct _person* self, char* text);
} person_t;


void person_speak(person_t* self, char* text)
{
    printf("%s (%d) said: %s\n", self->name, self->age, text);
}


person_t* person_init(char* name, int age)
{
    person_t* p = malloc(sizeof(person_t));
    strcpy(p->name, name);
    p->age = age;
    p->speak = person_speak;
    return p;
}


void person_free(person_t* p) { free(p); }


int main(int argc, char* argv[])
{
    person_t* p = person_init("sam", 21);
    p->speak(p, "hello there");
    person_free(p);
    return 0;
}