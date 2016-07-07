#include "common.h"

typedef struct Object Object;

typedef struct Vtable
{
    void (*dance)(Object *);
    void (*jump)(Object *, int how_high);
} Vtable;

typedef struct Object
{
    Vtable *vtable;
    /* Object members */
} Object;

void Object_dance(Object *obj)
{
    obj->vtable->dance(obj);
}

void Object_jump(Object *obj, int how_high)
{
    obj->vtable->jump(obj, how_high);
}

typedef struct Person
{
    Object super;
    /* Person members */
} Person;

void Person_dance(Person *p)
{
    /* implementation of Person's dance function */
    debug("Person_dance");
}

void Person_jump(Person *p, int how_high)
{
    /* implementation of Person's jump function */
    debug("Person_jump: %i", how_high);
}

/* global vtable for Person */
Vtable Person_vtable =
{
    (void *)&Person_dance, /* you might get a warning here about incompatible pointer types */
    (void *)&Person_jump   /* you can ignore it, or perform a cast to get rid of it */
};

void Person_init(Person *p)
{
    p->super.vtable = &Person_vtable;
    /* init base members d->super.foo */
    /* init derived1 members d->foo */
}




int main(void)
{
    Person p;
    Person_init(&p);

    Person *p_ptr;
    p_ptr = &p;
    Person_dance(p_ptr);
    Person_jump(p_ptr, 100);


    Object *obj_ptr = (Object *)&p;

    Object_dance(obj_ptr);  /* calls derived1_dance */
    Object_jump(obj_ptr, 42);  /* calls derived1_jump */


    return 0;
}