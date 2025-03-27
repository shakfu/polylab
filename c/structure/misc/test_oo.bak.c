#include "common.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// new.h
void* new(const void* class, ...);
void delete(void* item);
void draw(const void* self);


// new.r
struct Class {
    size_t size;
    void* (*ctor)(void* self, va_list* app);
    void* (*dtor)(void* self);
    void (*draw)(const void* self);
};


// new.c
void* new(const void* _class, ...)
{
    const struct Class* class = _class;
    void* p = calloc(1, class->size);

    assert(p);
    *(const struct Class**)p = class;

    if (class->ctor) {
        va_list ap;

        va_start(ap, _class);
        p = class->ctor(p, &ap);
        va_end(ap);
    }
    return p;
}

void delete(void* self)
{
    const struct Class** cp = self;

    if (self && *cp && (*cp)->dtor)
        self = (*cp)->dtor(self);
    free(self);
}

void draw(const void* self)
{
    const struct Class* const* cp = self;

    assert(self && *cp && (*cp)->draw);
    (*cp)->draw(self);
}

// Point.h
extern const void* Point; /* new(Point, x, y); */

void move(void* point, int dx, int dy);

// Point.r
struct Point {
    const void* class;
    int x, y; /* coordinates */
};

#define x(p) (((const struct Point*)(p))->x)
#define y(p) (((const struct Point*)(p))->y)

// Point.c
static void* Point_ctor(void* _self, va_list* app)
{
    struct Point* self = _self;

    self->x = va_arg(*app, int);
    self->y = va_arg(*app, int);
    return self;
}

static void Point_draw(const void* _self)
{
    const struct Point* self = _self;

    printf("\".\" at %d,%d\n", self->x, self->y);
}

static const struct Class _Point = { sizeof(struct Point), Point_ctor, 0,
                                     Point_draw };

const void* Point = &_Point;

void move(void* _self, int dx, int dy)
{
    struct Point* self = _self;

    self->x += dx, self->y += dy;
}


// other

// typedef struct Party {
//     // data
//     char *name;
//     int age;
//     // methods
//     int (*show)(void *self);
// } Party


// typedef struct Person {
//     Party super;
// } Person;

// int Person_show(void *self)
// {
//     debug("person name:%s age:%i", self->name, self->age);
// }

// Person *Person_new(char *name, int age)
// {
//     Party *p;
//     p = malloc(sizeof(*p));
//     p->name = strdup(name);
//     p-age = age;
//     return (Person *)p;
// }

int main(int argc, char** argv)
{
    void* p;

    p = new (Point, 1, 2);
    draw(p);
    move(p, 10, 20);
    draw(p);
    delete (p);

    return 0;
}
