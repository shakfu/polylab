#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef enum { HIGH, MEDIUM, LOW } Status;

typedef struct point {
    double x;
    double y;
} point;

typedef struct record {
    char* name;
    int age;
} record;

// typedef on pointer to function
typedef double (*operation)(double x, double y);

int counter(void)
{
    static unsigned count;
    count += 1;
    debug("count: %i", count);
    return count;
}

double add(double x, double y) { return x + y; }

double subtract(double x, double y) { return x - y; }

// without typedef we would have to write it as:
// double point_op(point * p, double (*operation) (double x, double y))
// with typedef it is simplified as:
double point_op(point* p, operation func) { return func(p->x, p->y); }

point* point_new(double x, double y)
{
    point* p = malloc(sizeof(point));
    if (p == NULL)
        return NULL;

    p->x = x;
    p->y = y;

    return p;
}

void point_display(point* p)
{
    printf("x: %f\n", p->x);
    printf("y: %f\n", p->y);
}

void point_delete(point* p)
{
    if (p != NULL) {
        free(p);
    }
}

void test_point()
{
    point* p = point_new(20.1, 5.6);
    point_display(p);
    printf("add: %f\n", point_op(p, add));
    printf("sub: %f\n", point_op(p, subtract));
    point_delete(p);
}

record* record_new(const char* name, int age)
{
    record* r = malloc(sizeof(record));
    if (r == NULL)
        return NULL;

    // set name
    r->name = strdup(name);

    // set age
    r->age = age;
}

void record_delete(record* r)
{
    if (r != NULL) {
        if (r->name != NULL)
            free(r->name);
        // if (r->age) free(r->age);
        free(r);
    }
}

void record_display(record* r)
{
    printf("name: %s\n", r->name);
    printf("age:  %d\n", r->age);
}

void test_record()
{
    record* r = record_new("Sandy Fuller", 10);
    record_display(r);
    record_delete(r);
}

void test_array()
{
    char* colors[] = { COLOR_BOLD_BLUE "blue" COLOR_RESET,
                       COLOR_BOLD_YELLOW "yellow" COLOR_RESET,
                       COLOR_BOLD_RED "red" COLOR_RESET };

    int size = ARRAY_SIZE(colors);

    for (int i = 0; i < size; i++) {
        printf("color: %s\n", colors[i]);
    }
}

void dispatch(Status s)
{
    switch (s) {
    case HIGH:
        debug("status: HIGH");
        break;
    case MEDIUM:
        debug("status: MEDIUM");
        break;
    case LOW:
        debug("status: LOW");
        break;
    default:
        log_err("must be enum HIGH, MEDIUM, LOW!");
    }
}

void test_enum()
{
    Status high = HIGH;
    Status med = MEDIUM;
    Status low = LOW;
    dispatch(high);
    dispatch(med);
    dispatch(low);
    dispatch(5);
}


int main()
{
    test_point();
    test_record();
    test_array();
    test_enum();
    counter();
    counter();
    return 0;
}
