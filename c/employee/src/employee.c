#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "employee.h"

#define MAX_EMPLOYEES 2

// is needed because of -std=c99
extern char *strdup(const char *s);

void employee_display(Employee * e)
{
    printf("e.id: %d\n", e->id);
    printf("e.name: %s %s\n", e->firstname, e->lastname);
    printf("e.age: %d\n", e->age);
    printf("e.cash: %f\n", e->cash);
    printf("\n\n");
}

Employee *employee_new()
{
    Employee *e = malloc(sizeof(Employee));
    if (e == NULL)
        return NULL;

    return e;
}

void employee_delete(Employee * e)
{
    if (e->firstname != NULL) {
        free(e->firstname);
    }

    if (e->lastname != NULL) {
        free(e->lastname);
    }

    // mmust be last naturally
    if (e != NULL) {
        free(e);
    }


}

void employees_delete(Employee ** employees)
{
    for (int i = 0; i < MAX_EMPLOYEES; i++) {
        employee_delete(employees[i]);
    }
    if (employees != NULL) {
        free(employees);
    }
}

void employees_populate(Employee ** employees)
{
    for (int i = 0; i < MAX_EMPLOYEES; i++) {
        employees[i]->id = i;
        employees[i]->firstname = strdup("firstname");
        employees[i]->lastname = strdup("lastname");
        employees[i]->age = 10 + i;
        employees[i]->cash = 101.0 + i;
    }
}

void employees_display(Employee ** employees)
{
    for (int i = 0; i < MAX_EMPLOYEES; i++) {
        employee_display(employees[i]);
    }
}

Employee **employees_init()
{
    Employee **employees = malloc(MAX_EMPLOYEES * sizeof(Employee *));
    for (int i = 0; i < MAX_EMPLOYEES; i++) {
        employees[i] = employee_new();
    }
    return employees;
}

void test_employees()
{
    Employee **employees = employees_init();
    employees_populate(employees);
    employees_display(employees);
    employees_delete(employees);
}
