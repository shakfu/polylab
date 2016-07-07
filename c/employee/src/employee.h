#ifndef EMPLOYEE_H
#define EMPLOYEE_H

typedef struct Employee {
    int id;
    char *firstname;
    char *lastname;
    int age;
    double cash;
} Employee;

Employee *employee_new();
void employee_delete(Employee *e);
void employee_display(Employee *e);

Employee **employees_init();
void employees_populate(Employee **employees);
void employees_display(Employee **employees);
void employees_delete(Employee **employees);

void test_employees();

#endif // EMPLOYEE_H
