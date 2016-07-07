#include <stdio.h>
#include "mlib.h"


typedef struct person {
    char * name;
    int age;
    int (*m_add)(int, int); 
} person;

typedef struct node node;

struct node {
    char * name;
    int x;
    int y;
    int (*node_add)(node *);
};

int node_add(node *self) {
    return self->x + self->y;
};

int main(int argc, char **argv)
{
    int x = 30;
    int y = 10;

    float k = 15.0;
    float j = 3.0;

    person p = {"sam", 10, &m_add};
    person *g;
    g = &p;


    point pnt = {10.9, 20.3};
    point *pnt_ptr;
    pnt_ptr = &pnt;

    node n = {"node1", 100, 200, &node_add};


    // point arithmetic
    printf("point_add: %f\n", point_add(pnt_ptr));
    printf("point_sub: %f\n", point_sub(pnt_ptr));
    printf("point_mul: %f\n", point_mul(pnt_ptr));
    printf("point_div: %f\n", point_div(pnt_ptr));
    //printf("point_pow: %f\n", point_pow(pnt_ptr));

    int add_result   = m_add(x, y);
    int padd_result  = p.m_add(x, y);
    int gadd_result  = g->m_add(x, y);
    int sub_result   = m_sub(x, y);
    float div_result = m_div(x, y);
    int node_result  = n.node_add(&n);



    printf("add result: %d\n", add_result);
    printf("sub result: %d\n", sub_result);
    printf("div result: %f\n", div_result);
    printf("p.add result: %d\n", padd_result);
    printf("g->add result: %d\n", gadd_result);
    printf("n.node_add result: %d\n", node_result);

    return 0;
}
