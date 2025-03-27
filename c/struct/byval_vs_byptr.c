#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct _Vector2D {
    float x;
    float y;
} Vector2D;

void set_vec(Vector2D* v)
{
    v->x = 5.1;
    v->y = 2.1;
}

Vector2D set_vec_by_val(Vector2D v)
{
    v.x = 5.1;
    v.y = 2.1;

    return v;
}

void show_vec(Vector2D* v) { printf("x: %f y: %f\n", v->x, v->y); }


void test_vec_by_ptr(void)
{
    Vector2D* vec;
    vec = malloc(sizeof(Vector2D));
    if (vec == NULL)
        printf("null vec\n");
    memset(vec, 0, sizeof(Vector2D));
    set_vec(vec);
    show_vec(vec);
    free(vec);
}

void test_vec_by_val(void)
{
    Vector2D vec;
    Vector2D res = set_vec_by_val(vec);
    show_vec(&res);
}

int main()
{
    test_vec_by_ptr();
    test_vec_by_val();
}
