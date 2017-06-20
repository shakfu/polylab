
typedef struct point {
    double x;
    double y;
} point;

double point_add(point *p)
{
    return p->x + p->y;
}

double point_sub(point *p)
{
    return p->x - p->y;
}

double point_mul(point *p)
{
    return p->x * p->y;
}

double point_div(point *p)
{
    return p->x / p->y;
}

/************************************************/

int m_add(int x, int y)
{
    return x + y;
}

int m_sub(int x, int y)
{
    return x - y;
}

int m_mul(int x, int y)
{
    return x * y;
}

int m_pow(int x, int y)
{
    return x ^ y;
}

float m_div(float x, float y)
{
    return x / y;
}