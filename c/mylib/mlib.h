typedef struct point {
    double x;
    double y;
} point;

double point_add(point *p);
double point_sub(point *p);
double point_mul(point *p);
double point_div(point *p);
//double point_pow(point *p);


int m_add(int x, int y);
int m_sub(int x, int y);
int m_mul(int x, int y);
int m_pow(int x, int y);
float m_div(float x, float y);