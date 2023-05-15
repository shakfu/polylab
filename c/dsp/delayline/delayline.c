// delayline.c -- circular buffer (dynamic memory)
// variation on + fix of: https://www.dsprelated.com/freebooks/pasp/Variable_Delay_Lines.html

#include <stdio.h>
#include <stdlib.h>

typedef struct t_delayline_ {
    double *buf;
    double *rptr;
    double *wptr;
    size_t size;
    int delay;
} t_delayline;

t_delayline *delayline_init(size_t size);
void delayline_setdelay(t_delayline *c, int delay);
double delayline_process(t_delayline *c, double x);
void delayline_free(t_delayline *c);

t_delayline *delayline_init(size_t size) {
    t_delayline *delayline = malloc(sizeof(t_delayline));
    if (!delayline || !size) {
        printf("error: !delayline || !size");
        return NULL;
    }
    delayline->buf = (double *)calloc(size, sizeof(double));
    if (!delayline->buf) {
        free(delayline);
        return NULL;
    }

    delayline->delay = 0;
    delayline->size = size;

    delayline->rptr = delayline->buf;
    delayline->wptr = delayline->buf;
    return delayline;
}

// void delayline_setdelay_orig(t_delayline *c, int delay) {
//     c->delay = delay;
//     c->rptr  = c->wptr - c->delay;

//     while (c->wptr < c->buf) {
//         c->rptr += c->size;
//     }
// }

void delayline_setdelay(t_delayline *c, int delay) {
    c->delay = delay;
    c->rptr  = c->wptr - c->delay;

    while (c->rptr < c->buf) {
        c->rptr += c->size;
    }
}


double delayline_process(t_delayline *c, double x)
{
    double y;
    *c->wptr++ = x;
    y = *c->rptr++;
    if ((c->wptr - c->buf) >= c->size) { c->wptr -= c->size; }
    if ((c->rptr - c->buf) >= c->size) { c->rptr -= c->size; }
    return y;
}

void delayline_free(t_delayline *c) {
    free(c->buf);
    free(c);
}

int main() {
    int length = 10;
    int delay = 5;
    t_delayline *c = delayline_init(length);
    delayline_setdelay(c, delay);
    double y = 0.0;

    if (c) {
        double x = 0.0;
        for (int i = 0; i < length*4; i++) {
            x += i;            
            y = delayline_process(c, x);
            printf("x: %f y: %f\n", x, y);
        }
        delayline_free(c);
        return 0;
    }
    return -1;
}
