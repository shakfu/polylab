// cbuf_s.c -- circular buffer (static memory)
// variation on: https://en.wikipedia.org/wiki/Circular_buffer

#include <stdio.h>
#include <stdlib.h>

#define BUFLEN 4


typedef struct _t_cbuf {
    double buf[BUFLEN]; /* array as circular buffer of BUFLEN ints */
    double current;
    int end;     /* write index */
    int start;   /* read index */

} t_cbuf;

t_cbuf cbuf_put(t_cbuf c, double item);
t_cbuf cbuf_get(t_cbuf c);


t_cbuf cbuf_put(t_cbuf c, double item) {
    c.buf[c.end++] = item;
    c.end %= BUFLEN;
    return c;
}

t_cbuf cbuf_get(t_cbuf c) {
    c.current = c.buf[c.start++];
    c.start %= BUFLEN;
    return c;
}

int main() {
    t_cbuf c = {.buf = {0}, .current = 0.0, .end = 0, .start = 0};
    double entry = 0.0;
    for (int i = 0; i < 10; i++) {
        entry += i;
        c = cbuf_put(c, entry);
        c = cbuf_get(c);
        printf("c.buf[%d] = %f\n", c.start, c.current);
    } 
    return 0;
}