// cbuf.c -- circular buffer
// variation on: https://en.wikipedia.org/wiki/Circular_buffer

#include <stdio.h>
#include <stdlib.h>

typedef struct _t_cbuf {
    double *buf; /* array as circular buffer of `size_t` ints */
    int end;     /* write index */
    int start;   /* read index */
    size_t size; /* size of buffer */
} t_cbuf;

t_cbuf *cbuf_init(size_t size);
void cbuf_put(t_cbuf *c, double item);
double cbuf_get(t_cbuf *c);
void cbuf_free(t_cbuf *c);

t_cbuf *cbuf_init(size_t size) {
    t_cbuf *cbuf = malloc(sizeof(t_cbuf));
    if (!cbuf || !size) {
        printf("error: !cbuf || !size");
        return NULL;
    }
    cbuf->end = 0;
    cbuf->start = 0;
    cbuf->size = size;
    cbuf->buf = (double *)calloc(size, sizeof(double));
    if (!cbuf->buf) {
        free(cbuf);
        return NULL;
    }
    return cbuf;
}

void cbuf_put(t_cbuf *c, double item) {
    c->buf[c->end++] = item;
    c->end %= c->size;
}

double cbuf_get(t_cbuf *c) {
    double item = c->buf[c->start++];
    c->start %= c->size;
    return item;
}

void cbuf_free(t_cbuf *c) {
    free(c->buf);
    free(c);
}

int main() {
    t_cbuf *c = cbuf_init(4);
    if (c) {
        double entry = 0.0;
        for (int i = 0; i < 10; i++) {
            entry += i;
            cbuf_put(c, entry);
            double res = cbuf_get(c);
            printf("%d: %f\n", i, res);
        }
        cbuf_free(c);
        return 0;
    }
    return -1;
}
