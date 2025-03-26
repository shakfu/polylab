#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h> 

// ref: https://www.bytesbeneath.com/p/the-arena-custom-memory-allocators

#define DEFAULT_ALIGNMENT (2 * sizeof(void *))

#define make(T, n, a) ((T *)((a).alloc(sizeof(T) * n, (a).context)))

#define release(s, p, a) ((a).free(s, p, (a).context))

typedef struct {
    void *(*alloc)(size_t size, void *context);
    void (*free)(size_t size, void *ptr, void *context);
    void *context;
} Allocator;

typedef struct {
    void *base;
    size_t size;
    size_t offset;
    size_t committed;
} Arena;

#define arena_alloc_init(a) (Allocator){arena_alloc, arena_free, a}

#define is_power_of_two(x) ((x != 0) && ((x & (x - 1)) == 0))

uintptr_t align_forward(uintptr_t ptr, size_t alignment) {
    uintptr_t p, a, modulo;
    if (!is_power_of_two(alignment)) {
        return 0;
    }

    p = ptr;
    a = (uintptr_t)alignment;
    modulo = p & (a - 1);

    if (modulo) {
        p += a - modulo;
    }

    return p;
}

void *arena_alloc_aligned(Arena *a, size_t size, size_t alignment) {
    uintptr_t curr_ptr = (uintptr_t)a->base + (uintptr_t)a->offset;
    uintptr_t offset = align_forward(curr_ptr, alignment);
    offset -= (uintptr_t)a->base;
		
    if (offset + size > a->size) {
        return 0;
    }

    a->committed += size;
    void *ptr = (uint8_t *)a->base + offset;
    a->offset = offset + size;

    return ptr;
}

void *arena_alloc(size_t size, void *context) {
    if (!size) {
        return 0;
    }
    return arena_alloc_aligned((Arena *)context, size, DEFAULT_ALIGNMENT);
}

// Does nothing.
void arena_free(size_t size, void *ptr, void *context) {
    (void)ptr; (void)size; (void)context;
}

void arena_free_all(void *context) {
    Arena *a = context;
    a->offset = 0;
    a->committed = 0;
}

Arena arena_init(void *buffer, size_t size) {
    return (Arena){.base = buffer, .size = size};
}

int main()
{
    size_t size = 1024 * 1024 * 64;
    void *buffer = malloc(size);
    Arena arena = arena_init(buffer, size);
    Allocator allocator = arena_alloc_init(&arena);

    int *x = make(int, 420, allocator);
    size_t *y = make(size_t, 23, allocator);
    char *z = make(char, 69, allocator);

    for (int i = 0; i < 420; i += 1) x[i] = i;
    for (int i = 0; i < 23; i += 1)  y[i] = (size_t)i;
    for (int i = 0; i < 69; i += 1)  z[i] = (char)i + '!';

    arena_free_all(allocator.context);
}


