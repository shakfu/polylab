#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

// Define the arena structure
typedef struct {
    void* buffer;
    size_t size;
    size_t offset;
} Arena;

// Initialize the arena
int arena_init(Arena* arena, size_t size)
{
    arena->buffer = malloc(size);
    if (arena->buffer == NULL) {
        return -1; // Allocation failed
    }
    arena->size = size;
    arena->offset = 0;
    return 0;
}

// Allocate memory from the arena
void* arena_alloc(Arena* arena, size_t size)
{
    if (arena->offset + size > arena->size) {
        return NULL; // Not enough space
    }
    void* ptr = (void*)((char*)arena->buffer + arena->offset);
    arena->offset += size;
    return ptr;
}

// Reset the arena (free all allocations at once)
void arena_reset(Arena* arena) { arena->offset = 0; }

// Destroy the arena
void arena_destroy(Arena* arena)
{
    free(arena->buffer);
    arena->buffer = NULL;
    arena->size = 0;
    arena->offset = 0;
}

int main()
{
    // Create an arena with a size of 1024 bytes
    Arena arena;
    if (arena_init(&arena, 1024) != 0) {
        fprintf(stderr, "Failed to initialize arena\n");
        return 1;
    }

    // Allocate some memory from the arena
    int* num1 = (int*)arena_alloc(&arena, sizeof(int));
    if (num1 == NULL) {
        fprintf(stderr, "Failed to allocate memory for num1\n");
        return 1;
    }
    *num1 = 10;

    int* num2 = (int*)arena_alloc(&arena, sizeof(int));
    if (num2 == NULL) {
        fprintf(stderr, "Failed to allocate memory for num2\n");
        return 1;
    }
    *num2 = 20;

    // Print the values
    printf("num1: %d\n", *num1);
    printf("num2: %d\n", *num2);
    printf("Current offset: %zu\n", arena.offset);

    // Reset the arena (free all allocations at once)
    arena_reset(&arena);
    printf("Current offset after reset: %zu\n", arena.offset);

    // Allocate again after reset
    int* num3 = (int*)arena_alloc(&arena, sizeof(int));
    if (num3 == NULL) {
        fprintf(stderr, "Failed to allocate memory for num3\n");
        return 1;
    }
    *num3 = 30;
    printf("num3: %d\n", *num3);
    printf("Current offset after allocating num3: %zu\n", arena.offset);

    // Destroy the arena
    arena_destroy(&arena);

    return 0;
}
