/*
To use the Two-Level Segregated Fit (TLSF) memory allocator, you need to initialize it with a memory pool and then use tlsf_malloc to allocate memory and tlsf_free to release it. TLSF is designed for real-time systems due to its predictable allocation and deallocation times (O(1) complexity). 
Here's a breakdown of the process:

1. Initialization:
Provide a memory pool:
TLSF requires a contiguous block of memory to manage. This is often allocated using malloc or calloc from the standard library or allocated through other means in embedded systems.
Initialize the TLSF structure:
Call tlsf_create or tlsf_create_with_pool to initialize the TLSF data structures. The tlsf_create function takes a pointer to the memory pool and the pool size as arguments. The tlsf_create_with_pool function is used when a pre-existing pool is used. 

2. Allocation:
Use tlsf_malloc:
Call tlsf_malloc with the desired size of the memory block to allocate. This function searches for a free block of the appropriate size and returns a pointer to the allocated memory.
Error Handling:
tlsf_malloc returns NULL if it cannot find a suitable free block. You should always check the return value to handle allocation failures. 

3. Deallocation:
Use tlsf_free: Call tlsf_free with the pointer to the memory block to be released. TLSF will return the block to the free list, potentially merging it with adjacent free blocks to reduce fragmentation. 

4. Other functions (optional):
tlsf_realloc: To resize a previously allocated block.
tlsf_calloc: To allocate memory and initialize it to zero.
tlsf_size: To get the size of an allocated block.
tlsf_free_all: To free all allocated memory in the pool.
tlsf_destroy: To free resources used by TLSF. 
*/

#include "tlsf.h"   // Include the TLSF header file
#include <stdio.h>  // For printf, etc.
#include <stdlib.h> // For malloc, free

#define POOL_SIZE 1024 * 1024 // Example pool size (1MB)

int main()
{
    // 1. Allocate a memory pool
    void* memory_pool = malloc(POOL_SIZE);
    if (memory_pool == NULL) {
        perror("Failed to allocate memory pool");
        return 1;
    }

    // 2. Initialize TLSF
    tlsf_t tlsf = tlsf_create_with_pool(memory_pool, POOL_SIZE);
    if (tlsf == NULL) {
        fprintf(stderr, "Failed to initialize TLSF\n");
        free(memory_pool);
        return 1;
    }

    // 3. Allocate memory
    void* ptr1 = tlsf_malloc(tlsf, 128);
    if (ptr1 == NULL) {
        fprintf(stderr, "Allocation failed\n");
    } else {
        printf("Allocated memory at: %p\n", ptr1);
        // ... use the allocated memory ...
    }

    void* ptr2 = tlsf_malloc(tlsf, 256);
    if (ptr2 == NULL) {
        fprintf(stderr, "Allocation failed\n");
    } else {
        printf("Allocated memory at: %p\n", ptr2);
        // ... use the allocated memory ...
    }


    // 4. Free memory
    if (ptr1 != NULL) {
        tlsf_free(tlsf, ptr1);
        printf("Freed memory at: %p\n", ptr1);
    }

    if (ptr2 != NULL) {
        tlsf_free(tlsf, ptr2);
        printf("Freed memory at: %p\n", ptr2);
    }

    // 5. Destroy TLSF and free the memory pool
    tlsf_destroy(tlsf);
    free(memory_pool);

    return 0;
}
