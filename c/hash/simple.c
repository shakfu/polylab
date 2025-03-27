#include <stdio.h>
#include <stdlib.h>

typedef char* string;

typedef struct {
    int size;
    void** keys;
    void** values;
} Hash;

Hash* hash_new(int size)
{
    Hash* hash = calloc(1, sizeof(Hash));
    hash->size = size;
    hash->keys = calloc(size, sizeof(void*));
    hash->values = calloc(size, sizeof(void*));
    return hash;
}

int hash_index(Hash* hash, void* key)
{
    int index = (long int)key % hash->size;
    while (hash->keys[index] && hash->keys[index] != key)
        index = (index + 1) % hash->size;
    return index;
}

void hash_insert(Hash* hash, void* key, void* value)
{
    int index = hash_index(hash, key);
    hash->keys[index] = key;
    hash->values[index] = value;
}

void* hash_lookup(Hash* hash, void* key)
{
    int index = hash_index(hash, key);
    return hash->values[index];
}

void hash_destroy(Hash* hash)
{
    free(hash->keys);
    free(hash->values);
    free(hash);
}

int main()
{
    Hash* hash = hash_new(15);
    hash_insert(hash, "hello", "world");
    hash_insert(hash, "a", "b");
    printf("hello => %s\n", (string)hash_lookup(hash, "hello"));
    printf("bro => %s\n", (string)hash_lookup(hash, "bro"));
    printf("a => %s\n", (string)hash_lookup(hash, "a"));
    hash_destroy(hash);
    return 0;
}
