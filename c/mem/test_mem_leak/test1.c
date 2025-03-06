
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    char *buffer = malloc(1024);
    sprintf(buffer, "%d", argc);
    printf("%s\n", buffer);
    buffer = malloc(10);
    buffer[0] = 'A';
    buffer[9] = '\0';
    printf("%s\n", buffer);
}

