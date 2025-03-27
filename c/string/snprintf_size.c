#include <stdio.h>
#define BUFSIZE 9

void init_buf(char* buf, size_t size);
void print_buf(char* buf);

int main()
{
    char buf[BUFSIZE];
    print_buf(buf);

    snprintf(buf, BUFSIZE, "turtle");
    printf("%s\n", buf);
    print_buf(buf);

    init_buf(buf, BUFSIZE);
    print_buf(buf);

    // hello there! == 12 characters, > BUFSIZE
    init_buf(buf, BUFSIZE);
    snprintf(buf, BUFSIZE, "hello there!");
    print_buf(buf);

    // turtle == 6 charaters, < BUFSIZE
    init_buf(buf, BUFSIZE);
    snprintf(buf, BUFSIZE, "turtle");
    print_buf(buf);

    // 2222220 == 7 charaters, > 5
    init_buf(buf, BUFSIZE);
    snprintf(buf, 5, "%d", 222222 * 10);
    print_buf(buf);

    return 0;
}

void init_buf(char* buf, size_t size)
{
    int i;
    for (i = 0; i < size; i++) {
        buf[i] = i + '0'; // int to char conversion
    }
}

void print_buf(char* buf)
{
    int i;
    char c;
    for (i = 0; i < BUFSIZE; i++) {
        c = buf[i];
        if (c == '\0') {
            printf("\\0");

        } else {
            printf("%c", buf[i]);
        }
    }
    printf("\n");
}