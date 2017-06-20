#include <stdio.h>

long int fib(long int n)
{
    long int a = 0;
    long int b = 1;
    while (n-- > 1) {
        long int t = a;
        a = b;
        b += t;
    }
    return b;
}

void test(long int n) {
    printf("fib(%li) -> %li\n", n, fib(n));
}

int main()
{
    test(100);
}