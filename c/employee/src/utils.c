#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


#define ARRAY_SIZE(array) sizeof(array) / sizeof(array[0])

// if global errorno is set print the error
// otherwise print the error msg
void die(const char* message)
{
    if (errno) {
        perror(message);
    } else {
        printf("ERROR: %s\n", message);
    }

    exit(1);
}

// example of variadic function
double average(int count, ...)
{
    va_list args;
    double sum = 0.0;

    va_start(args, count);
    for (int i = 0; i < count; i++) {
        sum += va_arg(args, double);
        // printf("i: %d count: %d sum: %f\n", i, count, sum);
    }
    va_end(args);
    return sum / count;
}

double avg1(double array[], size_t size)
{
    double avg, sum;
    for (int i = 0; i < size; ++i) {
        sum += array[i];
    }
    avg = sum / size;
    return avg;
}


void test_utils()
{
    printf("avg: %f\n", average(2, 3.2, 2.1));
    double vals[3] = { 10.2, 21.1, 32.1 };
    double avg;
    avg = avg1(vals, ARRAY_SIZE(vals));
    printf("avg1: %f\n", avg);
}
