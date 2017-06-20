#ifndef COMMON_H
#define COMMON_H

// common stdlibs
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

// common externs (for remove c99 warning)
extern char *strdup(const char *s);

// array size
#define ARRAY_SIZE(array) sizeof(array) / sizeof(array[0])

#define ARRAY_D_SHOW(array, n)                      \
    for(int i=0; i < (n); i++) {                    \
        printf(#array "[%i] -> %f\n",               \
            i, array[i]);                           \
    }                                               \

#define ARRAY_C_SHOW(array, n)                      \
    for(int i=0; i < (n); i++) {                    \
        printf(#array "[%i] -> %s\n",               \
            i, array[i]);                           \
    }                                               \

#define ARRAY_I_SHOW(array, n)                      \
    for(int i=0; i < (n); i++) {                    \
        printf(#array "[%i] -> %i\n",               \
            i, array[i]);                           \
    }                                               \

#define MATRIX_D_SHOW(matrix, rows, cols)           \
    for (int i=0; i < rows; i++) {                  \
        for (int j=0; j < cols; j++) {              \
            printf(#matrix "[%i][%i] -> %f\n",      \
                i, j, matrix[i][j]);                \
        }                                           \
    }                                               \

#define MATRIX_I_SHOW(matrix, rows, cols)           \
    for (int i=0; i < rows; i++) {                  \
        for (int j=0; j < cols; j++) {              \
            printf(#matrix "[%i][%i] -> %i\n",      \
                i, j, matrix[i][j]);                \
        }                                           \
    }                                               \

// memory mgmt
#define ARRAY_ALLOC(target, type, n_rows, n_cols)   \
    target = malloc((n_rows) * sizeof(type*));      \
    for(int i=0; i < (n_rows); i++) {               \
        target[i] = calloc((n_cols), sizeof(type)); \
    }                                               \

#define ARRAY_FREE(target, n)                       \
    if (target != NULL) {                           \
        for(int i=0; i < (n); i++) {                \
            free(target[i]);                        \
        }                                           \
    }                                               \
    free(target);                                   \

#define FREE(target)                                \
    if (target != NULL) {                           \
        free(target);                               \
    }                                               \

// bools
typedef int BOOLEAN;
#define TRUE  (1)
#define FALSE (0)

// colors
#define COLOR_RESET         "\033[m"
#define COLOR_BOLD          "\033[1m"
#define COLOR_RED           "\033[31m"
#define COLOR_GREEN         "\033[32m"
#define COLOR_YELLOW        "\033[33m"
#define COLOR_BLUE          "\033[34m"
#define COLOR_MAGENTA       "\033[35m"
#define COLOR_CYAN          "\033[36m"
#define COLOR_BOLD_RED      "\033[1;31m"
#define COLOR_BOLD_GREEN    "\033[1;32m"
#define COLOR_BOLD_YELLOW   "\033[1;33m"
#define COLOR_BOLD_BLUE     "\033[1;34m"
#define COLOR_BOLD_MAGENTA  "\033[1;35m"
#define COLOR_BOLD_CYAN     "\033[1;36m"

// debugging
#define debug(M, ...) fprintf(stderr, "[" COLOR_BOLD_CYAN "DEBUG" COLOR_RESET "] %s:%d: " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)
#define clean_errno() (errno == 0 ? "None" : strerror(errno))
#define log_err(M, ...) fprintf(stderr, "[" COLOR_BOLD_RED "ERROR" COLOR_RESET "] (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)
#define log_warn(M, ...) fprintf(stderr, "[" COLOR_BOLD_YELLOW "WARN" COLOR_RESET "]  (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)
#define log_info(M, ...) fprintf(stderr, "[" COLOR_BOLD_GREEN "INFO" COLOR_RESET "]  (%s:%d) " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)
#define check(A, M, ...) if(!(A)) { log_err(M, ##__VA_ARGS__); errno=0; goto error; }
#define sentinel(M, ...)  { log_err(M, ##__VA_ARGS__); errno=0; goto error; }
#define check_mem(A) check((A), "Out of memory.")
#define check_debug(A, M, ...) if(!(A)) { debug(M, ##__VA_ARGS__); errno=0; goto error; }


// utils
#define foreach(i, n)      \
    for(int i=0; i < n; i++)

// error reporting
// if global errorno is set print the error
// otherwise print the error msg
static inline void die(const char *message)
{
    if(errno) {
        perror(message);
    } else {
        log_err("%s", message);
    }
    exit(EXIT_FAILURE);
}


// testing
#define mu_suite_start() char *message = NULL

#define mu_assert(test, message) if (!(test)) { log_err(message); return message; }
#define mu_run_test(test) debug("\n-----%s", " " #test); \
    message = test(); tests_run++; if (message) return message;

#define RUN_TESTS(name) int main(int argc, char *argv[]) {\
    argc = 1; \
    printf(COLOR_BOLD_MAGENTA "----- RUNNING: %s -----" COLOR_RESET "\n", argv[0]);\
    char *result = name();\
    if (result != 0) {\
        printf(COLOR_BOLD_RED "FAILED: %s\n" COLOR_RESET, result);\
    }\
    else {\
        printf(COLOR_BOLD_GREEN "ALL TESTS PASSED\n" COLOR_RESET);\
    }\
    printf("Tests run: %d\n", tests_run);\
    exit(result != 0);\
}


int tests_run;

#endif // COMMON_H
