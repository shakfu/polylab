#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define STACK_SIZE 256
#define WORD_SIZE 32
#define BUFFER_SIZE 1024

int stack[STACK_SIZE];
int sp = -1;
char words[BUFFER_SIZE][WORD_SIZE];
int word_count = 0;

// Stack operations
void push(int value) {
    if (sp >= STACK_SIZE - 1) {
        printf("Stack overflow\n");
        exit(1);
    }
    stack[++sp] = value;
}

int pop() {
    if (sp < 0) {
        printf("Stack underflow\n");
        exit(1);
    }
    return stack[sp--];
}

// Core arithmetic operations
void do_plus() {
    int b = pop();
    int a = pop();
    push(a + b);
}

void do_minus() {
    int b = pop();
    int a = pop();
    push(a - b);
}

void do_multiply() {
    int b = pop();
    int a = pop();
    push(a * b);
}

void do_divide() {
    int b = pop();
    int a = pop();
    if (b == 0) {
        printf("Division by zero\n");
        exit(1);
    }
    push(a / b);
}

// Stack manipulation
void do_dot() {
    printf("%d ", pop());
}

void do_drop() {
    pop();
}

void do_swap() {
    int b = pop();
    int a = pop();
    push(b);
    push(a);
}

void do_dup() {
    int a = pop();
    push(a);
    push(a);
}

// Number handling
int is_number(const char* str) {
    if (str == NULL || *str == '\0') return 0;
    
    char* endptr;
    strtol(str, &endptr, 10);
    return (*endptr == '\0');
}

void execute_word(const char* word) {
    // Handle numbers
    if (is_number(word)) {
        push(atoi(word));
        return;
    }
    
    // Built-in words
    if (strcmp(word, "+") == 0) do_plus();
    else if (strcmp(word, "-") == 0) do_minus();
    else if (strcmp(word, "*") == 0) do_multiply();
    else if (strcmp(word, "/") == 0) do_divide();
    else if (strcmp(word, ".") == 0) do_dot();
    else if (strcmp(word, "drop") == 0) do_drop();
    else if (strcmp(word, "swap") == 0) do_swap();
    else if (strcmp(word, "dup") == 0) do_dup();
    else {
        printf("Unknown word: %s\n", word);
    }
}

void parse_and_execute(const char* input) {
    char buffer[BUFFER_SIZE];
    strcpy(buffer, input);
    
    char* token = strtok(buffer, " \t\n");
    while (token != NULL) {
        execute_word(token);
        token = strtok(NULL, " \t\n");
    }
}

int main() {
    printf("Minimal Forth interpreter. Type 'quit' to exit.\n");
    
    char input[BUFFER_SIZE];
    while (1) {
        printf("> ");
        if (!fgets(input, sizeof(input), stdin)) break;
        
        if (strncmp(input, "quit", 4) == 0) break;
        
        parse_and_execute(input);
        printf("\n");
    }
    
    return 0;
}

