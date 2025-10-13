#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>

// ============================================================================
// BYTECODE INSTRUCTION SET
// ============================================================================

typedef enum {
    OP_HALT,        // Stop execution
    OP_LOAD_IMM,    // Load immediate value onto stack
    OP_LOAD_VAR,    // Load variable by index
    OP_STORE_VAR,   // Store top of stack to variable
    OP_ADD,         // Add top two stack values
    OP_SUB,         // Subtract
    OP_MUL,         // Multiply
    OP_DIV,         // Divide
    OP_GT,          // Greater than comparison
    OP_LT,          // Less than comparison
    OP_EQ,          // Equality comparison
    OP_JMP,         // Unconditional jump
    OP_JMP_IF_FALSE,// Jump if top of stack is false
    OP_PRINT,       // Print top of stack
    OP_DUP,         // Duplicate top of stack
    OP_POP,         // Pop and discard top of stack
} OpCode;

typedef struct {
    OpCode op;
    int32_t operand;
} Instruction;

// ============================================================================
// VIRTUAL MACHINE
// ============================================================================

#define STACK_SIZE 256
#define MAX_VARS 64
#define MAX_CODE 1024

typedef struct {
    int32_t stack[STACK_SIZE];
    int sp;                         // Stack pointer
    int32_t vars[MAX_VARS];         // Variable storage
    Instruction code[MAX_CODE];
    int code_size;
    int pc;                         // Program counter
    bool running;
} VM;

void vm_init(VM *vm) {
    vm->sp = 0;
    vm->pc = 0;
    vm->code_size = 0;
    vm->running = false;
    memset(vm->stack, 0, sizeof(vm->stack));
    memset(vm->vars, 0, sizeof(vm->vars));
}

void vm_push(VM *vm, int32_t value) {
    if (vm->sp >= STACK_SIZE) {
        fprintf(stderr, "Stack overflow\n");
        exit(1);
    }
    vm->stack[vm->sp++] = value;
}

int32_t vm_pop(VM *vm) {
    if (vm->sp <= 0) {
        fprintf(stderr, "Stack underflow\n");
        exit(1);
    }
    return vm->stack[--vm->sp];
}

void vm_emit(VM *vm, OpCode op, int32_t operand) {
    if (vm->code_size >= MAX_CODE) {
        fprintf(stderr, "Code size exceeded\n");
        exit(1);
    }
    vm->code[vm->code_size].op = op;
    vm->code[vm->code_size].operand = operand;
    vm->code_size++;
}

void vm_execute(VM *vm) {
    vm->pc = 0;
    vm->sp = 0;
    vm->running = true;
    
    while (vm->running && vm->pc < vm->code_size) {
        Instruction inst = vm->code[vm->pc++];
        
        switch (inst.op) {
            case OP_HALT:
                vm->running = false;
                break;
                
            case OP_LOAD_IMM:
                vm_push(vm, inst.operand);
                break;
                
            case OP_LOAD_VAR:
                if (inst.operand < 0 || inst.operand >= MAX_VARS) {
                    fprintf(stderr, "Invalid variable index\n");
                    exit(1);
                }
                vm_push(vm, vm->vars[inst.operand]);
                break;
                
            case OP_STORE_VAR:
                if (inst.operand < 0 || inst.operand >= MAX_VARS) {
                    fprintf(stderr, "Invalid variable index\n");
                    exit(1);
                }
                vm->vars[inst.operand] = vm_pop(vm);
                break;
                
            case OP_ADD: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a + b);
                break;
            }
            
            case OP_SUB: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a - b);
                break;
            }
            
            case OP_MUL: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a * b);
                break;
            }
            
            case OP_DIV: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                if (b == 0) {
                    fprintf(stderr, "Division by zero\n");
                    exit(1);
                }
                vm_push(vm, a / b);
                break;
            }
            
            case OP_GT: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a > b ? 1 : 0);
                break;
            }
            
            case OP_LT: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a < b ? 1 : 0);
                break;
            }
            
            case OP_EQ: {
                int32_t b = vm_pop(vm);
                int32_t a = vm_pop(vm);
                vm_push(vm, a == b ? 1 : 0);
                break;
            }
            
            case OP_JMP:
                vm->pc = inst.operand;
                break;
                
            case OP_JMP_IF_FALSE:
                if (vm_pop(vm) == 0) {
                    vm->pc = inst.operand;
                }
                break;
                
            case OP_PRINT:
                printf("%d\n", vm_pop(vm));
                break;
                
            case OP_DUP:
                if (vm->sp <= 0) {
                    fprintf(stderr, "Cannot duplicate empty stack\n");
                    exit(1);
                }
                vm_push(vm, vm->stack[vm->sp - 1]);
                break;
                
            case OP_POP:
                vm_pop(vm);
                break;
        }
    }
}

// ============================================================================
// COMPILER - Parses DSL and generates bytecode
// ============================================================================

typedef struct {
    char name[32];
    int index;
} Variable;

typedef struct {
    Variable vars[MAX_VARS];
    int var_count;
    VM *vm;
    const char *input;
    int pos;
} Compiler;

void compiler_init(Compiler *c, VM *vm, const char *input) {
    c->vm = vm;
    c->input = input;
    c->pos = 0;
    c->var_count = 0;
}

void skip_whitespace(Compiler *c) {
    while (c->input[c->pos] && isspace(c->input[c->pos])) {
        c->pos++;
    }
}

int find_or_create_var(Compiler *c, const char *name) {
    for (int i = 0; i < c->var_count; i++) {
        if (strcmp(c->vars[i].name, name) == 0) {
            return c->vars[i].index;
        }
    }
    
    if (c->var_count >= MAX_VARS) {
        fprintf(stderr, "Too many variables\n");
        exit(1);
    }
    
    strncpy(c->vars[c->var_count].name, name, 31);
    c->vars[c->var_count].name[31] = '\0';
    c->vars[c->var_count].index = c->var_count;
    return c->var_count++;
}

bool match(Compiler *c, const char *str) {
    skip_whitespace(c);
    int len = strlen(str);
    if (strncmp(c->input + c->pos, str, len) == 0) {
        c->pos += len;
        return true;
    }
    return false;
}

int parse_number(Compiler *c) {
    skip_whitespace(c);
    int sign = 1;
    if (c->input[c->pos] == '-') {
        sign = -1;
        c->pos++;
    }
    
    int num = 0;
    while (isdigit(c->input[c->pos])) {
        num = num * 10 + (c->input[c->pos] - '0');
        c->pos++;
    }
    return sign * num;
}

void parse_identifier(Compiler *c, char *buf, int size) {
    skip_whitespace(c);
    int i = 0;
    while (i < size - 1 && (isalnum(c->input[c->pos]) || c->input[c->pos] == '_')) {
        buf[i++] = c->input[c->pos++];
    }
    buf[i] = '\0';
}

// Forward declarations
void parse_expression(Compiler *c);
void parse_statement(Compiler *c);

void parse_primary(Compiler *c) {
    skip_whitespace(c);
    
    if (isdigit(c->input[c->pos]) || c->input[c->pos] == '-') {
        int num = parse_number(c);
        vm_emit(c->vm, OP_LOAD_IMM, num);
    } else if (isalpha(c->input[c->pos]) || c->input[c->pos] == '_') {
        char var[32];
        parse_identifier(c, var, sizeof(var));
        int idx = find_or_create_var(c, var);
        vm_emit(c->vm, OP_LOAD_VAR, idx);
    } else if (match(c, "(")) {
        parse_expression(c);
        if (!match(c, ")")) {
            fprintf(stderr, "Expected ')'\n");
            exit(1);
        }
    } else {
        fprintf(stderr, "Unexpected character: %c\n", c->input[c->pos]);
        exit(1);
    }
}

void parse_term(Compiler *c) {
    parse_primary(c);
    
    skip_whitespace(c);
    while (c->input[c->pos] == '*' || c->input[c->pos] == '/') {
        char op = c->input[c->pos++];
        parse_primary(c);
        vm_emit(c->vm, op == '*' ? OP_MUL : OP_DIV, 0);
    }
}

void parse_arithmetic(Compiler *c) {
    parse_term(c);
    
    skip_whitespace(c);
    while (c->input[c->pos] == '+' || c->input[c->pos] == '-') {
        char op = c->input[c->pos++];
        parse_term(c);
        vm_emit(c->vm, op == '+' ? OP_ADD : OP_SUB, 0);
    }
}

void parse_expression(Compiler *c) {
    parse_arithmetic(c);
    
    skip_whitespace(c);
    if (c->input[c->pos] == '>' || c->input[c->pos] == '<' || c->input[c->pos] == '=') {
        char op = c->input[c->pos++];
        if (op == '=' && c->input[c->pos] == '=') {
            c->pos++;
        }
        parse_arithmetic(c);
        
        if (op == '>') vm_emit(c->vm, OP_GT, 0);
        else if (op == '<') vm_emit(c->vm, OP_LT, 0);
        else vm_emit(c->vm, OP_EQ, 0);
    }
}

void parse_statement(Compiler *c) {
    skip_whitespace(c);
    
    if (match(c, "let")) {
        char var[32];
        parse_identifier(c, var, sizeof(var));
        if (!match(c, "=")) {
            fprintf(stderr, "Expected '=' after variable\n");
            exit(1);
        }
        parse_expression(c);
        int idx = find_or_create_var(c, var);
        vm_emit(c->vm, OP_STORE_VAR, idx);
        match(c, ";");
    } else if (match(c, "print")) {
        parse_expression(c);
        vm_emit(c->vm, OP_PRINT, 0);
        match(c, ";");
    } else if (match(c, "if")) {
        parse_expression(c);
        int jmp_false_addr = c->vm->code_size;
        vm_emit(c->vm, OP_JMP_IF_FALSE, 0);  // Placeholder
        
        if (!match(c, "{")) {
            fprintf(stderr, "Expected '{'\n");
            exit(1);
        }
        
        while (!match(c, "}")) {
            parse_statement(c);
        }
        
        // Patch jump address
        c->vm->code[jmp_false_addr].operand = c->vm->code_size;
    } else {
        fprintf(stderr, "Unknown statement\n");
        exit(1);
    }
}

void compile(Compiler *c) {
    while (c->input[c->pos]) {
        parse_statement(c);
        skip_whitespace(c);
    }
    vm_emit(c->vm, OP_HALT, 0);
}

// ============================================================================
// MAIN - Example Programs
// ============================================================================

int main(void) {
    VM vm;
    Compiler compiler;
    
    // Example 1: Basic arithmetic and variables
    printf("=== Example 1: Basic Arithmetic ===\n");
    const char *prog1 = 
        "let x = 10;\n"
        "let y = 20;\n"
        "let sum = x + y;\n"
        "print sum;\n"
        "print x * y;\n";
    
    vm_init(&vm);
    compiler_init(&compiler, &vm, prog1);
    compile(&compiler);
    vm_execute(&vm);
    
    // Example 2: Conditional logic
    printf("\n=== Example 2: Conditional ===\n");
    const char *prog2 = 
        "let temp = 75;\n"
        "if temp > 70 {\n"
        "  print 1;\n"
        "}\n"
        "if temp < 60 {\n"
        "  print 0;\n"
        "}\n";
    
    vm_init(&vm);
    compiler_init(&compiler, &vm, prog2);
    compile(&compiler);
    vm_execute(&vm);
    
    // Example 3: Data transformation pipeline
    printf("\n=== Example 3: Data Transform ===\n");
    const char *prog3 = 
        "let input = 100;\n"
        "let scaled = input * 2;\n"
        "let adjusted = scaled + 50;\n"
        "if adjusted > 200 {\n"
        "  let capped = 200;\n"
        "  print capped;\n"
        "}\n";
    
    vm_init(&vm);
    compiler_init(&compiler, &vm, prog3);
    compile(&compiler);
    vm_execute(&vm);
    
    return 0;
}

