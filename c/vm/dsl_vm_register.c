#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>

// ============================================================================
// BYTECODE INSTRUCTION SET - Register-based
// ============================================================================

typedef enum {
    OP_HALT,        // Stop execution
    OP_LOADI,       // Load immediate: reg[A] = B
    OP_MOVE,        // Move: reg[A] = reg[B]
    OP_LOADV,       // Load variable: reg[A] = var[B]
    OP_STOREV,      // Store variable: var[A] = reg[B]
    OP_ADD,         // Add: reg[A] = reg[B] + reg[C]
    OP_SUB,         // Subtract: reg[A] = reg[B] - reg[C]
    OP_MUL,         // Multiply: reg[A] = reg[B] * reg[C]
    OP_DIV,         // Divide: reg[A] = reg[B] / reg[C]
    OP_GT,          // Greater than: reg[A] = reg[B] > reg[C]
    OP_LT,          // Less than: reg[A] = reg[B] < reg[C]
    OP_EQ,          // Equal: reg[A] = reg[B] == reg[C]
    OP_JMP,         // Unconditional jump to A
    OP_JMPF,        // Jump to A if reg[B] is false
    OP_PRINT,       // Print reg[A]
} OpCode;

// Three-address code instruction format
typedef struct {
    OpCode op;
    uint8_t a;      // Destination register or jump target
    uint8_t b;      // Source register 1 or immediate value index
    uint8_t c;      // Source register 2
} Instruction;

// ============================================================================
// VIRTUAL MACHINE
// ============================================================================

#define NUM_REGISTERS 32
#define MAX_VARS 64
#define MAX_CODE 1024
#define MAX_CONSTANTS 256

typedef struct {
    int32_t registers[NUM_REGISTERS];
    int32_t vars[MAX_VARS];
    int32_t constants[MAX_CONSTANTS];
    int constant_count;
    Instruction code[MAX_CODE];
    int code_size;
    int pc;
    bool running;
} VM;

void vm_init(VM *vm) {
    vm->pc = 0;
    vm->code_size = 0;
    vm->constant_count = 0;
    vm->running = false;
    memset(vm->registers, 0, sizeof(vm->registers));
    memset(vm->vars, 0, sizeof(vm->vars));
    memset(vm->constants, 0, sizeof(vm->constants));
}

int vm_add_constant(VM *vm, int32_t value) {
    if (vm->constant_count >= MAX_CONSTANTS) {
        fprintf(stderr, "Too many constants\n");
        exit(1);
    }
    vm->constants[vm->constant_count] = value;
    return vm->constant_count++;
}

void vm_emit(VM *vm, OpCode op, uint8_t a, uint8_t b, uint8_t c) {
    if (vm->code_size >= MAX_CODE) {
        fprintf(stderr, "Code size exceeded\n");
        exit(1);
    }
    vm->code[vm->code_size].op = op;
    vm->code[vm->code_size].a = a;
    vm->code[vm->code_size].b = b;
    vm->code[vm->code_size].c = c;
    vm->code_size++;
}

void vm_execute(VM *vm) {
    vm->pc = 0;
    vm->running = true;
    
    while (vm->running && vm->pc < vm->code_size) {
        Instruction inst = vm->code[vm->pc++];
        
        switch (inst.op) {
            case OP_HALT:
                vm->running = false;
                break;
                
            case OP_LOADI:
                // Load immediate from constant pool
                if (inst.b >= vm->constant_count) {
                    fprintf(stderr, "Invalid constant index\n");
                    exit(1);
                }
                vm->registers[inst.a] = vm->constants[inst.b];
                break;
                
            case OP_MOVE:
                vm->registers[inst.a] = vm->registers[inst.b];
                break;
                
            case OP_LOADV:
                if (inst.b >= MAX_VARS) {
                    fprintf(stderr, "Invalid variable index\n");
                    exit(1);
                }
                vm->registers[inst.a] = vm->vars[inst.b];
                break;
                
            case OP_STOREV:
                if (inst.a >= MAX_VARS) {
                    fprintf(stderr, "Invalid variable index\n");
                    exit(1);
                }
                vm->vars[inst.a] = vm->registers[inst.b];
                break;
                
            case OP_ADD:
                vm->registers[inst.a] = vm->registers[inst.b] + vm->registers[inst.c];
                break;
                
            case OP_SUB:
                vm->registers[inst.a] = vm->registers[inst.b] - vm->registers[inst.c];
                break;
                
            case OP_MUL:
                vm->registers[inst.a] = vm->registers[inst.b] * vm->registers[inst.c];
                break;
                
            case OP_DIV:
                if (vm->registers[inst.c] == 0) {
                    fprintf(stderr, "Division by zero\n");
                    exit(1);
                }
                vm->registers[inst.a] = vm->registers[inst.b] / vm->registers[inst.c];
                break;
                
            case OP_GT:
                vm->registers[inst.a] = vm->registers[inst.b] > vm->registers[inst.c] ? 1 : 0;
                break;
                
            case OP_LT:
                vm->registers[inst.a] = vm->registers[inst.b] < vm->registers[inst.c] ? 1 : 0;
                break;
                
            case OP_EQ:
                vm->registers[inst.a] = vm->registers[inst.b] == vm->registers[inst.c] ? 1 : 0;
                break;
                
            case OP_JMP:
                vm->pc = inst.a;
                break;
                
            case OP_JMPF:
                if (vm->registers[inst.b] == 0) {
                    vm->pc = inst.a;
                }
                break;
                
            case OP_PRINT:
                printf("%d\n", vm->registers[inst.a]);
                break;
        }
    }
}

// ============================================================================
// COMPILER - Register allocation and code generation
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
    int next_register;  // Next available register for allocation
} Compiler;

void compiler_init(Compiler *c, VM *vm, const char *input) {
    c->vm = vm;
    c->input = input;
    c->pos = 0;
    c->var_count = 0;
    c->next_register = 0;
}

// Simple register allocator - just uses next available register
int alloc_register(Compiler *c) {
    if (c->next_register >= NUM_REGISTERS) {
        fprintf(stderr, "Out of registers\n");
        exit(1);
    }
    return c->next_register++;
}

void free_register(Compiler *c, int reg) {
    // Simple strategy: just decrement if it's the most recent
    if (reg == c->next_register - 1) {
        c->next_register--;
    }
    // More sophisticated allocators would maintain a free list
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
int parse_expression(Compiler *c);
void parse_statement(Compiler *c);

int parse_primary(Compiler *c) {
    skip_whitespace(c);
    
    if (isdigit(c->input[c->pos]) || c->input[c->pos] == '-') {
        int num = parse_number(c);
        int const_idx = vm_add_constant(c->vm, num);
        int reg = alloc_register(c);
        vm_emit(c->vm, OP_LOADI, reg, const_idx, 0);
        return reg;
    } else if (isalpha(c->input[c->pos]) || c->input[c->pos] == '_') {
        char var[32];
        parse_identifier(c, var, sizeof(var));
        int var_idx = find_or_create_var(c, var);
        int reg = alloc_register(c);
        vm_emit(c->vm, OP_LOADV, reg, var_idx, 0);
        return reg;
    } else if (match(c, "(")) {
        int reg = parse_expression(c);
        if (!match(c, ")")) {
            fprintf(stderr, "Expected ')'\n");
            exit(1);
        }
        return reg;
    } else {
        fprintf(stderr, "Unexpected character: %c\n", c->input[c->pos]);
        exit(1);
    }
}

int parse_term(Compiler *c) {
    int left = parse_primary(c);
    
    skip_whitespace(c);
    while (c->input[c->pos] == '*' || c->input[c->pos] == '/') {
        char op = c->input[c->pos++];
        int right = parse_primary(c);
        int result = alloc_register(c);
        
        vm_emit(c->vm, op == '*' ? OP_MUL : OP_DIV, result, left, right);
        
        free_register(c, left);
        free_register(c, right);
        left = result;
    }
    
    return left;
}

int parse_arithmetic(Compiler *c) {
    int left = parse_term(c);
    
    skip_whitespace(c);
    while (c->input[c->pos] == '+' || c->input[c->pos] == '-') {
        char op = c->input[c->pos++];
        int right = parse_term(c);
        int result = alloc_register(c);
        
        vm_emit(c->vm, op == '+' ? OP_ADD : OP_SUB, result, left, right);
        
        free_register(c, left);
        free_register(c, right);
        left = result;
    }
    
    return left;
}

int parse_expression(Compiler *c) {
    int left = parse_arithmetic(c);
    
    skip_whitespace(c);
    if (c->input[c->pos] == '>' || c->input[c->pos] == '<' || c->input[c->pos] == '=') {
        char op = c->input[c->pos++];
        if (op == '=' && c->input[c->pos] == '=') {
            c->pos++;
        }
        
        int right = parse_arithmetic(c);
        int result = alloc_register(c);
        
        if (op == '>') vm_emit(c->vm, OP_GT, result, left, right);
        else if (op == '<') vm_emit(c->vm, OP_LT, result, left, right);
        else vm_emit(c->vm, OP_EQ, result, left, right);
        
        free_register(c, left);
        free_register(c, right);
        left = result;
    }
    
    return left;
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
        int reg = parse_expression(c);
        int var_idx = find_or_create_var(c, var);
        vm_emit(c->vm, OP_STOREV, var_idx, reg, 0);
        free_register(c, reg);
        match(c, ";");
    } else if (match(c, "print")) {
        int reg = parse_expression(c);
        vm_emit(c->vm, OP_PRINT, reg, 0, 0);
        free_register(c, reg);
        match(c, ";");
    } else if (match(c, "if")) {
        int cond_reg = parse_expression(c);
        int jmp_false_addr = c->vm->code_size;
        vm_emit(c->vm, OP_JMPF, 0, cond_reg, 0);  // Placeholder
        free_register(c, cond_reg);
        
        if (!match(c, "{")) {
            fprintf(stderr, "Expected '{'\n");
            exit(1);
        }
        
        while (!match(c, "}")) {
            parse_statement(c);
        }
        
        // Patch jump address
        c->vm->code[jmp_false_addr].a = c->vm->code_size;
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
    vm_emit(c->vm, OP_HALT, 0, 0, 0);
}

// ============================================================================
// DISASSEMBLER - For debugging and illustration
// ============================================================================

const char* opcode_name(OpCode op) {
    switch(op) {
        case OP_HALT:   return "HALT";
        case OP_LOADI:  return "LOADI";
        case OP_MOVE:   return "MOVE";
        case OP_LOADV:  return "LOADV";
        case OP_STOREV: return "STOREV";
        case OP_ADD:    return "ADD";
        case OP_SUB:    return "SUB";
        case OP_MUL:    return "MUL";
        case OP_DIV:    return "DIV";
        case OP_GT:     return "GT";
        case OP_LT:     return "LT";
        case OP_EQ:     return "EQ";
        case OP_JMP:    return "JMP";
        case OP_JMPF:   return "JMPF";
        case OP_PRINT:  return "PRINT";
        default:        return "UNKNOWN";
    }
}

void disassemble(VM *vm) {
    printf("\n--- Bytecode Disassembly ---\n");
    printf("Constants: ");
    for (int i = 0; i < vm->constant_count; i++) {
        printf("[%d]=%d ", i, vm->constants[i]);
    }
    printf("\n\n");
    
    for (int i = 0; i < vm->code_size; i++) {
        Instruction inst = vm->code[i];
        printf("%04d: %-8s ", i, opcode_name(inst.op));
        
        switch(inst.op) {
            case OP_HALT:
                printf("\n");
                break;
            case OP_LOADI:
                printf("r%d, const[%d] (=%d)\n", inst.a, inst.b, vm->constants[inst.b]);
                break;
            case OP_MOVE:
            case OP_LOADV:
            case OP_PRINT:
                printf("r%d, %d\n", inst.a, inst.b);
                break;
            case OP_STOREV:
                printf("var[%d], r%d\n", inst.a, inst.b);
                break;
            case OP_ADD:
            case OP_SUB:
            case OP_MUL:
            case OP_DIV:
            case OP_GT:
            case OP_LT:
            case OP_EQ:
                printf("r%d, r%d, r%d\n", inst.a, inst.b, inst.c);
                break;
            case OP_JMP:
                printf("%d\n", inst.a);
                break;
            case OP_JMPF:
                printf("%d, r%d\n", inst.a, inst.b);
                break;
        }
    }
    printf("\n");
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
    disassemble(&vm);
    printf("Output:\n");
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
    disassemble(&vm);
    printf("Output:\n");
    vm_execute(&vm);
    
    // Example 3: Complex expression
    printf("\n=== Example 3: Complex Expression ===\n");
    const char *prog3 = 
        "let a = 5;\n"
        "let b = 3;\n"
        "let c = 2;\n"
        "let result = (a + b) * c;\n"
        "print result;\n";
    
    vm_init(&vm);
    compiler_init(&compiler, &vm, prog3);
    compile(&compiler);
    disassemble(&vm);
    printf("Output:\n");
    vm_execute(&vm);
    
    return 0;
}

