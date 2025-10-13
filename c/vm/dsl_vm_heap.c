#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>

// ============================================================================
// HEAP-BASED VM - Graph Reduction Model
// ============================================================================
// This VM represents computation as a graph of nodes in a heap.
// Execution proceeds by reducing (evaluating) nodes until reaching values.
// This is similar to how functional languages like Haskell work internally.

typedef enum {
    NODE_INT,           // Integer literal
    NODE_VAR,           // Variable reference
    NODE_ADD,           // Addition operation
    NODE_SUB,           // Subtraction
    NODE_MUL,           // Multiplication
    NODE_DIV,           // Division
    NODE_GT,            // Greater than
    NODE_LT,            // Less than
    NODE_EQ,            // Equality
    NODE_IF,            // Conditional (if-then-else)
    NODE_LET,           // Let binding
    NODE_LAMBDA,        // Lambda abstraction (function)
    NODE_APP,           // Function application
    NODE_PRINT,         // Print side effect
} NodeType;

// Forward declaration
typedef struct Node Node;

// A node in the heap - represents part of the computation graph
struct Node {
    NodeType type;
    bool evaluated;         // Has this node been reduced to a value?
    int ref_count;          // Simple reference counting for GC
    
    union {
        // Leaf nodes
        int32_t int_val;
        char var_name[32];
        
        // Binary operations
        struct {
            Node *left;
            Node *right;
        } binary;
        
        // If-then-else
        struct {
            Node *condition;
            Node *then_branch;
            Node *else_branch;
        } conditional;
        
        // Let binding: let var = value in body
        struct {
            char var[32];
            Node *value;
            Node *body;
        } let_binding;
        
        // Lambda: λx. body
        struct {
            char param[32];
            Node *body;
        } lambda;
        
        // Function application: func arg
        struct {
            Node *func;
            Node *arg;
        } app;
        
        // Print
        struct {
            Node *expr;
        } print;
    } data;
};

// ============================================================================
// HEAP MEMORY MANAGER
// ============================================================================

#define HEAP_SIZE 4096

typedef struct {
    Node heap[HEAP_SIZE];
    int next_free;
    int alloc_count;        // For statistics
    int gc_count;           // Number of GC runs
} Heap;

void heap_init(Heap *h) {
    h->next_free = 0;
    h->alloc_count = 0;
    h->gc_count = 0;
    memset(h->heap, 0, sizeof(h->heap));
}

Node* heap_alloc(Heap *h) {
    if (h->next_free >= HEAP_SIZE) {
        fprintf(stderr, "Heap exhausted (allocated %d nodes)\n", h->alloc_count);
        fprintf(stderr, "In production, this would trigger garbage collection\n");
        exit(1);
    }
    
    Node *node = &h->heap[h->next_free++];
    node->evaluated = false;
    node->ref_count = 0;
    h->alloc_count++;
    return node;
}

void node_retain(Node *n) {
    if (n) n->ref_count++;
}

void node_release(Node *n) {
    if (!n) return;
    
    n->ref_count--;
    if (n->ref_count <= 0) {
        // In a full implementation, recursively release children
        // and add node back to free list
    }
}

// ============================================================================
// NODE CONSTRUCTORS
// ============================================================================

Node* make_int(Heap *h, int32_t value) {
    Node *n = heap_alloc(h);
    n->type = NODE_INT;
    n->evaluated = true;
    n->data.int_val = value;
    return n;
}

Node* make_var(Heap *h, const char *name) {
    Node *n = heap_alloc(h);
    n->type = NODE_VAR;
    strncpy(n->data.var_name, name, 31);
    n->data.var_name[31] = '\0';
    return n;
}

Node* make_binary(Heap *h, NodeType type, Node *left, Node *right) {
    Node *n = heap_alloc(h);
    n->type = type;
    n->data.binary.left = left;
    n->data.binary.right = right;
    node_retain(left);
    node_retain(right);
    return n;
}

Node* make_if(Heap *h, Node *cond, Node *then_br, Node *else_br) {
    Node *n = heap_alloc(h);
    n->type = NODE_IF;
    n->data.conditional.condition = cond;
    n->data.conditional.then_branch = then_br;
    n->data.conditional.else_branch = else_br;
    node_retain(cond);
    node_retain(then_br);
    node_retain(else_br);
    return n;
}

Node* make_let(Heap *h, const char *var, Node *value, Node *body) {
    Node *n = heap_alloc(h);
    n->type = NODE_LET;
    strncpy(n->data.let_binding.var, var, 31);
    n->data.let_binding.var[31] = '\0';
    n->data.let_binding.value = value;
    n->data.let_binding.body = body;
    node_retain(value);
    node_retain(body);
    return n;
}

Node* make_lambda(Heap *h, const char *param, Node *body) {
    Node *n = heap_alloc(h);
    n->type = NODE_LAMBDA;
    strncpy(n->data.lambda.param, param, 31);
    n->data.lambda.param[31] = '\0';
    n->data.lambda.body = body;
    node_retain(body);
    return n;
}

Node* make_app(Heap *h, Node *func, Node *arg) {
    Node *n = heap_alloc(h);
    n->type = NODE_APP;
    n->data.app.func = func;
    n->data.app.arg = arg;
    node_retain(func);
    node_retain(arg);
    return n;
}

Node* make_print(Heap *h, Node *expr) {
    Node *n = heap_alloc(h);
    n->type = NODE_PRINT;
    n->data.print.expr = expr;
    node_retain(expr);
    return n;
}

// ============================================================================
// ENVIRONMENT - Variable bindings
// ============================================================================

typedef struct EnvNode {
    char name[32];
    Node *value;
    struct EnvNode *next;
} EnvNode;

typedef struct {
    EnvNode *head;
} Environment;

void env_init(Environment *env) {
    env->head = NULL;
}

void env_bind(Environment *env, const char *name, Node *value) {
    EnvNode *node = malloc(sizeof(EnvNode));
    strncpy(node->name, name, 31);
    node->name[31] = '\0';
    node->value = value;
    node->next = env->head;
    env->head = node;
    node_retain(value);
}

Node* env_lookup(Environment *env, const char *name) {
    for (EnvNode *n = env->head; n != NULL; n = n->next) {
        if (strcmp(n->name, name) == 0) {
            return n->value;
        }
    }
    return NULL;
}

Environment env_copy(Environment *env) {
    Environment new_env;
    new_env.head = NULL;
    
    // Shallow copy - shares nodes
    for (EnvNode *n = env->head; n != NULL; n = n->next) {
        env_bind(&new_env, n->name, n->value);
    }
    return new_env;
}

void env_free(Environment *env) {
    EnvNode *current = env->head;
    while (current) {
        EnvNode *next = current->next;
        node_release(current->value);
        free(current);
        current = next;
    }
    env->head = NULL;
}

// ============================================================================
// GRAPH REDUCTION ENGINE
// ============================================================================

// Reduce a node to its value (evaluation)
Node* reduce(Node *node, Environment *env) {
    if (!node) return NULL;
    
    // Already evaluated - return as-is
    if (node->evaluated) {
        return node;
    }
    
    switch (node->type) {
        case NODE_INT:
            // Already a value
            node->evaluated = true;
            return node;
            
        case NODE_VAR: {
            // Look up variable in environment
            Node *value = env_lookup(env, node->data.var_name);
            if (!value) {
                fprintf(stderr, "Undefined variable: %s\n", node->data.var_name);
                exit(1);
            }
            return reduce(value, env);
        }
        
        case NODE_ADD:
        case NODE_SUB:
        case NODE_MUL:
        case NODE_DIV:
        case NODE_GT:
        case NODE_LT:
        case NODE_EQ: {
            // Reduce operands
            Node *left = reduce(node->data.binary.left, env);
            Node *right = reduce(node->data.binary.right, env);
            
            if (left->type != NODE_INT || right->type != NODE_INT) {
                fprintf(stderr, "Type error: expected integers\n");
                exit(1);
            }
            
            int32_t result;
            switch (node->type) {
                case NODE_ADD: result = left->data.int_val + right->data.int_val; break;
                case NODE_SUB: result = left->data.int_val - right->data.int_val; break;
                case NODE_MUL: result = left->data.int_val * right->data.int_val; break;
                case NODE_DIV:
                    if (right->data.int_val == 0) {
                        fprintf(stderr, "Division by zero\n");
                        exit(1);
                    }
                    result = left->data.int_val / right->data.int_val;
                    break;
                case NODE_GT: result = left->data.int_val > right->data.int_val ? 1 : 0; break;
                case NODE_LT: result = left->data.int_val < right->data.int_val ? 1 : 0; break;
                case NODE_EQ: result = left->data.int_val == right->data.int_val ? 1 : 0; break;
                default: result = 0;
            }
            
            // Update node in-place (graph reduction!)
            node->type = NODE_INT;
            node->data.int_val = result;
            node->evaluated = true;
            return node;
        }
        
        case NODE_IF: {
            // Reduce condition
            Node *cond = reduce(node->data.conditional.condition, env);
            if (cond->type != NODE_INT) {
                fprintf(stderr, "Type error: condition must be integer\n");
                exit(1);
            }
            
            // Select branch based on condition
            if (cond->data.int_val != 0) {
                return reduce(node->data.conditional.then_branch, env);
            } else {
                return reduce(node->data.conditional.else_branch, env);
            }
        }
        
        case NODE_LET: {
            // Evaluate the bound value
            Node *value = reduce(node->data.let_binding.value, env);
            
            // Create new environment with binding
            Environment new_env = env_copy(env);
            env_bind(&new_env, node->data.let_binding.var, value);
            
            // Evaluate body in extended environment
            Node *result = reduce(node->data.let_binding.body, &new_env);
            
            env_free(&new_env);
            return result;
        }
        
        case NODE_LAMBDA:
            // Lambdas are values (closures in full implementation)
            node->evaluated = true;
            return node;
            
        case NODE_APP: {
            // Reduce function
            Node *func = reduce(node->data.app.func, env);
            
            if (func->type != NODE_LAMBDA) {
                fprintf(stderr, "Type error: attempting to apply non-function\n");
                exit(1);
            }
            
            // Don't evaluate argument yet (lazy evaluation)
            // In strict evaluation, would reduce here
            
            // Create environment with parameter bound to argument
            Environment new_env = env_copy(env);
            env_bind(&new_env, func->data.lambda.param, node->data.app.arg);
            
            // Evaluate function body
            Node *result = reduce(func->data.lambda.body, &new_env);
            
            env_free(&new_env);
            return result;
        }
        
        case NODE_PRINT: {
            Node *value = reduce(node->data.print.expr, env);
            if (value->type != NODE_INT) {
                fprintf(stderr, "Can only print integers\n");
                exit(1);
            }
            printf("%d\n", value->data.int_val);
            return value;
        }
    }
    
    return node;
}

// ============================================================================
// PARSER - Builds graph structure
// ============================================================================

typedef struct {
    const char *input;
    int pos;
    Heap *heap;
} Parser;

void parser_init(Parser *p, Heap *h, const char *input) {
    p->input = input;
    p->pos = 0;
    p->heap = h;
}

void skip_whitespace(Parser *p) {
    while (p->input[p->pos] && isspace(p->input[p->pos])) {
        p->pos++;
    }
}

bool match(Parser *p, const char *str) {
    skip_whitespace(p);
    int len = strlen(str);
    if (strncmp(p->input + p->pos, str, len) == 0) {
        p->pos += len;
        return true;
    }
    return false;
}

int parse_number(Parser *p) {
    skip_whitespace(p);
    int sign = 1;
    if (p->input[p->pos] == '-') {
        sign = -1;
        p->pos++;
    }
    
    int num = 0;
    while (isdigit(p->input[p->pos])) {
        num = num * 10 + (p->input[p->pos] - '0');
        p->pos++;
    }
    return sign * num;
}

void parse_identifier(Parser *p, char *buf, int size) {
    skip_whitespace(p);
    int i = 0;
    while (i < size - 1 && (isalnum(p->input[p->pos]) || p->input[p->pos] == '_')) {
        buf[i++] = p->input[p->pos++];
    }
    buf[i] = '\0';
}

Node* parse_expression(Parser *p);

Node* parse_primary(Parser *p) {
    skip_whitespace(p);
    
    if (isdigit(p->input[p->pos]) || p->input[p->pos] == '-') {
        int num = parse_number(p);
        return make_int(p->heap, num);
    } else if (match(p, "(")) {
        Node *expr = parse_expression(p);
        if (!match(p, ")")) {
            fprintf(stderr, "Expected ')'\n");
            exit(1);
        }
        return expr;
    } else if (match(p, "fn")) {
        // Lambda: fn(x) { body }
        if (!match(p, "(")) {
            fprintf(stderr, "Expected '('\n");
            exit(1);
        }
        char param[32];
        parse_identifier(p, param, sizeof(param));
        if (!match(p, ")")) {
            fprintf(stderr, "Expected ')'\n");
            exit(1);
        }
        if (!match(p, "{")) {
            fprintf(stderr, "Expected '{'\n");
            exit(1);
        }
        Node *body = parse_expression(p);
        if (!match(p, "}")) {
            fprintf(stderr, "Expected '}'\n");
            exit(1);
        }
        return make_lambda(p->heap, param, body);
    } else if (isalpha(p->input[p->pos]) || p->input[p->pos] == '_') {
        char name[32];
        parse_identifier(p, name, sizeof(name));
        return make_var(p->heap, name);
    } else {
        fprintf(stderr, "Unexpected character: %c\n", p->input[p->pos]);
        exit(1);
    }
}

Node* parse_call(Parser *p) {
    Node *expr = parse_primary(p);
    
    // Function application
    skip_whitespace(p);
    while (p->input[p->pos] == '(') {
        p->pos++;
        Node *arg = parse_expression(p);
        if (!match(p, ")")) {
            fprintf(stderr, "Expected ')'\n");
            exit(1);
        }
        expr = make_app(p->heap, expr, arg);
    }
    
    return expr;
}

Node* parse_term(Parser *p) {
    Node *left = parse_call(p);
    
    skip_whitespace(p);
    while (p->input[p->pos] == '*' || p->input[p->pos] == '/') {
        char op = p->input[p->pos++];
        Node *right = parse_call(p);
        left = make_binary(p->heap, op == '*' ? NODE_MUL : NODE_DIV, left, right);
    }
    
    return left;
}

Node* parse_arithmetic(Parser *p) {
    Node *left = parse_term(p);
    
    skip_whitespace(p);
    while (p->input[p->pos] == '+' || p->input[p->pos] == '-') {
        char op = p->input[p->pos++];
        Node *right = parse_term(p);
        left = make_binary(p->heap, op == '+' ? NODE_ADD : NODE_SUB, left, right);
    }
    
    return left;
}

Node* parse_comparison(Parser *p) {
    Node *left = parse_arithmetic(p);
    
    skip_whitespace(p);
    if (p->input[p->pos] == '>' || p->input[p->pos] == '<' || p->input[p->pos] == '=') {
        char op = p->input[p->pos++];
        if (op == '=' && p->input[p->pos] == '=') {
            p->pos++;
        }
        Node *right = parse_arithmetic(p);
        
        if (op == '>') return make_binary(p->heap, NODE_GT, left, right);
        else if (op == '<') return make_binary(p->heap, NODE_LT, left, right);
        else return make_binary(p->heap, NODE_EQ, left, right);
    }
    
    return left;
}

Node* parse_expression(Parser *p) {
    skip_whitespace(p);
    
    if (match(p, "let")) {
        char var[32];
        parse_identifier(p, var, sizeof(var));
        if (!match(p, "=")) {
            fprintf(stderr, "Expected '='\n");
            exit(1);
        }
        Node *value = parse_expression(p);
        if (!match(p, "in")) {
            fprintf(stderr, "Expected 'in'\n");
            exit(1);
        }
        Node *body = parse_expression(p);
        return make_let(p->heap, var, value, body);
    } else if (match(p, "if")) {
        Node *cond = parse_expression(p);
        if (!match(p, "then")) {
            fprintf(stderr, "Expected 'then'\n");
            exit(1);
        }
        Node *then_br = parse_expression(p);
        if (!match(p, "else")) {
            fprintf(stderr, "Expected 'else'\n");
            exit(1);
        }
        Node *else_br = parse_expression(p);
        return make_if(p->heap, cond, then_br, else_br);
    } else if (match(p, "print")) {
        Node *expr = parse_expression(p);
        return make_print(p->heap, expr);
    }
    
    return parse_comparison(p);
}

// ============================================================================
// GRAPH VISUALIZATION
// ============================================================================

void print_node(Node *node, int indent) {
    for (int i = 0; i < indent; i++) printf("  ");
    
    if (!node) {
        printf("NULL\n");
        return;
    }
    
    switch (node->type) {
        case NODE_INT:
            printf("INT(%d)\n", node->data.int_val);
            break;
        case NODE_VAR:
            printf("VAR(%s)\n", node->data.var_name);
            break;
        case NODE_ADD: printf("ADD\n"); goto binary;
        case NODE_SUB: printf("SUB\n"); goto binary;
        case NODE_MUL: printf("MUL\n"); goto binary;
        case NODE_DIV: printf("DIV\n"); goto binary;
        case NODE_GT: printf("GT\n"); goto binary;
        case NODE_LT: printf("LT\n"); goto binary;
        case NODE_EQ: printf("EQ\n"); goto binary;
        binary:
            print_node(node->data.binary.left, indent + 1);
            print_node(node->data.binary.right, indent + 1);
            break;
        case NODE_IF:
            printf("IF\n");
            print_node(node->data.conditional.condition, indent + 1);
            print_node(node->data.conditional.then_branch, indent + 1);
            print_node(node->data.conditional.else_branch, indent + 1);
            break;
        case NODE_LET:
            printf("LET %s =\n", node->data.let_binding.var);
            print_node(node->data.let_binding.value, indent + 1);
            for (int i = 0; i < indent; i++) printf("  ");
            printf("IN\n");
            print_node(node->data.let_binding.body, indent + 1);
            break;
        case NODE_LAMBDA:
            printf("LAMBDA(%s)\n", node->data.lambda.param);
            print_node(node->data.lambda.body, indent + 1);
            break;
        case NODE_APP:
            printf("APP\n");
            print_node(node->data.app.func, indent + 1);
            print_node(node->data.app.arg, indent + 1);
            break;
        case NODE_PRINT:
            printf("PRINT\n");
            print_node(node->data.print.expr, indent + 1);
            break;
    }
}

// ============================================================================
// MAIN
// ============================================================================

int main(void) {
    Heap heap;
    Parser parser;
    Environment env;
    
    // Example 1: Basic arithmetic with let bindings
    printf("=== Example 1: Let Bindings ===\n");
    const char *prog1 = "let x = 10 in let y = 20 in print x + y";
    
    heap_init(&heap);
    parser_init(&parser, &heap, prog1);
    Node *ast1 = parse_expression(&parser);
    
    printf("\nGraph structure:\n");
    print_node(ast1, 0);
    
    printf("\nOutput:\n");
    env_init(&env);
    reduce(ast1, &env);
    env_free(&env);
    printf("Heap usage: %d nodes allocated\n", heap.alloc_count);
    
    // Example 2: Conditional
    printf("\n=== Example 2: Conditional ===\n");
    const char *prog2 = "let temp = 75 in if temp > 70 then print 1 else print 0";
    
    heap_init(&heap);
    parser_init(&parser, &heap, prog2);
    Node *ast2 = parse_expression(&parser);
    
    printf("\nGraph structure:\n");
    print_node(ast2, 0);
    
    printf("\nOutput:\n");
    env_init(&env);
    reduce(ast2, &env);
    env_free(&env);
    printf("Heap usage: %d nodes allocated\n", heap.alloc_count);
    
    // Example 3: Higher-order functions (lambda)
    printf("\n=== Example 3: Lambda Functions ===\n");
    const char *prog3 = "let double = fn(x) { x + x } in print double(21)";
    
    heap_init(&heap);
    parser_init(&parser, &heap, prog3);
    Node *ast3 = parse_expression(&parser);
    
    printf("\nGraph structure:\n");
    print_node(ast3, 0);
    
    printf("\nOutput:\n");
    env_init(&env);
    reduce(ast3, &env);
    env_free(&env);
    printf("Heap usage: %d nodes allocated\n", heap.alloc_count);
    
    // Example 4: Nested computations
    printf("\n=== Example 4: Complex Expression ===\n");
    const char *prog4 = "let a = 5 in let b = a * 2 in let c = b + 3 in print c * 2";
    
    heap_init(&heap);
    parser_init(&parser, &heap, prog4);
    Node *ast4 = parse_expression(&parser);
    
    printf("\nGraph structure:\n");
    print_node(ast4, 0);
    
    printf("\nOutput:\n");
    env_init(&env);
    reduce(ast4, &env);
    env_free(&env);
    printf("Heap usage: %d nodes allocated\n", heap.alloc_count);
    
    return 0;
}
