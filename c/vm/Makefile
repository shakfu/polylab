.PHONY: all clean

all: dsl_stack dsl_register dsl_vm_heap


dsl_stack:
	@gcc -std=c11 -Wall -Wextra -o $@ dsl_vm_stack.c

dsl_register:
	@gcc -std=c11 -Wall -Wextra -o $@ dsl_vm_register.c

dsl_heap:
	@gcc -std=c11 -Wall -Wextra -o $@ dsl_vm_heap.c

clean:
	@rm -f dsl_stack dsl_register dsl_vm_heap