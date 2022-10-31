// see: http://edne.net/2016/11/03/scheme-of.html

#include <stdio.h>
#include <libguile.h>

SCM hello_world() {
    printf("Hello World!\n");
    return SCM_UNSPECIFIED;
}

void* register_functions(void* data) {
    scm_c_define_gsubr("hello-world", 0, 0, 0, &hello_world);
    return NULL;
}

int main(int argc, char* argv[]) {
    scm_with_guile(&register_functions, NULL);
    scm_c_eval_string("(use-modules (ice-9 readline))");
    scm_c_eval_string("(activate-readline)");
    scm_shell(0, NULL); 
    return EXIT_SUCCESS;
}