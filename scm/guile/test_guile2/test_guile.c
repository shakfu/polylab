// see: http://edne.net/2016/11/03/scheme-of.html

#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

SCM hello_world() {
    printf("Hello World!\n");
    return SCM_UNSPECIFIED;
}

SCM my_home(void) {
    char *s = getenv("HOME");
    if (s == NULL)
        return SCM_BOOL_F;
    else
        return scm_from_locale_string(s);
}

void* register_functions(void* data) {
    scm_c_define_gsubr("hello-world", 0, 0, 0, &hello_world);
    scm_c_define_gsubr("my-home", 0, 0, 0, &my_home);
    return NULL;
}

int main(int argc, char* argv[]) {
    scm_with_guile(&register_functions, NULL);
    scm_c_eval_string("(use-modules (ice-9 readline))");
    scm_c_eval_string("(activate-readline)");
    scm_shell(0, NULL); 
    return EXIT_SUCCESS;
}