
// /usr/local/Cellar/guile/3.0.8

#include <libguile.h>
#include <stdlib.h>

static SCM my_home(void) {
    char *s = getenv("HOME");
    if (s == NULL)
        return SCM_BOOL_F;
    else
        return scm_from_locale_string(s);
}

static void inner_main(void *data, int argc, char **argv) {
    scm_c_define_gsubr("my-home", 0, 0, 0, my_home);
    scm_shell(argc, argv);
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0; /* never reached */
}
