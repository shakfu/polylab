/* Loads a plugin with dlopen() */
#include <assert.h>
#include <dlfcn.h>
#include <errno.h>
#include <popt.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

char* sys_err_str()
{
    static char msgstr[200];

    if (errno != 0) {
        sprintf(msgstr, "%s", strerror(errno));
    } else {
        msgstr[0] = '\0';
    }
    return (msgstr);
}

void fatal(const char* message, ...)
{
    va_list args;

    va_start(args, message);
    vfprintf(stderr, message, args);
    fputc('\n', stderr);
    va_end(args);

    exit(1);
}

void usage(const char* progname)
{
    fatal("Usage: %s [-v] [-m plugin-name]", progname);
}

int main(const int argc, const char* argv[])
{

    char* plugin_name;
    char file_name[80];
    void* plugin;
    char* result;
    int value;
    int verbose = FALSE;
    int module_find = FALSE;
    int remaining = argc;
    char** leftover;

    /* The functions we will find in the plugin */
    typedef void (*init_f)();
    init_f init;
    typedef int (*query_f)();
    query_f query;

    /* popt variables */
    struct poptOption options[] = {
        { "verbose", 'v', POPT_ARG_NONE, &verbose, 'v' },
        { "module", 'm', POPT_ARG_STRING, &plugin_name, 'm' },
        { NULL, 0, 0, NULL, 0, NULL, NULL }
    };
    poptContext poptcon = poptGetContext(NULL, argc, argv, options, 0);

    while ((!module_find) && (value = poptGetNextOpt(poptcon)) != -1) {
        printf("DEBUG: after poptGetNextOpt, value is %d\n", value);
        if (value < -1) {
            fatal("%s: %s", poptBadOption(poptcon, POPT_BADOPTION_NOALIAS),
                  poptStrerror(value));
        }
        remaining--;
        switch ((char)value) {
        case 'v':
            break;
        case 'm':
            remaining--;
            module_find = TRUE;
            break;
        default:
            usage(argv[0]);
        }
    }

    if (!strcmp(plugin_name, "")) {
        printf("No plugin, quitting...\n");
        exit(0);
    }
    sprintf(file_name, "%s.so", plugin_name);
    if (verbose) {
        printf("Opening %s...\n", file_name);
    }
    plugin = dlopen(file_name, RTLD_NOW);
    if (!plugin) {
        fatal("Cannot load %s: %s", plugin_name, dlerror());
    }
    init = dlsym(plugin, "init");
    result = dlerror();
    if (result) {
        fatal("Cannot find init in %s: %s", plugin_name, result);
    }
    if (verbose) {
        printf("Running init()...\n");
    }
    remaining--; /* No argv[0] this time */
    leftover = (char**)&argv[argc - remaining];
    printf("DEBUG: argc=%d remaining=%d leftover[0]=%s\n", argc, remaining,
           leftover[0]);
    init(remaining, leftover);
    query = dlsym(plugin, "query");
    result = dlerror();
    if (result) {
        fatal("Cannot find query in %s: %s", plugin_name, result);
    }
    printf("Result of plugin %s is %d\n", plugin_name, query());
    exit(0);
}
