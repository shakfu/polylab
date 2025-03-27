#include <assert.h>
#include <errno.h>
#include <popt.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
    fatal("Usage: %s [-v] [-m a-name] [other options]", progname);
}

int main(const int argc, const char* argv[])
{

    int verbose = 0;
    int module_find = 0;
    char* name = "";

    char* result;
    int value;
    int remaining = argc;
    char** leftover;

    /* popt variables */
    struct poptOption options[] = {
        { "verbose", 'v', POPT_ARG_NONE, &verbose, 'v' },
        { "module", 'm', POPT_ARG_STRING, &name, 'm' },
        { NULL, '\0', 0, NULL, 0, NULL, 0 }
    };
    poptContext poptcon = poptGetContext(NULL, argc, argv, options, 0);

    while ((!module_find) && (value = poptGetNextOpt(poptcon)) > 0) {
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
            module_find = 1;
            break;
        default:
            usage(argv[0]);
        }
    }

    if (!strcmp(name, "")) {
        printf("No name, quitting...\n");
        exit(0);
    }
    remaining--;
    leftover = (char**)&argv[argc - remaining];
    assert(leftover != NULL); /* See popt(3), leftobver should hold at least
                                 one NULL string */
    printf("DEBUG: argc=%d remaining=%d leftover[0]=%s leftover[1]=%s "
           "leftover[2]=%s\n",
           argc, remaining, leftover[0], leftover[1], leftover[2]);
    exit(0);
}
