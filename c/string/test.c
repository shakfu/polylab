#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>


int main(void)
{
    char pathname[MAXPATHLEN];
    const char* default_dir = "/home/sa/src";

    // test path operations
    // char filename[] = "/home/a/b/file.py";
    char* filename = "/home/sa/src/file.py";
    // print dirname
    // string dir = (char *) dirname(filename);
    strcpy(pathname, filename);
    //    char * dir = dirname(filename);
    printf("dirname: %s\n", dirname(pathname));

    return EXIT_SUCCESS;
}
