#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *combine(const char *path1, const char *path2)
{
	char *destination = malloc(   (path1==NULL ? 0 : strlen(path1))
                                + 1 // for path separator
                                + (path2==NULL ? 0 : strlen(path2))
                                + 1 // for string terminator
                              );

    if (path1 == NULL && path2 == NULL) {
		strcpy(destination, "");
	} else if (path2 == NULL || !path2[0]) {
		strcpy(destination, path1);
	} else if (path1 == NULL || !path1[0]) {
		strcpy(destination, path2);
	} else {
		char directory_separator[] = "/";

	    const char last_char = path1[strlen(path1) - 1];
		strcpy(destination, path1);
		
        if (last_char != directory_separator[0])
			strcat(destination, directory_separator);
		strcat(destination, path2);
	}
    return destination;
}

int main(int argc, char **argv)

{
    char *target = combine("path1", "path2");
    printf("res: %s\n", target);
    free(target);
    return EXIT_SUCCESS;
}
