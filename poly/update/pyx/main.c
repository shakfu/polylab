#include <stdlib.h>
#include <stdio.h>
#include "update.h"


char *DEFAULT_DIR = "/home/sa/src";

int main(int argc, char **argv)
{
	int i;

	//printf("argc = %d\n", argc);

	if (argc < 2)
		update_srcdir(DEFAULT_DIR);
	else
		for (i = 0; i < argc; i++) {
			printf("argv[%d] = \"%s\"\n", i, argv[i]);
			update_srcdir(argv[i]);
		}
	return 0;
}
