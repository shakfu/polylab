#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <ctype.h>

#include "employee.h"
#include "db.h"
#include "utils.h"
#include "common.h"

int main(int argc, char **argv)
{
	test_employees();
	// test_db();
	// test_utils();

	int aflag = 0;
	int bflag = 0;
	char *cvalue = NULL;
	int opt;

	opterr = 0;

	while ((opt = getopt(argc, argv, "abc:")) != -1)
		switch (opt) {
		case 'a':
			aflag = 1;
			break;
		case 'b':
			bflag = 1;
			break;
		case 'c':
			cvalue = optarg;
			break;
		case '?':
			if (optopt == 'c')
				log_err("Option -%c requires an argument.", optopt);
			else if (isprint(optopt))
				log_err("Unknown option `-%c'.", optopt);
			else
				log_err("Unknown option character `\\x%x'.", optopt);
			return 1;
		default:
			abort();
		}

	log_info("aflag = %d, bflag = %d, cvalue = %s", aflag, bflag, cvalue);

	for (int i=optind; i < argc; i++)
		debug("Non-option argument %s", argv[i]);
	return 0;
}
