/*
 * Plugin example. query() stats the file given as argument and returns the
 * ID of the owner
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <popt.h>

/* Defaults */
int user = 0;
int group = 0;

/* Globals */
char *file;

void
usage (const char *msg)
{
  if (msg)
    {
      printf ("Error: %s\n", msg);
    }
  printf ("Plugin stat usage: -u|-g file-name\n");
  exit (1);
}

void
init (const int argc, const char *argv[])
{
  int value;
  char *msg = malloc (80);
  /* popt variables */
  struct poptOption options[] = {
    {"user", 'u', POPT_ARG_NONE, &user, 'u'},
    {"group", 'g', POPT_ARG_NONE, &group, 'g'},
    {NULL, '\0', 0, NULL, 0, NULL, NULL}
  };
  poptContext poptcon;

  poptcon = poptGetContext (NULL, argc,
					argv,
					options,
					POPT_CONTEXT_KEEP_FIRST);
  printf ("DEBUG: argc=%d argv[0]=%s\n", argc, argv[0]);

  while ((value = poptGetNextOpt (poptcon)) > 0)
    {
      if (value < -1)
	{
	  sprintf (msg, "%s: %s",
		   poptBadOption (poptcon, POPT_BADOPTION_NOALIAS),
		   poptStrerror (value));
	  usage (msg);
	}
      switch ((char) value)
	{
	case 'u':
	  user = 1;
	  break;
	case 'g':
	  group = 1;
	  break;
	default:
	  printf ("Unknown option for the plugin %d (%s)\n", value, optarg);
	  usage ("");
	}
    }
  if (user && group)
    {
      usage ("-u or -g but not both");
    }
  /* TODO: check we only have one arg 
     if (argc != 1) {
     usage("One and only one file name");
     } */
  file = (char *) poptGetArg (poptcon);
}

int
query ()
{
  int result;
  struct stat *buf = malloc (sizeof (struct stat));
  result = stat (file, buf);
  if (result)
    {
      printf ("Cannot stat %s: %s\n", file, strerror (errno));
      exit (1);
    }
  if (user)
    {
      return buf->st_uid;
    }
  else if (group)
    {
      return buf->st_gid;
    }
  else
    {
      usage ("Internal error, neither user nor group set");
      /* Never called */
      return 0;
    }
}
