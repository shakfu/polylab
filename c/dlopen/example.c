/* Plugin example. query() returns the length of its argument */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int state;

void
usage ()
{
  printf ("Plugin example usage: frobnication-name\n");
  exit (1);
}

void
init (int argc, char *argv[])
{
  if (argc != 1)
    {
      usage ();
    }
  state = strlen (argv[0]);
}

int
query ()
{
  return state;
}
