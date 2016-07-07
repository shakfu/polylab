#include <stdio.h>
#include "Judy.h"
 
int main()
{
  Pvoid_t assarray = (Pvoid_t) NULL;
  PWord_t value;
  int retval;
 
  /* populating */
  JSLI(value, assarray, "red");
  *value = 0xff0000;
  JSLI(value, assarray, "green");
  *value = 0x00ff00;
  JSLI(value, assarray, "blue");
  *value = 0x0000ff;
 
  /* retrieving existent */
  JSLG(value, assarray, "blue");
  printf("blue is #%06lx\n", (unsigned long)*value);
 
  /* unknown key */
  JSLG(value, assarray, "nonexistingkey");
  if ( value == NULL ) { fprintf(stderr, "key 'nonexistingkey' does not exists\n"); }
 
  /* deleting */
  JSLD(retval, assarray, "red");
  JSLG(value, assarray, "red");
  if ( value == NULL ) { fprintf(stderr, "key red does not exist anymore\n"); }
 
  JudySLFreeArray(&assarray, PJE0);
 
  return 0;
}