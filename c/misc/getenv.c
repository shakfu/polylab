#include <stdio.h>
#include <stdlib.h>
#include "common.h"

int main ()
{
  log_info("test");
  const char *s = getenv ("PATH");
  debug("PATH :%s", (s != NULL) ? s : "getenv returned NULL");
  log_info("end test");
}
