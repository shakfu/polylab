#include <stdio.h>
#include <dlfcn.h>
void
test (void)
{
  void *dl = dlopen ("./Test.so", RTLD_LAZY);
  printf ("%d\n", ((int (*)(int)) dlsym (dl, "hsfun")) (5));
  dlclose (dl);
}

int
main (int argc, char *argv[])
{
  test ();
  test ();
  test ();
  return 0;
}
