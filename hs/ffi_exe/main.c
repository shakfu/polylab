#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Foo_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_Foo (void);
#endif

int
main (int argc, char *argv[])
{
  int i;

  hs_init (&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root (__stginit_Foo);
#endif

  for (i = 0; i < 5; i++)
    {
      printf ("%d\n", foo (2500));
    }

  hs_exit ();
  return 0;
}
