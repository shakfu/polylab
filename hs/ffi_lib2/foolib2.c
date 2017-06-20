#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "foo_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_Foo ( void );
#endif


extern void foolib_init(void);

extern void foolib_setup(void) { 

    // Initialize Haskell runtme 
    hs_init(0, 0); 

    // Tell Haskell about all root modules 
    hs_add_root(__stginit_Foo); 

    // do any other initialization here and 
    // return false if there was a problem 
    foolib_init();
} 

void foolib_end(void){ 
    hs_exit(); 
}

