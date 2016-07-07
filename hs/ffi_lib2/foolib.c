#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "foo_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_Foo ( void );
#endif


HsBool foolib_init(void){ 
    // int argc = 0;
    // char *argv[] = {'\0'};

    // Initialize Haskell runtme 
    hs_init(0, 0); 

    // Tell Haskell about all root modules 
    hs_add_root(__stginit_Foo); 

    // do any other initialization here and 
    // return false if there was a problem 
    return HS_BOOL_TRUE; 
} 

void foolib_end(void){ 
    hs_exit(); 
}

