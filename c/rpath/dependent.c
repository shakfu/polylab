/* Dependent.c
 * Tests libRatings.A.dylib 1.0 as a dependent library.
 *****************************************************/
 
#include <stdio.h>
#include <string.h>
#include "ratings.h"
 
#define PASSFAIL "Passed":"Failed"
#define UNTST "Untested"
 
int main(int argc, char **argv) {
    printf("[start_test]\n");
 
    // Setup.
    addRating(NULL);
    addRating("");
    addRating("*");
    addRating("**");
    addRating("***");
    addRating("*****");
    addRating("*****");
 
    // ratings.
    printf("[%s] ratings(): %s\n",
        __FILE__, (ratings() == 6? PASSFAIL));
 
    // meanRating.
    printf("[%s] meanRating(): %s\n",
        __FILE__, (strcmp(meanRating(), "**") == 0)? PASSFAIL);
 
    // clearRatings.
    clearRatings();
    printf("[%s] clearRatings(): %s\n",
        __FILE__, (ratings() == 0? PASSFAIL));
 
    printf("[end_test]\n");
    return 0;
}

