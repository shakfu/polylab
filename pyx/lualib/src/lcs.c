//      lcs.c
//
//      Copyright 2010 sa <sa@pysrv>
//
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.


#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX(A,B) (((A)>(B))? (A) : (B))

char * lcs(const char *a, const char * b) {
    int lena = strlen(a)+1;
    int lenb = strlen(b)+1;

    int bufrlen = 40;
    char bufr[40], *result;

    int i,j;
    const char *x, *y;
    int *la = (int *)calloc(lena*lenb, sizeof( int));
    int  **lengths = (int **)malloc( lena*sizeof( int*));
    for (i=0; i<lena; i++) lengths[i] = la + i*lenb;

    for (i=0,x=a; *x; i++, x++) {
        for (j=0,y=b; *y; j++,y++ ) {
            if (*x == *y) {
               lengths[i+1][j+1] = lengths[i][j] +1;
            }
            else {
               int ml = MAX(lengths[i+1][j], lengths[i][j+1]);
               lengths[i+1][j+1] = ml;
            }
        }
    }

    result = bufr+bufrlen;
    *--result = '\0';
    i = lena-1; j = lenb-1;
    while ( (i>0) && (j>0) ) {
        if (lengths[i][j] == lengths[i-1][j])  i -= 1;
        else if (lengths[i][j] == lengths[i][j-1]) j-= 1;
        else {
//          assert( a[i-1] == b[j-1]);
            *--result = a[i-1];
            i-=1; j-=1;
        }
    }
    free(la); free(lengths);
    return strdup(result);
}
