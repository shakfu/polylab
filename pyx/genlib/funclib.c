#include <stdio.h>
#include <string.h>
#include <stddef.h>

/*[[[cog
import cog
fnames = ['f1', 'f2']
for fn in fnames:
  cog.outl("void %s();" % fn)

]]]*/
//[[[end]]]


void println(char *string)
{
  printf("There is a something in the work where I could " 
         "be making a nice %s and it would nice of you indeed\n",
        string);
}

int findMin(int d1, int d2, int d3)
{
   /*
    * return min of d1, d2 and d3.
    */
   if(d1 < d2 && d1 < d3)
       return d1;
   else if(d1 < d3)
       return d2;
   else if(d2 < d3)
       return d2;
   else
      return d3;
}

int findEditDistance(char *s1, char *s2) 
{
    /*
     * returns edit distance between s1 and s2.
     */
   int d1, d2, d3;

   if(*s1 == 0)
       return strlen(s2);
   if(*s2 == 0)
       return strlen(s1);
   if(*s1 == *s2)
       d1 = findEditDistance(s1+1, s2+1);
   else
       d1 = 1 + findEditDistance(s1+1, s2+1);    // update.
   d2 = 1+findEditDistance(s1, s2+1);                   // insert.
   d3 = 1+findEditDistance(s1+1, s2);                   // delete.

   return findMin(d1, d2, d3);
}

double add(double x, double y)
{
    return x + y;
}


