#include <stdio.h>

int change(int* pnumber);   /* Function prototype               */

int main()
{
   int number = 20;
   int* pnumber = &number;
   int result = 0;

   result = change(pnumber);
   printf("\nIn main, result = %d\tnumber = %d\n", result, number);
}

int change(int *pnumber)
{
   *pnumber *= 2;
   printf("\n In function change, *pnumber = %d\n", *pnumber );
   return *pnumber;
}
