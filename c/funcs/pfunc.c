#include <stdio.h>

int sum(int x, int y)
{
   return x + y;
}

int product(int x, int y)
{
   return x * y;
}


int main()
{
   int a = 13;
   int b = 5;
   int result = 0;
   int (*pfun)(int, int);

   pfun = sum;
   result = pfun(a, b);
   printf("\npfun = sum   result = %d\n", result);

   pfun = product;
   result = pfun(a, b);
   printf("\npfun = product         result = %d\n", result);

}
