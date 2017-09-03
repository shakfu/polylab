#include <stdio.h>

int sum(int,int);
int product(int,int);
int difference(int,int);
int any_function(int(*pfun)(int, int), int x, int y);

int main()
{
   int a = 13;
   int b = 51;
   int result = 0;
   int (*pf)(int, int) = sum;

   result = any_function(pf, a, b);

   printf("\nresult = %d", result );

   result = any_function(product, a, b);

   printf("\nresult = %d", result );

   printf("\nresult = %d\n", any_function(difference, a, b));
}

int any_function(int(*pfun)(int, int), int x, int y){
   return pfun(x, y);
}

int sum(int x, int y){
   return x + y;
}

int product(int x, int y){
   return x * y;
}

int difference(int x, int y){
   return x - y;
}

