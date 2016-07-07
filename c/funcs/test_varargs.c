#include <stdio.h>
#include <stdarg.h>

double average(double v1 , double v2,...);

int main()
{

  printf("\n Average = %lf", average(3.5, 4.5, 0.0));
  printf("\n Average = %lf", average(1.0, 2.0));
  printf("\n Average = %lf\n", average(0.0,1.2,1.5));
}

double average( double v1, double v2,...)
{
  va_list parg;
  double sum = v1+v2;
  double value = 0;
  int count = 2;

  va_start(parg,v2);

  while((value = va_arg(parg, double)) != 0.0)
  {
    sum += value;
    printf("\n in averge = %.2lf", value);
    count++;
  }
  va_end(parg);                /* End variable argument process      */
  return sum/count;
}