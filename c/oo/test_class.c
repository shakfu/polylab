#include <stdlib.h>
#include <stdio.h>
#include <string.h>



typedef struct sMyClass
{
  int variable;
} *MyClass;

MyClass  MyClass_new()
{
  MyClass pthis = (MyClass)malloc( sizeof(struct sMyClass) );
  //memset(pthis, 0, sizeof(struct sMyClass) );
  pthis->variable = 0;
  return pthis;
}

void MyClass_delete(MyClass* pthis)
{
  if(pthis && *pthis)
  {
    free(*pthis);
    *pthis = NULL;
  }
}

void MyClass_someMethod(MyClass pthis)
{
  pthis->variable = 1;
  printf("var: %d\n", pthis->variable);
}

int main(void)
{
    MyClass obj = MyClass_new();
    MyClass_someMethod(obj);
    MyClass_delete(&obj);

    return 0;
}

