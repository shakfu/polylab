#ifdef C2NIM
#  dynlib calcdll
#  cdecl
#  if defined(windows)
#    define calcdll "calc.dll"
#  elif defined(macosx)
#    define calcdll "libcalc.dylib"
#  else
#    define calcdll "libcalc.so"
#  endif
#endif

#define MY_CONSTANT 10

int add(int x, int y);

int sum_array(int length, int *array);