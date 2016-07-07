#include <Python.h>


void exec_pyfile(char *filename) {
    Py_Initialize();
    FILE *fp;
    fp = fopen(filename, "r");
    PyRun_AnyFile(fp, filename);
    Py_Finalize();
}

int main(int argc, char *argv[])
{
  exec_pyfile("ok.py");
  return 0;
}

