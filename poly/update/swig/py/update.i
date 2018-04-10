%module update

%{
#define SWIG_FILE_WITH_INIT
extern char *DEFAULT_DIR;
extern void update_srcdir(char *path);
%}

extern char *DEFAULT_DIR;
extern void update_srcdir(char *path);
