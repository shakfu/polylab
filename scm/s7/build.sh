# see: https://ccrma.stanford.edu/software/snd/snd/s7.html

gcc -c s7.c -I.
#gcc -o repl repl.c s7.o -lm -I. -ldl
#gcc -o repl repl.c s7.o -Wl,-export-dynamic -lm -I. -ldl
gcc s7.c -o repl -DWITH_MAIN -I. -O2 -g -lreadline -lncurses -lrt -ldl -lm -Wl,-export-dynamic
strip repl

gcc -o ext extension.c s7.o -Wl,-export-dynamic -lm -I. -ldl
