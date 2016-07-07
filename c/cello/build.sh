
# see: http://libcello.org

targets="example test"

for target in example test
do
    gcc -std=gnu99 -o $target $target.c -lCello -lpthread -lm -ldl
done
