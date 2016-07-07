PWD=$(pwd)

# demonstrates two methods: dynamic linking and embedding of luacode
# in the executable and hybrid static linking (which looks good)


echo "compiling lua files into library"
LUAFILES=person.lua
for f in $LUAFILES; do
    ./luajit -b $f `basename $f .lua`.o
done
ar rcus libmyluafiles.a *.o
rm *.o

echo "shared library compiling"
gcc -o embed embed.c -I. -L. -lluajit -lm -ldl -Wl,--whole-archive -lmyluafiles -Wl,--no-whole-archive -Wl,-E
rm $PWD/libluajit-5.1.so.2
ln -s $PWD/libluajit.so $PWD/libluajit-5.1.so.2

# see http://comments.gmane.org/gmane.comp.lang.lua.luajit/1393
# note: dlopen (from glibc) is dynamically linked
echo "static library compilation (mixed)"
gcc -o embed_static embed.c -I. -L. -Wl,-Bstatic -lluajit -lm -Wl,--whole-archive -lmyluafiles -Wl,--no-whole-archive -Wl,-E -Wl,-Bdynamic -ldl

echo "rename lua file for testing"
mv person.lua person.lua.bak

echo "testing shared exec"
LD_LIBRARY_PATH=. ./embed

echo "testing static exec" 
./embed_static

echo "revert lua file for testing"
mv person.lua.bak person.lua
