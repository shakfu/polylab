echo "test lua embed"
gcc main.c -o swap -I/usr/local/opt/lua/include/lua -llua

echo "test lua extension"
gcc mylib.c -shared -o mylib.so -fPIC -I/usr/local/opt/lua/include/lua -llua
