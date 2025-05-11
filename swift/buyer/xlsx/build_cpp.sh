# assumes
# brew install libxlswriter

name=app

echo "build dynamic"
g++ ${name}.cpp -o ${name}_dynamic \
    -I/usr/local/include -L/usr/local/lib -lxlsxwriter

echo "build static"
g++ ${name}.cpp \
    /usr/local/opt/libxlsxwriter/lib/libxlsxwriter.a \
    ./libz.a \
    -o ${name}_static -I/usr/local/include -L/usr/local/lib





