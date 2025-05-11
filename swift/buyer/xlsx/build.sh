# assumes
# brew install libxlswriter

name=demo

echo "build dynamic"
cc ${name}.c -o ${name}_dynamic \
    -I/usr/local/include -L/usr/local/lib -lxlsxwriter

echo "build static"
cc ${name}.c \
    /usr/local/opt/libxlsxwriter/lib/libxlsxwriter.a \
    ./libz.a \
    -o ${name}_static -I/usr/local/include -L/usr/local/lib





