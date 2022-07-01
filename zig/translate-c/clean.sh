echo "cleaning translated units"
rm -rf zig-cache
for target in *.c; do
    name=`basename -s .c $target`
    rm -f ${name}.zig
done
rm -f *.dylib *.a
