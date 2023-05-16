echo "cleaning built executables"
rm -rf zig-cache
rm *.o
for target in *.zig; do
    executable=`basename -s .zig $target`
    rm -f $executable
done

