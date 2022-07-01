echo "cleaning built executables"
rm -rf zig-cache
for target in *.zig; do
    executable=`basename -s .zig $target`
    rm -f $executable
done

