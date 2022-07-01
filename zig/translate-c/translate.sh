
echo "translating c files to zig"
for target in *.c; do
    [ -f "$target" ] || break
    name=`basename -s .c $target`
    zig translate-c $target > ${name}.zig
    zig build-lib ${name}.zig -O ReleaseSmall -dynamic
    zig build-lib ${name}.zig -O ReleaseSmall -static
done

