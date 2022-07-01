echo "building zig executables"
for target in *.zig; do
    [ -f "$target" ] || break
    zig build-exe $target
done

