echo "building zig translations"
for target in *.zig; do
    [ -f "$target" ] || break
    zig build-exe $target
done
