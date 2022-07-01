# make a static lib (default)
zig build-lib mathtest.zig

# make d dynamic lib
zig build-lib mathtest.zig -dynamic

zig build test