const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

fn fibonacci(index: u32) u32 {
    if (index < 2) return index;
    return fibonacci(index - 1) + fibonacci(index - 2);
}

fn foo(x: anytype) @TypeOf(x) {
    // note that this if statement happens at compile-time, not runtime.
    if (@TypeOf(x) == i64) {
        return x + 2;
    } else {
        return 2 * x;
    }
}

pub fn main() void {
    var x: i64 = 47;
    var y: i32 = 47;

    print("i64-foo: {}\n", .{foo(x)});
    print("i32-foo: {}\n", .{foo(y)});
}

test "fibonacci" {
    // test fibonacci at run-time
    try expect(fibonacci(30) == 832040);

    // test fibonacci at compile-time
    comptime {
        try expect(fibonacci(30) == 832040);
    }
}
