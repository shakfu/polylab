const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

fn add(x: i8, y: i8) i8 {
    return x + y;
}

pub fn main() void {
    const string = "this is a string";
    print("str: {s}\n", .{string});
}

// higher level functions

const bfunc = fn (x: i8, y: i8) i8;

fn call_f(f: bfunc, x: i8, y: i8) i8 {
    return f(x, y);
}

test "functions" {
    try expect(add(1, 2) == 3);
    try expect(add(0, 10) == 10);
    try expect(call_f(add, 2, 3) == 5);
}
