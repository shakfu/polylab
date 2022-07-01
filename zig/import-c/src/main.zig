const std = @import("std");
const print = std.debug.print;
// const expect = std.testing.expect;

const c = @cImport({
    @cInclude("calc.h");
});

pub fn main() anyerror!void {
    var sum = c.add(1, 2);
    // Note that info level log messages are by default printed only in Debug
    // and ReleaseSafe build modes.
    std.log.info("log sum: {d}", .{sum});

    // print outputs in all cases
    print("print sum: {d}\n", .{sum});
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
