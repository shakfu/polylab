const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;
const mem = std.mem;

const Person = struct {
    age: i8,
    name: []const u8,
};

var p = Person{
    .age = 10,
    .name = "sam",
};

var p_ptr: *Person = &p;

const Vector = struct {
    x: f32,
    y: f32,
    z: f32,

    pub fn init(x: f32, y: f32, z: f32) Vector {
        return Vector{
            .x = x,
            .y = y,
            .z = z,
        };
    }

    pub fn dot(self: Vector, other: Vector) f32 {
        return (self.x * other.x +
            self.y * other.y +
            self.z * other.z);
    }
};

pub fn main() void {
    const string = "this is the structs file";
    print("file: {s}\n", .{string});
}

fn age_one_year(person: *Person) void {
    person.age += 1;
}

test "structs" {
    try expect(p.age == 10);
    age_one_year(p_ptr);
    try expect(p.age == 11);
    age_one_year(&p);
    try expect(p.age == 12);
    try expect(mem.eql(u8, p.name, "sam"));
}

test "vector" {
    const v1 = Vector.init(1.0, 0.0, 0.0);
    const v2 = Vector.init(0.0, 1.0, 0.0);
    try expect(v1.dot(v2) == 0.0);
    try expect(Vector.dot(v1, v2) == 0.0);
}
