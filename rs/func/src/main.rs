type Action<T> = fn(T, T) -> T;


fn add(x: i16, y: i16) -> i16 {
    x + y
}

fn adder<T>(f: Action<T>, x: T, y: T) -> T {
    f(x, y)
}

// fn adder<T>(f: fn(T, T) -> T, x: T, y: T) -> T {
//     f(x, y)
// }

fn get_result() -> i16 {
    adder(add, 1, 2)
}


fn main() {
    println!("result: {:?}", get_result());
}
