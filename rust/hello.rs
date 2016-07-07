
// use std::f64::consts::PI;

mod farm {
    pub fn chicken() { println!("cluck cluck"); }
}

static DUDE: i32 = 50;

enum Direction {
    North,
    East,
    South,
    West
}

struct Point {
    x: f64,
    y: f64
}

trait Printable {
    fn print(&self);
}

impl Printable for Point {
    fn print(&self) {
        println!("<Point ({}, {})>", self.x, self.y); 
    }
}

enum Shape {
    Circle(Point, f64),
    Rectangle(Point, Point)
}

fn show_circle(p: Point, r: f64) {
    println!("<Circle ({}, {}) r: {}>", p.x, p.y, r);
}

fn show_rectangle(p1: Point, p2: Point) {
    println!("<Rectangle ({}, {} & {}, {})>", p1.x, p1.y, p2.x, p2.y);
}

impl Shape {
    fn show(self) {
        match self {
            Shape::Circle(p, f) => show_circle(p, f),
            Shape::Rectangle(p1, p2) => show_rectangle(p1, p2)
        }
    }
}


fn is_large(x: i32) -> bool {
    x > 10
}

fn point_from_direction(dir: Direction) -> Point {
    match dir {
        Direction::North => Point { x:  0.0, y:  1.0 },
        Direction::East  => Point { x:  1.0, y:  0.0 },
        Direction::South => Point { x:  0.0, y: -1.0 },
        Direction::West  => Point { x: -1.0, y:  0.0 }
    }
}

fn main() {
    let p = Point {x: 1.1, y: 2.2};
    p.print();

    let x = 11;
    if is_large(x) {
        println!("hello?");
        println!("x:{} y:{}", p.x, p.y);
    }
    // a comment is here
    println!("DUDE: {}", DUDE);

    let my_number:i32 = 1;
    match my_number {
      0      => println!("zero"),
      1 | 2  => println!("one or two"),
      3...10 => println!("three to ten"),
      _      => println!("something else")
    }

    let c = Shape::Circle(Point {x: 1.0, y: 2.2}, 3.0);
    c.show();

    let c1 = Shape::Circle(point_from_direction(Direction::North), 2.1);
    c1.show();

    let c2 = Shape::Circle(point_from_direction(Direction::East), 2.1);
    c2.show();

    let c3 = Shape::Circle(point_from_direction(Direction::West), 2.1);
    c3.show();

    let c4 = Shape::Circle(point_from_direction(Direction::South), 2.1);
    c4.show();

    let r = Shape::Rectangle(Point{x:1.1, y:2.1}, Point{x:2.3,y:3.1});
    r.show();

    ::farm::chicken();
}