struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl Circle {
    fn new(x: f64, y: f64, radius: f64) -> Self {
        Self {x, y, radius}
    }
    fn area(&self) -> f64 {
        std::f64::consts::PI * f64::powf(self.radius, 2.0)
    }

    fn circumference(&self) -> f64 {
        2.0 * std::f64::consts::PI * self.radius
    }
}


fn main() {
    let c = Circle::new(21.0, 30.0, 10.0);
    println!("area: {}", c.area());
    println!("circumference: {}", c.circumference());
    println!("x: {}", c.x);
    println!("y: {}", c.y);
}
