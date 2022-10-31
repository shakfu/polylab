
pub mod demo;

struct Person {
    name: String,
    email: String,
    active: bool,
    count: u64,
}

impl Person {
    fn new(name: String, email: String) -> Person {
        Person {name: name, email: email, active: false, count: 0 }
    }

    fn display(&self) {
        println!("name: {} email: {} active: {} count: {}", 
            self.name,
            self.email,
            self.active,
            self.count,
        );
    }
}

struct Party {
    name: &'static str,
    age: u32,
}

impl Party {
    fn new(name: &'static str, age: u32) -> Party {
        Party {name: name, age: age}
    }
}


fn main() {

    let m = demo::Machine {};
    println!("Hello {:?}", m);

    let p1: Party = Party::new("foo", 20);
    println!("p1: name {} age {}", p1.name, p1.age);

    let s1 = "a string";
    println!("s1: {}", s1);
    let slice1: &'static str = "nice one (readonly)";
    let slice2: &str = "good one";
    let p = Person::new(
        "sam".to_string(), 
        "me@me.org".to_string()
    );

    println!("slice1: {}", slice1);
    let words = slice1.split_whitespace().rev();
    for w in words {
        println!("w: {}", w);
    }

    println!("slice2: {}", slice2);

    p.display();
}
