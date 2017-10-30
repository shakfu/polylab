use std::fmt;

struct Person {
    name: String,
    age: u8,
    cash: f64,
}

impl Person {
    fn deposit(&mut self, value: f64){
        self.cash = self.cash + value
    }
}

impl fmt::Display for Person {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {})", self.name, self.age, self.cash)
    }
}


fn foo(person: &Person) -> u8 {
    person.age * 10
}


fn main() {
    let mut person: Person = Person { 
        name: String::from("sam"),
        age: 21,
        cash: 100.0,
    };

    person.deposit(50.0);

    println!("person name: {} with age: {} has cash: {}", 
             person.name,
             person.age,
             person.cash);

    println!("age * 10: {}", foo(&person));

    println!("The person is {}", person);

}
