mod models {

    pub enum PersonType {
        GoodPerson,
        BadPerson,
    }

    pub struct Person {
        pub ptype: PersonType,
        pub name: String,
    }


    pub fn display(person: Person) {
        match person.ptype {
            PersonType::GoodPerson => println!("{} is a good person.", person.name),
            PersonType::BadPerson => println!("{} is a bad person.", person.name)
        }
    }
}


fn main() {

    let gperson = models::Person {
        ptype: models::PersonType::GoodPerson,
        name: String::from("sam"),
    };

    let bperson = models::Person {
        ptype: models::PersonType::BadPerson,
        name: String::from("joe"),
    };

    let persons = vec![gperson, bperson];

    for p in persons {
        models::display(p);
    }

}
