package main

import (
	"database/sql"
	"fmt"
	_ "github.com/lib/pq"
	"log"
)

type Person struct {
	Id   int
	Name string
	Age  int
}

func (p *Person) Save(db *sql.DB) {
	_, err := db.Exec(
		"insert into person(name, age) values ($1, $2)",
		p.Name, p.Age)
	if err != nil {
		log.Fatal(err)
	}
}

func GetPersonById(db *sql.DB, id int) (*Person, error) {
	const query = "select name, age from person where id = $1"
	var person Person
	err := db.QueryRow(query, id).Scan(&person.Name, &person.Age)
	person.Id = id
	return &person, err
}

func main() {
	db, err := sql.Open("postgres", "user=sa dbname=sa password=sa sslmode=disable")
	if err != nil {
		log.Fatal(err)
	}

	people := []Person{
		Person{Name: "Jon", Age: 21},
		Person{Name: "Josh", Age: 11},
	}

	for _, p := range people {
		fmt.Printf("person.Name: %s\n", p.Name)
		p.Save(db)
	}

	names := []string{"Sandy", "Tom", "Mouse"}
	for i, name := range names {
		fmt.Printf("id: %d name: %s\n", i, name)
	}

	fmt.Printf("name is %s\n", people[1].Name)

	// create and save person
	person := Person{Name: "larry", Age: 100}
	person.Save(db)


	// test get by id
	p1, err := GetPersonById(db, 1)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("p.Name: %s\n", p1.Name)


	// basic query
	age := 21
	rows, err := db.Query("SELECT 10 + $1", age)
	for rows.Next() {
		var result int
		if err := rows.Scan(&result); err != nil {
			log.Fatal(err)
		}
		fmt.Printf("age is %d\n", result)
	}
}
