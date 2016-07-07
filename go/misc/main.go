/*
The documentation of the package
*/

package main

import (
	"fmt"
	"os"
	"text/template"
	"time"
)

// rename a type
type Day int
type Hash map[string]string

const Pi = 3.14

// constants
const (
	KING = "animal"
	//QUEEN = 10
)



var m = map[string]int{
	"hello": 10,
	"nice":  200,
}

type MyError struct {
	When time.Time
	What string
}

func (e *MyError) Error() string {
	return fmt.Sprintf("at %v, %s",
		e.When, e.What)
}

func run() error {
	return &MyError{
		time.Now(),
		"it didn't work",
	}
}

func gen() {
	i := 1
	for i <= 10 {
		fmt.Println(i)
		i += 1
	}
}

func dispatch(i int) {
	switch i {
	case 0:
		fmt.Println("Zero")
	case 1:
		fmt.Println("One")
	case 2:
		fmt.Println("Two")
	case 3:
		fmt.Println("Three")
	case 4:
		fmt.Println("Four")
	case 5:
		fmt.Println("Five")
	default:
		fmt.Println("Unknown Number")
	}
}

type PersonI interface {
	SetAge(int)   // name of the object
	Grow(int) int // its value
}

type Reader interface {
	Read(b []byte) (n int, err error)
}

type Writer interface {
	Write(b []byte) (n int, err error)
}

type ReadWriter interface {
	Reader
	Writer
}

type Person struct {
	Name string // name of the object
	Age  int    // its value
}

func (p *Person) SetAge(yrs int) {
	p.Age = yrs
}

func (p *Person) Grow(yrs int) int {
	return yrs + p.Age
}

func add(x int, y int) int {
	return x + y
}

func main() {
	// tests go functions
	var p PersonI
	var w Writer

	// os.Stdout implements Writer
	w = os.Stdout
	fmt.Fprintf(w, "hello writer\n")

	// generate list
	gen()

	person := Person{"jack", 10}
	sam := Person{Name: "sam", Age: 100}
	tmpl, err := template.New("test").Parse("{{.Name}} - {{.Age}}\n")
	if err != nil {
		panic(err)
	}
	err = tmpl.Execute(os.Stdout, sam)
	if err != nil {
		panic(err)
	}
	sue := new(Person)
	sue.SetAge(10)
	p = &sam
	fmt.Println(sue.Grow(10))
	fmt.Println(p.Grow(100))
	fmt.Println("Hello There")
	fmt.Printf("example: %d\n", add(10, 20))
	person.Age = 20
	sue.Name, sue.Age = "sue", 21
	fmt.Println(person, sam, sue, m)
	if err := run(); err != nil {
		fmt.Println(err)
	}
	dispatch(4)
}
