package core

import (
	"testing"
)

// func display(name string, age int) {
//     log.Printf("name: %s age: %d", name, age)
// }

// func GetDispatcher() goevent.Table {
//     table := goevent.NewTable()

//     table.On("display", display)

//     table.On("foo", func(i int) {
//      log.Printf("foo: %d\n", i)
//     })

//     table.On("bar", func(s string) {
//      log.Printf("bar: %s\n", s)
//     })

//     return table
// }

func TestEvent(t *testing.T) {

	d := EventDispatcher()

	err := d.Trigger("display", "sam", 2012)
	if err != nil {
		t.Fatal(err)
	}

	err = d.Trigger("foo", 199, 21)
	if err == nil {
		t.Fatal(err)
	}
}
