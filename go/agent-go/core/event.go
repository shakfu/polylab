package core

import (
	"github.com/pocke/goevent"
	"log"
)

func display(name string, age int) {
	log.Printf("name: %s age: %d", name, age)
}

func EventDispatcher() goevent.Table {
	table := goevent.NewTable()

	table.On("display", display)

	table.On("foo", func(i int) {
		log.Printf("foo: %d\n", i)
	})

	table.On("bar", func(s string) {
		log.Printf("bar: %s\n", s)
	})

	return table
}
