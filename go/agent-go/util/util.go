package util

import (
	"log"
)

func Check(e error) {
	if e != nil {
		log.Fatal(e)
	}
}

func OptionalString(defaultValue string, options []string) string {
	var input string
	if len(options) > 0 {
		input = options[0]
	} else {
		input = defaultValue
	}
	return input
}

func OptionalInt(defaultValue int, options []int) int {
	var input int
	if len(options) > 0 {
		input = options[0]
	} else {
		input = defaultValue
	}
	return input
}
