package util

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestOptionalParams(t *testing.T) {
	assert := assert.New(t)
	f1 := func(inputs ...string) string {
		input := OptionalString("password: ", inputs)
		return input
	}
	assert.Equal(f1("a"), "a")
	assert.Equal(f1(), "password: ")

	f2 := func(inputs ...int) int {
		input := OptionalInt(100, inputs)
		return input
	}
	assert.Equal(f2(10), 10)
	assert.Equal(f2(), 100)
}
