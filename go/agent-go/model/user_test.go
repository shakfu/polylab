package model

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestUser(t *testing.T) {
	assert := assert.New(t)
	// add tests here
	user := new(User)
	user.SetPassword("hello")
	assert.True(user.CheckPassword("hello"))
	assert.False(user.CheckPassword("bye"))
}
