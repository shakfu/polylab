package model

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestDatabase(t *testing.T) {
	assert := assert.New(t)

	// creation
	db := NewDatabase("sa", "sa", "sa")
	assert.NotNil(db)

	// connect
	// db.Open()

	// execute
	db.Execute("select 1+1")

	// close
	db.Close()

	// reopen
	db.Open()

	// re-close
	db.Close()
}
