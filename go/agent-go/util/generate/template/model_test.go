package model

import (
    "github.com/stretchr/testify/assert"
    "testing"
)

func Test{{.Name}}(t *testing.T) {
    assert := assert.New(t)
    // add tests here
    assert.Equal(1, 1)
}
