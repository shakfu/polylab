package clean

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestClean(t *testing.T) {
	txt := "Happy New Year 1, ok.!? Lovely"
	assert.Equal(t, Clean(txt), "happy old year ok lovely")
}
