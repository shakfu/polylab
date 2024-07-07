package render

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestRender(t *testing.T) {
	tmpl := "{{ .key }}"
	dict := map[string]interface{}{"key": "value"}
	expected := "value"
	assert.Equal(t, expected, Render(tmpl, dict))
}
