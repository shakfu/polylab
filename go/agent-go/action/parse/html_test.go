package parse

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

const HTML = `
<html>
    <head>
        <title>Mytitle</title>
    </head>
    <body>
        <p>Links:</p>
        <ul>
            <li><a href="foo">Foo</a></li>
            <li><a href="/bar/baz">BarBaz</a></li>
        </ul>
    </body>
</html>
`

func TestParseHtml(t *testing.T) {
	entries := ParseHtml(HTML)
	assert.Equal(t, entries[0].Url, "foo")
	assert.Equal(t, entries[0].Link, "Foo")
}
