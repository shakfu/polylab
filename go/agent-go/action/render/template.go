package render

import (
	"../../util"
	"bytes"
	"text/template"
)

func Render(tmpl string, dict map[string]interface{}) string {
	var doc bytes.Buffer
	t := template.Must(template.New("").Parse(tmpl))
	err := t.Execute(&doc, dict)
	util.Check(err)
	return doc.String()
}
