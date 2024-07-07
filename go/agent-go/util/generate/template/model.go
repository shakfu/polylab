package model

import (
    "database/sql"
    "encoding/json"
    "fmt"
    "gopkg.in/yaml.v2"
    "io"
    "../util/log"
    "strings"
)

type {{.Name}} struct {
{{ range $key := .Fields }}    {{$key}}   {{ index $.FieldMap $key }}
{{end}}
}

// check for model implementation
var _ Model = &{{.Name}}{}

func New{{.Name}}({{range $f := .Fields}}{{$t := index $.FieldMap $f}}{{lower $f}} {{$t}}, {{end}}) *{{.Name}} {
    return &{{.Name}} { {{range .Fields}}{{. | lower}}, {{end}} }
}

func ({{.Handle}} *{{.Name}}) String() string {
    return {{.ToStringFunc}}
}

func ({{.Handle}} *{{.Name}}) Display() {
    log.Info({{.Handle}}.String())
}

func ({{.Handle}} *{{.Name}}) Insert(db *sql.DB) {
    _, err := db.Exec({{.SqlInsert}})
    if err != nil {
        log.Fatal("cannot create entry: %v", err)
    }
}

func Get{{.Name}}ByID(db *sql.DB, id int) (*{{.Name}}, error) {
    const query = "select * from {{.Lower}} where id = $1"
    var {{.Lower}} {{.Name}}
    err := db.QueryRow(query, id).Scan({{.Accessors "scan"}})
    return &{{.Lower}}, err
}

func ({{.Handle}} *{{.Name}}) ToYAML() string {
    yml, err := yaml.Marshal({{.Handle}})
    if err != nil {
        log.Fatal("cannot convert {{.Lower}} %v to yaml: %v", {{.Handle}}.Id, err)
    }
    return string(yml)
}

func ({{.Handle}} *{{.Name}}) FromYAML(yml string) {
    err := yaml.Unmarshal([]byte(yml), {{.Handle}})
    if err != nil {
        log.Fatal("cannot convert {{.Lower}} %v from yaml: %v", {{.Handle}}.Id, err)
    }
}

func ({{.Handle}} *{{.Name}}) ToJSON() string {
    js, err := json.Marshal({{.Handle}})
     if err != nil {
        log.Fatal("cannot convert {{.Lower}} %v to json: %v", {{.Handle}}.Id, err)
    }
    return string(js)
}

func ({{.Handle}} *{{.Name}}) FromJSON(js string) {
    decoder := json.NewDecoder(strings.NewReader(js))
    for {
        if err := decoder.Decode({{.Handle}}); err == io.EOF {
            break
        } else if err != nil {
            log.Fatal("cannot convert {{.Lower}} %v from json: %v", {{.Handle}}.Id, err)
        }
    }
}

