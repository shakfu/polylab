package model

import (
	"fmt"
	text "github.com/asaskevich/govalidator"
	"github.com/gedex/inflector"
	"strings"
)

// MetaModel: Model definition objects (used for code generation)
type MetaModel struct {
	Name     string
	Fields   []string
	FieldMap map[string]string
}

type transformer func(string) string

func (m *MetaModel) Handle() string {
	return strings.ToLower(string(m.Name[0]))
}

func (m *MetaModel) Classname() string {
	return text.UnderscoreToCamelCase(m.Name)
}

func (m *MetaModel) TransformFields(function transformer) string {
	var results []string
	for _, f := range m.Fields {
		results = append(results, function(f))
	}
	return strings.Join(results, ", ")
}

// can set handle as first arg of options
func (m *MetaModel) Accessors(options ...string) string {
	var accessors []string
	var handle string
	if len(options) > 0 {
		switch options[0] {
		case "scan":
			handle = "&" + m.Lower()
		default:
			handle = m.Lower()
		}
	} else {
		handle = m.Handle()
	}
	for _, f := range m.Fields {
		accessors = append(accessors, fmt.Sprintf("%s.%s", handle, f))
	}
	return strings.Join(accessors, ", ")
}

func (m *MetaModel) LowerFields() string {
	return m.TransformFields(strings.ToLower)
}

func (m *MetaModel) Representers() string {
	var reps []string
	for _, f := range m.Fields {
		reps = append(reps, fmt.Sprintf("%s: %%v", f))
	}
	return strings.Join(reps, ", ")
}

func (m *MetaModel) ToStringFunc() string {
	representers := m.Representers()
	accessors := m.Accessors()
	return fmt.Sprintf("fmt.Sprintf(\"%s{%s}\", %s)",
		m.Name, representers, accessors)
}

func (m *MetaModel) SqlFields() string {
	return m.TransformFields(text.CamelCaseToUnderscore)
}

func (m *MetaModel) SqlInsert() string {
	var nums []string
	var fields []string
	accessors := strings.Split(m.Accessors(), ",")[1:]

	for i, f := range m.Fields[1:] {
		nums = append(nums, fmt.Sprintf("$%d", i+1))
		fields = append(fields, text.CamelCaseToUnderscore(f))
	}

	return fmt.Sprintf("\"insert into %s (%s) values (%s)\", %s",
		m.Lower(),
		strings.Join(fields, ", "),
		strings.Join(nums, ", "),
		strings.Join(accessors, ", "))
}

func (m *MetaModel) Title() string {
	return strings.ToTitle(m.Name)
}

func (m *MetaModel) Upper() string {
	return strings.ToUpper(m.Name)
}

func (m *MetaModel) Lower() string {
	return strings.ToLower(m.Name)
}

func (m *MetaModel) Plural() string {
	return inflector.Pluralize(m.Name)
}

func (m *MetaModel) LowerPlural() string {
	return inflector.Pluralize(m.Lower())
}
